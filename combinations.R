###
### MegaSena is a Brazilian Lotter where 6 numbers are extracted out of 60.
### 
### This scripts creates a list of all possible combinations of 60 by 6, and adds
### information about combinations that were already extracted in the past.
###
### Source for past extractions: https://loteriadacaixa.net.br/mega-sena/todos-os-resultados-da-mega-sena-na-ordem-do-sorteio
### Last download: 14Jun2024


### Load packages -----
library(readxl)
library(writexl)
library(tibble)
library(dplyr)
library(tidyr)
library(entropy)
library(tictoc)
library(beepr)
library(R.utils)

### Load data -----

# Data of previous MegaSena extractions
dat <- read_xlsx("data/data.xlsx")

# All possible combinations of 60 numbers by 6
if(!file.exists('results/allcomb.rds')){ #if not available, then create
    tic()
    allcomb <- t(combn(60, 6)) %>% as_tibble()
    colnames(allcomb) <- paste0('N', 1:6)
    saveRDS(allcomb, 'results/allcomb.rds')
    toc()
    beep()
} else {
    allcomb <- readRDS('results/allcomb.rds')
}

### Data Manipulation -----

dat <-
    dat %>% 
    #sort numers extracted from lower to higher
    pivot_longer(cols=starts_with('N')) %>% 
    group_by(Concurso, Data) %>% 
    mutate(id=min_rank(value)) %>% 
    ungroup() %>% 
    mutate(name=paste0('N', id)) %>% 
    arrange(name) %>% 
    # go back to wide format
    pivot_wider(id_cols=Concurso:premio, 
                names_from=name, values_from=value)  %>% 
    #create unique id to merge with all combination dataframe
    unite('id', starts_with('N'), sep='', remove=FALSE) %>% 
    #add a flag to display, in the final data, which combinations have been already extracted
    mutate(saiufl='Y')



tic()
allcomb <- 
    allcomb %>%
    as_tibble() %>% 
    # Make extracted numbers character and create unique id for merging
    mutate(across(starts_with('N'), ~sprintf(.x, fmt="%02d"), .names="c{.col}")) %>% 
    unite('id', starts_with('cN'), sep='', remove=TRUE) %>% 
    # merge with dat
    left_join(dat %>% select(-starts_with('N')), by='id')
beep()
toc()    

# calculate entropy of each combination (slow, needs >4Gb of memory)
# tic()
# entropy <- withTimeout({
#     allcomb %>% 
#         group_by(id) %>% 
#         summarise(entropy=entropy(unlist(across(starts_with('N'))))) %>% 
#         ungroup() 
# }, timeout = 20*60, onTimeout = "warning")
# 
# allcomb <- left_join(allcomb, entropy, by='id')
# rm(entropy)
# toc()
# beep()

saveRDS(allcomb, 'results/allcomb_final.rds')
## the dataset has too many rows to be exported in excel, so split it in multiple
## parts and save multiple excel files
allcomb <- 
    allcomb %>% 
    rowid_to_column() %>% 
    mutate(s = as.integer(rowid/(8*10^5))) %>% 
    split(., .$s)


lapply(names(allcomb), function(x){
    write_xlsx(allcomb[[x]] %>% select(-s), paste0('results/mega_tabela_', x, '.xlsx'))
})
beep()

