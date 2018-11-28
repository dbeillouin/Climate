
rm(list=ls())

# load package
library("tidyverse")
multi_join <- function(list_of_loaded_data, join_func, ...){
  require("dplyr")
  output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
  return(output)
}

############# I/ DATALOAD  ##############################################################################
 # Load and Change format of climate data
 
 Climate_Detrend <-  read.csv2("~/Documents/CLAND/Files_txt/Climate_Detrend.csv") %>% 
  select(-X) %>%
  mutate(departement=toupper(gsub("[']", '-', departement)))

  X <- split(Climate_Detrend, Climate_Detrend$clim_var)
 for (i in 1: length(X)){
   NAMES<- c(names(X[[i]][c(1:4)]),
             paste(unique(X[[i]]$clim_var), names(X[[i]][5:7]),sep="_"),
             names(X[[i]][8]),
             paste(unique(X[[i]]$clim_var), names(X[[i]][9:14]),sep="_"))
   names(X[[i]])<- NAMES
   X[[i]]<- X[[i]] %>% select(-clim_var)
 }

  Climate_Detrend <- multi_join(X, full_join) 

  # Load Yield data
  
 Yield_anomalies <- read.csv2("~/Documents/CLAND/Files_txt/Yield_anomalies.csv")  %>% select(-X) %>%
                    rename(year_harvest = year,
                           departement  = department) %>% 
                     mutate(departement=toupper(gsub('[_]', '-', departement)))
 
                           
 gsub('[.]', '-', x)
  
 ############# II/ Merge  ##############################################################################
 
 Climate_Detrend <- Climate_Detrend %>% filter(!departement %in% c("PARIS", "TERRITOIRE"))
 Yield_Climate<- full_join(Climate_Detrend,Yield_anomalies, by= c("departement","year_harvest", "sp"))
 write.csv2(Yield_Climate,"Yield_Climate.csv")

# setdiff(Climate_Detrend$departement,Yield_anomalies$departement)
 
 