
rm(list=ls())
setwd("~/Documents/CLAND/files_txt")

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
  select(-X) %>%                                                  # supress useless column
  mutate(departement=toupper(gsub("[']", '-', departement)))      # change format departement

  X <- split(Climate_Detrend, Climate_Detrend$clim_var)           # create a list of dataframe
 for (i in 1: length(X)){
   NAMES<- c(names(X[[i]][c(1:4)]),                               # Change the column's name of each dataframe
             paste(unique(X[[i]]$clim_var), names(X[[i]][5:7]),sep="_"),
             names(X[[i]][c(8)]),
             paste(unique(X[[i]]$clim_var), names(X[[i]][9:15]),sep="_"))
   names(X[[i]])<- NAMES
   X[[i]]<- X[[i]] %>% select(-clim_var)
 }

  Climate_Detrend <- multi_join(X, full_join)

  Climate_Detrend <- Climate_Detrend %>% 
         filter(!departement %in% c("PARIS", "TERRITOIRE"))      # Supress useless departement
  
  Climate_Detrend<-Climate_Detrend %>%                           # Recode name species
    mutate(sp = fct_recode(sp,
                           "barley_spring"=  "bar_spr_gs",          `barley_total` = "bar_tot_gs",
                           `barley_winter` = "bar_win_gs",           `maize_total` = "mai_tot_gs",
                           `oats_spring` = "oat_spr_gs",             `oats_total` = "oat_tot_gs",
                           `oats_winter` = "oat_win_gs",             `potatoes_total` = "pot_tot_gs",
                           `rape_spring` = "rape_spring",             `rape_total` = "rap_tot_gs",
                           `rape_winter` = "rap_win_gs",             `sugarbeet_total` = "sug_tot_gs",
                           `sunflower_total` = "sun_tot_gs",         `wheat_durum_total` = "x",
                           `wheat_spring` = "wht_spr_gs",            `wheat_total` = "wht_tot_gs",
                           `wheat_winter` = "wht_win_gs",            `wine_total` = "wne_tot_gs"))
  

  # Load Yield data
  
 Yield_anomalies <- read.csv2("~/Documents/CLAND/Files_txt/Yield_anomalies.csv")  %>% select(-X) %>%
                    rename(year_harvest = year,
                           departement  = department) %>% 
                     mutate(departement=toupper(gsub('[_]', '-', departement)))
 
  
 ############# II/ Merge  ##############################################################################
 
 Yield_Climate<- full_join(Climate_Detrend,Yield_anomalies, by= c("departement","year_harvest", "sp"))
 
 write.csv2(Yield_Climate,"Yield_Climate.csv")

# setdiff(Climate_Detrend$departement,Yield_anomalies$departement)
 
 