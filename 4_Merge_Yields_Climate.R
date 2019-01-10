
rm(list=ls())
setwd("~/Documents/CLAND/files_txt")

# load package
x<- c("broom","purrr","tidyverse","data.table","furrr", "magrittr")
lapply(x, require, character.only = TRUE)

multi_join <- function(list_of_loaded_data, join_func, ...){
  require("dplyr")
  output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
  return(output)
}

############# I/ DATALOAD  ##############################################################################
 # Load and Change format of climate data
 Climate_Detrend <-  fread("~/Documents/CLAND/Files_txt/Climate_Detrend.csv") %>% 
  mutate(departement=toupper(gsub("[']", '-', departement)))  # change format departement


List_SP <- unique(Climate_Detrend$sp)                         # define a list of all species

for (n in 1: length(List_SP)) {                               # loop on species (file too big otherwise)
  
SP      <- subset(Climate_Detrend, sp == List_SP[n])          # Define e sub-data_frame

FIRST<- SP %>%  filter(month== "ALL")                         # Subset climatic data calculated over sevral month (all month or lobell method)

  X <- split(FIRST, paste(FIRST$clim_var))                    # create a list of dataframe by climatic variable 
 for (i in 1: length(X)){                                     # The folowing code use data.table (faster than dplyr)

   A<- data.table::melt(data.table(X[[i]]),                   # Change format of the data
       id.vars = c("departement","sp", "clim_var", "year_harvest", "method", "month"),
       measure.vars = c("Var_mean_gs",".fitted", ".resid","y58", "y65", "y70" , "y75" , "y78" ,"y80" ,"y82"))
   A<-A[, variable :=  paste0(variable, "_",month)]
   A<-A[, !"month"]
   A<-dcast(A, departement+sp+clim_var+year_harvest+method  ~ variable, value.var = "value")
   print(paste(A$sp[1], A$clim_var[1]))
    NAMES<- c(names(A)[1:5],                                # Change the column's name of each dataframe
             paste(unique(A$clim_var), names(A)[6:length(names(A))],sep="_"))

   names(A)<- NAMES
   X[[i]] <- A[, !"clim_var"] 
 }
FIRST<-multi_join(X,full_join)
  

CLIMAT <- FIRST %>% 
  filter(!departement %in% c("PARIS", "TERRITOIRE"))        %>%       # Recode name species
  mutate(sp = fct_recode(sp,
                         "barley_spring"=  "bar_spr_gs",          `barley_total` = "bar_tot_gs",
                         `barley_winter` = "bar_win_gs",           `maize_total` = "mai_tot_gs",
                         `oats_spring` = "oat_spr_gs",             `oats_total` = "oat_tot_gs",
                         `oats_winter` = "oat_win_gs",             `potatoes_total` = "pot_tot_gs",
                         `rape_total` = "rap_tot_gs",
                         `rape_winter` = "rap_win_gs",             `sugarbeet_total` = "sug_tot_gs",
                         `sunflower_total` = "sun_tot_gs",         `wheat_durum_total` = "x",
                         `wheat_spring` = "wht_spr_gs",            `wheat_total` = "wht_tot_gs",
                         `wheat_winter` = "wht_win_gs",            `wine_total` = "wne_tot_gs"))

# Load Yield data
Yield_anomalies <- fread("~/Documents/CLAND/Files_txt/Yield_anomalies.csv") %>% 
  select(-V1)                                               %>%
  rename(year_harvest = year,
         departement  = department)                         %>% 
  mutate(departement=toupper(gsub('[_]', '-', departement)))
SP<- unique(CLIMAT$sp)
YIELD           <- subset(Yield_anomalies, sp == SP)

############# II/ Merge  ##############################################################################

Yield_Climate<- full_join(CLIMAT,YIELD, by= c("departement","year_harvest", "sp"))

fwrite(Yield_Climate,paste("Yield_Climate", "_LOB_",List_SP[n],".csv", sep=""))
}



  
##### For each month. 

for (n in 1: length(List_SP)) {                               # loop on species (file too big otherwise)
  SP      <- subset(Climate_Detrend, sp == List_SP[n])          # Define e sub-data_frame
  
  SECOND<- SP %>%  filter(!month== "ALL")                    # Subset climatic data calculated for each month
  
  X <- split(SECOND, paste(SECOND$clim_var))               # create a list of dataframe
  for (i in 1: length(X)){
    
    Y <- split(X[[i]], X[[i]]$month)                       # create a list of dataframe by climatic variable 
    
    for (j in 1: length(Y)){
      A<- data.table::melt(data.table(Y[[j]]),             # Same as described before
                           id.vars = c("departement","sp", "clim_var", "year_harvest", "method", "month"),
                           measure.vars = c("Var_mean_gs",".fitted", ".resid","y58", "y65", "y70" , "y75" , "y78" ,"y80" ,"y82")) 
      
      A<-A[, variable :=  paste0(variable, "_",month)]
      A<-A[, !"month"]
      A<-dcast(A, departement+sp+clim_var+year_harvest+method  ~ variable, value.var = "value")
      
      NAMES<- c(names(A)[1:5],                          # Change the column's name of each dataframe
                paste(unique(A$clim_var), names(A)[6:length(names(A))],sep="_"))
      names(A)<- NAMES
      Y[[j]]<- A[, !"clim_var"]
    } 
    X[[i]] <- multi_join(Y, full_join)
    print(paste(A$sp[1], A$clim_var[1]))
  }
  SECOND<-multi_join(X,full_join)

  CLIMAT <- SECOND %>% 
    filter(!departement %in% c("PARIS", "TERRITOIRE"))        %>%       # Recode name species
    mutate(sp = fct_recode(sp,
                           "barley_spring"=  "bar_spr_gs",          `barley_total` = "bar_tot_gs",
                           `barley_winter` = "bar_win_gs",           `maize_total` = "mai_tot_gs",
                           `oats_spring` = "oat_spr_gs",             `oats_total` = "oat_tot_gs",
                           `oats_winter` = "oat_win_gs",             `potatoes_total` = "pot_tot_gs",
                           `rape_total` = "rap_tot_gs",
                           `rape_winter` = "rap_win_gs",             `sugarbeet_total` = "sug_tot_gs",
                           `sunflower_total` = "sun_tot_gs",         `wheat_durum_total` = "x",
                           `wheat_spring` = "wht_spr_gs",            `wheat_total` = "wht_tot_gs",
                           `wheat_winter` = "wht_win_gs",            `wine_total` = "wne_tot_gs"))
  
  # Load Yield data
  Yield_anomalies <- fread("~/Documents/CLAND/Files_txt/Yield_anomalies.csv") %>% 
    select(-V1)                                               %>%
    rename(year_harvest = year,
           departement  = department)                         %>% 
    mutate(departement=toupper(gsub('[_]', '-', departement)))
  SP<- unique(CLIMAT$sp)
  YIELD           <- subset(Yield_anomalies, sp == SP)
  
  ############# II/ Merge  ##############################################################################
  
  Yield_Climate<- full_join(CLIMAT,YIELD, by= c("departement","year_harvest", "sp"))
  
  fwrite(Yield_Climate,paste("Yield_Climate", "_EACH_",List_SP[n],".csv", sep=""))
}
