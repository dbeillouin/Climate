########### May 2018 - Tamara Ben Ari
######## This code merges all climate obs and similarly with no CC together with yield and anomaly data into one single file

rm(list=ls())

############# I/ DATALOAD  ##############################################################################
 Climate_Detrend <-  read.csv2("~/Documents/CLAND/Files_txt/Climate_Detrend.csv")
 Yield_anomalies <- read.csv2("~/Documents/CLAND/Files_txt/Yield_anomalies.csv") %>%
                    rename(new_name = old_name) 
  
 
# I/ load data 
path              <- "/Users/Damien_Beillouin/Documents/Stage_JULIA/codes_julia"
Resultfolder      <- "/Users/Damien_Beillouin/Documents/Stage_JULIA/codes_julia/Results"

All_crops <- read.table(paste0(path,"/Files_txt/0_all_crops_names_clean.txt"),header=TRUE , sep=";", dec=".", na.strings="NA")
crop_name <- All_crops$crop_type

#II/ create a function for merging
 FUN_MERGE<-function(Crop, method){

# Find and load climate data
   CLIM  <- dir(Resultfolder, pattern = "LOBELL",full.names=FALSE) 
   CLIM  <- Filter(function(x) grepl(Crop, x), CLIM)

  ifelse(method=="ALL_month",CLIM<-CLIM[2],CLIM<-CLIM[1])     # choose climatic file corresponding to method ( ALL_month or Lobell) 

    CLIMAT     <-read.csv(paste0(Resultfolder,"/",CLIM), header=TRUE, sep=" ", dec=",", na.strings="NA")%>%
    mutate(departement = tolower(departement))%>%
    mutate(departement = gsub('\\-', '_',departement))

# Find, load yield data and merge with climate data

    YIELDS  <- dir(Resultfolder, pattern = "TABLE_Yield_Pred",full.names=FALSE) 
    YIELDS  <- Filter(function(x) grepl(Crop, x), YIELDS)
    YIELDS  <-read.csv(paste0(Resultfolder,"/",YIELDS[1]), header=TRUE, sep=" ", dec=",", na.strings="NA") %>%
         mutate(departement = tolower(departement))


TABLE  <-full_join(YIELDS,CLIMAT,by=c('year_harvest','departement'))


# Save the final file
print(paste("yield and climate file of: ",Crop," saved in",Resultfolder,sep=""))
write.csv2(TABLE,paste(Resultfolder,"/",'TABLE_MERGED_YIELD_CLIMATE_',ifelse(grepl("4month", CLIM), "_4month_", ""),Crop,".csv",sep=''),row.names = F)
}

#III/ merge  
 crop_name %>% map(function(Crop)  FUN_MERGE(Crop,method="ALL_month"))
 crop_name %>% map(function(Crop)  FUN_MERGE(Crop,method="4_month"))
 