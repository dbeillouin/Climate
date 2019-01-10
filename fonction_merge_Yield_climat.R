function_merge_Yield_Climate<- function(TAB,method){
  
# Load Yield data
Yield_anomalies <- fread("~/Documents/CLAND/Files_txt/Yield_anomalies.csv",dec=',') %>% 
  select(-V1)                                               %>%
  dplyr::rename(year_harvest = year,
         departement  = department)                         %>% 
  mutate(departement=toupper(gsub('[_]', '-', departement)))


SP2<-unique(Yield_anomalies$sp)
SP2 =grep(substr(SP,1,3), SP2, value = TRUE)
SP2 =grep(substr(SP,5,7), SP2, value = TRUE)

YIELD           <- subset(Yield_anomalies, sp == SP2)


############# II/ Merge  ##############################################################################

Yield_Climate<- full_join(TAB,YIELD, by= c("departement","year_harvest"))
#Yield_Climate<- full_join(Yield_Climate,Yield, by= c("departement","year_harvest"))


fwrite(Yield_Climate,paste("Yield_Climate", method,SP,".csv", sep=""))

return(Yield_Climate)
}