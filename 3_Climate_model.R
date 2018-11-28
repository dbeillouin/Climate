# Create Dataframe of climatic variable without climate change
rm(list=ls())

library("broom")
library("purrr")


############# I/ DATALOAD  ##############################################################################
Climate <- read.csv("~/Documents/CLAND/Files_txt/Climate.csv", sep=";")      %>%
        select(-X)%>%
        mutate(value   = as.numeric(gsub(",", ".", gsub("\\.", "", value)))) %>%    # transform variables     
        filter(!value  ==-9999)                           %>%                       # C'est quoi ces valeurs à -9999????????????????
  spread_(key_ = "clim_var",value_ = "value")             %>%                       # Change format data
  mutate(Tmean =(Tn+Tx)/2)      %>%                                                 # Calculate Tmean
  gather(clim_var, value, -year_cal,-departement,-month,-unit,-year_harvest,-sp) %>%  # get back to initial format
  filter(!is.na(value))                                   %>% 
  na.omit(TAB)                                            %>%                       # Pourquio na omit???
  group_by (year_harvest,clim_var,departement,sp)         %>%                       # Group
  mutate(ID =paste(clim_var,departement, sp))

#####
DD <- Climate %>% filter(clim_var %in% c("PR", "Tmean"))
unique(DD$clim_var)

DD <- DD %>% filter(ID %in% SELECT$ID)

###########II/ Select climate data  #############################################################################

SELECT    <- Climate                                  %>%                         
  summarise(Var_mean_gs = mean(as.numeric(value)))    %>%                           # Summarize data
  group_by(clim_var,departement, sp)                  %>% 
  count()                                             %>%
  mutate(ID =paste(clim_var,departement, sp))         %>%
  filter(n>4)

Climate2 <- Climate %>% filter(ID %in% SELECT$ID)
#æClimate <- Climate %>% filter(clim_var %in% c("PR", "Tmean"))

###########II/ Calculation #############################################################################
  ## Define the types of models
  model_lin     <-function(DAT) {lm(Var_mean_gs~year_harvest, data=DAT)}
  model_loess   <-function(DAT) {loess(Var_mean_gs~year_harvest, data=DAT)}


##### ALL month of the growing season are kept to calculate climatic mean values
TAB_ALL_month  <- Climate                              %>%                           # Create a table for all month    
   summarise(Var_mean_gs = mean(as.numeric(value)))    %>%                           # Summarize data
   group_by(clim_var,departement, sp)                  %>%                           # Group
   nest() %>%
   mutate(lin           = purrr::map(data, model_lin),                               # linear model
         pred_lin       = purrr::map(lin, augment),
         loess          = purrr::map(data, model_loess),                             # loess model
         pred_loess     = purrr::map(loess, augment))


lin_ALL   <- TAB_ALL_month  %>%   unnest(pred_lin, .drop = TRUE)   %>%               # Extract results of the lin model
  select(departement,  sp, clim_var,year_harvest, Var_mean_gs,.fitted,.resid)%>%     # Choose variable
  mutate(method="lin_ALL")                                                           # Create a key

loess_ALL <- TAB_ALL_month  %>%   unnest(pred_loess, .drop = TRUE) %>%               # Extract results of the loess model
  select(departement,  sp, clim_var, year_harvest,Var_mean_gs, .fitted,  .resid)%>%  # Choose variable
  mutate(method="loess_ALL")                                                         # Create a key

ALL<- rbind(lin_ALL,loess_ALL)

# Verif
FF<- ALL %>% filter(clim_var== "0<Tx<10")
FF$SUM<-FF$.fitted+ FF$.resid
ggplot(FF)+geom_point(aes(x=Var_mean_gs,SUM))

############ Lobell method (keep only the last 4 month to calculate climatic variables )
Month_to_keep<- seq((max(Climate$month[Climate$month<=9])-3),max(Climate$month[Climate$month<=9]),by=1)

TAB_Lobell   <-  Climate                               %>%                          # Create a table for Lobell
   filter (month %in% Month_to_keep )                  %>%                          # Filter month
   summarise(Var_mean_gs = mean(as.numeric(value)))    %>%                          # Summarize data
   group_by(clim_var, departement, sp)                 %>%                          # Group
  nest()%>%
  mutate(lin            = purrr::map(data, model_lin),                              # linear model
         pred_lin       = purrr::map(lin, augment),
         loess          = purrr::map(data, model_loess),                            # loess model
         pred_loess     = purrr::map(loess, augment))

lin_lob   <- TAB_Lobell  %>%   unnest(pred_lin, .drop = TRUE)   %>%                 # Extract results of the lin model
  select(departement,  sp,clim_var, year_harvest,Var_mean_gs, .fitted,.resid) %>%   # Choose variable
  mutate(method="lin_Lob")                                                          # Create a key
loess_lob <- TAB_Lobell  %>%   unnest(pred_loess, .drop = TRUE) %>%                 # Extract results of the loess model
  select(departement,  sp,clim_var, year_harvest,Var_mean_gs, .fitted, .resid) %>%  # Choose variable
  mutate(method="loess_Lob")                                                        # Create a key

LOB<- rbind(lin_lob,loess_lob)

TAB<-rbind(ALL, LOB)%>%                                                                    # Bind all data
  group_by(departement,sp, clim_var)%>%                                                    # Group
  mutate(y65= ifelse(year_harvest<=1965,Var_mean_gs,.resid+.fitted[year_harvest==1965]),  # Calculate detrended values
         y70= ifelse(year_harvest<=1970,Var_mean_gs,.resid+.fitted[year_harvest==1970]),   # ...
         y75= ifelse(year_harvest<=1975,Var_mean_gs,.resid+.fitted[year_harvest==1975]),   # ...
         y78= ifelse(year_harvest<=1978,Var_mean_gs,.resid+.fitted[year_harvest==1978]),   # ...
         y80= ifelse(year_harvest<=1980,Var_mean_gs,.resid+.fitted[year_harvest==1980]),   # ...
         y82= ifelse(year_harvest<=1982,Var_mean_gs,.resid+.fitted[year_harvest==1982]))   # ...

#### write file
write.csv2(TAB,"Climate_Detrend.csv")
