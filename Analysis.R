# Create Dataframe of climatic variable without climate change
rm(list=ls())

x<- c("broom","purrr","tidyverse","data.table","furrr", "magrittr", "ggpubr")
lapply(x, require, character.only = TRUE)
plan(multiprocess, workers = 4)
source('~/Documents/ClimateGIT/function_lin_and_loess.R', echo=TRUE)
source('~/Documents/ClimateGIT/function_year_detrend.R', echo=TRUE)
source('~/Documents/ClimateGIT/function_spread.R', echo=TRUE)
source('~/Documents/ClimateGIT/fonction_SQUARE_and_NORM.R', echo=TRUE)
source('~/Documents/ClimateGIT/fonction_merge_Yield_climat.R', echo=TRUE)
source('~/Documents/ClimateGIT/fonction_CAL_model.R', echo=TRUE)
source('~/Documents/ClimateGIT/fonction_Cross_VALID.R', echo=TRUE)
source('~/Documents/ClimateGIT/fonction_Cross_VALID_ALL_DEP.R', echo=TRUE)


############# I/ DATALOAD  ##############################################################################
Climate <- fread("~/Documents/CLAND/Files_txt/Climate.csv", sep=";")             %>% 
  dplyr::select(-V1)%>%
  mutate(value   = as.numeric(gsub(",", ".", gsub("\\.", "", value))))     %>%    # transform variables     
  filter(!value  ==-9999)                                                  %>%    # C'est quoi ces valeurs à -9999????????????????
  spread(key= "clim_var",value = "value")                                        %>%                              # Change format data
  mutate(Tmean =(Tn+Tx)/2)      %>%                                                     # Calculate Tmean
  gather(clim_var, value, -year_cal,-departement,-month,-unit,-year_harvest,-sp) %>%    # get back to initial format
  filter(!is.na(value))                                                          %>% 
  na.omit(TAB)                                                                   %>%    # Pourquoi na omit???
  group_by (year_harvest,clim_var,departement,sp)                                %>%    # Group
  mutate(ID =paste(clim_var,departement))


###########I/ INITIALISATION #############################################################################


list_sp <- unique(Climate$sp)
SP <- list_sp[1]
Climate      <- subset(Climate, sp == SP) 


######### TAB each month ############################################################################################## 
TAB_EACH_month  <- Climate                                                    %>%   # Create a table for EACH month    
  group_by(clim_var,departement, month)                                       %>% 
  dplyr::rename(Var_mean_gs=value)

#### Etape 1 #######
      ### Calculation of detrended climate : linear, loess and spline
  LIN_LOESS_SPLINE <-function_lin_AND_loess(TAB=TAB_EACH_month)
      # Verification -
          head(LIN_LOESS_SPLINE)
          VERIF <- subset(LIN_LOESS_SPLINE, clim_var =="0<Tx<10")
          ggplot(VERIF)+ geom_point(aes(Var_mean_gs,.fitted)) +facet_wrap(~method) + theme_pubr()

#### Etape 2 #######
          ## Define different year for the beginning of climate change
  CLIMAT         <- function_year_detrend(TAB=LIN_LOESS_SPLINE %>% group_by(departement, clim_var, method, month)  )
            # Verification -
            VERIF <- subset(CLIMAT, clim_var =="Tmean")
            VERIF <- subset(VERIF, departement =="AIN")
            VERIF <- subset(VERIF, month =="8")
            VERIF %<>% select(-.fitted, -.resid)
            VERIF <- gather(VERIF, variable, value, -departement, -clim_var, -method,-year_harvest, -month, -method)
            ggplot(VERIF)+ geom_line(aes(year_harvest,value, group=variable, color=variable)) +facet_wrap(~method) + theme_pubr()

#### Etape 3 #######
            # re-organize the data and calculate the square of each variable, and then normalized the data with observed values
            #Check data availability
            reshape::cast(CLIMAT, year_harvest ~ clim_var, length, value = '.fitted') 
            # select variable with few missing data: 
            
   CLIMAT1            <- function_spread(TAB=CLIMAT %>% filter(clim_var %in% c("ETP", "Tmean","PR","RV","Seq PR","Tn","Tx")))
                DD<-CLIMAT1 %>% filter(is.na(CLIMAT1$'0<Tx<10_.fitted_1'))
                DD <- DD %>% filter(year_harvest=="2001") 
            
    CLIMAT2           <- function_SQUARE_AND_NORM(TAB=CLIMAT1)
                glimpse(CLIMAT)
                DD<-CLIMAT2 %>% filter(is.na(CLIMAT2$X0.Tx.10_1))
                DD<-DD %>% filter(!year_harvest== '1958')

#### Etape 4 #######
              # Associate climate and yield data
  YIELD_CLIMAT     <- function_merge_Yield_Climate(TAB=CLIMAT2, method="EACH")
              #rm(list=setdiff(ls(), c("YIELD_CLIMAT")))

#### Etape 5 #######
              # Calibrate the models
  RES<- function_CAL_model(TAB=YIELD_CLIMAT)
              CAL <- RES[[1]];# VAL <- RES[[2]]
              # Verification -
              head(CAL)
              ggplot(CAL)+ geom_point(aes(exp(pred), yield))+ facet_wrap(type~method+model, ncol=6)+ theme_pubr()+ geom_abline(aes(intercept=0, slope=1))

#### Etape 6  #######  Cross validation by departements
              # Check data availability
              library(mice); library(VIM) 
              aggr(YIELD_CLIMAT %>% select(-contains("carre")) %>%  filter(!is.na(yield)) %>%  filter(!year_harvest=="1958"), prop = F, numbers = T)
              
              # Cross validation of the models
  CROSS_VALID<- function_Cross_VALID(TAB=YIELD_CLIMAT)
              QUALI <- CROSS_VALID[[1]]; RES <- CROSS_VALID[[2]]
              # Verification -  
              head(QUALI %>% arrange(desc(R2, MSE)), n=10 )
              head(RES)
              ggplot(RES) + geom_point(aes(exp(pred), yield, color=factor(ROUND)))+ facet_wrap(type~method+model, ncol=5, scales="free")+ theme_pubr()+
                geom_abline(aes(intercept=0,, slope=1, color= "gray", linetype="2"))

#### Etape 7  #######  Cross validation over all departements
              # Check data availability
              # Cross validation of the models
  CROSS_VALID<- function_Cross_VALID_ALL_DEP(TAB=YIELD_CLIMAT)
               QUALI <- CROSS_VALID[[1]]; RES <- CROSS_VALID[[2]]
              # Verification -  
              head(QUALI %>% arrange(desc(R2, MSE)), n=10 )
              head(RES)
              ggplot(RES) + geom_point(aes(exp(pred), yield, color=factor(ROUND)))+ facet_wrap(type~method+model, ncol=5, scales="free")+ theme_pubr()+
                geom_abline(aes(intercept=0,, slope=1, color= "gray", linetype="2"))
              
              

#########################################################################################################################
              
              
              
              ######### TAB ALL month  together  type LOBELL ############################################################################################## 
             
              
              Month_to_keep<- seq((max(Climate$month[Climate$month<=9])-3),max(Climate$month[Climate$month<=9]),by=1)
              
              TAB_Lobell   <-  Climate                                                         %>%  # Create a table for Lobell
                filter (month %in% Month_to_keep )                                             %>% 
                group_by(clim_var,departement,year_harvest)                                    %>% 
                summarise(Var_mean_gs = mean(as.numeric(value)))                               %>% 
                group_by(clim_var,departement)                                             
                
              
              #### Etape 1 #######
              ### Calculation of detrended climate : linear, loess and spline
              LIN_LOESS_SPLINE <-function_lin_AND_loess(TAB=TAB_Lobell)
              # Verification -
              head(LIN_LOESS_SPLINE)
              VERIF <- subset(LIN_LOESS_SPLINE, clim_var =="Tmean") 
              VERIF <- subset(VERIF, departement =="AIN") 
              ggplot(VERIF)+ geom_point(aes(Var_mean_gs,.fitted)) +facet_wrap(~method) + theme_pubr()
              
              #### Etape 2 #######
              ## Define different year for the beginning of climate change
              CLIMAT         <- function_year_detrend(TAB=LIN_LOESS_SPLINE %>% group_by(departement, clim_var, method)  )
              # Verification -
              VERIF <- subset(CLIMAT, clim_var =="Tmean")
              VERIF <- subset(VERIF, departement =="AIN")
              VERIF %<>% select(-.fitted, -.resid)
              VERIF <- gather(VERIF, variable, value, -departement, -clim_var, -method,-year_harvest, -method)
              ggplot(VERIF)+ geom_line(aes(year_harvest,value, group=variable, color=variable)) +facet_wrap(~method) + theme_pubr()
              
              #### Etape 3 #######
              # re-organize the data and calculate the square of each variable, and then normalized the data with observed values
              #Check data availability
              reshape::cast(CLIMAT, year_harvest ~ clim_var, length, value = '.fitted') 
              # select variable with few missing data: 
              
              CLIMAT1            <- function_spread(TAB=CLIMAT %>% select(-.se.fit) %>% filter(clim_var %in% c("ETP", "Tmean","PR","RV","Seq PR","Tn","Tx")))
              DD<-CLIMAT1 %>% filter(is.na(CLIMAT1$'0<Tx<10_.fitted_1'))
              DD <- DD %>% filter(year_harvest=="2001") 
              
              CLIMAT2           <- function_SQUARE_AND_NORM(TAB=CLIMAT1)
              glimpse(CLIMAT)
              DD<-CLIMAT2 %>% filter(is.na(CLIMAT2$X0.Tx.10_1))
              DD<-DD %>% filter(!year_harvest== '1958')
              
              #### Etape 4 #######
              # Associate climate and yield data
              YIELD_CLIMAT     <- function_merge_Yield_Climate(TAB=CLIMAT2, method="EACH")
              #rm(list=setdiff(ls(), c("YIELD_CLIMAT")))
              
              #### Etape 5 #######
              # Calibrate the models
              RES<- function_CAL_model_ALL(TAB=YIELD_CLIMAT)
              CAL <- RES[[1]];# VAL <- RES[[2]]
              # Verification -
              head(CAL)
              ggplot(CAL)+ geom_point(aes(exp(pred), yield))+ facet_wrap(type~method+model, ncol=6)+ theme_pubr()+ geom_abline(aes(intercept=0, slope=1))

              #### Etape 6  #######  Cross validation by departements
              # Check data availability
              library(mice); library(VIM) 
              aggr(YIELD_CLIMAT %>% select(-contains("carre")) %>%  filter(!is.na(yield)) %>%  filter(!year_harvest=="1958"), prop = F, numbers = T)
              
              # Cross validation of the models
              CROSS_VALID<- function_Cross_VALID_ALL(TAB=YIELD_CLIMAT)
              QUALI <- CROSS_VALID[[1]]; RES <- CROSS_VALID[[2]]
              # Verification -  
              head(QUALI %>% arrange(desc(R2, MSE)), n=10 )
              head(RES)
              ggplot(RES) + geom_point(aes(exp(pred), yield, color=factor(ROUND)))+ facet_wrap(type~method+model, ncol=5, scales="free")+ theme_pubr()+
                geom_abline(aes(intercept=0,, slope=1, color= "gray", linetype="2"))
              
              #### Etape 7  #######  Cross validation over all departements
              # Check data availability
              # Cross validation of the models
              CROSS_VALID<- function_Cross_VALID_ALL_DEP_ALL_MONTH(TAB=YIELD_CLIMAT)
              QUALI <- CROSS_VALID[[1]]; RES <- CROSS_VALID[[2]]
              # Verification -  
              head(QUALI %>% arrange(desc(R2, MSE)), n=10 )
              head(RES)
              ggplot(RES) + geom_point(aes(exp(pred), yield, color=factor(ROUND)))+ facet_wrap(type~method+model, ncol=5, scales="free")+ theme_pubr()+
                geom_abline(aes(intercept=0,, slope=1, color= "gray", linetype="2"))
              

              #########################################################################################################################
              
              ######### TAB each month ############################################################################################## 
              TAB_ALL_month  <- Climate                                                    %>%   # Create a table for EACH month    
                group_by(clim_var,departement,year_harvest)                                %>% 
                summarise(Var_mean_gs = mean(as.numeric(value)))                           %>% 
                group_by(clim_var,departement)                                             
              
              
              #### Etape 1 #######
              ### Calculation of detrended climate : linear, loess and spline
              LIN_LOESS_SPLINE <-function_lin_AND_loess(TAB=TAB_ALL_month)
              # Verification -
              head(LIN_LOESS_SPLINE)
              VERIF <- subset(LIN_LOESS_SPLINE, clim_var =="Tmean") 
              VERIF <- subset(VERIF, departement =="AIN") 
              ggplot(VERIF)+ geom_point(aes(Var_mean_gs,.fitted)) +facet_wrap(~method) + theme_pubr()
              
              #### Etape 2 #######
              ## Define different year for the beginning of climate change
              CLIMAT         <- function_year_detrend(TAB=LIN_LOESS_SPLINE %>% group_by(departement, clim_var, method)  )
              # Verification -
              VERIF <- subset(CLIMAT, clim_var =="Tmean")
              VERIF <- subset(VERIF, departement =="AIN")
              VERIF %<>% select(-.fitted, -.resid)
              VERIF <- gather(VERIF, variable, value, -departement, -clim_var, -method,-year_harvest, -method)
              ggplot(VERIF)+ geom_line(aes(year_harvest,value, group=variable, color=variable)) +facet_wrap(~method) + theme_pubr()
              
              #### Etape 3 #######
              # re-organize the data and calculate the square of each variable, and then normalized the data with observed values
              #Check data availability
              reshape::cast(CLIMAT, year_harvest ~ clim_var, length, value = '.fitted') 
              # select variable with few missing data: 
              
              CLIMAT1            <- function_spread(TAB=CLIMAT %>% select(-.se.fit) %>% filter(clim_var %in% c("ETP", "Tmean","PR","RV","Seq PR","Tn","Tx")))
              DD<-CLIMAT1 %>% filter(is.na(CLIMAT1$'0<Tx<10_.fitted_1'))
              DD <- DD %>% filter(year_harvest=="2001") 
              
              CLIMAT2           <- function_SQUARE_AND_NORM(TAB=CLIMAT1)
              glimpse(CLIMAT)
              DD<-CLIMAT2 %>% filter(is.na(CLIMAT2$X0.Tx.10_1))
              DD<-DD %>% filter(!year_harvest== '1958')
              
              #### Etape 4 #######
              # Associate climate and yield data
              YIELD_CLIMAT     <- function_merge_Yield_Climate(TAB=CLIMAT2, method="EACH")
              #rm(list=setdiff(ls(), c("YIELD_CLIMAT")))
              
              #### Etape 5 #######
              # Calibrate the models
              RES<- function_CAL_model_ALL(TAB=YIELD_CLIMAT)
              CAL <- RES[[1]];# VAL <- RES[[2]]
              # Verification -
              head(CAL)
              ggplot(CAL)+ geom_point(aes(exp(pred), yield))+ facet_wrap(type~method+model, ncol=6)+ theme_pubr()+ geom_abline(aes(intercept=0, slope=1))
              
              #### Etape 6  #######  Cross validation by departements
              # Check data availability
              library(mice); library(VIM) 
              aggr(YIELD_CLIMAT %>% select(-contains("carre")) %>%  filter(!is.na(yield)) %>%  filter(!year_harvest=="1958"), prop = F, numbers = T)
              
              # Cross validation of the models
              CROSS_VALID<- function_Cross_VALID_ALL(TAB=YIELD_CLIMAT)
              QUALI <- CROSS_VALID[[1]]; RES <- CROSS_VALID[[2]]
              # Verification -  
              head(QUALI %>% arrange(desc(R2, MSE)), n=10 )
              head(RES)
              ggplot(RES) + geom_point(aes(exp(pred), yield, color=factor(ROUND)))+ facet_wrap(type~method+model, ncol=5, scales="free")+ theme_pubr()+
                geom_abline(aes(intercept=0,, slope=1, color= "gray", linetype="2"))
              
              #### Etape 7  #######  Cross validation over all departements
              # Check data availability
              # Cross validation of the models
              CROSS_VALID<- function_Cross_VALID_ALL_DEP_ALL_MONTH(TAB=YIELD_CLIMAT)
              QUALI <- CROSS_VALID[[1]]; RES <- CROSS_VALID[[2]]
              # Verification -  
              head(QUALI %>% arrange(desc(R2, MSE)), n=10 )
              head(RES)
              ggplot(RES) + geom_point(aes(exp(pred), yield, color=factor(ROUND)))+ facet_wrap(type~method+model, ncol=5, scales="free")+ theme_pubr()+
                geom_abline(aes(intercept=0,, slope=1, color= "gray", linetype="2"))
              
              
              
              
              