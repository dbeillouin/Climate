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
        CLIMAT           <- function_year_detrend(TAB=LIN_LOESS_SPLINE %>% group_by(departement, clim_var, method, month)  )
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
         ggplot(CAL)+ geom_point(aes(exp(pred), yield))+ facet_wrap(type~method+model, ncol=4)+ theme_pubr()+ geom_abline(aes(intercept=0, slope=1))
         # il est normal que nous avons plusieurs fois les mêmes graphiques. 
         #ggplot(VAL)+ geom_point(aes(exp(pred), yield))+ facet_wrap(type~method+model, ncol=5)+ theme_pubr()+ theme_pubr()+ geom_abline(aes(intercept=0, slope=1))
         
     #### Etape 6  #######
         # Check data availability
         library(mice); library(VIM) 
         aggr(YIELD_CLIMAT %>% select(-contains("carre")) %>%  filter(!is.na(yield)) %>%  filter(!year_harvest=="1958"), prop = F, numbers = T)
         
         # Cross validation of the models
      CROSS_VALID<- function_Cross_VALID(TAB=YIELD_CLIMAT)
         QUALI <- CROSS_VALID[[1]]; RES <- CROSS_VALID[[2]]
         # Verification -  
         head(QUALI %>% arrange(desc(R2, MSE)), n=10 )
         head(RES)
         ggplot(RES)+ geom_point(aes(exp(pred), yield, color=factor(ROUND)))+ facet_wrap(type~method+model, ncol=6)+ theme_pubr()+
           geom_abline(aes(intercept=0,, slope=1, color= "gray", linetype="2"))
         
  
  
  
#DD <-   EACH %>%  bind_rows()
#  write.csv2(DD, "CLim_mois.csv")

################ ALL month of the growing season are kept to calculate climatic mean values#############################
TAB_ALL_month  <- Climate                                                       %>%  # Create a table for all month    
   summarise(Var_mean_gs = mean(as.numeric(value)))                             %>%  # Summarize data
   group_by(clim_var,departement, sp)                                           %>%  # Group
   nest()                                                                       %>%
   mutate(lin           = furrr::future_map(data, model_lin,  .progress = TRUE),      # linear model
         pred_lin       = furrr::future_map(lin, augment,     .progress = TRUE),
         loess          = furrr::future_map(data, model_loess,.progress = TRUE),      # loess model
         pred_loess     = furrr::future_map(loess, augment,   .progress = TRUE))


lin_ALL   <- TAB_ALL_month  %>%   unnest(pred_lin, .drop = TRUE)                 %>%  # Extract results of the lin model
  select(departement,  sp, clim_var,year_harvest, Var_mean_gs,.fitted,.resid)    %>%  # Choose variable
  mutate(method="lin_ALL")                                                            # Create a key

loess_ALL <- TAB_ALL_month  %>%   unnest(pred_loess, .drop = TRUE)               %>%  # Extract results of the loess model
  select(departement,  sp, clim_var, year_harvest,Var_mean_gs, .fitted,  .resid) %>%  # Choose variable
  mutate(method="loess_ALL")                                                          # Create a key

SELECT2    <- Climate                                                            %>%
  group_by(clim_var,departement, sp)                                             %>% 
  summarise(IQR = IQR(value))                                                    %>%
  mutate(ID =paste(clim_var,departement, sp))                                    %>%
  filter(IQR==0)

TAB_ALL_month  <- Climate                                                  %>%   
  ungroup()                                                                      %>%
  mutate(ID =paste(clim_var,departement, sp))                                    %>%
  filter(!ID %in%  SELECT2$ID)                                                   %>%
  group_by(clim_var,departement, sp)                                             %>%
  nest()                                                                         %>%
  mutate(spline         = furrr::future_map(data, model_spline,.progress = TRUE),     # spline model
         pred_spline    = furrr::future_map(spline, augment,.progress = TRUE))

spline_ALL <- TAB_ALL_month  %>%   unnest(pred_spline, .drop = TRUE)             %>%  # Extract results of the loess model
  rename( year_harvest = x, Var_mean_gs= y)                                      %>% 
  select(departement,  sp, clim_var, year_harvest,Var_mean_gs, .fitted,  .resid) %>%  # Choose variable
  mutate(method="spline_ALL")     

ALL<- do.call("rbind", list(lin_ALL, loess_ALL, spline_ALL))
# write.csv2(ALL, "CLim_ALL.csv")

# Verif
FF<- ALL %>% filter(clim_var== "0<Tx<10")
FF$SUM<-FF$.fitted+ FF$.resid
ggplot(FF)+geom_point(aes(x=Var_mean_gs,SUM))

########################### Lobell method (keep only the last 4 month to calculate climatic variables )#################
Month_to_keep<- seq((max(Climate$month[Climate$month<=9])-3),max(Climate$month[Climate$month<=9]),by=1)

TAB_Lobell   <-  Climate                                                         %>%  # Create a table for Lobell
   filter (month %in% Month_to_keep )                                            %>%  # Filter month
   summarise(Var_mean_gs = mean(as.numeric(value)))                              %>%  # Summarize data
   group_by(clim_var, departement, sp)                                           %>%  # Group
  nest()                                                                         %>%
  mutate(lin            = furrr::future_map(data, model_lin,  .progress = TRUE),      # linear model
         pred_lin       = furrr::future_map(lin, augment,     .progress = TRUE),
         loess          = furrr::future_map(data, model_loess,.progress = TRUE),      # loess model
         pred_loess     = furrr::future_map(loess, augment,   .progress = TRUE))

lin_lob   <- TAB_Lobell  %>%   unnest(pred_lin, .drop = TRUE)                    %>%  # Extract results of the lin model
  select(departement,  sp,clim_var, year_harvest,Var_mean_gs, .fitted,.resid)    %>%  # Choose variable
  mutate(method="lin_Lob")                                                            # Create a key
loess_lob <- TAB_Lobell  %>%   unnest(pred_loess, .drop = TRUE)                  %>%                 # Extract results of the loess model
  select(departement,  sp,clim_var, year_harvest,Var_mean_gs, .fitted, .resid)   %>%  # Choose variable
  mutate(method="loess_Lob")                                                          # Create a key


SELECT2    <- Climate                                                            %>%
  filter (month %in% Month_to_keep )                                             %>%   # Filter month
  group_by(clim_var,departement, sp)                                             %>% 
  summarise(IQR = IQR(value))                                                    %>%
  mutate(ID =paste(clim_var,departement, sp))                                    %>%
  filter(IQR==0)

TAB_Lobell  <- Climate                                                          %>%
  ungroup()                                                                      %>%
  mutate(ID =paste(clim_var,departement, sp))                                    %>%
  filter(!ID %in%  SELECT2$ID)                                                   %>%
  group_by(clim_var,departement, sp)                                             %>%
  nest()                                                                         %>%
  mutate(spline         = furrr::future_map(data, model_spline,.progress = TRUE),     # spline model
         pred_spline    = furrr::future_map(spline, augment,.progress = TRUE))

spline_lob <- TAB_Lobell  %>%   unnest(pred_spline, .drop = TRUE)                 %>%  # Extract results of the loess model
  rename( year_harvest = x, Var_mean_gs= y)                                       %>% 
  select(departement,  sp, clim_var, year_harvest,Var_mean_gs, .fitted,  .resid)  %>%  # Choose variable
  mutate(method="spline_lob")    

LOB<- do.call("rbind", list(lin_lob, loess_lob, spline_lob))
# write.csv2(LOB, "CLim_LOB.csv")


##################################### Different years of detrending ###################################################
rm(list=ls())
EACH <- fread("~/Documents/ClimateGIT/CLim_mois.csv",dec=",")  %>% dplyr::select(-V1)
LOB  <- fread("~/Documents/ClimateGIT/CLim_LOB.csv",dec=",")    %>% dplyr::select(-V1) %>% mutate(month= "ALL")
ALL  <- fread("~/Documents/ClimateGIT/CLim_ALL.csv",dec=",")    %>% dplyr::select(-V1) %>% mutate(month= "ALL")


TAB<-do.call("rbind", list(ALL, LOB, EACH))


function_year_detrend<- function(TAB){
TAB %<>% 
  group_by(departement,sp, clim_var, method)                                     %>%  # Group
  mutate(y58 = case_when(year_harvest <= 1959      ~ Var_mean_gs,
                         year_harvest > 1959      ~ .resid+.fitted[year_harvest==1959]),
         y65 = case_when(year_harvest <= 1965      ~ Var_mean_gs,
                         year_harvest > 1965      ~ .resid+.fitted[year_harvest==1965]),
         y70 = case_when(year_harvest <= 1970      ~ Var_mean_gs,
                         year_harvest > 1970      ~ .resid+.fitted[year_harvest==1970]),
         y75 = case_when(year_harvest <= 1975      ~ Var_mean_gs,
                         year_harvest > 1975      ~ .resid+.fitted[year_harvest==1975]),
         y78 = case_when(year_harvest <= 1978      ~ Var_mean_gs,
                         year_harvest > 1978      ~ .resid+.fitted[year_harvest==1978]),
         y80 = case_when(year_harvest <= 1980      ~ Var_mean_gs,
                         year_harvest > 1980      ~ .resid+.fitted[year_harvest==1980]),
         y82 = case_when(year_harvest <= 1982      ~ Var_mean_gs,
                         year_harvest > 1982      ~ .resid+.fitted[year_harvest==1982]))
return(TAB)
}

#### write file
fwrite(TAB,"Climate_Detrend.csv")



########################### PLOT #######################################################################################
#write.csv(TAB,"TAB09_12.csv")
TAB_PLOT <- TAB %>% 
  mutate(y58 =y58 -Var_mean_gs,
         y65 = y65 -Var_mean_gs,
         y70 = y70 -Var_mean_gs,
         y75 = y75 -Var_mean_gs,
         y78 = y78 -Var_mean_gs,
         y80 = y80 -Var_mean_gs,
         y82 = y82 -Var_mean_gs )
TAB_PLOT <- TAB_PLOT %>% gather(variable, value,-departement,-sp, -clim_var,-year_harvest, -method)
TAB_PLOT <- TAB_PLOT %>% filter(clim_var %in% c("Tmean", "PR","ETP","Tx>34")) %>%
      filter(sp== "wht_tot_gs")
TAB_PLOT <- TAB_PLOT %>% filter(!variable %in% c(".fitted", ".resid"))
TAB_PLOT$value<-as.numeric(as.character(TAB_PLOT$value))
TAB_PLOT <- TAB_PLOT %>% filter(!variable  %in% c("Var_mean_gs"))
TAB_PLOT <- TAB_PLOT %>% group_by(sp,clim_var,year_harvest,variable, method) %>%
  summarise_all(funs(mean))
  
ggplot(TAB_PLOT %>% filter(variable %in% c("y58", "y78")))+ 
  geom_line(aes(x=year_harvest,y=value, color=method)) +facet_wrap(~clim_var+variable, scales="free", ncol=4)+
  theme_pubr(base_size = 20)
  
dev.copy2pdf(file="Diff_climate.pdf",width = 20, height = 10)


TAB_PLOT <- TAB_PLOT %>% filter(variable  %in% c("Var_mean_gs"))
TAB_PLOT <- TAB_PLOT %>% group_by(sp,clim_var,year_harvest,variable, method) %>%
  summarise_all(funs(mean))
ggplot(TAB_PLOT )+ 
  geom_line(aes(x=year_harvest,y=value, color=method)) +
  facet_wrap(~clim_var+variable, scales="free", ncol=4)+
  theme_pubr(base_size = 20)
dev.copy2pdf(file="Raw_climate.pdf",width = 15, height = 8)



TAB2<- TAB %>%   group_by(sp, clim_var, method,year_harvest ) %>%
                 summarise_all(funs(mean, sd))
TAB3<- TAB2 %>% gather(variable, value, -sp, -clim_var, -method, -year_harvest) %>%
     filter(!variable=="departement_mean") %>%
  mutate(type= sub('.*_', '', variable),
         variable= sub('\\_.*', '', variable))   %>%
  spread(key = type, value)     %>%
  filter(!variable %in% c("departement", ".resid") )

TAB4 <- TAB3 %>% filter(variable== "Var") %>% filter(clim_var=="Tmean") %>%
   filter(sp=="wht_tot_gs") 
    group_by()
ggplot(TAB4)+ geom_point(aes(year_harvest,mean ))+facet_wrap(clim_var~method, scales="free")+
  geom_errorbar(aes(x=year_harvest,ymin=mean-sd, ymax=mean+sd), width=.1)



TAB2<- TAB %>%   group_by(sp, clim_var, method,year_harvest ) %>%
  summarise_all(funs(mean))
Climate_Detrend <- TAB2

DAT1<-Climate_Detrend %>% filter(method=="lin_Lob")# %>% filter(clim_var %in% c("PR"))
#DAT1$cyear_class<-cut(DAT1$year, 10)

DAT1<- DAT1 %>% select(-.resid)
DAT1<-DAT1 %>% gather(variable, value, -departement,-sp, -clim_var,-year_harvest,-method)
DAT1<-DAT1%>% filter(sp %in% c("wht_tot_gs","mai_tot_gs"))
DAT1<-DAT1%>% filter(method %in% c("lin_Lob"))

#DAT1<-DAT1 %>% filter(variable %in% c("Var_mean_gs"))
#DAT1<-DAT1 %>% filter(departement=="ARDECHE")
DAT1$value<-as.numeric(as.character(DAT1$value))
DAT1<- DAT1 %>% filter(!variable == ".fitted")

ggplot(data=DAT1 )  +
  geom_boxplot(aes(x=reorder(variable, value), y=value,color=variable))+theme_pubr()+
  scale_color_manual(values = c('orange','blue',"red", "gray70", "gray50", "gray30", "gray20", "gray10", "gray3"))+coord_flip()+
   theme(legend.position="none")+
  facet_wrap(clim_var~sp, scales="free")

PLOTA<-ggplot(data=DAT1 %>% filter(sp== "wht_tot_gs"))  +
  geom_density(aes(x=value, color=variable))+theme_pubr()+
  scale_color_manual(values = c('orange','blue',"red", "gray70", "gray50", "gray30", "gray20", "gray10", "gray3"))+coord_flip()#+
  facet_wrap(~sp, scales="free")
  
  DAT2<-DAT1 %>% filter(sp=="wht_tot_gs")
  PLOT1<-ggplot(data=DAT2 )  +
    geom_line(aes(x= year_harvest, y=value, group=variable, color=variable))+theme_pubr() + #facet_wrap(~sp,scales="free")+
    scale_color_manual(values = c('orange','blue',"red", "gray70", "gray50", "gray30", "gray20", "gray10", "gray3"))
  
plot_grid(PLOT1, PLOTA)

dev.copy2pdf(file="Climate_Wheat_lin_Lob.pdf")
dev.copy2pdf(file="Climate_Wheat_loess_Lob.pdf")
dev.copy2pdf(file="Climate_Wheat_lin_ALL.pdf")
dev.copy2pdf(file="Climate_Wheat_loess_ALL.pdf")



#########


VERIF<- DAT1 %>% filter(method =="y58")
TT<-DAT1 %>% filter(variable== "Var_mean_gs") %>% filter(sp== "wht_tot_gs")
hist(TT$value)

TT2<-Climate %>% filter(clim_var== "Tn") %>%
                 filter(sp== "wht_tot_gs")
TT2 <-  TT2%>%  group_by(sp, clim_var,year_harvest ) %>%
  summarise_all(funs(mean))
hist(TT2$value)
plot(TT2$year_harvest, TT2$value)
ggplot(data=TT2)  +
  geom_density(aes(x=value),bw=0.2)+theme_pubr()
plot(density(TT2$value))


ggplot(data=DAT1 )  +
  geom_boxplot(aes(y=value, x=variable, color=variable))+theme_pubr()+
  scale_color_manual(values = c('blue',"red", "gray70", "gray50", "gray30", "gray20", "gray10", "gray3"))+
  facet_wrap(~sp)

dev.copy2pdf(file="Anomaly_climate_Wheat_maizeboxplot.pdf")

DATF<-DAT1[DAT1$variable=="Var_mean_gs",]
names(DATF)[7]<-"init"
DATF<-DATF %>% ungroup() %>%select(-variable)
DATF<-full_join(DAT1%>% filter(!variable=="Var_mean_gs"),DATF, by=c())
DATF$diff<-DATF$init-DATF$value
#DATF<-DATF %>% filter(!variable=="Var_mean_gs")
DATF <- DATF %>% filter(!diff==0)
ggplot(data=DATF)  +
  geom_density(aes(x=diff, color=variable))+theme_pubr()+
  scale_color_manual(values = c('blue',"red", "gray70", "gray50", "gray30", "gray20", "gray10", "gray3"))+
  facet_wrap(~sp)+
  xlab("difference observed -detrend")
dev.copy2pdf(file="Anomaly_climate_Wheat_maizeDiff.pdf")

facet_wrap(~sp)

dev.copy2pdf(file="Anomaly_climate_Wheat_maize.pdf")




DAT1$ALPHA<-DAT1$variable
DAT1$ALPHA[!DAT1$variable=="Var_mean_gs"]<-"NA"
  
ggplot(DAT1, aes(value,color=variable,fill=ALPHA)) + 
  geom_density(aes(alpha=0.5)) + 
  scale_fill_manual(values = c('NA',"#999999"))
+ 
  scale_alpha_discrete(range=c(0.5,1))
,breaks=c(0,1))

  scale_alpha_continuous( range = c(0, 1)



VERIF <- DAT1 %>% filter(variable ==".resid")
VERIF <- DAT1 %>% filter(variable ==".fitted")



dev.copy2pdf(file="Anomaly_Maize.pdf")



RES %>%
  group_by(sp) %>%
  ggplot(data=.)  +
  geom_density(aes(x=anomaly_spline, color=factor(cyear_class)))+facet_wrap(~sp, scales="free")+theme_pubr()

dev.copy2pdf(file="Anomaly_ALL.pdf")



