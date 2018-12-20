# load packages
library("tidyverse")
library("broom")

#### I/ DATALOAD  ##############################################################################
setwd("~/Documents/CLAND/files_txt")
temp = list.files(pattern="*16.txt")
Yield = lapply(temp, function(x) read.table(x,header=TRUE , sep=";", dec=".", na.strings="NA") %>%
                   mutate(sp=rep(sub('_data_1900-2016.txt', '', x))))%>% 
  bind_rows()%>%
  mutate(department  = tolower(department),
         ID          = paste0(department, sp)) %>%
  filter(complete.cases(yield))     
#####

## Define the types of models
model_lin     <-function(DAT) {lm(yield~year, data=DAT)}
model_quad    <-function(DAT) {lm(yield~year+I(year^2), data=DAT)}
model_cub     <-function(DAT) {lm(yield~year+I(year^2)+I(year^3), data=DAT)}
model_loess   <-function(DAT) {loess(yield~year, data=DAT)}
model_spline  <-function(DAT) {smooth.spline(DAT$year,DAT$yield)}

# identify number of yield data per Departemnt X sp
FILTRE <- Yield  %>%     filter(complete.cases(yield)) %>% group_by(department,sp) %>% count() %>%
  mutate(ID =paste0(department, sp)) 

## linear models
tab_lin <- Yield  %>% 
           filter(ID %in% FILTRE[FILTRE$n>4,]$ID)%>%
           group_by(department,sp) %>%
           nest()%>%
           mutate(lin           = purrr::map(data, model_lin),             # linear model
                  AIC_lin       = purrr::map(lin, stats::AIC),
                  pred_lin      = purrr::map(lin, augment),
                  quad          = purrr::map(data, model_quad),            # quadratic model
                  AIC_quad      = purrr::map(quad, stats::AIC),
                  pred_quad     = purrr::map(quad, augment),
                  cub           = purrr::map(data, model_cub),             # cub model
                  AIC_cub       = purrr::map(cub, stats::AIC),
                  pred_cub      = purrr::map(cub, augment))

# Table of AIC (for the linear models)
AIC_lin=tab_lin  %>%   unnest(AIC_lin, .drop = TRUE)
AIC_cub=tab_lin  %>%   unnest(AIC_cub, .drop = TRUE)
AIC_quad=tab_lin %>%   unnest(AIC_quad, .drop = TRUE)
TAB_AIC<-data.frame(AIC_lin[,c(1,2,3)],AIC_cub[,3],AIC_quad[,3])
TAB_AIC<- TAB_AIC %>%
  mutate(mini = pmin(AIC_lin,AIC_cub,AIC_quad))

# Table of residuals    
tab_lin<- data.frame(
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(department),
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(year),
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(sp),
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(.resid,.fitted),
  tab_lin    %>%   unnest(pred_quad, .drop = TRUE)  %>% select(.resid,.fitted),
  tab_lin    %>%   unnest(pred_cub, .drop = TRUE)   %>% select(.resid,.fitted))
names(tab_lin)<- c("department", "year", "sp", "anomaly_lin", "prediction_lin", "anomaly_quad", "prediction_quad", "anomaly_cub", "prediction_cub")
tab_lin     <-full_join(tab_lin,TAB_AIC,by=c("department","sp"))

## We add a column with result for the best model
tab_lin     <- tab_lin %>%
  mutate(anomaly_poly = case_when( AIC_lin  == mini ~ anomaly_lin,
                                   AIC_cub  == mini ~ anomaly_cub,
                                   AIC_quad == mini ~ anomaly_quad))
tab_lin     <- tab_lin %>%
  mutate(prediction_poly = case_when( AIC_lin  == mini ~ prediction_lin,
                                      AIC_cub  == mini ~ prediction_cub,
                                      AIC_quad == mini ~ prediction_quad))

## Loes model
tab_loess <- Yield  %>% 
           filter(ID %in% FILTRE[FILTRE$n>4,]$ID)%>%
           group_by(department,sp) %>%
           nest()%>%
           mutate(loess        = purrr::map(data, model_loess),         
                  pred_loess   = purrr::map(loess, augment))

tab_loess<-tab_loess  %>%   unnest(pred_loess, .drop = TRUE) %>% select(department, sp,year,.resid,.fitted)
names(tab_loess)<-c("department", "sp","year","anomaly_loess", "prediction_loess")

# Spline model
tab_spline <- Yield  %>% 
      filter(ID %in% FILTRE[FILTRE$n>5,]$ID)%>%
       group_by(department,sp) %>%
       nest()%>%
        mutate(spline         = purrr::map(data, model_spline),  # spline model
               pred_spline    = purrr::map(spline, augment))

tab_spline<-  tab_spline %>%   unnest(pred_spline, .drop = TRUE)%>% select(department, sp,x, .resid,.fitted)
names(tab_spline)<-c("department", "sp","year","anomaly_spline", "prediction_spline")

# Join all data together
RES<-full_join(full_join(tab_lin,tab_loess, by=c("department", "sp","year")), tab_spline)
RES<-full_join(RES, Yield) %>% select(-contains("AIC"), -mini)

# calculate mean and sd value 
SUM<-RES %>% group_by(department,sp) %>%
  summarize(sd_lin= sd(anomaly_lin,na.rm=TRUE),       mean_lin =mean(prediction_lin,na.rm=TRUE),
            sd_cub= sd(anomaly_cub,na.rm=TRUE),       mean_cub = mean(prediction_cub,na.rm=TRUE),
            sd_quad= sd(anomaly_quad,na.rm=TRUE),     mean_quad =mean(prediction_quad,na.rm=TRUE),
            sd_spline= sd(anomaly_spline,na.rm=TRUE), mean_spline = mean(prediction_spline,na.rm=TRUE),
            sd_loess = sd(anomaly_loess,na.rm=TRUE),  mean_loess= mean(prediction_loess,na.rm=TRUE),
            sd_poly= sd(anomaly_poly,na.rm=TRUE),     mean_poly= mean(prediction_poly,na.rm=TRUE))
## il y a des Na : vérifier

VERIF <- SUM[rowSums(is.na(SUM)) > 0,] %>%
         mutate(ID= paste0(department, sp))
AA<-FILTRE %>% filter(ID %in% VERIF$ID)


# calculate standardize and normalize residuals
RES<-full_join(RES,SUM)%>%
  mutate(anomaly_lin_stand    = anomaly_lin/ sd_lin,
         anomaly_lin_norm     = anomaly_lin/ prediction_lin*100,
         anomaly_cub_stand    = anomaly_cub/ sd_cub,
         anomaly_cub_norm    = anomaly_cub/ prediction_cub*100,
         anomaly_quad_stand   = anomaly_quad/ sd_quad,
         anomaly_quad_norm   = anomaly_quad/ prediction_quad*100,
         anomaly_slpine_stand = anomaly_spline/ sd_spline,
         anomaly_spline_norm = anomaly_spline/ prediction_spline*100,
         anomaly_loess_stand  = anomaly_loess/ sd_loess,
         anomaly_loess_norm  = anomaly_loess/ prediction_loess*100,
         anomaly_poly_stand   = anomaly_poly/ sd_poly,
         anomaly_poly_norm   = anomaly_poly/ prediction_poly*100) 
#%>%    select(-contains("sd_"), -contains("mean_"))
  
  
DAT1<-RES %>% select(contains("anomal"), year, sp) %>%
  group_by(year,sp)%>%
  summarise_all(funs(mean)) %>%
  gather(variable, value, -year, -sp) %>%
  separate(variable, c("first", "type","method") )
  
TT<-DAT1 %>% filter(sp=="maize_total")
ggplot(TT)+ geom_point(aes(x=year,y=value ))+ facet_wrap(type~method, scales="free", ncol=6)

TT$cyear_class<-cut(TT$year, breaks= c(1900,1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), 
                             labels=c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020"))

library(RColorBrewer)
my_orange = brewer.pal(n = 9, "Oranges")[3:9] #there are 9, I exluded the two lighter hues
ggplot(TT)+ geom_density(aes(x=value , color=cyear_class))+ facet_wrap(type~method, scales="free", ncol=6)+
  theme_pubr()+
  scale_colour_manual(values=c("#74C476" ,"#41AB5D", "#238B45","#FD8D3C" ,"#F16913" ,"#D94801", "#A63603" ,"black"))
dev.copy2pdf(file="Anomaly_Maize.pdf",width = 15, height = 8)


  filter(sp=="maize_total")
# dep<- c(eure,)
# COUNT<-DAT1 %>% group_by(department) %>% count() %>% arrange(desc(n)) %>% filter(n>110)

#FF<-DAT1 %>% filter(department %in%c("charente_maritime","aude", "charente", "ain", "vendee","deux-sevres", "vienne"))

ggplot(data=DAT1 %>% filter(department  %in% c("charente_maritime","aude", "charente", "ain", "vendee","deux-sevres", "vienne") ))  +
  geom_density(aes(x=anomaly_spline_norm, color=factor(cyear_class)))+facet_wrap(~sp, scales="free")+theme_pubr()+facet_wrap(~department)+
  scale_colour_brewer(palette = "Greens")

scale_color_gradient(values=c("#132B43", "#56B1F7"))

ggplot(data=DAT1 %>% filter(department  %in% c("charente_maritime","aude", "charente", "ain", "vendee","deux-sevres", "vienne") ))  +
  geom_point(aes(y=anomaly_spline_norm,x=year))+facet_wrap(~sp, scales="free")+theme_pubr()+facet_wrap(~department)

#verif
ggplot(data=DAT1 %>% filter(department  %in% c("charente_maritime","aude", "charente", "ain", "vendee","deux-sevres", "vienne") ))  +
  geom_point(aes(y=yield,x=year))+facet_wrap(~sp, scales="free")+
  geom_point(aes(y=anomaly_lin+prediction_lin,x=year),pch=21, color='red')+facet_wrap(~sp, scales="free")+theme_pubr()+facet_wrap(~department)
###
  


DAT1$pr

EX<- DAT1 %>%filter(cyear_class=="(2e+03,2.02e+03]") %>%
   filter(department=="aude") %>% select(contains("lin"))
unique(DAT1$cyear_class)
dev.copy2pdf(file="Anomaly_Maize_depNorm.pdf")


  
RES$cyear_class<-cut(RES$year, 10)
  RES %>%
  group_by(sp) %>%
  ggplot(data=.)  +
  geom_density(aes(x=anomaly_spline, color=factor(cyear_class)))+facet_wrap(~sp, scales="free")+theme_pubr()

dev.copy2pdf(file="Anomaly_ALL.pdf")
     
  
ggplot(data=DAT1)  +
  geom_density(aes(x=anomaly_spline, color=factor(cyear_class)))+facet_wrap(~sp, scales="free")+theme_pubr()


  dev.off()
  geom_density(aes(data=DAT2,x=anomaly_spline), color="blue")+facet_wrap(~sp, scales="free")
+
  geom_density(aes(RES %>% filter(year>1990), aes(anomaly_spline)))


+facet_wrap(~sp, scales="free")
  
  
summary(RES$anomaly_spline)
