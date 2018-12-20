# load packages
library("magrittr")
library(tidyr); library(dplyr)
library(gsubfn); library(maptools)
library(classInt); library(stringr)
library("broom")
library(tidyverse)
library("lme4")
library(MCMCglmm) 
library("randomForest")
library("TigR")
rm(list=ls())
setwd("~/Documents/CLAND/files_txt")


Yield_Climate <- read.csv2("~/Documents/CLAND/Files_txt/Yield_Climate.csv") %>%
  setNames(gsub('_Var_mean_gs', "_OBS",names(.)))

############# I/ DATALOAD  ##############################################################################

# filter à vérifer!!!!
Yield_Climate <-  Yield_Climate  %>% dplyr::select(-X)
Yield_Climate <- Yield_Climate   %>% filter(!is.na(yield))
Yield_Climate <- Yield_Climate   %>% filter(!is.na(Tmean_OBS))
Yield_Climate <- Yield_Climate   %>% filter(!yield==0)


VERIF<- Yield_Climate2 %>% filter(departement=="AIN", year_harvest=="1959")
# Add quare of all variables
Yield_Climate2 <- bind_cols(Yield_Climate,
                            data.frame(lapply(Yield_Climate                         %>%
                                                dplyr::select_if(is.numeric)               %>%
                                                dplyr::select(-contains(".fitted"),-contains(".resid"),
                                                       -contains("anomaly"), - contains("prediction"),
                                                       -"production", -"area"),
                                        "^",2))  %>%
                           setNames(paste0(sub('_.*', '', names(.)),"2_",sub('.*_', '', names(.)))))
 
`# Normalise the data (1. calculate mean and sd of original data. 
#Normalise detrended climate variable with mean and sd calculated)

MEAN_SD <- Yield_Climate2                                   %>%
           group_by(sp, method)                            %>% 
           summarise_all(funs(mean,sd))                    %>%
           dplyr::select( sp, method, contains("OBS"))    %>%
           gather(variable,value,-sp, -method)             %>%
           mutate(variable  = gsub('_OBS', "",variable),
                  type      = sub('.*_', '', variable), 
                  variable  = sub('_.*', '', variable))     %>%
           spread(type, value)
  
list_sp<- unique(Yield_Climate2$sp)

SPREAD <- Yield_Climate2  %>% 
          dplyr::select(-contains("prediction"), -contains("anomaly"))                                 %>%
          filter(sp == list_sp[[15]])                                                           %>%
          gather(variable, value, -departement,-method, -sp, -year_harvest,-ID, -year2_harvest) %>%
          mutate(type     = gsub(".*_","",variable))                                            %>%
          mutate(variable  = sub('_.*', '', variable))

 VERIF<- SPREAD %>% filter(variable=="Tmean") %>% filter(departement=="AIN", year_harvest=="1959")
            #     variable = gsub("\\_.*","",variable))

INFO<- SPREAD %>% 
          filter(variable %in% c("department", "area", "yield"))                             %>% 
          dplyr::select(-type) %>% spread(variable, value)

SPREAD <- full_join(SPREAD %>% filter(!variable %in% c("production", "area", "yield")), INFO )

# ggplot(SPREAD %>% filter(!type == ".resid") %>%
#          filter(departement=="AIN")         %>%
#          filter(method=="lin_ALL"))+ 
#   geom_line(aes(x=year_harvest, y=value, color=type, group=type))+
#   facet_wrap(~variable, scales="free")
# dev.copy2pdf(file="Clmate_detrend.pdf",width = 10, height = 8)

# TT <- SPREAD %>% filter(type == "gs")     %>%
#           #  filter(departement=="AIN")         %>%
#             filter(method=="lin_ALL")         %>%
#           filter(variable=="Tmean")
# (mean(TT$value)-10.17)/

 CLIMATE_NORM <- SPREAD                                                                         %>%
      full_join(MEAN_SD %>% filter(sp == list_sp[[15]]), by= c("method", "sp","variable"))      %>%
   mutate(value_norm = (value - mean)/sd)                                                       %>%
                  dplyr::select(-mean, -sd, -value) %>%
                  spread(variable, value_norm)

 
 VERFI<- CLIMATE_NORM %>% filter(departement=="AIN", year_harvest=="1959")
   
 VERFI<- CLIMATE_NORM %>% filter(is.na(Tmean))
# ggplot(CLIMATE_NORM %>% filter(!type == ".resid") %>%
#          filter(departement=="AIN")         %>%
#          filter(method=="lin_ALL"))+ 
#   geom_line(aes(x=year_harvest, y=Tmean, color=type, group=type))

# AA<-CLIMATE_NORM %>% filter(type == "gs") %>%
#   filter(method=="lin_ALL")
# mean(AA$Tmean)
#dev.copy2pdf(file="Clmate_detrend.pdf",width = 10, height = 8)


# Define models relating crop yield and climate
model_Lin     <-function(DAT) {model<-glm (log(yield)  ~ departement + departement*year_harvest + departement*I(year_harvest^2) + Tmean + I(Tmean)^2 + PR + I(PR)^2 ,data=DAT)}

model_LinW     <-function(DAT) {model<-glm (log(yield)  ~ departement + departement*year_harvest + departement*I(year_harvest^2) + Tmean + I(Tmean)^2 + PR + I(PR)^2 ,weights= area, data=DAT)}

model_LMER       <-function(DAT) {model<-lmer(log(yield)  ~ year_harvest + I(year_harvest)^2 + Tmean + I(Tmean)^2 + PR + I(PR)^2 + (1+year_harvest+I(year_harvest)^2|departement),data=DAT)}

model_LMERW       <-function(DAT) {model<-lmer(log(yield)  ~ year_harvest + I(year_harvest)^2 + Tmean + I(Tmean)^2 + PR + I(PR)^2 + (1+year_harvest+I(year_harvest)^2|departement),
                                              weights=area,data=DAT)}

model_MCMC       <-function(DAT) {model<-MCMCglmm(log(yield) ~ year_harvest  +  I(year_harvest)^2 +
                                 Tmean  + I(Tmean)^2+ PR+I(PR)^2 ,
                                 random = ~us(1+year_harvest):departement, data=DAT, pr=TRUE)}

model_RF        <-function(DAT) {model<- randomForest(log(yield)~ Tmean+PR+year_harvest, ntree=401,data=DAT, proximity=TRUE, oob.prox=FALSE)}



unique(GG$departement)

BD1<-CLIMATE_NORM                                %>% 
  filter(type=="OBS")                            %>%
  group_by(sp, method)                           %>% 
  nest()                                         %>%        # group
  rename(myorigdata = data)                      %>%
  mutate(LOBELL     = purrr::map(myorigdata, model_Lin),
         OR_LOBELL  = purrr::map2(LOBELL, myorigdata, predict),  # Predict values Lobell model
         LMER       = purrr::map(myorigdata, model_LMER),        # Run the lmer model
         OR_LMER    = purrr::map2(LMER, myorigdata, predict))    # Predict values LMER model
         #MCMC       = purrr::map(myorigdata, model_MCMC),        # Run the MCMC model
         #OR_MCMC    = purrr::map2(MCMC, myorigdata, predict))    # Predict values MCMC model
#RF         = purrr::map(myorigdata, model_RF),        # Run the MCMC model
         #OR_RF      = purrr::map2(RF, myorigdata, predict))    # Predict values MCMC model

summary(CLIMATE_NORM)

ESSAI <- CLIMATE_NORM %>% filter(type=="OBS")          %>%
                          filter(sp  =="wheat_total") %>%
                          filter(method =="lin_ALL")
# ESSAI$year_harvest<-as.numeric(as.character(ESSAI$year_harvest))
# ESSAI$year2  <- ESSAI$year_harvest*ESSAI$year_harvest
# ESSAI$PR2    <- ESSAI$PR*ESSAI$PR
# ESSAI$Tmean2 <- ESSAI$Tmean*ESSAI$Tmean
ESSAI$year_harvest<-ESSAI$year_harvest- (min(ESSAI$year_harvest)-1)
ESSAI$year2_harvest<-data.frame(scale(ESSAI$year_harvest*ESSAI$year_harvest))[,1]
ESSAI$year_harvest<-data.frame(scale(ESSAI$year_harvest))[,1]
  
write.csv2(ESSAI,"ESSAI.csv")
mod  <-glm (log(yield)  ~ year_harvest*departement +year2_harvest*departement+ Tmean + Tmean2 + PR +PR2     ,data=ESSAI)
mod2 <-lmer(log(yield)  ~ Tmean + Tmean2 + PR + PR2 + (1+year_harvest+year2_harvest|departement) ,data=ESSAI)
plot(predict(mod), predict(mod2))
plot(exp(predict(mod)), ESSAI$yield)
plot(exp(predict(mod2)), ESSAI$yield)

ll<-MCMCglmm(log(yield) ~   year_harvest+year2_harvest+Tmean + Tmean2 + PR + PR2, random = ~us(1+year_harvest+year2_harvest):departement, data=ESSAI, pr=TRUE)
plot(predict(ll, marginal=NULL), predict(mod2))
plot(exp(predict(ll, marginal=NULL)), ESSAI$yield)

summary(ll)
summary(mod2)


# Mthode Julia retravaillée
PARAM<-data.frame(t(ll$Sol))   %>%
   mutate(Var = row.names(.))  %>%
   separate(Var, c("Var1", "Var2","dep","dep2"))%>%
   filter(is.na(dep2))

paste2 <- function(...,sep="_") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  gsub(paste0("(^",sep,"|",sep,"$)"),"",
       gsub(paste0(sep,sep),sep,
            do.call(paste,c(L,list(sep=sep)))))
}
NAMES<-gsub(" ", "", paste2(PARAM$Var1,PARAM$Var2)) %>% magrittr::extract(.!="Intercept")

# DAT<-BD1$myorigdata[[2]] %>% 
  DAT<-ESSAI             %>% 
    dplyr::dplyr::select(NAMES)          %>% 
    dplyr::mutate(Intercept=1)    %>%
  dplyr::dplyr::select(Intercept, everything())

RDT<-data.frame(as.matrix(t(PARAM[,1:1000])) %*% data.matrix(t(DAT)))
WW<-colMeans(RDT)

plot(WW, predict(ll, marginal=NULL))




glm(log(yield) ~ departement + departement*year_harvest + departement*year_harvest2 + Tmean + Tmean2 + PR + PR2, data=DATA_obs)
lmer(log(yield) ~ year_harvest + year_harvest2 + Tmean + Tmean2 + PR + PR2 + (1+year_harvest+year_harvest2|departement),data=DATA_obs)

#LIST<-list(NULL)
#ANN<-c("y65","y70", "y75", "y78", "y80", "y82")
#for (i in 1: length (ANN)){
    
    BD2<-CLIMATE_NORM            %>% 
     # filter(type %in% ANN[[i]]) %>%
      group_by(sp, method, type)       %>%
      nest() %>%
      rename(newdata = data)
    
    LIST <- BD1      %>% 
    full_join(BD2, by = c("method","sp","method"))    %>%  # Join original data and climate predictions
    mutate(new_Lobell   = purrr::map2(LOBELL, newdata, predict),   #  %>%   # Predict for detrended climate    
           new_lmer     = purrr::map2(LMER, newdata, predict))     #  %>%   # Predict for detrended climate    
           #new_mcmc     = purrr::map2(MCMC, newdata, predict),     #  %>%   # Predict for detrended climate    
           #new_RF       = purrr::map2(RF, newdata, predict),       #  %>%   # Predict for detrended climate    
           #pars_LO       =purrr::map(LOBELL, augment))
           #  mutate(method = ANN[i])  # Modif!!
#}

################### II/ Results ######################
RES <- LIST %>%  bind_rows() 
TT1  <- RES %>% unnest(newdata, new_Lobell)   %>% mutate(model="new_Lobell") %>% rename(pred=new_Lobell)
TT2  <- RES %>% unnest(newdata, new_lmer)     %>% mutate(model="new_lmer")   %>% rename(pred=new_lmer)
TT3  <- RES %>% unnest(newdata, new_mcmc)     %>% mutate(model="new_mcmc")   %>% rename(pred=new_mcmc)

TT4  <- RES %>% unnest(newdata, new_RF)       %>% mutate(model="new_RF")     %>% rename(pred=new_RF)
TT<-rbind(rbind(TT3,rbind(TT1,TT2)),TT4)
plot(TTA$pred, TTB$pred)

TTA <- RES %>% unnest(myorigdata, OR_LOBELL)  %>% mutate(model="Lobell")     %>% rename(pred=OR_LOBELL)
TTB <- RES %>% unnest(myorigdata, OR_LMER)    %>% mutate(model="lmer")       %>% rename(pred=OR_LMER)
TTC <- RES %>% unnest(myorigdata, OR_MCMC)    %>% mutate(model="mcmc")       %>% rename(pred=OR_MCMC)
TTD <- RES %>% unnest(myorigdata, OR_RF)      %>% mutate(model="rf")         %>% rename(pred=OR_RF)
TT2<-rbind(rbind(TTA,rbind(TTB,TTC)),TTD)

EXT<-TT2[,c(1,2,3,4,5,20)]
names(EXT)[3]<- "predCC"
EXT<-full_join(TT,EXT,by=c("sp", "method", "departement", "year_harvest"))
TT<- rbind(TT,TT2)
TT<-  cbind(TT[,c(1:9, 20)], TT2[,c(3,20)])
TT$Diff= (exp(TT2$pred) -exp(TT$pred))/exp(TT2$pred) *100

ggplot(TT %>% filter(method=="lin_ALL") %>%
         filter(departement=="AIN"))+geom_line(aes(y=Diff,x=year_harvest,color=type))+facet_wrap(model~method, scales="free")
# vérifications
plot(exp(TT2$),TT2$yield)
TAB<-TT %>% filter(type=="gs")
ggplot(data=TAB)+geom_point(aes(x=exp(pred), y=yield))+facet_wrap(model~method)+theme_pubr()+
  geom_abline(intercept = 0, slope=1)
dev.copy2pdf(file="Quality_model.pdf",width = 10, height = 8)

library(Metrics)
TAB2<-TAB %>% filter(model=="lmer",method=="loess_ALL")
rmse(exp(TAB2$pred),TAB2$yield)

## plot
LIST_DEP<- c('AISNE', "ARDENNES","AUBE","CALVADOS", "CHER","COTE-D'OR", "EURE","EURE-ET-LOIR" ,"INDRE", "INDRE-ET-LOIRE", 
"LOIR-ET-CHER", "LOIRET"  , "MARNE", "MEURTHE-ET-MOSELLE" ,"MEUSE", "MOSELLE" , "NORD"  , "OISE", "ORNE", "PAS-DE-CALAIS", 
"SARTHE","SEINE-ET-MARNE" , "SEINE-MARITIME", "SOMME","VIENNE", "YONNE", "YVELINES")


YY<-TT %>% filter(departement %in% c("EURE")) %>% #LIST_DEP) %>%
  filter(type %in% c("gs", "y65"))%>%
  filter(model %in% c("Lobell","new_Lobell","new_lmer")) %>%
  filter(method %in% c("lin_ALL","lin_Lob" )) %>%
  filter(year_harvest %in% c(2010:2018))
YY$PRED_PROD<-exp(YY$pred)*YY$area
YY$PROD<-YY$yield*YY$area
plot(YY$PROD, YY$PRED_PROD)
YY<- YY %>% dplyr::select(year_harvest,type,model, method,PRED_PROD,PROD,area) %>%
                     group_by(year_harvest,type,model, method)%>%  summarise_all(funs(sum))
YY$PRED_Yield<-exp(YY$pred)/YY$area
YY$Yield<-YY$yield/YY$area
plot(YY$Yield, YY$PRED_Yield)

summary(YY$PRED_Yield)
summary(YY$Yield)

ggplot(data=YY)+ geom_point(aes(x=exp(pred),y=yield, color=model),adjust = 3)+
  geom_point(data=YY[YY$year_harvest== "2018",],color="red",size=3)+
  facet_wrap(model~method, ncol=4)+ theme_pubr()+
  geom_abline(intercept=0, slope=1,linetype=2, color="gray")+
 # scale_colour_manual(values=c("#74C476" ,"#F16913", "#A63603" ,"black"))+
  geom_vline(xintercept = 7.7, linetype=2, color="gray")+xlim(0,15)

+ 
  geom_density(aes(x=yield), color="black",adjust = 3)
+
  geom_density(data=YY%>% filter(type=="y65"),aes(x=exp(pred), color=model),adjust = 1)+
  scale_color_manual(values = c('orange','blue',"green", "black", "gray50", "gray30", "gray20", "gray10", "gray3"))

1.5
dev.off()
dev.copy2pdf(file="Proba_ALL_DEPE.pdf",width = 10, height = 8)




hist(YY$)

ggplot(data=TT)+ geom_boxplot(aes(x=type, y=Diff, color=method))
ggplot(data=TT)+ geom_density(aes(x=Diff, color=method))
UU<-TT %>% filter(departement== "AIN")
ggplot(data=TT %>% filter(departement== "AIN"))+ geom_line(aes(y=Diff, x=year_harvest, color=method))


TT<-LIST[[1]]$my_new_pred


RES_PRED <- RES%>% 
unnest(myorigdata, OR_LOBELL) 
%>%
  mutate(Diff= yield -my_new_pred)

# RES65 <- y65 %>% unnest(newdata, my_new_pred) %>% mutate(my_new_pred = exp(my_new_pred))
# RES75 <- y75 %>% unnest(newdata, my_new_pred) %>% mutate(my_new_pred = exp(my_new_pred))
# RES85 <- y75 %>% unnest(newdata, my_new_pred) %>% mutate(my_new_pred = exp(my_new_pred))
# RES75 <- y75 %>% unnest(newdata, my_new_pred) %>% mutate(my_new_pred = exp(my_new_pred))
# RES75 <- y75 %>% unnest(newdata, my_new_pred) %>% mutate(my_new_pred = exp(my_new_pred))


plot(RES65$my_new_pred , RES75$my_new_pred)
# RES <- full_join(RES, RES2)

# verif:
plot(RES2$my_new_pred[1:1000], RES2$yield[1:1000])
  abline(0,1)
  
  
  
%>% 
  dplyr::select(Species, mynewdata, my_new_pred) %>% 
  unnest(mynewdata, my_new_pred) %>% 
  rename(modeled = my_new_pred, measured = Sepal.Length) %>%
  gather("Type", "Sepal.Length", modeled, measured)
  
  dplyr::select(Species, mynewdata, my_new_pred) %>% 
  unnest(mynewdata, my_new_pred) %>% 
  rename(modeled = my_new_pred, measured = Sepal.Length) %>%
  gather("Type", "Sepal.Length", modeled, measured)



MOD<- BD1 %>% filter(sp=="barley_spring" )
GG<- MOD %>% group_by(sp, method) %>%
  nest() %>%
  mutate(LOBELL         = purrr::map(data, model_LOBELL),                               # LOBELL model
         pred_LOB       = purrr::map(LOBELL, augment))


# shuffle the rows to mix up the species
set.seed(1234)
myiris <- iris[sample(nrow(iris), replace = F),]

# create first dataset - use the first 50 rows for running the model
iris_nested <- 
  myiris[1:50,] %>% 
  nest(-Species)%>% 
  rename(myorigdata = data)

# create second dataset - use the other 100 rows for making predictions
new_iris_nested <- 
  myiris[51:150,] %>% 
  nest(-Species) %>% 
  rename(mynewdata = data)

# make a model function
my_rlm <- function(df) {
  MASS::rlm(Sepal.Length ~ Petal.Length + Petal.Width, data = df)
}

# get the predictions (see the GitHub link above which breaks this into steps)
predictions_tall <- 
  iris_nested %>% 
  mutate(my_model = purrr::map(myorigdata, my_rlm)) %>% 
  full_join(new_iris_nested, by = "Species") %>% 
  mutate(my_new_pred = purrr::map2(my_model, mynewdata, predict)) %>% 
  dplyr::select(Species, mynewdata, my_new_pred) %>% 
  unnest(mynewdata, my_new_pred) %>% 
  rename(modeled = my_new_pred, measured = Sepal.Length) %>%
  gather("Type", "Sepal.Length", modeled, measured)

predictions_tall %>% nest(-Species, -type) %>% as.tibble()




TT<- Yield_Climate %>% gather()
model<-glm(log(yield)~ departement*year_harvest + departement*I(year_harvest^2) + Tmean_Var_mean_gs + Tmean_Var_mean_gs^2 + PR_Var_mean_gs + PR_Var_mean_gs^2 ,weights=area,data=SP1)

                                 unique(Yield_Climate$sp)
hist(SP1$PR_Var_mean_gs)

Yield_Climate$yield
#########III prediction ##################

#III.1.for the original dataset
TABLE.spread$predict<-exp(predict(model))
#plot(TABLE.spread$predict,log(TABLE.spread$yield))

#III.2.without CC
# define a function to predict based on new Tmean and PR values (with no CC)
FUN_PRED<-function(year_begin_Tmean,methodTmean,year_begin_PR,methodPR){
id_PR     <-Reduce(`&`, lapply(c(year_begin_PR, "PR", methodPR), grepl, names(TABLE.spread)))
id_Tmean  <-Reduce(`&`, lapply(c(year_begin_Tmean, "Tmean", methodTmean), grepl, names(TABLE.spread)))
id        <-c(names(TABLE.spread)[id_PR][1],names(TABLE.spread)[id_Tmean][1])

TMP  <- TABLE.spread %>% 
              dplyr::select(id, departement,year_harvest)             %>%
              set_names(c(sub("_.*", "", names(.))))           %>% 
              mutate(Tmean = as.numeric(as.character(Tmean)))  %>%
              mutate(PR = as.numeric(as.character(PR))) 
names(TMP)[4]<-"year_harvest"    # pas beau : ç faire mieux

AA<-paste0("predict_","Tmean",substitute(year_begin_Tmean),substitute(methodTmean),
           "PR",substitute(year_begin_PR),substitute(methodPR))
TABLE.spread[,AA]<<-exp(predict(model,newdata = TMP))
}

# prediction for new variables
FUN_PRED(year_begin_Tmean="82",methodTmean="lm",year_begin_PR="82",methodPR="lm")
FUN_PRED(year_begin_Tmean="82",methodTmean="loess",year_begin_PR="82",methodPR="loess")
FUN_PRED(year_begin_Tmean="",methodTmean="",year_begin_PR="82",methodPR="lm")  

# Pour faire trouner la fonction sur des vecteurs d'arguments:
#rm: https://stackoverflow.com/questions/48255041/run-a-function-multiple-times-with-different-arguments-each-run

#########IV. Plot verification ##################
names(TABLE.spread)
DEP<-unique(TABLE.spread$departement)[1:12]                                  # Choose some departements
ggplot(data=TABLE.spread %>% filter(departement %in% DEP)) +                 # filter the departements
  geom_line(aes(x=year_harvest, y=yield))+                                   # line of observed data
  facet_wrap(~departement)+                                                  # one plot by departement
  theme_bw()+                                                                # theme
  geom_line(aes(x=year_harvest,y=predict,color="red") )  +                   # add prediction with Tman and PR observed
  geom_line(aes(x=year_harvest,y=predict_Tmean82lmPR82lm,color="blue") ) +           # add prediction with CC
  geom_line(aes(x=year_harvest,y=predict_Tmean82loessPR82loess,color="green") ) +    # add prediction with CC
  geom_line(aes(x=year_harvest,y=predict_TmeanPR82lm,color="gray") )                 # add prediction with CC

#########V. Difference CC and noCC ##################

TAB_DIFF<-TABLE.spread %>%
     dplyr::select (departement,year_harvest,matches("predict"))
head(TAB_DIFF)
x<-TAB_DIFF[,c(3)]
TAB_DIFF[,-c(1:3)]<-sweep(TAB_DIFF[,-c(1:3)],1,x)

DEP<-unique(TAB_DIFF$departement)[1:12]                                      # Choose some departements
ggplot(data=TAB_DIFF %>% filter(departement %in% DEP)) +                     # filter the departements
  facet_wrap(~departement)+                                                  # one plot by departement
  theme_bw()+                                                                # theme
 geom_line(aes(x=year_harvest,y=predict_Tmean82lmPR82lm,color="blue") ) +           # add prediction with CC
  geom_line(aes(x=year_harvest,y=predict_Tmean82loessPR82loess,color="green") ) +    # add prediction with CC
  geom_line(aes(x=year_harvest,y=predict_TmeanPR82lm,color="gray") )                 # add prediction with CC

# Mean value over the period
TAB<-TAB_DIFF %>%
  dplyr::select(-year_harvest,-predict) %>%
  group_by(departement) %>%
  summarise_all(mean)%>%
  gather(key=variable,value=value,-departement)
  
ggplot(data=TAB)+geom_boxplot(aes(x=variable,y=value))+theme_bw()



# Map
graphics.off()
#par(mar=c(0,0,0,0))
for (v in 1: length(Value.variables)){

scenar<-Value.variables[v]
TEMP<-MEAN %>% filter (scenario==scenar)

# parameters 
fixedBreaks=seq(-0.1,0.1,by=0.02)
nclasses<-length(fixedBreaks)-1
leg<-list()
classe <- classIntervals(TEMP$mean.norm.diff[order(TEMP$dpt)], nclasses, digits=0,style='fixed',fixedBreaks=fixedBreaks)
for(K in 1:nclasses){                
  leg[[K]] = c(paste("[",round(classe$brks[K],3), "-", round(classe$brks[K+1],3),']'))};legende<-unlist(leg)
jet.colors<- colorRampPalette(c( "dark red","#FF7F00","yellow","#7FFF7F","cyan",'darkblue'))#, "blue","#00007F"))
color=jet.colors(nclasses)

# map
tab_map<-data.frame(admin_entity=TEMP$dpt[order(TEMP$dpt)],loss=TEMP$mean.norm.diff[order(TEMP$dpt)])
head(tab_map)
tab_map[,"classe"] <- cut(tab_map[,'loss'],breaks=classe$brks,labels=F,include.lowest=TRUE)
asso <- match(map$NOM_DEPT, tab_map$admin_entity)
plot(map[map$NOM_DEPT%in%unique(TEMP$dpt),],col='white',main=paste( 'Loss due to CC (%) for',sub('_SAFRAN.csv', "", Species_Climate_Name[i]),'in scenario',scenar))
plot(map,col=color[tab_map$classe[asso]],add=T)
legend("bottomleft",legende,col=color,pch=15,bty="n",cex=1, horiz=F)
dev.copy2pdf (file= paste0(Resultfolder,'Map_PerCent_Loss_due_to_CC_',sub('_SAFRAN.csv',"", str_sub(Species_Climate_Name[i],-18,-1)),'_for_scenario_',scenar,'.pdf'))

}






