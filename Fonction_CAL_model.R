function_CAL_model<- function(TAB){
  
  library("randomForest", lib.loc="~/Library/R/3.5/library")

# Define models relating crop yield and climate
model_Lin     <-function(DAT) {model<-glm (log(yield)  ~ departement + departement*year_harvest + departement*I(year_harvest^2) + Tmean_6 + I(Tmean_6)^2 + PR_6 + I(PR_6)^2 ,data=DAT)}

# model_LinW     <-function(DAT) {model<-glm (log(yield)  ~ departement + departement*year_harvest + departement*I(year_harvest^2) + Tmean + I(Tmean)^2 + PR + I(PR)^2 ,weights= area, data=DAT)}
# 
# model_LMER       <-function(DAT) {model<-lmer(log(yield)  ~ year_harvest + I(year_harvest)^2 + Tmean + I(Tmean)^2 + PR + I(PR)^2 + (1+year_harvest+I(year_harvest)^2|departement),data=DAT)}
# 
# model_LMERW       <-function(DAT) {model<-lmer(log(yield)  ~ year_harvest + I(year_harvest)^2 + Tmean + I(Tmean)^2 + PR + I(PR)^2 + (1+year_harvest+I(year_harvest)^2|departement),
#                                                weights=area,data=DAT)}
# 
# model_MCMC       <-function(DAT) {model<-MCMCglmm(log(yield) ~ year_harvest  +  I(year_harvest)^2 +
#                                                     Tmean  + I(Tmean)^2+ PR+I(PR)^2 ,
#                                                   random = ~us(1+year_harvest):departement, data=DAT, pr=TRUE)}
# 

model_RF         <-function(DAT) {model<- randomForest(log(yield)~ year_harvest+
                                                         Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                         PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                         ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                         RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8,
                                                       ntree=401,mtry=4,data=DAT, proximity=TRUE, oob.prox=FALSE)}
YIELD_CLIMAT <- TAB   %>% filter(!is.na(yield))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!yield==0)
YIELD_CLIMAT %<>% select(-contains("prediction"), - contains("anomaly"))


YIELD_CLIMAT$departement<-factor(YIELD_CLIMAT$departement)
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_1))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_2))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_3))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_4))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_5))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_6))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_7))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_8))


YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_1))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_2))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_3))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_4))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_5))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_6))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_7))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_8))

YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_1))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_2))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_3))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_4))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_5))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_6))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_7))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_8))

YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_1))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_2))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_3))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_4))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_5))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_6))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_7))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_8))

YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_1))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_2))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_3))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_4))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_5))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_6))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_7))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_8))

CAL<-YIELD_CLIMAT                                %>% 
  filter(type=="OBS")                            %>%
  group_by(method)                               %>% 
  nest()                                         %>%        # group
  rename(myorigdata = data)                      %>%
  mutate(LOBELL     = furrr::future_map(myorigdata, model_Lin,.progress = TRUE),
         OR_LOBELL  = furrr::future_map2(LOBELL, myorigdata, predict,.progress = TRUE),
         RF         = furrr::future_map(myorigdata, model_RF,.progress = TRUE),        # Run the MCMC model
         OR_RF      = furrr::future_map2(RF, myorigdata, predict,.progress = TRUE))  

# VAL<-YIELD_CLIMAT                               %>% 
#   filter(type=="OBS")                           %>%
#   group_by(method, type)                        %>%
#   nest() %>%
#   rename(newdata = data)

#ggplot(data=VAL)+ geom_point(aes(Tmean_1, Tmean_2))+facet_wrap(method~type)

# LIST <- CAL      %>% 
#   full_join(VAL, by = c("method"))             %>%  # Join original data and climate predictions
#   mutate(new_Lobell   =  furrr::future_map2(LOBELL, newdata, predict,.progress = TRUE),   #  %>%   # Predict for detrended climate    
#          new_RF       =  purrr::map2(RF, newdata, predict))   


# RES <- LIST %>% bind_rows() 
# TT1  <- RES %>% unnest(newdata, new_Lobell)   %>% mutate(model="new_Lobell")   %>% rename(pred=new_Lobell)
# TT2  <- RES %>% unnest(newdata, new_RF)       %>% mutate(model="new_RF")       %>% rename(pred=new_RF)
# VAL<-rbind(TT1,TT2)

TT1 <- CAL %>% unnest(myorigdata, OR_LOBELL)  %>% mutate(model="Lobell")       %>% rename(pred=OR_LOBELL)
TT2 <- CAL %>% unnest(myorigdata, OR_RF)      %>% mutate(model="RF")           %>% rename(pred=OR_RF)
CAL<-rbind(TT1,TT2)

return(list(CAL))

}