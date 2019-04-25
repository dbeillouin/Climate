function_CAL_model_ALL<- function(TAB){
  
  library("randomForest", lib.loc="~/Library/R/3.5/library")

# Define models relating crop yield and climate
model_Lin     <-function(DAT) {model<-glm (log(yield)  ~ departement + departement*year_harvest + departement*I(year_harvest^2) + Tmean_ + I(Tmean_)^2 + PR_ + I(PR_)^2 ,data=DAT)}

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

model_RF         <-function(DAT) {model<- randomForest(log(yield)~ year_harvest+ETP_+PR_+RV_+Seq.PR_+Tmean_+Tn_+Tx_,
                                                       ntree=401,mtry=4,data=DAT, proximity=TRUE, oob.prox=FALSE)}
YIELD_CLIMAT <- TAB   %>% filter(!is.na(yield))
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!yield==0)
YIELD_CLIMAT %<>% select(-contains("prediction"), - contains("anomaly"))


YIELD_CLIMAT$departement<-factor(YIELD_CLIMAT$departement)
YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(Tmean_))

YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_))

YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(ETP_))

YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(RV_))

YIELD_CLIMAT <- YIELD_CLIMAT   %>% filter(!is.na(PR_))


CAL<-YIELD_CLIMAT                                %>% 
  filter(type=="OBS")                            %>%
  group_by(method)                               %>% 
  nest()                                         %>%        # group
  rename(myorigdata = data)                      %>%
  mutate(LOBELL     = furrr::future_map(myorigdata, model_Lin,.progress = TRUE),
         OR_LOBELL  = furrr::future_map2(LOBELL, myorigdata, predict,.progress = TRUE),
         RF         = furrr::future_map(myorigdata, model_RF,.progress = TRUE),        # Run the MCMC model
         OR_RF      = furrr::future_map2(RF, myorigdata, predict,.progress = TRUE))  


TT1 <- CAL %>% unnest(myorigdata, OR_LOBELL)  %>% mutate(model="Lobell")       %>% rename(pred=OR_LOBELL)
TT2 <- CAL %>% unnest(myorigdata, OR_RF)      %>% mutate(model="RF")           %>% rename(pred=OR_RF)
CAL<-rbind(TT1,TT2)

return(list(CAL))

}