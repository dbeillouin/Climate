## Damien Beillouin
# fonction pour faire un validation croisée sur les années
# modèles calibrés tous les départrements
################################################


function_Cross_VALID_ALL_DEP <- function(TAB){
  
  # on normalise les années
  TAB$year_harvest      <- data.frame(scale(TAB$year_harvest))[,1]
  TAB$year_harvestcarre <- data.frame(scale(TAB$year_harvestcarre))[,1]
  
  # on sélectionne les données
  TAB<-TAB[TAB$type=="OBS",]
  TAB<-TAB[TAB$method=="lin_EACH",]
  TAB %<>% filter(!is.na(yield))
  TAB %<>% filter(!yield==0)
  TAB %<>% dplyr::select(-contains("prediction"), - contains("anomaly"))
  TAB<- TAB[complete.cases(TAB), ]
  
  # load packages
  set.seed(42)
  x<- c("keras","reticulate","glmnet","TigR","randomForest","MCMCglmm","lme4","broom", "classInt", "stringr","maptools","hydroGOF", "pls", "glmnetUtils")
  lapply(x, require, character.only = TRUE)
  
  
  # Define models relating crop yield and climate
  # linear model 
  model_Lin      <-function(DAT) {model<-glm (log(yield)  ~ year_harvest +I(year_harvest^2)+
                                                Tmean_3+Tmean_4+Tmean_5+Tmean_6+
                                                PR_3+PR_4+PR_5+PR_6+
                                                I(Tmean_3)^2 + I(Tmean_4)^2 +I(Tmean_5)^2 +I(Tmean_6)^2 +
                                                I(PR_3)^2+I(PR_4)^2+I(PR_5)^2+I(PR_6)^2     ,data=DAT)}
  
  # random forest avec l'ensemble des variables
  model_RF       <-function(DAT) {model<- randomForest(log(yield)~ year_harvest+
                                                         Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                         PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                         ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                         RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8,
                                                       ntree=500,mtry=10,data=DAT, proximity=TRUE, oob.prox=TRUE)}
  
  model_PLS      <- function(DAT) {model<-plsr(log(yield) ~ year_harvest+
                                                 Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                 PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                 ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                 RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, data = DAT,
                                               validation = "CV")}
  
  model_lasso   <-function(DAT) {model<- glmnet(log(yield)~ year_harvest+
                                                  Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                  PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                  ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                  RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, family="gaussian", alpha=1, data=DAT)}
  
  model_lassocv <-function(DAT) {model<- cv.glmnet(log(yield)~ year_harvest+
                                                     Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                     PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                     ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                     RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, family="gaussian", alpha=1, data=DAT)}
  
  model_ridge   <-function(DAT) {model<- glmnet(log(yield)~ year_harvest+
                                                  Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                  PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                  ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                  RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, family="gaussian", alpha=0, data=DAT)}
  
  model_ridgecv <-function(DAT) {model<- cv.glmnet(log(yield)~ year_harvest+
                                                     Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                     PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                     ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                     RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, family="gaussian", alpha=0, data=DAT)}
  
  model_elnet  <-function(DAT) {model<- glmnet(log(yield)~year_harvest+
                                                 Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                 PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                 ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                 RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, family="gaussian", alpha=0.5, data=DAT)}
  
  model_elnetcv<-function(DAT) {model<- cv.glmnet(log(yield)~ year_harvest+
                                                    Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
                                                    PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
                                                    ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
                                                    RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, family="gaussian", alpha=0.5, data=DAT)}
  
  
  model_MCMC  <-function(DAT) {model<-MCMCglmm(log(yield) ~year_harvest+
                                                 Tmean_1 +Tmean_2+Tmean_3,data=DAT, pr=TRUE,burnin=1000, nitt=3000)}
  
  model_MCMC  <-function(DAT) {model<-MCMCglmm(log(yield) ~year_harvest +I(year_harvest^2)+
                                                 Tmean_3+Tmean_4+Tmean_5+Tmean_6+
                                                 PR_3+PR_4+PR_5+PR_6+
                                                 I(Tmean_3)^2 + I(Tmean_4)^2 +I(Tmean_5)^2 +I(Tmean_6)^2 +
                                                 I(PR_3)^2+I(PR_4)^2+I(PR_5)^2+I(PR_6)^2,
                                               data=DAT, pr=TRUE,burnin=1000, nitt=3000)}
  
  
  ## define groups of year and groups of departemnt
  CORES<-TAB %>% dplyr::select(year_harvest) %>%  unique()
  CORES$year_harvest_Class<- runif(n=nrow(CORES), min=1e-12, max=.9999999999) 
  CORES<-CORES %>% 
    mutate(year_harvest_Class = case_when(year_harvest_Class <= 0.5      ~ "Y1",
                                          # year_harvest_Class <= 0.2      ~ "Y2",
                                          year_harvest_Class <= 1       ~ "Y3"))
  #year_harvest_Class <= 0.4       ~ "Y4",
  #    year_harvest_Class <= 0.75       ~ "Y5",
  #year_harvest_Class <= 0.6       ~ "Y6",
  #   year_harvest_Class <= 1       ~ "Y7"))
  #year_harvest_Class <= 0.8       ~ "Y8",
  # year_harvest_Class <= 1       ~ "Y9"))
  # year_harvest_Class <= 1         ~ "Y10"))
  
  # DEfine unique combination of groups
  GRID<-expand.grid(dep_Class=unique(CORES$dep_Class), year_Class=unique(CORES$year_harvest_Class))
  GRID<-unique(CORES$year_harvest_Class)
  
  # initialisation
  
  NCOMP <-NULL
  DEP   <-NULL
  ALL_DEP <-NULL
  
  
  for (i in 1: length(GRID)) {
    
    # Define list of dep and years for each iterations
    LIST_YEAR <- unlist(unique(CORES %>% filter(year_harvest_Class %in% GRID[i]) %>% dplyr::select(year_harvest)))
    
    # on définit les bases de calibration avec les X et les Y
    CAL     <- TAB[!TAB$year_harvest %in% LIST_YEAR, ]
    # on définit les bases de validation avec les X et les Y
    VAL    <- TAB[TAB$year_harvest %in% LIST_YEAR, ]
    
    VAL %<>%  filter(!departement %in% setdiff(unique(VAL$departement), unique(CAL$departement)))
    CAL %<>% filter(!departement %in% setdiff(unique(CAL$departement), unique(VAL$departement)))
    
    # Calibration
    CAL<-CAL                                             %>% 
      filter(!departement =="HAUTS-DE-SEINE")            %>% 
      mutate(GROUP= "group")                             %>% 
      group_by(GROUP)                                    %>% 
      nest()                                             %>%           # group
      rename(myorigdata  = data)                         %>%
      mutate(LOBELL      = furrr::future_map(myorigdata, model_Lin,     .progress = TRUE),          # run linear model
             RF          = furrr::future_map(myorigdata, model_RF ,     .progress = TRUE),           # Run the random Forest
             PLS         = furrr::future_map(myorigdata, model_PLS,     .progress = TRUE),           # Run the PLS model
             Lasso       = furrr::future_map(myorigdata, model_lasso,   .progress = TRUE),        # Run the lmer model
             Lassocv     = furrr::future_map(myorigdata, model_lassocv, .progress = TRUE),
             Ridge       = furrr::future_map(myorigdata, model_ridge,   .progress = TRUE),       # Run the ridge model
             Ridgecv     = furrr::future_map(myorigdata, model_ridgecv, .progress = TRUE),
             Elnet       = furrr::future_map(myorigdata, model_elnet,   .progress = TRUE),        # Run the Eldnet model
             Elnetcv     = furrr::future_map(myorigdata, model_elnetcv, .progress = TRUE),
             MCMC        = furrr::future_map(myorigdata, model_MCMC,    .progress = TRUE))# Run the Mcmc model
    
    
    #Calculate optimal number of components for the PLS model
    NCOMP<-list(NULL)
    for (j in 1: length(CAL$PLS)){
      NCOMP[[j]]  = selectNcomp(CAL$PLS[[j]], "onesigma", plot = FALSE)}
    
    ########################################################################################################################
    # Validation
    VAL<-VAL                                          %>% 
      mutate(GROUP="group")                           %>%
      group_by(GROUP)                                 %>% 
      nest()                                          %>% 
      rename(newdata = data)
    
    CAL_VAL <- CAL                                         %>% 
      full_join(VAL)    
    
    CAL_VAL<- cbind(CAL,VAL)
    
    GRID_2<-expand.grid(unique(CAL$departement))
    
    for(j in 1 :length(CAL_VAL$GROUP)){
      DEP[[j]]              <- CAL_VAL$newdata[[j]]

      DEP[[j]]$pred_MCMC   <- predict(CAL_VAL$MCMC[[j]],   newdata = CAL_VAL$newdata[[j]],marginal=NULL)[,1]
      DEP[[j]]$pred_LOB    <- predict(CAL_VAL$LOBELL[[j]], newdata = CAL_VAL$newdata[[j]])
      DEP[[j]]$pred_RF     <- predict(CAL_VAL$RF[[j]],     newdata = CAL_VAL$newdata[[j]])
      DEP[[j]]$pred_PLS    <- predict(CAL_VAL$PLS[[j]],    newdata = CAL_VAL$newdata[[j]], ncomp = max(1,NCOMP[[j]]))[,,1]
      DEP[[j]]$pred_Lasso  <- predict(CAL_VAL$Lasso[[j]],  newdata = CAL_VAL$newdata[[j]], s = CAL_VAL$Lassocv[[j]]$lambda.min)[,1]
      DEP[[j]]$pred_Ridge  <- predict(CAL_VAL$Ridge[[j]],  newdata = CAL_VAL$newdata[[j]], s = CAL_VAL$Ridgecv[[j]]$lambda.min)[,1]
      DEP[[j]]$pred_elnet  <- predict(CAL_VAL$Elnet[[j]],  newdata = CAL_VAL$newdata[[j]], s = CAL_VAL$Elnetcv[[j]]$lambda.min)[,1]
      
      DEP[[j]] %<>% 
        mutate(ROUND= i) %>% 
      dplyr::select(departement,year_harvest, yield, ROUND, contains("pred")) %>% 
        gather(model,pred, -year_harvest,-departement, -yield, -ROUND)
      
      DEP[[j]]$model <- gsub(".*_","",DEP[[j]]$model)
      
    }
    
    ALL_DEP[[i]] <- DEP %>%  bind_rows()
    DEP<- NULL; CAL<-NULL; VAL<-NULL; CAL_VAL<-NULL
    
    print(paste("round validation",i))
    
  }
  
  
  RES<- ALL_DEP %>%  bind_rows()
  
  # Calculate indicator of quality
  QUAL <- function(x) {
    QUAL <- gof(exp(x$pred), x$yield)
    return(QUAL)
  }
  
  QUALI<-RES %>% 
    group_by(model) %>% 
    do(as.data.frame(QUAL(.))) %>%
    mutate(IND=row.names(QUAL(RES) )) %>%
    spread(IND, V1)
  
  return(list(QUALI, RES))
}

