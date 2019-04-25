## Damien Beillouin
# fonction pour faire  la prédiction
# modèles calibrés département par département. 
################################################


function_Yield_predict_CC <- function(TAB){
  
  # on normalise les années
  TAB$year_harvest      <- data.frame(scale(TAB$year_harvest))[,1]
  TAB$year_harvestcarre <- data.frame(scale(TAB$year_harvestcarre))[,1]
  
  # on sélectionne les données
  TAB %<>% filter(!is.na(yield))
  TAB %<>% filter(!yield==0)
  TAB %<>% dplyr::select(-contains("prediction"), - contains("anomaly"))
  TAB<- TAB[complete.cases(TAB), ]
  
  # load packages
  set.seed(42)
  x<- c("keras","reticulate","glmnet","TigR","randomForest","MCMCglmm","lme4","broom", "classInt", "stringr","maptools","hydroGOF", "pls", "glmnetUtils")
  lapply(x, require, character.only = TRUE)
  

  # initialisation
  
  NCOMP <-NULL
  DEP   <-NULL
  ALL_DEP <-NULL
  

    # Calibration
    CAL<-TAB                                             %>% 
      filter(!departement =="HAUTS-DE-SEINE")            %>%
      filter(type=="OBS")                                %>%
      group_by(departement, type, method)                %>% 
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
    VAL<-TAB                                            %>% 
      filter(!departement =="HAUTS-DE-SEINE")           %>% 
      group_by(type, departement, method)               %>%
      nest()                                            %>%
      rename(newdata = data)
    
    CAL_VAL <- CAL                                      %>% 
      full_join(VAL, by = c("departement", "method"))    
    
    NCOMP<-rep(NCOMP, each=length(unique(CAL_VAL$type.y)))
  
    for(j in 1 :length(CAL_VAL$departement)){
      DEP[[j]]              <- CAL_VAL$newdata[[j]]
      DEP[[j]]$departement  <- CAL_VAL$departement[[j]]
      DEP[[j]]$type         <- CAL_VAL$type.y[[j]]
      DEP[[j]]$method       <- CAL_VAL$method[[j]]
      
      
      DEP[[j]]$pred_MCMC   <- predict(CAL_VAL$MCMC[[j]],   newdata = CAL_VAL$newdata[[j]],marginal=NULL)[,1]
      DEP[[j]]$pred_LOB    <- predict(CAL_VAL$LOBELL[[j]], newdata = CAL_VAL$newdata[[j]])
      DEP[[j]]$pred_RF     <- predict(CAL_VAL$RF[[j]],     newdata = CAL_VAL$newdata[[j]])
      DEP[[j]]$pred_PLS    <- predict(CAL_VAL$PLS[[j]],    newdata = CAL_VAL$newdata[[j]], ncomp = max(1,NCOMP[[j]]))[,,1]
      DEP[[j]]$pred_Lasso  <- predict(CAL_VAL$Lasso[[j]],  newdata = CAL_VAL$newdata[[j]], s = CAL_VAL$Lassocv[[j]]$lambda.min)[,1]
      DEP[[j]]$pred_Ridge  <- predict(CAL_VAL$Ridge[[j]],  newdata = CAL_VAL$newdata[[j]], s = CAL_VAL$Ridgecv[[j]]$lambda.min)[,1]
      DEP[[j]]$pred_elnet  <- predict(CAL_VAL$Elnet[[j]],  newdata = CAL_VAL$newdata[[j]], s = CAL_VAL$Elnetcv[[j]]$lambda.min)[,1]
      
      DEP[[j]] %<>% 
        mutate(ROUND= i) %>% 
        dplyr::select(departement,year_harvest, yield, ROUND,type, method, contains("pred")) %>% 
        gather(model,pred, -departement, -year_harvest, -yield, -ROUND, -type, -method)
      
      DEP[[j]]$model <- gsub(".*_","",DEP[[j]]$model)
      
    }
    
  
    RES<- DEP %>%  bind_rows()
    DEP<- NULL; CAL<-NULL; VAL<-NULL; CAL_VAL<-NULL
  
  # Calculate indicator of quality
  QUAL <- function(x) {
    QUAL <- gof(exp(x$pred), x$yield)
    return(QUAL)
  }
  
  QUALI<-RES %>% 
    group_by(model, type, method) %>% 
    do(as.data.frame(QUAL(.))) %>%
    mutate(IND=row.names(QUAL(RES) )) %>%
    spread(IND, V1)
  
  return(list(QUALI, RES))
}

