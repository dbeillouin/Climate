function_Cross_VALID_ALL_DEP_ALL_MONTH<- function(TAB){
  TAB$year_harvest<-data.frame(scale(TAB$year_harvest))[,1]
  TAB$year_harvestcarre<-data.frame(scale(TAB$year_harvestcarre))[,1]
  
  set.seed(42)
  # load packages
  x<- c("glmnet","TigR","MCMCglmm","lme4","broom", "classInt", "stringr","maptools","hydroGOF", "pls", "glmnetUtils")
  lapply(x, require, character.only = TRUE)
  #rm(list=ls())
  
  # Define models relating crop yield and climate
  model_Lin      <-function(DAT) {model<-glm (log(yield)  ~ year_harvest +I(year_harvest^2)+ Tmean_+ I(Tmean_)^2 + PR_ +I(PR_)^2     ,data=DAT)}
  
  
  model_RF       <-function(DAT) {model<- randomForest(log(yield)~ year_harvest+
                                                         Tmean_+
                                                         PR_+
                                                         ETP_+
                                                         RV_,
                                                       ntree=500,mtry=10,data=DAT, proximity=TRUE, oob.prox=TRUE)}
  
  model_PLS      <- function(DAT) {model<-plsr(log(yield) ~ year_harvest+
                                                 Tmean_+
                                                 PR_+
                                                 ETP_+
                                                 RV_, data = DAT,
                                               validation = "CV")}
  
  model_lasso   <-function(DAT) {model<- glmnet(log(yield)~ year_harvest+
                                                  Tmean_+
                                                  PR_+
                                                  ETP_+
                                                  RV_, family="gaussian", alpha=1, data=DAT)}
  
  model_lassocv <-function(DAT) {model<- cv.glmnet(log(yield)~ year_harvest+
                                                     Tmean_+
                                                     PR_+
                                                     ETP_+
                                                     RV_, family="gaussian", alpha=1, data=DAT)}
  
  
  model_ridge   <-function(DAT) {model<- glmnet(log(yield)~ year_harvest+
                                                  Tmean_+
                                                  PR_+
                                                  ETP_+
                                                  RV_, family="gaussian", alpha=0, data=DAT)}
  
  model_ridgecv <-function(DAT) {model<- cv.glmnet(log(yield)~ year_harvest+
                                                     Tmean_+
                                                     PR_+
                                                     ETP_+
                                                     RV_, family="gaussian", alpha=0, data=DAT)}
  
  model_elnet  <-function(DAT) {model<- glmnet(log(yield)~year_harvest+
                                                 Tmean_+
                                                 PR_+
                                                 ETP_+
                                                 RV_, family="gaussian", alpha=0.5, data=DAT)}
  
  model_elnetcv<-function(DAT) {model<- cv.glmnet(log(yield)~ year_harvest+
                                                    Tmean_+
                                                    PR_+
                                                    ETP_+
                                                    RV_, family="gaussian", alpha=0.5, data=DAT)}
  
  
  model_MCMC  <-function(DAT) {model<-MCMCglmm(log(yield) ~year_harvest+
                                                 Tmean_+
                                                 PR_+
                                                 ETP_+
                                                 RV_, random = ~us(1+year_harvest), data=DAT, pr=TRUE)}
  
  
  # model_MCMC  <-function(DAT) {model<-MCMCglmm(log(yield) ~
  #                                                Tmean_1 +Tmean_2+Tmean_3+Tmean_4+Tmean_5+Tmean_6+Tmean_7+Tmean_8+
  #                                                PR_1 +PR_2+PR_3+PR_4+PR_5+PR_6+PR_7+PR_8+
  #                                                ETP_1 +ETP_2+ETP_3+ETP_4+ETP_5+ETP_6+ETP_7+ETP_8+
  #                                                RV_1 +RV_2+RV_3+RV_4+RV_5+RV_6+RV_7+RV_8, random = ~us(1+year_harvest                                                          ): departement, data=DAT, pr=TRUE)}
  ### CROSS VALIDATION
    ## define groups of year and groups of departemnt
  CORES<-TAB %>% dplyr::select(year_harvest) %>%  unique()
  CORES$year_harvest_Class<- runif(n=nrow(CORES), min=1e-12, max=.9999999999) 
  CORES<-CORES %>% 
    mutate(year_harvest_Class = case_when(year_harvest_Class <= 0.5      ~ "Y1",
                                          year_harvest_Class > 0.5       ~ "Y2"))
  
  # DEfine unique combination of groups
  GRID<-expand.grid(dep_Class=unique(CORES$dep_Class), year_Class=unique(CORES$year_harvest_Class))
  GRID<-unique(CORES$year_harvest_Class)
  
  # initialisation
  TT1<-NULL;TT2<-NULL;TT3<-NULL;TT4<-NULL; TT5<- NULL; TT6<- NULL; TT7<- NULL; TT8<- NULL; TT9<- NULL
  DB<- NULL;PLS<- NULL;Lasso<-NULL;Ridge<- NULL; Elnet<- NULL; Pls <- NULL; Mcmc<- NULL
  LASSO<- NULL; RIDGE<- NULL; ELNET <- NULL; PLS<- NULL; MCMC<- NULL
  
  for (i in 1: length(GRID)) {
    
    # Define list of dep and years for each iterations
    LIST_YEAR <- unlist(unique(CORES %>% filter(year_harvest_Class %in% GRID[i]) %>% dplyr::select(year_harvest)))
    
    TAB %<>% filter(!is.na(yield))
    TAB %<>% filter(!yield==0)
    TAB %<>% dplyr::select(-contains("prediction"), - contains("anomaly"))
    
    TAB <- TAB   %>% filter(!is.na(Tmean_))
    TAB <- TAB   %>% filter(!is.na(PR_))
    TAB <- TAB   %>% filter(!is.na(ETP_))
    TAB <- TAB   %>% filter(!is.na(RV_))
    TAB <- TAB   %>% filter(!is.na(PR_))
    
    
    
    # define calibration and validation databases
    CAL <- TAB                    %>% 
      filter(!year_harvest %in% LIST_YEAR) 
    
    VAL <- TAB                    %>% 
      filter(year_harvest %in% LIST_YEAR)
    
    VAL %<>%  filter(!departement %in% setdiff(unique(VAL$departement), unique(CAL$departement)))
    CAL %<>% filter(!departement %in% setdiff(unique(CAL$departement), unique(VAL$departement)))
    
    # Calibration
    BD1<-CAL                                             %>% 
      filter(type=="OBS")                                %>%
      filter(!departement =="HAUTS-DE-SEINE")            %>% 
      group_by(method)                                   %>% 
      nest()                                             %>%           # group
      rename(myorigdata = data)                          %>%
      mutate(LOBELL     = purrr::map(myorigdata, model_Lin),          # run linear model
             OR_LOBELL  = purrr::map2(LOBELL, myorigdata, predict),   
             RF         = purrr::map(myorigdata, model_RF),           # Run the random Forest
             OR_RF      = purrr::map2(RF, myorigdata, predict),
             PLS        = purrr::map(myorigdata,model_PLS),           # Run the PLS model
             Lasso       =purrr::map(myorigdata, model_lasso),        # Run the lmer model
             Lassocv     =purrr::map(myorigdata, model_lassocv),
             OR_Lasoo    = furrr::future_map2(Lasso, myorigdata, predict,.progress = TRUE),
             Ridge       = furrr::future_map(myorigdata, model_ridge,.progress = TRUE),       # Run the ridge model
             Ridgecv     = furrr::future_map(myorigdata, model_ridgecv,.progress = TRUE),
             OR_Ridge    = furrr::future_map2(Ridge, myorigdata, predict,.progress = TRUE), 
             Elnet       = furrr::future_map(myorigdata, model_elnet,.progress = TRUE),        # Run the Eldnet model
             Elnetcv     = furrr::future_map(myorigdata, model_elnetcv,.progress = TRUE),
             OR_Elnet    = furrr::future_map2(Elnet, myorigdata, predict,.progress = TRUE))
             #MCMC        = furrr::future_map(myorigdata, model_MCMC,.progress = TRUE))         # Run the Mcmc model
  
  
    # Calculate optimal number of components for the PLS model
    NCOMP<-list(NULL)
    for (j in 1: length(BD1$PLS)){
      NCOMP[[j]]  = selectNcomp(BD1$PLS[[j]], "onesigma", plot = FALSE)}
    
    # Validation
    BD2<-VAL                                            %>% 
      filter(type=="OBS")                               %>%
      group_by(method, type)                            %>%
      nest()                                            %>%
      rename(newdata = data)
    
    LIST <- BD1                                         %>% 
      full_join(BD2, by = c("method"))                  %>%            # Join VAL and CAL database
      mutate(new_Lobell   = purrr::map2(LOBELL, newdata, predict),     # Predict for VAL database: linear model
             new_RF       = furrr::future_map2(RF, newdata, predict))  # Predict for VAL database: Random Forest model
    
    # Export the predictions
    RES <- LIST %>%  bind_rows() 
    TT1[[i]]  <- RES %>% unnest(newdata, new_Lobell)   %>% mutate(model="new_Lobell") %>% 
      rename(pred=new_Lobell) %>% mutate(ROUND= i) %>% 
      dplyr::select(method,type,pred,departement,year_harvest, yield,model, ROUND)
    TT2[[i]]  <- RES %>% unnest(newdata, new_RF)   %>% mutate(model="new_RF") %>%
      rename(pred=new_RF) %>% mutate(ROUND= i) %>% 
      dplyr::select(method,type,pred,departement,year_harvest, yield,model, ROUND)
    
    
    GRID_2<-expand.grid(unique(BD1$method))
    for(k in 1 :dim(GRID_2)[1]){
      DB[[k]] <- VAL %>% 
        filter(type=="OBS") %>%
        filter(method %in%  GRID_2$Var1[k]) 
      
      # MCMC[[k]]       <- DB[[k]]
      # MCMC[[k]]$pred  <-predict(LIST$MCMC[[k]], newdata=MCMC[[k]],marginal=NULL)
      # MCMC[[k]]$model <- "MCMC"
      # MCMC[[k]]       %<>% mutate(ROUND= i) %>% dplyr::select(method,type,pred,departement,year_harvest, yield,model, ROUND)
      # 
      
      Pls[[k]]       <- DB[[k]]
      Pls[[k]]$pred  <- data.frame(predict(LIST$PLS[[k]], newdata =  Pls[[k]], ncomp = min(1,NCOMP[[k]])))[,1]
      Pls[[k]]$model <- "PLS"
      Pls[[k]]       %<>% mutate(ROUND= i) %>% dplyr::select(method,type,pred,departement,year_harvest, yield,model, ROUND)
      
      
      Lasso[[k]]       <- DB[[k]]
      Lasso[[k]]$pred  <- predict(LIST$Lasso[[k]], newdata =  Lasso[[k]], s = BD1$Lassocv[[k]]$lambda.min)[,1]
      Lasso[[k]]$model <- "Lasso"
      Lasso[[k]]       %<>% mutate(ROUND= i) %>% dplyr::select(method,type,pred,departement,year_harvest, yield,model, ROUND)
      
      
      Ridge[[k]]       <- DB[[k]]
      Ridge[[k]]$pred  <- predict(LIST$Ridge[[k]], newdata =  Ridge[[k]], s = BD1$Ridgecv[[k]]$lambda.min)[,1]
      Ridge[[k]]$model <- "Ridge"
      Ridge[[k]]       %<>% mutate(ROUND= i) %>% dplyr::select(method,type,pred,departement,year_harvest, yield,model, ROUND)
      
      Elnet[[k]]       <- DB[[k]]
      Elnet[[k]]$pred  <- predict(LIST$Elnet[[k]], newdata =Elnet[[k]], s = BD1$Elnetcv[[k]]$lambda.min)[,1]
      Elnet[[k]]$model <- "Elnet"
      Elnet[[k]]       %<>% mutate(ROUND= i) %>% dplyr::select(method,type,pred,departement,year_harvest, yield,model, ROUND)
      
    }
    
    PLS[[i]] <- Pls %>%  bind_rows() ; Pls<- NULL
    LASSO[[i]] <- Lasso %>%  bind_rows() ; Lasso<- NULL
    RIDGE[[i]] <- Ridge %>%  bind_rows() ; Ridge<- NULL
    ELNET[[i]] <- Elnet %>%  bind_rows() ; Elnet<- NULL
    #MCMC[[i]]  <- Mcmc  %>%  bind_rows() ; Mcmc <- NULL
    
    rm(RES)
    rm(BD1,BD2)
    rm(CAL); rm(VAL)
    print(paste("round validation",i))
  }
  
  
  # concatene results for all the iterations
  for (t in length(GRID) ){
    PLS[[t]]   %>%  bind_rows() 
    LASSO[[t]] %>%  bind_rows() 
    RIDGE[[t]] %>%  bind_rows() 
    ELNET[[t]] %>%  bind_rows() 
    #MCMC[[t]]  %>%  bind_rows() 
  }
  
  RES1  <- TT1   %>%  bind_rows() 
  RES2  <- TT2   %>%  bind_rows() 
  RES8  <- LASSO %>%  bind_rows() 
  RES9  <- RIDGE %>%  bind_rows() 
  RES10 <- ELNET %>%  bind_rows() 
  RES11 <- PLS %>%  bind_rows() 
  # RES11 <- MCMC  %>%  bind_rows() 
  
  RES<-list(RES1, RES2,RES8,RES9,RES10, RES11) %>% bind_rows()
  
  
  # Calculate indicator of quality
  QUAL <- function(x) {
    QUAL <- gof(exp(x$pred), x$yield)
    return(QUAL)
  }
  
  QUALI<-RES %>% 
    group_by(method, model, type) %>% 
    do(as.data.frame(QUAL(.))) %>%
    mutate(IND=row.names(QUAL(RES) )) %>%
    spread(IND, V1)
  
  return(list(QUALI, RES))
}