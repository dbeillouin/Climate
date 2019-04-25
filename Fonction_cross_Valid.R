## Damien Beillouin
# fonction pour faire un validation croisée sur les années
# modèles calibrés département par département. 
################################################


function_Cross_VALID <- function(TAB){
  
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
    group_by(departement)                              %>% 
    nest()                                             %>%           # group
    rename(myorigdata  = data)                          %>%
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
      filter(!departement =="HAUTS-DE-SEINE")         %>% 
    group_by(type, departement)                       %>%
    nest()                                            %>%
    rename(newdata = data)
  
   CAL_VAL <- CAL                                         %>% 
    full_join(VAL, by = c("departement"))    

  
   GRID_2<-expand.grid(unique(CAL$departement))

  for(j in 1 :length(CAL_VAL$departement)){
     DEP[[j]]              <- CAL_VAL$newdata[[j]]
     DEP[[j]]$departement <-  CAL_VAL$departement[[j]]

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
       gather(model,pred, -departement, -year_harvest, -yield, -ROUND)

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

