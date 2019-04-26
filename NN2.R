## Réseau neuronaux pour prédire le rendement des cultures
# projet CLAND
# D.Beillouin, T Ben-Ari, D. Makowski
#17 Avril 2019
# Ceci est un example avec les rendement de l'orge en Fance
# nous analysons tous les département ensemble sur cet exemple
#################################################

rm(list=ls())
#On charge le package et le répertoire de travail
  library(keras)
  setwd("~/Documents/ClimateGIT")

# On charge la base de données
  #TAB2      <- read.csv("ESSAI_DB.csv")
  TAB      <- read.csv("YIELD_CLIMAT.csv")
  TAB$year_harvest<-(  TAB$year_harvest - mean(  TAB$year_harvest))/ sd(  TAB$year_harvest)
  TAB<-TAB[TAB$type=="OBS",]
  TAB<-TAB[TAB$method=="lin_EACH",]
  TAB<-TAB[!is.na(TAB$yield),]
  TAB<-TAB[!(TAB$yield==0),]
  TAB<- TAB[complete.cases(TAB), ]
  
  #TAB<-TAB[TAB$departement %in% levels(TAB$departement)[1:6],]
  
  TAB$departement <- as.numeric(TAB$departement)
  TAB$departement <- to_categorical(TAB$departement)
  #TAB      <- read.csv("ESSAI_DB_scale.csv")
  TAB$yield <- log(TAB$yield)
# TAB$yield<- ( TAB$yield- mean( TAB$yield))/sd( TAB$yield)
#write.csv(TAB,"ESSAI_DB_scale.csv")


# liste des variables analysées (je sur sur ma BD avec des variables mensuelles)
# je choisi Tmean, ETP, et PR pour les mois de janvier à juin.
  LIST_VAR <- c("year_harvest","departement",
                "Tmean_1","Tmean_2","Tmean_3","Tmean_4","Tmean_5","Tmean_6","Tmean_7",
                "PR_1","PR_2", "PR_3", "PR_4", "PR_5", "PR_6","PR_7",
                "ETP_1", "ETP_2", "ETP_3", "ETP_4", "ETP_5", "ETP_6","ETP_7",
                "Tn_1", "Tn_2","Tn_3", "Tn_4","Tn_5", "Tn_6","Tn_7",
                "Tx_1", "Tx_2","Tx_3", "Tx_4","Tx_5", "Tx_6","Tx_7",
                "RV_1", "RV_2","RV_3", "RV_4","RV_5", "RV_6","RV_7",
                "Seq.PR_1", "Seq.PR_2","Seq.PR_3", "Seq.PR_4","Seq.PR_5", "Seq.PR_6","Seq.PR_7")
                

# on restreint la base de données à ces variables 
  TAB       <- TAB[,c(LIST_VAR,"yield")]

# On crée des groupes pour les validations croisées
# Je fais des groupes aléatoires sur les années. 
  set.seed(32)
  CORES    <- data.frame(YEARS=unique(TAB$year_harvest))
  CORES$year_harvest_Class <- runif(n=length(CORES$YEARS), min=1e-12, max=.9999999999) 

  CORES$CLASS[CORES$year_harvest_Class <= 1]    <- "Y4"
  CORES$CLASS[CORES$year_harvest_Class <= 0.75] <- "Y3"
  CORES$CLASS[CORES$year_harvest_Class <= 0.50] <- "Y2"
  CORES$CLASS[CORES$year_harvest_Class <= 0.25] <- "Y1"

# on définit les objects pour notre boucle
  FOLD     <- unique(CORES$CLASS)
  CAL_FOLD <- NULL
  VAL_FOLD <- NULL

# on fait tourner le NN sur les groupes d'années (validation croisée)
  for (i in 1: length(FOLD)) {
  
  # PARTIE 1
    # liste des années considerees dans la boucle i
      LIST_YEAR <- CORES[CORES$CLASS %in% FOLD[i],"YEARS"]

    # on définit les bases de calibration avec les X et les Y
      CAL     <- TAB[!TAB$year_harvest %in% LIST_YEAR, ]
      X_train <- as.matrix(CAL[, LIST_VAR])
      y_train <- as.matrix(CAL[,c("yield") ])

   # on définit les bases de validation avec les X et les Y
      VAL    <- TAB[TAB$year_harvest %in% LIST_YEAR, ]
      X_test <- as.matrix(VAL[, LIST_VAR])

  #PARTIE2
    # on défnit notre modèle
      model <- keras_model_sequential() 
  
    # premier type de modèle
      
      model %>% 
        layer_dense(units = 32, activation = 'relu',
                    kernel_initializer='RandomNormal',
                    input_shape = c(length(X_train[1,]))) %>% 
        layer_dropout(rate = 0.2) %>% 
        layer_dense(units = 16, activation = 'relu') %>%
         layer_dropout(rate = 0.2) %>% 
        layer_dense(units = 1, activation = 'linear')
      
      
      
      # model %>% 
      #  layer_dense(units = 64, activation = 'linear',
      #             kernel_initializer='RandomNormal',
      #             input_shape = c(length(LIST_VAR))) %>%
      #  # layer_dropout(rate = 0.2) %>% 
      # layer_dense(units = 32, activation = 'linear') %>%
      #   layer_dropout(rate = 0.2) %>% 
      #   layer_dense(units = 1, activation = 'linear') 
      # 
   #     layer_dropout(rate = 0.2) %>% 
  #    layer_dense(units = 1, activation = 'linear')

  # # deuxième type de modèle
  # model <- keras_model_sequential() %>% 
  #   layer_dense(units = 16, activation = "relu", 
  #               input_shape = c(length(LIST_VAR))) %>% 
  #   layer_dense(units = 16, activation = "relu") %>% 
  #   layer_dense(units = 1) 
  
    # on vérifie la structure du modèle
     summary(model)

  # PARTIE 3
    # on paramètre la compilation et on paramètre le modèle
      model %>% compile(
        loss = 'mean_squared_error',    
        optimizer = 'sgd',              # quel optimizer choisir? 
        metrics = c('mse')              # Quelle métrique choisir? mse? mae? 
       )
    
      history <- model %>% fit(
        X_train, y_train,
        batch_size=1,            #  A t-on besoin de faire une validation croisée (ici= notre boucle), ou alors la fonction fit se base déja sur une VC pour ajuster les pramètres?  
        epochs = 5)             # doit-on fixer epoch à une valeur telle que le mean square error n'évolue plus?     
       # validation_split = 0.3 )

  #PARTIE 4
    # qualité d'estimation
      CAL$pred<- exp(data.frame(y = predict(model, as.matrix(X_train)))[,1])
      CAL$color=i
      plot(exp(CAL$yield), CAL$pred)
      abline(0,1)
      library(hydroGOF)
      gof(CAL$pred, CAL$yield)
    
    # qualité de prédiction
      VAL$pred<- exp(data.frame(y = predict(model, as.matrix(X_test)))[,1])
      VAL$color=i
      plot(exp(VAL$yield), VAL$pred)
      abline(0,1)
      gof(CAL$pred, CAL$yield)
    
      CAL_FOLD[[i]]<- CAL
      VAL_FOLD[[i]]<- VAL
}

# Analyse globale 

# Qualité d'estimation sur cross validation pour l'ensemble des FOLD
    CAL_FOLD <- rbind(rbind(rbind(CAL_FOLD[[1]],CAL_FOLD[[2]]),CAL_FOLD[[3]]),CAL_FOLD[[4]])
    plot(exp(CAL_FOLD$yield), CAL_FOLD$pred, col=as.factor(CAL_FOLD$color), pch=16,main= "qualité d'estimation \n cross Validation sur les années",xlab="Observations", ylab="Predictions")
    abline(0,1)
    gof(CAL$pred, exp(CAL$yield))
    # MSE= 0?18
    # RMSE= 0?43
    #R2= 0.96


# Qualité de prédiction sur cross validation pour l'ensemble des FOLD
    VAL_FOLD <- rbind(rbind(rbind(VAL_FOLD[[1]],VAL_FOLD[[2]]),VAL_FOLD[[3]]),VAL_FOLD[[4]])
    plot(exp(VAL_FOLD$yield), VAL_FOLD$pred,col=as.factor(VAL_FOLD$color), pch=16, main= "Qualité de prédiction \n cross Validation sur les années", xlab="Observations", ylab="Predictions")
    abline(0,1)
    gof(VAL$pred, VAL$yield)
    
    # MSE= 6.34
    # RMSE= 2.52
    #R2= 0.56
