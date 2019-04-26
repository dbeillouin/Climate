# Define models relating crop yield and climate

# on sélectionne les variables à prendre en compte
factors<- 
  intersect(c("Tmean_1","Tmean_2","Tmean_3","Tmean_4","Tmean_5","Tmean_6","Tmean_7","Tmean_8",
          "PR_1", "PR_2","PR_3","PR_4","PR_5","PR_6","PR_7","PR_8",
          "ETP_1","ETP_2","ETP_3","ETP_4","ETP_5","ETP_6","ETP_7","ETP_8",
          "RV_1", "RV_2","RV_3","RV_4","RV_5","RV_6","RV_7","RV_8"), names(YIELD_CLIMAT))


# même chose mais pour le modèle LOBELL
VEC<-names(YIELD_CLIMAT)[grepl("Tmean", names(YIELD_CLIMAT))]
VEC<-VEC[!grepl("carre", VEC)]
FOUR_LAST<- order(sub(".*_", "", VEC), decreasing=TRUE)[1:4]

factors2<- 
  intersect(c("Tmean_1","Tmean_2","Tmean_3","Tmean_4","Tmean_5","Tmean_6","Tmean_7","Tmean_8",
              "PR_1", "PR_2","PR_3","PR_4","PR_5","PR_6","PR_7","PR_8",
              "Tmean_1carre","Tmean_2carre","Tmean_3carre","Tmean_4carre","Tmean_5carre","Tmean_6carre","Tmean_7carre","Tmean_8carre",
              "PR_1carre", "PR_2carre","PR_3carre","PR_4carrecarre","PR_5carre","PR_6carre","PR_7carre","PR_8carre"), names(YIELD_CLIMAT))
factors2<-factors2[grepl(paste(FOUR_LAST,collapse="|"), factors2)]


# on défini nos modèles

# linear model 
model_Lin      <-function(DAT) {model<-glm(as.formula(paste("log(yield)~", paste(factors2, collapse="+"))) ,data=DAT)}

# random forest avec l'ensemble des variables
model_RF       <-function(DAT) {model<- randomForest(as.formula(paste("log(yield)~", paste(factors, collapse="+"))),
                                                     ntree=500,mtry=10,data=DAT, proximity=TRUE, oob.prox=TRUE)}

model_PLS      <- function(DAT) {model<-plsr(as.formula(paste("log(yield)~", paste(factors, collapse="+"))), data = DAT,
                                             validation = "LOO")}

model_lasso   <-function(DAT) {model<- glmnet(as.formula(paste("log(yield)~", paste(factors, collapse="+"))), family="gaussian", alpha=1, data=DAT)}

model_lassocv <-function(DAT) {model<- cv.glmnet(as.formula(paste("log(yield)~", paste(factors, collapse="+"))), family="gaussian", alpha=1, data=DAT)}

model_ridge   <-function(DAT) {model<- glmnet(as.formula(paste("log(yield)~", paste(factors, collapse="+"))), family="gaussian", alpha=0, data=DAT)}

model_ridgecv <-function(DAT) {model<- cv.glmnet(as.formula(paste("log(yield)~", paste(factors, collapse="+"))), family="gaussian", alpha=0, data=DAT)}

model_elnet  <-function(DAT) {model<- glmnet(as.formula(paste("log(yield)~", paste(factors, collapse="+"))), family="gaussian", alpha=0.5, data=DAT)}

model_elnetcv<-function(DAT) {model<- cv.glmnet(as.formula(paste("log(yield)~", paste(factors, collapse="+"))), family="gaussian", alpha=0.5, data=DAT)}

model_MCMC  <-function(DAT) {model<-MCMCglmm(as.formula(paste("log(yield)~", paste(factors2, collapse="+"))),
                                             data=DAT, pr=TRUE,burnin=1000, nitt=3000)}
