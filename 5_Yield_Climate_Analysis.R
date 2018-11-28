
rm(list=ls())

# load packages
library(tidyr); library(dplyr)
library(gsubfn); library(maptools)
library(classInt); library(stringr)


############# I/ DATALOAD  ##############################################################################



###I/ Load data and path
path              <- "/Users/Damien_Beillouin/Documents/Stage_JULIA/codes_julia"
map               <- readShapeSpatial(paste0(path,'/Data_shape/DEPARTEMENT_france.shp'))
Resultfolder      <- "/Users/Damien_Beillouin/Documents/Stage_JULIA/codes_julia/Results"
Merged_file_Path  <- "/Users/Damien_Beillouin/Documents/Stage_JULIA/codes_julia/Results"

# list of files
TAB  <- dir(Resultfolder, pattern = "TABLE_MERGED",full.names=FALSE) 
TAB  <- Filter(function(x) grepl("4month", x), TAB)

# load a file for one specie
TABLE.tot     <-read.csv(paste0(Resultfolder,"/",TAB[2]), header=TRUE,  sep=";", dec=",", na.strings="NA") %>%
                filter(!is.na(fitted_lm))    #### a modifier!!!

############### II Change the format of the data########################  
RAW_CLIM <- TABLE.tot %>%
  select(year_harvest, departement,yield,area,production,clim_var,Var_mean_gs)%>%
  spread(clim_var, "Var_mean_gs")

lobell_CLIM<- TABLE.tot %>%
  select(year_harvest, departement,clim_var,matches("lobell"))%>%
  gather(key=variable,value=value,-departement,-year_harvest,-clim_var) %>%
  mutate(variable= paste(clim_var,variable,sep="_")) %>%
  select(-clim_var)%>%
  spread_(key_ = "variable",value_ = "value") 

TABLE.spread<-full_join(RAW_CLIM,lobell_CLIM,by=c("year_harvest", "departement"))%>%
              mutate(yield=replace(yield, yield==0, 0.0000000001)) %>%
              mutate(yield = as.numeric(as.character(yield)))  %>%
              mutate(Tmean = as.numeric(as.character(Tmean)))  %>%
              mutate(PR = as.numeric(as.character(PR)))        %>%
                  filter(!is.na(yield)) ### j'ai des Na dans les valeurs de yields!!!!!
  

# CONTENT OF THE TABLE
# "year_harvest"  	"departement"     	 		# Growing season year computed to taken into account Winter/spring crops       
# "yield", "area"	"production"           		# Observations for yield production and climate
# "prediction_poly / _loess / _spline   		# Average trend yield from poly (best of 3) and loess
# "anomaly_poly / _loess / _spline 				# raw anomaly for each model
# "anomaly_stand_poly / _loess / _spline  		# Anomalies normalized by the average for each model
# "anomaly_norm_poly / _loess / _spline  		# Anomalies standardized by the time standard deviation for each model
# "clim_var" & "Var_mean_gs"          			# Name of climate variable & value as mean/sum over the growing season
# "residuals_lm/loess", "fitted_lm/loess"   	# Linear/loess model fitted for each climate variable over the totality of the time period (eg, 1958-2016 fr Safran)      
# "lobell.first/65/70/78/80/82 _ lm/loess       # Reconstructed no Climate change time series for each variable for various start years and the two models

# Lobell's model is written as:
# Log(Y) ~ year + year2 + Temp + Temp2 + Precip + Precip2 with fixed effects on departements
model<-glm(log(yield)~ departement*year_harvest + departement*I(year_harvest^2) + Tmean + Tmean^2 + PR + PR^2 ,weights=area,data=TABLE.spread)

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
              select(id, departement,year_harvest)             %>%
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
     select (departement,year_harvest,matches("predict"))
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
  select(-year_harvest,-predict) %>%
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






