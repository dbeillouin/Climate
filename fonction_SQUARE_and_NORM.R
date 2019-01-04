function_SQUARE_AND_NORM<- function(TAB){
  TAB %<>%  setNames(gsub('_Var_mean_gs', "_OBS",names(.))) 
    
# Add square of each variables

CARRE<-data.frame(lapply(TAB                                %>%
                              dplyr::select_if(is.numeric)               %>%
                              dplyr::select(-contains(".fitted"),-contains(".resid"),
                                            -contains("anomaly"), - contains("prediction")),
                            "^",2))         %>%
            setNames(paste0(str_sub(names(.),-19,-2),"carre_",str_sub(names(.),-1,-1)))


TAB2 <- data.frame(cbind(TAB,CARRE))
                                    
# Calculate mean and sd of original data. 

MEAN_SD <- TAB2                                   %>%
  dplyr::select(method, contains("OBS"))          %>%
  group_by(method)                                %>% 
  summarise_all(funs(mean,sd),na.rm=TRUE)      %>%
  gather(variable,value, -method)             %>%
  mutate(type      = sub('.*_', '', variable),
         variable2  = gsub("__", "_",(gsub('OBS','',sub('_[m,s].*', "\\2", variable))))) %>% 
  filter(!method=="") %>% 
  select(-variable)  %>% 
  spread(type, value) 
MEAN_SD<- data.table(MEAN_SD)


# Add the mean and sd value to the detrended variables
SPREAD <- TAB2  %>%  dplyr::select(-contains(".resid"))           
SPREAD  <- data.table::melt(SPREAD,             # Same as described before
                     id.vars = c("departement", "method", "year_harvest", "year_harvescarre_t"))
SPREAD$variable2 <- SPREAD$variable
SPREAD$variable2 %<>% fct_relabel(~ gsub('__','_', gsub('OBS|y78|y80|y82|y58|y65|y70|y75|.fitted_',"", .x))) 

rm(list=setdiff(ls(), c("SPREAD","MEAN_SD")))
SPREAD<-data.table(SPREAD)
CLIMATE_NORM <- merge(SPREAD, MEAN_SD ,by= c("method","variable2"),allow.cartesian=TRUE)
CLIMATE_NORM[, value_norm := c(value - mean)/sd]

CLIMATE_NORM <- CLIMATE_NORM[, -c(2,7,8,9)]

CLIMATE_NORM<- data.table::dcast(CLIMATE_NORM, method +departement+year_harvest+year_harvescarre_t ~ variable, value.var = "value_norm")

 return(CLIMATE_NORM)
}
