
function_spread<- function(TAB){
  
  multi_join <- function(list_of_loaded_data, join_func, ...){
    require("dplyr")
    output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
    return(output)
  }
  
  
X <- split(TAB, paste(TAB$clim_var))                    # create a list of dataframe by climatic variable 
for (i in 1: length(X)) {                                     # The folowing code use data.table (faster than dplyr)
  
  ID.VARS<-intersect(names(TAB),  c("departement", "clim_var", "year_harvest", "method", "month"))
  
  A<- data.table::melt(data.table(X[[i]]),                   # Change format of the data
                       id.vars = ID.VARS,
                       measure.vars = c("Var_mean_gs",".fitted", ".resid","y58", "y65", "y70" , "y75" , "y78" ,"y80" ,"y82"))
 
  if(!is.na(match("month", names(TAB)))){
    A<-A[, variable :=  paste0(variable, "_",month)]
  A<-A[, !"month"]} else {}
    
  
  A<-dcast(A, departement+clim_var+year_harvest+method  ~ variable, value.var = "value")
  print(paste(A$sp[1], A$clim_var[1]))
  NAMES<- c(names(A)[1:4],                                # Change the column's name of each dataframe
            paste(unique(A$clim_var), names(A)[5:length(names(A))],sep="_"))
  
  names(A)<- NAMES
  X[[i]] <- A %>%  select(-clim_var) #A[, !"clim_var"] 
}

TAB<-multi_join(X,full_join)
return(TAB)
}
