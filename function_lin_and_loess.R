function_lin_AND_loess<- function(TAB){
  
  multi_join <- function(list_of_loaded_data, join_func, ...){
    require("dplyr")
    output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
    return(output)
  }
  
  ## Define the types of models
  model_lin     <-function(DAT) {lm(Var_mean_gs~year_harvest, data=DAT)}
  model_loess   <-function(DAT) {loess(Var_mean_gs~year_harvest, data=DAT)}
  model_spline  <-function(DAT) {smooth.spline(DAT$year_harvest,DAT$Var_mean_gs)}
  
  
  TAB1 <-TAB                                                                      %>%  
  nest()                                                                          %>% 
  mutate(lin            = furrr::future_map(data, model_lin  ,.progress = TRUE),      # linear model
         pred_lin       = furrr::future_map(lin, augment     ,.progress = TRUE),
         loess          = furrr::future_map(data, model_loess,.progress = TRUE),      # loess model
         pred_loess     = furrr::future_map(loess, augment   ,.progress = TRUE))

lin_EACH   <- TAB1  %>%   unnest(pred_lin, .drop = TRUE)                          %>%     # Extract results of the lin model
  dplyr::select(departement, month, clim_var,year_harvest, Var_mean_gs,.fitted,.resid)   %>%     # Choose variable
  mutate(method="lin_EACH")                                                           # Create a key

loess_EACH <- TAB1  %>%   unnest(pred_loess, .drop = TRUE) %>%               # Extract results of the loess model
  dplyr::select(departement, clim_var,month, year_harvest,Var_mean_gs, .fitted,  .resid ) %>%   # Choose variable
  mutate(method="loess_EACH")  

select2    <- TAB                                                                  %>%
  summarise(IQR = IQR(Var_mean_gs))                                                %>%
  mutate(ID =paste(clim_var,departement))                                          %>%
  filter(IQR==0)

TAB2      <-TAB %>%  filter(!ID %in% select2$ID)

TAB2 %<>%    group_by(clim_var,departement, month)                                 %>%
  nest()                                                                           %>%
  mutate(spline         = furrr::future_map(data, model_spline,.progress = TRUE),     # spline model
         pred_spline    = furrr::future_map(spline, augment   ,.progress = TRUE))



spline_EACH <- TAB2  %>%   unnest(pred_spline, .drop = TRUE)                       %>%  # Extract results of the loess model
  rename( year_harvest = x, Var_mean_gs= y) %>% 
  dplyr::select(departement, month, clim_var, year_harvest,Var_mean_gs, .fitted,  .resid) %>%  # Choose variable
  mutate(method="sline_EACH")

TAB<-multi_join(list(lin_EACH, loess_EACH,spline_EACH),full_join)
}