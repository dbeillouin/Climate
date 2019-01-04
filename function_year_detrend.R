function_year_detrend<- function(TAB){
  TAB  %<>%
    mutate(y58 = case_when(year_harvest <= 1959      ~ Var_mean_gs,
                           year_harvest > 1959      ~ .resid+.fitted),
           y65 = case_when(year_harvest <= 1965      ~ Var_mean_gs,
                           year_harvest > 1965      ~ .resid+.fitted[year_harvest==1965]),
           y70 = case_when(year_harvest <= 1970      ~ Var_mean_gs,
                           year_harvest > 1970      ~ .resid+.fitted[year_harvest==1970]),
           y75 = case_when(year_harvest <= 1975      ~ Var_mean_gs,
                           year_harvest > 1975      ~ .resid+.fitted[year_harvest==1975]),
           y78 = case_when(year_harvest <= 1978      ~ Var_mean_gs,
                           year_harvest > 1978      ~ .resid+.fitted[year_harvest==1978]),
           y80 = case_when(year_harvest <= 1980      ~ Var_mean_gs,
                           year_harvest > 1980      ~ .resid+.fitted[year_harvest==1980]),
           y82 = case_when(year_harvest <= 1982      ~ Var_mean_gs,
                           year_harvest > 1982      ~ .resid+.fitted[year_harvest==1982]))
  print(TAB)
}

