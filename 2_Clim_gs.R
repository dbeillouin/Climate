
## Data frame: climate variables per species 
## Using the growing seasons  
## => per MONTHs since 1958     

rm(list=ls())


#### I/ DATALOAD  ##############################################################################
setwd("~/Documents/CLAND/files_txt/")


#I.1. Climate files
Climate   <- read.csv("~/Documents/CLAND/Files_txt/data_clim_fr_new.csv", header=FALSE)%>%
             rename(year_cal      =V1,  clim_var      =V2,       departement   =V3,
                    month         =V4,  value         =V5,       unit          =V6) 

Climate_Spring_crops<-Climate %>% 
                mutate(year_harvest = year_cal)

Climate_Winter_crops<-Climate %>% 
                mutate(year_harvest = year_cal) %>%
    mutate(year_harvest = ifelse(month %in% c(8,9,10,11,12),year_cal+1,year_cal))
#####

#############  II. Create new data.frame with only months of growing season   ######################
## II.1.  Spring crops
wne_tot_gs <- Climate_Spring_crops %>% filter(month %in% c(3:10)) %>% mutate(sp ="wne_tot_gs")
bar_spr_gs <- Climate_Spring_crops %>% filter(month %in% c(1:8))  %>% mutate(sp ="bar_spr_gs")
oat_spr_gs <- Climate_Spring_crops %>% filter(month %in% c(1:8))  %>% mutate(sp ="oat_spr_gs")
wht_spr_gs <- Climate_Spring_crops %>% filter(month %in% c(2:10)) %>% mutate(sp ="wht_spr_gs")
mai_tot_gs <- Climate_Spring_crops %>% filter(month %in% c(4:11)) %>% mutate(sp ="mai_tot_gs")
pot_tot_gs <- Climate_Spring_crops %>% filter(month %in% c(4:10)) %>% mutate(sp ="pot_tot_gs")
sug_tot_gs <- Climate_Spring_crops %>% filter(month %in% c(4:11)) %>% mutate(sp ="sug_tot_gs")
sun_tot_gs <- Climate_Spring_crops %>% filter(month %in% c(4:9))  %>% mutate(sp ="sun_tot_gs")
##
#for spring crops, we consider only 1 season


## II.2.  Winter crops

# ATTENTION! change month of gs to CROP => 10 ou 8
# Remove 01 to 08.1958 or 10.1958 => because growing season not full (no winter in 1957)
Climate_Winter_crops<-Climate_Winter_crops[-which(Climate_Winter_crops$year_cal==1958&Climate_Winter_crops$month<8),]

bar_win_gs <- Climate_Winter_crops %>% filter(month %in% c(10:12,1:7)) %>% mutate(sp ="bar_win_gs")
oat_win_gs <- Climate_Winter_crops %>% filter(month %in% c(10:12,1:8)) %>% mutate(sp ="oat_win_gs")
wht_win_gs <- Climate_Winter_crops %>% filter(month %in% c(10:12,1:8)) %>% mutate(sp ="wht_win_gs")
rap_win_gs <- Climate_Winter_crops %>% filter(month %in% c(8:12,1:7))  %>% mutate(sp ="rap_win_gs")
# wdu_win_gs<- c(10:12,1:8) Removed because of missing data

## II.3. crops grown in the spring or in the winter
# because we don't know the proportions before 1943 
# we make the hypo that all are winter.

bar_tot_gs <- Climate_Winter_crops %>% filter(month %in% c(10:12,1:7)) %>% mutate(sp ="bar_tot_gs")
oat_tot_gs <- Climate_Winter_crops %>% filter(month %in% c(10:12,1:8)) %>% mutate(sp ="oat_tot_gs")
wht_tot_gs <- Climate_Winter_crops %>% filter(month %in% c(10:12,1:8)) %>% mutate(sp ="wht_tot_gs")
rap_tot_gs <- Climate_Winter_crops %>% filter(month %in% c(8:12,1:7))  %>% mutate(sp ="rap_tot_gs")
#####

################III/ Merge Save  files #####################################

LIST<-ls(pattern="gs")

CLIM<- get(LIST) %>%   bind_rows()

write.csv2(CLIM,"Climate.csv", dec=".")
