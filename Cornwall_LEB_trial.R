### ##################################################
### CORNWALL LEB AND KITE TRIALS - test of LOOMING EYE BUOY (LEB) AND KITES TO DETER BIRDS
### written by steffen.oppel@rspb.org.uk
### ##################################################

## copied from Lithuanian LEB trials
## modified 29 March 2022
# modified 1 April to include mesh size

# modified 23 April to account for sampling imbalance of mesh size 

# completely revised on 29 June 2023 after including latest data
# CHANGED INPUT DATA:
# - corrected incorrect Haul Date (22 instead of 2023)
# - added Haul_Time for missing fields based on soak time estimate
# - added 2 Haul times for PeterJohn based on other setting/hauling time patterns
# - added two missing depths by Trevally to match depths of control nets on same trip


## DISCUSSION WITH YANN on 7 July 2023
## decided on using 'decision tree' to limit bycatch
## fishing permitted in water <40 m only with either small (<80 mm) mesh width or at night
## calculate reduction in bycatch if those regulations would be obeyed
## assume that fishing effort would move either to deeper water or to night
## cannot assume that people would change mesh width as it would target a different fish species

## NEED TO DO: look up port coordinates
### sense check estimates - WHY are extrapolations orders of magnitude too high??


### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(suncalc)
filter<-dplyr::filter
select<-dplyr::select



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

#setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\RawData")
setwd("C:/Users/sop/Documents/Steffen/RSPB/Bycatch")

# Read the data from formatted CSV files (one for each mitigation trial)
sets <- read_excel("Database_Cornwall_Y2_Final_08062023.xlsx", sheet="Sheet1")
hour(sets$Depl_Date)=hour(sets$Depl_Time)
minute(sets$Depl_Date)=minute(sets$Depl_Time)
hour(sets$Haul_Date)=hour(sets$Haul_Time)
minute(sets$Haul_Date)=minute(sets$Haul_Time)
# names(sets)
# dim(sets)
# str(sets)

### FORMAT DATA (remove columns we don't need and format the rest)
#names(sets)
#unique(sets$Mesh_Size)
sets_orig<-sets %>% rename(bycatch=`Total Bird Bycatch`,
                      fish_catch=`Total_Fish Catch (kg)`,
                      CORM=`Cormorant/Shag`,
                      GUIL=`Common Guillemot`,
                      GANN=`Gannet`,
                      Mitigation=`Mitigation [control/LEB/Kite]`,
                      depth=`Depth Category`,
                      height=`Net height (m)`,
                      length=`Net_Lenght (m)`,
                      depth_m=`Av. Depth (m)`,
                      mesh=`Mesh_Size (mm)`,
                      fisher=`#Fisherman`) %>%
  #mutate(start=ymd_hms(paste(Depl_Date,Depl_Time)), end=ymd_hms(paste(Haul_Date,Haul_Time)))
  mutate(effort=as.numeric(difftime(Haul_Date,Depl_Date,'hours')))

sets<-sets_orig %>% select (Trip_ID,Port,Set_ID,Depl_Date, Haul_Date,Mitigation,effort, bycatch, length, height,mesh,depth,depth_m,fish_catch,CORM,GUIL,GANN,fisher) %>%
  mutate(BPUE=(bycatch/(effort*length*height))) %>%  ### scale number of cormorants over 5 days
  mutate(CPUE=(fish_catch/(effort*length*height))) %>%  ### scale number of cormorants over 5 days
  mutate(CORM_PUE=(CORM/(effort*length*height))) %>%  ### scale number of cormorants over 5 days
  mutate(GUIL_PUE=(GUIL/(effort*length*height))) %>%  ### scale number of cormorants over 5 days
  mutate(GANN_PUE=(GANN/(effort*length*height))) %>%  ### scale number of cormorants over 5 days
  mutate(Month=month(Depl_Date)) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(bycatch_bin=ifelse(bycatch>0,1,0))

dim(sets)
str(sets)



### troubleshoot missing data
sets %>% filter(is.na(BPUE))
unique(sets$mesh)
unique(sets$depth)
unique(sets$depth)
hist(sets$depth_m)
hist(sets$Month)
sum(sets$GUIL)

### enter port coordinates and calculate twilight times
ports<-data.frame(Port=unique(sets_orig$Port),
                  Lat=c(50.269837,50.106364,49.983581,50.146927,50.146927,50.181692,50.096620,50.269837,50.201015,50.425365),
                  Long=c(-4.779858, -5.537730,-5.172268,-5.029085,-5.029085,-5.036813,-5.090484,-4.779858,-5.438911,-5.085408))

### calculate sunset times
### with nautical Dawn and dusk only 1 trip qualified
### with dusk and dawn it is only 4 sets


sets<-sets %>% 
  mutate(lat=ports$Lat[match(Port,ports$Port)], lon=ports$Long[match(Port,ports$Port)])
sundata<-sets %>% mutate(date=as.Date(Depl_Date))
sets<-getSunlightTimes(data=sundata,keep=c("sunrise", "sunset"),tz = "UTC")[,4:5] %>%
  bind_cols(sets) %>%
  mutate(depl_night= if_else(Depl_Date > sunset | Depl_Date < sunrise,1,0)) %>%
  rename(setsunrise=sunrise,setsunset=sunset)
sundata<-sets %>% mutate(date=as.Date(Haul_Date))
sets<-getSunlightTimes(data=sundata,keep=c("sunrise", "sunset"),tz = "UTC")[,4:5] %>%
  bind_cols(sets) %>%
  mutate(haul_night= if_else(Haul_Date > sunset | Haul_Date < sunrise,1,0)) %>%
  rename(haulsunrise=sunrise,haulsunset=sunset) %>%
  mutate(NightSet=if_else(depl_night==1 & haul_night==1 & effort<24,1,0))

table(sets$NightSet)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  SENSE CHECK OF BYCATCH PER UNIT EFFORT RATES ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
sum(sets$CORM)
sum(sets$CORM_PUE*sets$effort*sets$length*sets$height)
  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  BASIC SUMMARY OF FISHING EFFORT AND BYCATCH BY DEPTH AND MESH ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


### quantify effort and bycatch with threshold of 30 m
sets_orig %>% mutate(deep=if_else(depth_m>30,"deep","shallow")) %>%
  group_by(deep) %>%
  summarise(Eff=sum(effort *height * length), bycatch=sum(bycatch)) %>%
  mutate(Ratio = round(Eff / sum(Eff), 2),Byc_ratio = round(bycatch / sum(bycatch), 2))

### quantify effort and bycatch with threshold of 80 m mesh width
sets_orig %>% mutate(small=if_else(mesh>80,"large","small")) %>%
  group_by(small) %>%
  summarise(Eff=sum(effort *height * length), bycatch=sum(bycatch)) %>%
  mutate(Ratio = round(Eff / sum(Eff), 2),Byc_ratio = round(bycatch / sum(bycatch), 2))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT SIMPLE BOOTSTRAP ANALYSIS LUMPING ALL DATA ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye")

####### LOOP OVER 5 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(Mitigation=unique(sets$Mitigation),
                 Response=c("BPUE","CPUE","CORM_PUE","GUIL_PUE","GANN_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% 
  dplyr::select(BPUE,CPUE,CORM_PUE,GUIL_PUE,GANN_PUE,Trip_ID,Mitigation) %>%
  gather(key="Response", value="value", -Trip_ID,-Mitigation)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(Mitigation==OUT$Mitigation[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))

  if(dim(samples)[1]>2){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
                    )




### PLOT BOOTSTRAPPED OUTPUT ###

OUT %>% filter(Response!="CPUE") %>%
  mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags",
                                ifelse(Response=="GUIL_PUE","Guillemots","Gannets")))) %>%
  
  
  ggplot(aes(y=mean, x=Mitigation, ymin=lcl, ymax=ucl)) +
  geom_point(size=3,col="firebrick")+
  geom_errorbar(width=.05, col="firebrick")+
  facet_wrap(~Response,ncol=2, scale="free_y") + 
  #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  xlab("") +
  ylab("Bycatch in 300 m gillnet (birds / day)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# ggsave("Cornwall_bycatch_trial_birds_total.jpg", width=11, height=8)
# fwrite(OUT,"Cornwall_bycatch_rates_birds_total.csv")



#####  SENSE CHECK OF BYCATCH PER UNIT EFFORT RATES ######
## to check that the mean rates are somewhat on the same order of magnitude as actual catch numbers
sum(sets$CORM)
sum(sets$effort)/24
OUT %>% filter(Response=="CORM_PUE") %>%
  mutate(CATCH=mean*sum(sets$effort)/24)




#### CALCULATE DIFFERENCES


####### LOOP OVER 5 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

DIFF<-expand.grid(Mitigation=unique(sets$Mitigation),
                 Response=c("BPUE","CPUE","CORM_PUE","GUIL_PUE","GANN_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0,rel.mean=0,rel.lcl=0,rel.ucl=0) %>%
  filter(Mitigation!="Control")

for (i in 1:dim(DIFF)[1]){
  
  samples<- data %>%
    filter(Mitigation==DIFF$Mitigation[i]) %>%
    filter(Response==DIFF$Response[i]) %>%
    filter(!is.na(value))
  control.samples<- data %>%
    filter(Mitigation=="Control") %>%
    filter(Response==DIFF$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>2){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    control.boot.samples <- matrix(sample(control.samples$value, size = 10000 * nrow(control.samples), replace = TRUE),10000, nrow(control.samples))
    control.boot.statistics <- apply(control.boot.samples, 1, mean, na.rm=F)
    DIFF$mean[i]=mean(boot.statistics-control.boot.statistics)
    DIFF$lcl[i]=quantile((boot.statistics-control.boot.statistics),0.025)
    DIFF$ucl[i]=quantile((boot.statistics-control.boot.statistics),0.975)
    DIFF$rel.mean[i]=quantile(((boot.statistics-control.boot.statistics)/max(control.boot.statistics,0.0000000000000000000000001)),0.5)
    DIFF$rel.lcl[i]=quantile(((boot.statistics-control.boot.statistics)/max(control.boot.statistics,0.0000000000000000000000001)),0.025)
    DIFF$rel.ucl[i]=quantile(((boot.statistics-control.boot.statistics)/max(control.boot.statistics,0.0000000000000000000000001)),0.975)
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

DIFF<-DIFF %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)


### PLOT BOOTSTRAPPED OUTPUT ###

DIFF %>% filter(Response!="CPUE") %>%
  mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags",
                                ifelse(Response=="GUIL_PUE","Guillemots","Gannets")))) %>%
  
  
  ggplot(aes(y=rel.mean, x=Mitigation, ymin=rel.lcl, ymax=rel.ucl)) +
  geom_point(size=3,col="firebrick")+
  geom_errorbar(width=.05, col="firebrick")+
  geom_hline(yintercept=0, col="forestgreen", linewidth=2,linetype="dashed")+
  facet_wrap(~Response,ncol=2) + 
  scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1,0.2)) +
  xlab("") +
  ylab("Relative difference in bycatch to control sets") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# ggsave("Cornwall_bycatch_trial_effects.jpg", width=11, height=8)
# fwrite(DIFF,"Cornwall_bycatch_differences.csv")

sets %>% filter(Mitigation=="LEB") %>%
  filter(GUIL>0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT ANALYSIS WITH DEPTH AND MESH SIZE ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####### LOOP OVER 5 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(Mitigation=unique(sets$Mitigation),
                 Mesh=c("small","medium","large"),
                 Depth=c("shallow","deep"),
                 Response=c("BPUE","CPUE","CORM_PUE","GUIL_PUE","GANN_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% mutate(Depth_cat=ifelse(depth_m<20,"shallow","deep")) %>%
  mutate(Mesh_cat=ifelse(mesh>80,ifelse(mesh>120,"large","medium"),"small")) %>%
  dplyr::select(BPUE,CPUE,CORM_PUE,GUIL_PUE,GANN_PUE,Mesh_cat,Depth_cat,Trip_ID,Mitigation) %>%
  gather(key="Response", value="value", -Trip_ID,-Mesh_cat,-Depth_cat,-Mitigation)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(Mitigation==OUT$Mitigation[i]) %>%
    filter(Mesh_cat==OUT$Mesh[i]) %>%
    filter(Depth_cat==OUT$Depth[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>2){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)




### PLOT BOOTSTRAPPED OUTPUT ###

OUT %>% filter(Response!="CPUE") %>%
  mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags",
                                ifelse(Response=="GUIL_PUE","Guillemots","Gannets")))) %>%
  
  ggplot(aes(y=mean, x=Mitigation, ymin=lcl, ymax=ucl, colour=Mesh, shape=Depth)) +
  geom_point(size=2,position = position_dodge(width=0.5))+
  geom_errorbar(position = position_dodge(width=0.5), width=.03)+
  facet_wrap(~Response,ncol=2, scale="free_y") + 
  #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  xlab("") +
  ylab("Bycatch in 300 m gillnet (birds / day)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# ggsave("Cornwall_bycatch_trial_mitigation_depth_mesh.jpg", width=12, height=8)
# fwrite(OUT,"Cornwall_bycatch_rates_mitigation_depth_mesh.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  ESTIMATE NOCTURNAL BYCATCH RATE FOR ALL NETS ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####### LOOP OVER 3 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(NightSet=c(0,1),
                 Response=c("BPUE","CORM_PUE","GUIL_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% 
  dplyr::select(BPUE,CORM_PUE,GUIL_PUE,Trip_ID,NightSet) %>%
  gather(key="Response", value="value", -Trip_ID,-NightSet)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(NightSet==OUT$NightSet[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>0){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}

#### SUMMARISE OUTPUT ON CREDIBLE SCALE

NIGHT_OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  ESTIMATE BYCATCH RATE FOR ALL SMALL MESH NETS ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####### LOOP OVER 3 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(Mesh=c("small","large"),
                 Response=c("BPUE","CORM_PUE","GUIL_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% 
  mutate(Mesh_cat=ifelse(mesh>80,"large","small")) %>%
  dplyr::select(BPUE,CORM_PUE,GUIL_PUE,Mesh_cat,Trip_ID) %>%
  gather(key="Response", value="value", -Trip_ID,-Mesh_cat)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(Mesh_cat==OUT$Mesh[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>0){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

MESH_OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  ESTIMATE BYCATCH RATE FOR ALL DEEP SET NETS ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####### LOOP OVER 3 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(Depth=c("shallow","deep"),
                 Response=c("BPUE","CORM_PUE","GUIL_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% 
  mutate(Depth_cat=ifelse(depth_m<=40,"shallow","deep")) %>%
  dplyr::select(BPUE,CORM_PUE,GUIL_PUE,Depth_cat,Trip_ID) %>%
  gather(key="Response", value="value", -Trip_ID,-Depth_cat)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(Depth_cat==OUT$Depth[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>0){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

DEPTH_OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                         lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                         ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT ANALYSIS WITH NIGHT, DEPTH AND MESH SIZE ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####### LOOP OVER 3 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(NightSet=c(0,1),
                 Mesh=c("small","large"),
                 Depth=c("shallow","deep"),
                 Response=c("BPUE","CORM_PUE","GUIL_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% mutate(Depth_cat=ifelse(depth_m<40,"shallow","deep")) %>%
  mutate(Mesh_cat=ifelse(mesh>80,"large","small")) %>%
  dplyr::select(BPUE,CORM_PUE,GUIL_PUE,Mesh_cat,Depth_cat,Trip_ID,NightSet) %>%
  gather(key="Response", value="value", -Trip_ID,-Mesh_cat,-Depth_cat,-NightSet)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(NightSet==OUT$NightSet[i]) %>%
    filter(Mesh_cat==OUT$Mesh[i]) %>%
    filter(Depth_cat==OUT$Depth[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>0){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)




### PLOT BOOTSTRAPPED OUTPUT ###

OUT %>%   mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags",
                                ifelse(Response=="GUIL_PUE","Guillemots","Gannets")))) %>%
  
  ggplot(aes(y=mean, x=NightSet, ymin=lcl, ymax=ucl, colour=Mesh, shape=Depth)) +
  geom_point(size=2,position = position_dodge(width=0.5))+
  geom_errorbar(position = position_dodge(width=0.5), width=.03)+
  facet_wrap(~Response,ncol=2, scale="free_y") + 
  #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  xlab("") +
  ylab("Bycatch in 300 m gillnet (birds / day)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
# 
# fwrite(OUT,"Cornwall_bycatch_rates_night_depth_mesh.csv")








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT ANALYSIS WITH DEPTH AND MONTH ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####### LOOP OVER 5 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(Mitigation=unique(sets$Mitigation),
                 Month=unique(sets$Month),
                 #Depth=c("shallow","deep"),
                 Response=c("BPUE","CPUE","CORM_PUE","GUIL_PUE","GANN_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% mutate(Depth_cat=ifelse(depth_m<20,"shallow","deep")) %>%
  dplyr::select(BPUE,CPUE,CORM_PUE,GUIL_PUE,GANN_PUE,Month,Depth_cat,Trip_ID) %>%
  gather(key="Response", value="value", -Trip_ID,-Month,-Depth_cat)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(Month==OUT$Month[i]) %>%
    #filter(Depth_cat==OUT$Depth[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>2){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)




### PLOT BOOTSTRAPPED OUTPUT ###

OUT %>% filter(Response!="CPUE") %>%
  mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags",
                                ifelse(Response=="GUIL_PUE","Guillemots","Gannets")))) %>%
  
  ggplot(aes(y=mean, x=Month, ymin=lcl, ymax=ucl)) +    ## , colour=Depth
  geom_point(size=2,position = position_dodge(width=0.5))+
  geom_errorbar(position = position_dodge(width=0.5), width=.03)+
  scale_x_continuous(labels = function(x) month.abb[x]) +
  facet_wrap(~Response,ncol=2, scale="free_y") + 
  #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  xlab("") +
  ylab("Bycatch in 300 m gillnet (birds / day)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# ggsave("Cornwall_bycatch_trial_mitigation_depth_month.jpg", width=12, height=8)
# fwrite(OUT,"Cornwall_bycatch_rates_mitigation_depth_month.csv")









#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     EXPLORATORY ANALYSIS OF BIRD BYCATCH BASED ON DIFFERENT VARIABLES          ~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
library(randomForest)


### FIT RANDOM FOREST MODELS FOR VARIOUS SUBSETS OF DATA AND RESPONSE VARIABLES
head(sets)
sets$jday<-yday(sets$Depl_Date)  ### to account for changing fishing behaviour over season
sets$year<-year(sets$Depl_Date)  ### to account for changing fishing behaviour between years
sets$bycatch_bin<-ifelse(sets$bycatch==0,"no","yes")  ### to distinguish between any and no bycatch

### BINARY BYCATCH YES/NO IS POORLY EXPLAINED BY EXPLANATORY VARIABLES
RFbinary<- randomForest(as.factor(bycatch_bin)~Mitigation+effort+length+height+jday+year+mesh+depth_m+fisher+NightSet,
                        data=sets, mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFbinary
varImpPlot(RFbinary)



### BIRD BYCATCH NUMERIC IS POORLY EXPLAINED BY EXPLANATORY VARIABLES
RFnum<- randomForest(bycatch~Mitigation+effort+length+height+jday+year+mesh+depth_m+fisher+NightSet,
                     data=sets, mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFnum
varImpPlot(RFnum)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  PLOT VARIABLE IMPORTANCE FOR BOTH MODELS ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## advanced plot for publication
VARbinary<-importance(RFbinary, type=1)
VARnum<-importance(RFnum, type=1)


IMP<-bind_rows(VARbinary[,1],VARnum[,1]) %>%
  mutate(MAX = do.call(pmax, (.))) %>%
  mutate(model=c("occurrence","abundance")) %>%
  gather(key=variable, value=MSE,-model,-MAX) %>%
  mutate(IMP=(MSE/MAX)*100) %>%
  arrange(model,desc(IMP))

mylevels<-IMP$variable[10:1]
IMP %>%  dplyr::mutate(variable=forcats::fct_relevel(variable,mylevels)) %>%
ggplot(aes(x=variable, y=IMP)) +
  geom_bar(stat='identity', fill='lightblue') +
  #geom_text(aes(label=variable), position=position_dodge(width=0.1), hjust=-0.1,size=5, color="darkgrey")+
  coord_flip()+
  facet_wrap(~model, ncol=2) +
  ylab("Variable importance (%)") +
  xlab("Explanatory variable") +
  scale_y_continuous(limits=c(-20,115), breaks=seq(0,100,20), labels=seq(0,100,20))+

  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        #axis.text.y=element_blank(), 
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())


# ggsave("Cornwall_bycatch_trial_variable_importance.jpg", width=12, height=8)








#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY DEPTH AT WHICH SPECIES ARE CAUGHT ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
names(sets_orig)
totals<- sets_orig %>% 
  dplyr::select(Trip_ID,bycatch,CORM,GUIL) %>%
  gather(key="Species", value="Catch",-Trip_ID) %>%
  mutate(Catch=ifelse(is.na(Catch),0,Catch)) %>%
  group_by(Species) %>%
  summarise(N=sum(Catch)) %>%
  mutate(Species=ifelse(Species=="bycatch","All birds",
                        ifelse(Species=="CORM","Cormorants and shags","Guillemots"))) %>%
  arrange(desc(N))


depthcaptures<-sets_orig %>% 
  dplyr::select(Trip_ID,fisher,mesh,depth_m,bycatch,CORM,GUIL) %>%
  gather(key="Species", value="Catch",-depth_m,-mesh,-Trip_ID,-fisher) %>%
  mutate(Catch=ifelse(is.na(Catch),0,Catch)) %>%
  filter(Catch>0)

alldepthcaptures <- data.frame()
for(x in 1:dim(depthcaptures)[1]){
  l<-depthcaptures[x,]
  out_l<-data.frame(Species=l$Species,depth=l$depth_m,Catch=rep(1,l$Catch))
  alldepthcaptures <- bind_rows(alldepthcaptures, out_l)
}
dim(depthcaptures)
dim(alldepthcaptures)

depthmeans<-alldepthcaptures %>% group_by(Species) %>%
  summarise(depth=mean(depth)) %>%
  arrange(depth) %>%
  mutate(Species=ifelse(Species=="bycatch","All birds",
                                        ifelse(Species=="CORM","Cormorants and shags","Guillemots")))

alldepthcaptures %>%
  mutate(Species=ifelse(Species=="bycatch","All birds",
                                        ifelse(Species=="CORM","Cormorants and shags","Guillemots"))) %>%

  left_join(totals, by="Species") %>%

  
  ggplot(aes(y=depth, x=Species)) +
  geom_boxplot() +
  coord_flip() +
  ylab("Mean catch depth (m)") +
  geom_text(aes(x=Species, y=0, label = paste0("(n = ", N, ")")), nudge_y = -0.25) + 
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# ggsave("Cornwall_bycatch_captures_by_depth.jpg", width=14, height=8)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY MESH SIZES IN WHICH SPECIES ARE CAUGHT ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
names(sets_orig)

allmeshcaptures <- data.frame()
for(x in 1:dim(depthcaptures)[1]){
  l<-depthcaptures[x,]
  out_l<-data.frame(Species=l$Species,mesh=l$mesh,Catch=rep(1,l$Catch))
  allmeshcaptures <- bind_rows(allmeshcaptures, out_l)
}

meshmeans<-allmeshcaptures %>% group_by(Species) %>%
  summarise(mesh=mean(mesh)) %>%
  arrange(mesh) %>%
  mutate(Species=ifelse(Species=="bycatch","All birds",
                        ifelse(Species=="CORM","Cormorants and shags","Guillemots")))

allmeshcaptures %>%
  mutate(Species=ifelse(Species=="bycatch","All birds",
                        ifelse(Species=="CORM","Cormorants and shags","Guillemots"))) %>%
  
  left_join(totals, by="Species") %>%
  
  
  ggplot(aes(y=mesh, x=Species)) +
  geom_boxplot() +
  coord_flip() +
  ylab("Mean mesh width (mm)") +
  geom_text(aes(x=Species, y=0, label = paste0("(n = ", N, ")")), nudge_y = -0.25) + 
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# ggsave("Cornwall_bycatch_captures_by_mesh.jpg", width=14, height=8)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  QUANTIFY DEPTH AND MESH DISTRIBUTION OF DATA ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
names(sets_orig)

hist(sets_orig$depth_m)
hist(sets_orig$mesh)

hist(sets_orig$depth_m[sets_orig$mesh<80])
hist(sets_orig$depth_m[sets_orig$mesh>80])
hist(sets_orig$mesh[sets_orig$depth_m>20])
hist(sets_orig$mesh[sets_orig$depth_m<20])




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  QUANTIFY BIRD BYCATCH RATES BY DEPTH AND MESH SIZE ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####### LOOP OVER 5 RESPONSES OF INTEREST TO PRODUCE BOOTSTRAP TESTS FOR EACH

OUT<-expand.grid(Mesh=c("small","medium","large"),
                 Depth=c("shallow","deep"),
                 Response=c("BPUE","CORM_PUE","GUIL_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% mutate(Depth_cat=ifelse(depth_m<20,"shallow","deep")) %>%
  mutate(Mesh_cat=ifelse(mesh>80,ifelse(mesh>120,"large","medium"),"small")) %>%
  dplyr::select(BPUE,CORM_PUE,GUIL_PUE,Mesh_cat,Depth_cat,Trip_ID) %>%
  gather(key="Response", value="value", -Trip_ID,-Mesh_cat,-Depth_cat)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(Mesh_cat==OUT$Mesh[i]) %>%
    filter(Depth_cat==OUT$Depth[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>2){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}



#### SUMMARISE OUTPUT ON CREDIBLE SCALE

OUT<-OUT %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                    ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24
)




### PLOT BOOTSTRAPPED OUTPUT ###

OUT %>% 
  mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags","Guillemots"))) %>%
  
  ggplot(aes(y=mean, x=Depth, ymin=lcl, ymax=ucl, colour=Mesh)) +
  geom_point(size=2,position = position_dodge(width=0.5))+
  geom_errorbar(position = position_dodge(width=0.5), width=.03)+
  facet_wrap(~Response,ncol=1, scale="free_y") + 
  xlab("") +
  ylab("Bycatch in 300 m gillnet (birds / day)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

# ggsave("Cornwall_bycatch_rates_depth_mesh.jpg", width=9, height=12)
# fwrite(OUT,"Cornwall_bycatch_rates_birds_depth_mesh.csv")




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY CHANGE IN BYCATCH UNDER DEPTH RESTRICTIONS  ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

## assuming that we cannot restrict mesh size as that might limit fish catch
# What would be the seabird bycatch % reduction if a 40 m set depth limit is implemented, compared to the currently observed bycatch levels?


## APPROACH:
# estimate bycatch rate per depth category
# simulate redistribution of fishing effort to deeper depths and re-calculate bycatch
# this ignores potential availability of water at that depth with sufficient fish ressource
# only valid for our sample data - no extrapolation to overall fishing fleet


## catch and bycatch per depth category
## sets %>% filter(is.na(depth)) 
depthrates<-expand.grid(depth=unique(sets$depth),
                   Response=c("BPUE","CPUE","CORM_PUE","GUIL_PUE")) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% 
  dplyr::select(BPUE,CPUE,CORM_PUE,GUIL_PUE,depth,Trip_ID) %>%
  gather(key="Response", value="value", -Trip_ID,-depth)


for (i in 1:dim(depthrates)[1]){
  
  samples<- data %>%
    filter(depth==depthrates$depth[i]) %>%
    filter(Response==depthrates$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>2){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    depthrates$mean[i]=mean(boot.statistics)
    depthrates$lcl[i]=quantile(boot.statistics,0.025)
    depthrates$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    depthrates$mean[i]=NA
    depthrates$lcl[i]=NA
    depthrates$ucl[i]=NA
  }
}




depthrates_table<-depthrates %>%
  select(-lcl,-ucl) %>%
  spread(key=Response, value=mean)


deptheffort<-sets %>%
  group_by(depth) %>%
  summarise(tot_eff=sum(effort)) %>%
  mutate(prop=tot_eff/sum(tot_eff))


## extrapolate the estimated effort at depth categories to bycatch rates to estimate total bycatch per depth
depthbycatch<-depthrates %>%
  left_join(deptheffort, by="depth") %>%
  mutate(depthnum=stringr::str_extract(depth, stringr::regex("(\\d+)(?!.*\\d)"))) %>%
  mutate(depthnum=ifelse(depth==">40",50,depthnum))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  SIMULATE FISHING CLOSURE AT CERTAIN DEPTH AND REALLOCATION OF EFFORT TO DEEPER WATER ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## assume fishing closed in waters < specified depth
## all fishing effort would go into water > specified depth

closure_depth_simulation<-data.frame()
for(r in unique(depthbycatch$Response)){
  
  for(d in c(0,10, 20, 30, 40)) {
    yeff <- depthbycatch %>% 
      filter(Response==r) %>%
      mutate(IN = if_else(depthnum > d, 1, 0))
    
    ### summarise effort in closed waters and redistribute to open waters
    mean.eff.add <-
      sum(yeff$tot_eff[yeff$IN == 0]) / length(yeff$tot_eff[yeff$IN == 1])
    
    out <- yeff %>% filter(IN == 1) %>%
      mutate(tot_eff = tot_eff + mean.eff.add) %>%
      mutate(mean=mean*tot_eff*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T),
            lcl=lcl*tot_eff*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T),
            ucl=ucl*tot_eff*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)
      )    %>%
      
      group_by(Response) %>%
      summarise(
        mean = sum(mean),
        lcl = sum(lcl),
        ucl = sum(ucl)
      ) %>%
      mutate(closure_depth = d)
    
    closure_depth_simulation <- bind_rows(closure_depth_simulation, out)
  }
}


SUMMARY <- closure_depth_simulation %>% 
  mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags",
                                ifelse(Response=="CPUE","fish","Guillemots"))))
#fwrite(SUMMARY,"Cornwall_simulated_bycatch_depth_restriction.csv")



#### CALCULATING RELATIVE CHANGE IN FISH CATCH AND CHANGE IN BIRD BYCATCH
BASELINE<- SUMMARY %>% filter(closure_depth==0) %>%
  rename(base=mean,base.lcl=lcl,base.ucl=ucl) %>%
  select(-closure_depth)

SUMMARY %>% filter(closure_depth>0) %>%
  left_join(BASELINE, by="Response") %>%
  mutate(rel.mean=((mean-base)/base)*100,
         rel.lcl=((lcl-base.lcl)/base.lcl)*100,
         rel.ucl=((ucl-base.ucl)/base.ucl)*100) %>%

  ggplot(aes(y = rel.mean, x = closure_depth, ymin=rel.lcl, ymax=rel.ucl,fill = Response,colour=Response)) +
  geom_bar(stat="identity",position = position_dodge(width=8))+
  geom_hline(aes(yintercept=0), linetype="dashed", linewidth=1)+
  geom_errorbar(width=2,position = position_dodge(width=8))+
  ylab("Change in total catch (in %)") +
  xlab("Closure depth for fishing (m)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=16, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.position=c(0.15,0.16),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Cornwall_simulated_bycatch_reduction_depth_restriction.jpg", width=12, height=10)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  SIMULATE FISHING CLOSURE AT NIGHT OR WITH SMALL MESH IN SHALLOW WATERS AND REALLOCATE EFFORT TO DAY OR DEEPER WATER ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## need bycatch rates for day and night for large >80 mm net in waters <40m
## need bycatch rates for <80 mm net in waters <40 m
## assume fishing closed in waters < specified depth unless done at night OR with small mesh width

## all fishing effort would go into water > specified depth


### Step 1: calculate bycatch rates for nocturnal nets
## calculate effort in non-legal groups
group_effort<-sets %>%
  mutate(neteffort=effort*length*height) %>%
  mutate(Depth_cat=ifelse(depth_m<=40,"shallow","deep")) %>%
  mutate(Mesh_cat=ifelse(mesh>80,"large","small")) %>%
  mutate(Mesh_cat=ifelse(Depth_cat=="shallow" & NightSet==1,"all",Mesh_cat)) %>%
  group_by(Depth_cat,Mesh_cat,NightSet) %>%
  summarise(tot_eff=sum(neteffort)) %>%
  ungroup() %>%
  mutate(prop=tot_eff/sum(tot_eff)) %>%
  mutate(LEGAL=ifelse(Depth_cat=="deep","yes",ifelse(NightSet==1 | Mesh_cat=="small","yes", "no")))

### Step 2: calculate bycatch rates for all necessary groups

OUT<-bind_rows(group_effort,group_effort,group_effort) %>%
  mutate(Response=rep(c("BPUE","CORM_PUE","GUIL_PUE"),each=dim(group_effort)[1])) %>%
  mutate(mean=0,lcl=0,ucl=0)

data<-sets %>% mutate(Depth_cat=ifelse(depth_m<=40,"shallow","deep")) %>%
  mutate(Mesh_cat=ifelse(mesh>80,"large","small")) %>%
  mutate(Mesh_cat=ifelse(Depth_cat=="shallow" & NightSet==1,"all",Mesh_cat)) %>%
  dplyr::select(BPUE,CORM_PUE,GUIL_PUE,Depth_cat,Trip_ID,Mesh_cat,NightSet) %>%
  gather(key="Response", value="value", -Trip_ID,-Mesh_cat,-Depth_cat,-NightSet)


for (i in 1:dim(OUT)[1]){
  
  samples<- data %>%
    filter(Mesh_cat==OUT$Mesh_cat[i]) %>%
    filter(Depth_cat==OUT$Depth_cat[i]) %>%
    filter(NightSet==OUT$NightSet[i]) %>%
    filter(Response==OUT$Response[i]) %>%
    filter(!is.na(value))
  
  if(dim(samples)[1]>2){
    boot.samples <- matrix(sample(samples$value, size = 10000 * nrow(samples), replace = TRUE),10000, nrow(samples))
    boot.statistics <- apply(boot.samples, 1, mean, na.rm=F)
    OUT$mean[i]=mean(boot.statistics)
    OUT$lcl[i]=quantile(boot.statistics,0.025)
    OUT$ucl[i]=quantile(boot.statistics,0.975)
  }else{
    OUT$mean[i]=NA
    OUT$lcl[i]=NA
    OUT$ucl[i]=NA
  }
}

#### SUMMARISE OUTPUT For scaling with effort

group.bycatch.rates<-OUT ## %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
#                                     lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
#                                     ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24)


mean(sets$length)
mean(sets$height)

##### Step 3: SET UP SIMULATION TO REDISTRIBUTE EFFORT
SIMUL.OUT<-data.frame()

## run a random simulation of random effort re-allocation
baserate<-group_effort$tot_eff[2]/(group_effort$tot_eff[1]+group_effort$tot_eff[2])

for(r in unique(group.bycatch.rates$Response)){
  x<-group.bycatch.rates %>% filter(Response==r)
#  for(d in 1:1000) {
      tonight<-rbinom(1,as.integer(group_effort$tot_eff[3]),baserate)
      todeep<-group_effort$tot_eff[3]-tonight
      
      SIMUL.OUT <- x %>% mutate(tot_eff=ifelse(LEGAL=="no",0,
                                       ifelse(NightSet==1,tot_eff+tonight,
                                              ifelse(Depth_cat=="deep",tot_eff+todeep,tot_eff)))) %>%
      mutate(byc.mean=tot_eff*mean, byc.ucl=tot_eff*ucl,byc.lcl=tot_eff*lcl) %>%
      group_by(Response) %>%
      summarise(byc=sum(byc.mean),lcl=sum(byc.lcl),ucl=sum(byc.ucl)) %>%
        bind_rows(SIMUL.OUT)
#  }
}



##### Step 4: SUMMARISE OUTPUT
BASELINE <- group.bycatch.rates %>% mutate(byc.mean=tot_eff*mean, byc.ucl=tot_eff*ucl,byc.lcl=tot_eff*lcl) %>%
  group_by(Response) %>%
  summarise(base=sum(byc.mean),base.lcl=sum(byc.lcl),base.ucl=sum(byc.ucl))
### for comparison, actual catch numbers
#BASELINE <- sets %>% summarise(CORM=sum(CORM),GUIL=sum(GUIL))



#### CALCULATING RELATIVE CHANGE IN BIRD BYCATCH
SUMMARY<-SIMUL.OUT %>%
  left_join(BASELINE, by="Response") %>%
  mutate(rel.mean=((byc-base)/base)*100,
         rel.lcl=((lcl-base.lcl)/base.lcl)*100,
         rel.ucl=((ucl-base.ucl)/base.ucl)*100) %>%
  mutate(Response=ifelse(Response=="BPUE","All birds",
                         ifelse(Response=="CORM_PUE","Cormorants and shags",
                                ifelse(Response=="CPUE","fish","Guillemots"))))

fwrite(SUMMARY,"Cornwall_simulated_bycatch_multi_option.csv")


ggplot(SUMMARY,aes(y = rel.mean, x = Response, ymin=rel.lcl, ymax=rel.ucl)) +
  geom_bar(stat="identity")+
  geom_hline(aes(yintercept=0), linetype="dashed", size=1)+
  geom_errorbar(width=0.5)+
  ylab("Change in total catch (in %)") +
  xlab("") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=16, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.position=c(0.15,0.16),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("Cornwall_simulated_bycatch_reduction_multi_option.jpg", width=12, height=8)




### TRANSLATE GROUP BYCATCH RATES TO SENSIBLE SCALE BEFORE EXPORTING

group.bycatch.rates<-OUT  %>% mutate(mean=mean*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                                     lcl=lcl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24,
                                     ucl=ucl*mean(sets$length,na.rm=T)*mean(sets$height,na.rm=T)*24)

fwrite(group.bycatch.rates,"Cornwall_group_bycatch_rates.csv")