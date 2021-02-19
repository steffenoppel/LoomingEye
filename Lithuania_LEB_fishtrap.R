### ##################################################
### LITHUANIA fish trap bycatch of cormorants - test of LOOMING EYE BUOY (LEB) TO DETER BIRDS
### written by steffen.oppel@rspb.org.uk
### ##################################################

## originally created 9 DEC 2020

### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(randomForest)
library(glmmTMB)
filter<-dplyr::filter
select<-dplyr::select



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\RawData")

# Read the data from formatted CSV files (one for each mitigation trial)
sets <- read_excel("Lithuania_DATA_2020_buoys.xlsx", sheet="Sheet1")
hour(sets$Depl_Date)=hour(sets$Depl_Time)
minute(sets$Depl_Date)=minute(sets$Depl_Time)
hour(sets$Haul_Date)=hour(sets$Haul_Time)
minute(sets$Haul_Date)=minute(sets$Haul_Time)


sets %>% filter(is.na(Haul_Date))



### FORMAT DATA (remove columns we don't need and format the rest)
names(sets)
unique(sets$Mesh_Size)
sets<-sets %>% mutate(effort=as.numeric(difftime(Haul_Date,Depl_Date,'hours'))) %>%
  mutate(BPUE=(Cormorant/effort)*5*24) %>%  ### scale number of cormorants over 5 days
  mutate(Month=month(Haul_Date)) %>%
  select (-Fishtraps_no,-Depl_Time,-Haul_Time,-Effort,-Mesh_Size) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(bycatch=ifelse(Cormorant>0,1,0))

dim(sets)
str(sets)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT SIMPLE BOOTSTRAP ANALYSIS ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye")
controls<- sets %>% filter(Fishtrap_with_buoy=="no")
LEBs<- sets %>% filter(Fishtrap_with_buoy=="yes")
table(sets$BPUE)
table(sets$Cormorant)
range(LEBs$Depl_Date)
range(LEBs$BPUE)
range(controls$BPUE)

## bootstrap test
boot.samples <- matrix(sample(controls$BPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
boot.statistics <- apply(boot.samples, 1, mean)
RATE_control<-data.frame(treatment="no LEB",mean=mean(boot.statistics),
                  lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))
LEB.samples <- matrix(sample(LEBs$BPUE, size = 10000 * nrow(LEBs), replace = TRUE),10000, nrow(LEBs))
LEB.statistics <- apply(LEB.samples, 1, mean)
RATE_LEB<-data.frame(treatment="with LEB",mean=mean(LEB.statistics),
                         lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))

RATE_DIFF<-data.frame(treatment="difference",mean=mean(LEB.statistics-boot.statistics),
                     lcl=quantile(LEB.statistics-boot.statistics,0.025),ucl=quantile(LEB.statistics-boot.statistics,0.975))
(RATE_DIFF[,2:4]/RATE_control[,2:4])*100




### PLOT predicted OUTPUT ###

bind_rows(RATE_control,RATE_LEB) %>%
  ggplot(aes(y=mean, x=treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(0,0.5), breaks=seq(0,0.5,0.1)) +
  xlab("") +
  ylab("Bycatch in fishtrap over 5 days") +
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

ggsave("LEB_Fishtrap_bootstrap_summary.jpg", width=8, height=11)




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     FORMAL ANALYSIS OF CORMORANT BYCATCH BETWEEN LEB and no LEB           ~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### FIT RANDOM FOREST MODEL
head(sets)
summary(sets)
RF<- randomForest(as.factor(bycatch)~effort+Fishing_Zone+Fisherman+Location+Month+Fishtrap_with_buoy, data=sets, mtry=5,ntree=1500, importance=T,na.action=na.omit)
RF



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CALCULATE VARIABLE IMPORTANCE AND PLOT IMPORTANCE ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## the default basic plot
varImpPlot(RF)

## advanced plot for publication
VAR<-importance(RF, type=1)
IMP<-data.frame(variable=row.names(VAR), MSE=-VAR)
IMP<-IMP[order(IMP$MeanDecreaseAccuracy, decreasing=T),]  ## SORTED BY increase in mean squared error
IMP$rel_imp<-round((IMP$MeanDecreaseAccuracy/IMP$MeanDecreaseAccuracy[1])*100,2)
IMP$variable <- factor(IMP$variable, levels = IMP$variable[order(IMP$MeanDecreaseAccuracy)])  ## this ensures correct presentation on the plot
IMP

ggplot(IMP, aes(x=variable, y=rel_imp)) +
  geom_bar(stat='identity', fill='lightblue') +
  geom_text(aes(label=variable), position=position_dodge(width=0.1), hjust=-0.1,size=5, color="darkgrey")+
  coord_flip()+
  ylab("Variable importance (%)") +
  xlab("Explanatory variable") +
  scale_y_continuous(limits=c(-20,115), breaks=seq(0,100,20), labels=seq(0,100,20))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_blank(), 
        axis.title=element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
ggsave("LBE_cormorants_fishtrap_variable_importance.jpg", width=8, height=9)






