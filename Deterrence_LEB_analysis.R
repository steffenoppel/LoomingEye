### ##################################################
### BALTIC SEA gillnet bycatch - test of LOOMING EYE BUOY (LEB) TO DETER DUCKS
### written by steffen.oppel@rspb.org.uk
### ##################################################

## originally created 19 April 2020
## modified on 28 April 2020 to include Yann's comments
## temporarily removed counts from Maris as they seem to be completely opposite to anyone else - but then reinserted
## updated on 3 May 2020: included sections for other species

## NEED TO DO: remove counts>20 as outliers for all non-LTDU analyses

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


#setwd("A:\\RSPB\\Marine\\Bycatch\\GillnetBycatch")
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye")

# Read the data from formatted CSV files (one for each mitigation trial)
surveys <- read_excel("Estonia_Seabird_Bycatch_Project_FINAL_DATA_YR.xlsx", sheet="Estonia_Seabird_Bycatch_Pro_0")
counts <- read_excel("Estonia_Seabird_Bycatch_Project_FINAL_DATA_YR.xlsx", sheet="Count Session bird groups SUM")
counts$GlobalID<-counts$ParentGlobalID

### FORMAT DATA (remove columns we don't need and format the rest)
names(surveys)
surveys<-surveys[,c(2,3,8:19)] %>%
  rename(Date=`Survey Date`, 
         PeriodCode=`Survey Period Code:`,Location=`Location:`,Phase=`Phase:`, Observer=`Observer(s):`,
         weather=`Sun or Precipitation:`,wind_dir=`Wind Direction:`,Temp=`Air Temperature (�C):`,
         wind_speed=`Wind Speed (km/h):`, cloud=`Cloud Cover:`, sea=`Sea Conditions:`,vis=`Visibility:`,
         OthObs=`Other Observer(s):`) %>%
  mutate(OthObs=ifelse(is.na(OthObs),Observer,OthObs)) %>%
  mutate(Observer=ifelse(OthObs=="Maris","Maris",Observer)) %>%
  select (-OthObs)

dim(surveys)

counts<-counts[,c(1:103)] %>% select(-c(1,3,5,which(grepl("SUM",names(counts))),which(grepl("Additional",names(counts))))) %>%  ## THIS ELIMINATES 40 birds across all phases that are lumped in 'additional 1 species'
  rename(Time=`Count Time:`, CountSeq=`Count Number:`) %>%
  gather(key=spec,value=Number,-GlobalID,-Phase,-Time,-CountSeq) %>%
  separate(spec,into=c("Treatment","Species"),sep="_") %>%
  left_join(surveys, by=c("GlobalID","Phase")) %>%
  mutate(Start=dmy_hm(paste(Date,Time,sep=" ")))
head(counts)
dim(counts)
dim(counts %>% filter(Observer != "Maris"))
summary(counts$Number)
table(counts$Observer)
range(dmy(surveys$Date))


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     PLOT SIMPLE HISTOGRAM  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

counts %>% filter(!is.na(Number)) %>% #filter(Number<25) %>%
  group_by(Phase,Treatment,GlobalID) %>%
  summarise(N=sum(Number)) %>%
ggplot() + geom_histogram(aes(x=N)) + facet_wrap(Phase~Treatment)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     PLOT SIMPLE RAW DATA COMPARISONS WITH ERROR BARS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

SUMMARY<-counts %>% filter(!is.na(Number)) %>% #filter(Number<25) %>%
  #filter(Observer != "Maris") %>%
  #filter(Species=="Long-tailed Duck") %>%
  group_by(Phase,Treatment, GlobalID) %>%
  summarise(N=sum(Number)) %>%
  ungroup() %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(N), lcl=quantile(N,0.025),ucl=quantile(N,0.975))  
#fwrite(SUMMARY,"LEB_raw_data_summary.csv")

SUMMARY %>% #filter(Phase!="PrePhase1") %>%

ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  #guides(colour=guide_legend(title="Phase"))+
  facet_wrap(~Phase, ncol=2) +
  xlab("") +
  ylab("Number of birds") +
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
#ggsave("LEB_raw_data_LTDU_withMaris.jpg", width=8, height=11)
ggsave("LEB_raw_data_LTDU_withoutMaris.jpg", width=8, height=11)



### plot only seaducks ###
counts %>% filter(!is.na(Number)) %>% 
  filter(Species %in% c("Unknown Scoters","Velvet Scoter","Common Scoter","Unknown Eiders","Steller's Eider","Common Eider")) %>%
  group_by(Phase,Treatment,GlobalID) %>%
  summarise(N=sum(Number)) %>%
  ungroup() %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(N), lcl=quantile(N,0.025),ucl=quantile(N,0.975)) %>%
  filter(Phase!="PrePhase1") %>%
  
  ##plot
  ggplot(aes(y=mean, x=Treatment, colour=Phase)) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  guides(colour=guide_legend(title="Phase"))+
  facet_wrap(~Phase) +
  xlab("") +
  ylab("Number of seaducks") +
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
#ggsave("LEB_raw_data_seaducks.jpg", width=8, height=8)



### plot only long-tailed ducks ###
counts %>% filter(!is.na(Number)) %>% 
  filter(Species=="Long-tailed Duck") %>%
  group_by(Phase,Treatment,GlobalID) %>%
  summarise(N=sum(Number)) %>%
  ungroup() %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(N), lcl=quantile(N,0.025),ucl=quantile(N,0.975)) %>%
  filter(Phase!="PrePhase1") %>%
  
  ##plot
  ggplot(aes(y=mean, x=Treatment, colour=Phase)) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  guides(colour=guide_legend(title="Phase"))+
  facet_wrap(~Phase) +
  xlab("") +
  ylab("Number of seabirds (except LTDU)") +
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
#ggsave("LEB_raw_data_LTDU.jpg", width=8, height=8)
#ggsave("LEB_raw_data_allbirds_without_LTDU.jpg", width=8, height=8)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     FORMAL ANALYSIS OF LTDU ABUNDANCE BETWEEN CONTROL AND TREATMENT       ~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### SELECT ONLY THE RECORDS RELEVANT TO THE TEST
data<- counts %>% filter(!is.na(Number)) %>% 
  filter(Species=="Long-tailed Duck") %>%
  #filter(Phase %in% c("Phase1","Phase2")) %>%
  #filter(Observer != "Maris") %>%
  mutate(Observer=ifelse(Observer=="Ainar,Mati","Mati,Ainar",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Ainar,Andrus,Rein,Veljo,Mati,Andres,OtherObservers","OtherObservers",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Veljo,OtherObservers","Veljo",Observer)) %>%
  group_by(Phase,Treatment,CountSeq,PeriodCode,Observer,weather,wind_dir,Temp,wind_speed,cloud,sea,vis,Start) %>%
  summarise(N=sum(Number)) %>%
  mutate(day=yday(Start)) %>%
  mutate(hour=hour(Start)) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor)

hist(data$N)
dim(data)
table(data$Observer)
which(is.na(data)==T) ### no missing values



### FIT RANDOM FOREST MODEL
head(data)
RF<- randomForest(N~Phase+Treatment+day+hour+weather+wind_dir+Temp+wind_speed+cloud+sea+vis+Observer, data=data, mtry=5,ntree=500, importance=T)
RF

baselineRF<- randomForest(N~Treatment+day+hour+weather+wind_dir+Temp+wind_speed+cloud+sea+vis+Observer, data=data[data$Phase=="PrePhase1",], mtry=5,ntree=500, importance=T)
baselineRF



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 2. EVALUATE EXPLANATORY POWER OF RANDOM FOREST MODEL  ##########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## predict the 'out-of-bag' (OOB) data response from the model (those are the withheld cross-validation data 
data$pred<-predict(RF,OOB=T)

## calculate correlation between actual data and predicted data
cor(data$pred,data$N)

## plot the correlation to visually check how well the model fits
ggplot(data, aes(x=N, y=pred)) +
  geom_point(size=1.5, colour='lightblue') +
  geom_abline(aes(intercept = 0, slope=1),colour="lightgray", size=0.5)+
  geom_smooth(method=lm, colour="red", fill="red", size=1.5)+
  ylab("Predicted N of LTDU") +
  xlab("Observed N of LTDU") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3. CALCULATE VARIABLE IMPORTANCE AND PLOT IMPORTANCE ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## the default basic plot
varImpPlot(RF)
varImpPlot(baselineRF)

## advanced plot for publication
VAR<-importance(RF, type=1)
IMP<-data.frame(variable=row.names(VAR), MSE=VAR)
IMP<-IMP[order(IMP$X.IncMSE, decreasing=T),]  ## SORTED BY increase in mean squared error
IMP$rel_imp<-round((IMP$X.IncMSE/IMP$X.IncMSE[1])*100,2)
IMP$variable <- factor(IMP$variable, levels = IMP$variable[order(IMP$X.IncMSE)])  ## this ensures correct presentation on the plot
IMP

ggplot(IMP, aes(x=variable, y=rel_imp)) +
  geom_bar(stat='identity', fill='lightblue') +
  geom_text(aes(label=variable), position=position_dodge(width=0.1), hjust=-0.1,size=5, color="darkgrey")+
  coord_flip()+
  ylab("Variable importance (%)") +
  xlab("Explanatory variable") +
  scale_y_continuous(limits=c(0,115), breaks=seq(0,100,20), labels=seq(0,100,20))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_blank(), 
        axis.title=element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
ggsave("LBE_seabirds_variable_importance.jpg", width=8, height=9)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 4. CREATE PARTIAL DEPENDENCE PLOT FOR TREATMENT ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## this is the predicted effect of the different treatments accounting for all other variables
#partialPlot(RF, data, x.var=Treatment) ## does not work for character variables


## how to create your own partial Plot with 'party': http://stats.stackexchange.com/questions/83534/what-is-the-purpose-of-working-on-a-logit-scale-in-partial-dependence-plots?rq=1

table(data$Observer)

plotdat<-bind_rows(data,data) %>%
  mutate(Treatment=rep(unique(data$Treatment), each=dim(data)[1])) %>%
  filter(Observer=="Mati,Ainar") ## curtail to only one observer to plot effect irrespective of observer variation

plotdat %>%
  mutate(pred.num=predict(RF, newdat=plotdat)) %>%
  group_by(Treatment) %>%
  #summarise(mean=mean(pred.num), lcl=quantile(pred.num,0.25),ucl=quantile(pred.num,0.75)) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%

  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(0,8)) +
  xlab("") +
  ylab("Predicted number of LTDU") +
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

#ggsave("LEB_Treatment_effect_OtherObservers.jpg", width=8, height=8)
ggsave("LEB_Treatment_effect_AinarMati.jpg", width=8, height=8)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 5. CREATE PARTIAL DEPENDENCE PLOT FOR TREATMENT AND PHASE ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## this is the predicted effect of the different treatments accounting for all other variables
#partialPlot(RF, data, x.var=Treatment) ## does not work for character variables

plotdat<-bind_rows(replicate(10,data, simplify=F)) %>%
  mutate(Treatment=rep(rep(unique(data$Treatment), each=dim(data)[1]),5)) %>%
  mutate(Phase=rep(rep(unique(data$Phase), each=dim(data)[1]), each=2)) %>%
  filter(Observer=="Mati,Ainar") ## curtail to only one observer to plot effect irrespective of observer variation

#plotdat %>% group_by(Phase,Treatment) %>% summarise(N=length(Observer)) ## just to check whether all was done correctly

plotdat %>%
  mutate(pred.num=predict(RF, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  #summarise(mean=mean(pred.num), lcl=quantile(pred.num,0.25),ucl=quantile(pred.num,0.75)) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  
  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(0,8)) +
  facet_wrap(~Phase, ncol=2) +
  xlab("") +
  ylab("Predicted number of LTDU") +
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

ggsave("LEB_Treatment_effect_byPhase.jpg", width=8, height=11)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 5. CONDUCT PARAMETRIC BACI ANALYSIS ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### MANIPULATE DATA TO INCLUDE BEFORE-AFTER
head(data)

BACIdata <- data %>% mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0))
  

## fit models
m1<-glmmTMB(N~Before+Treatment+Before*Treatment+PhaseNum*Treatment+Memory*Treatment +day+hour+Observer+(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)
m0<-glmmTMB(N~Before+Treatment+PhaseNum*Treatment+Memory*Treatment+day+hour+Observer+(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)

## assess significance of interaction effect
anova(m0,m1)
m1sum<-summary(m1)


### PLOT predicted OUTPUT ###

plotdat<-bind_rows(replicate(10,data, simplify=F)) %>%
  mutate(Treatment=rep(rep(unique(data$Treatment), each=dim(data)[1]),5)) %>%
  mutate(Phase=rep(rep(unique(data$Phase), each=dim(data)[1]), each=2)) %>%
  mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0)) #%>%
  #filter(Observer=="Mati,Ainar")

plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  
  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(0,2.5)) +
  facet_wrap(~Phase, ncol=2) +
  xlab("") +
  ylab("Predicted number of LTDU") +
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

ggsave("LEB_Treatment_effect_byPhase_BACI.jpg", width=8, height=11)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 6. CALCULATE EFFECT SIZE ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
effsize<-plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num)) %>%
  spread(key=Treatment, value=mean) %>%
  mutate(reduction=((Control-Treatment)/Control)*100)
fwrite(effsize,"LEB_BACI_effect_size_perPhase.csv")






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 7. ASSESS MEMORY EFFECT ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## assess memory effect - is there habituation or memory effect?
### Hypothesis is that deterrence effect diminishes over time, i.e. pre- and post-phase should be similar
### only done for Phase 1 when we have pre- and post-phase observations
## requested by Yann Rouxel on 10 Aug 2020

## exclude pre-phase 1 data and call presence of LEB 'before' and post-phase 'after' to check for difference and direction of effect

MEMOdata <- data %>% filter(!Phase=="PrePhase1") %>%
  mutate(Before=ifelse(Phase %in% c("Phase2","Phase1"),"before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1))


m1mem<-glmmTMB(N~Before+Treatment+Before*Treatment+PhaseNum*Before+day+Observer+(1|PhaseNum), data=MEMOdata, ziformula=~1,family=nbinom2)
m0mem<-glmmTMB(N~Before+Treatment+PhaseNum*Before+day+Observer+(1|PhaseNum), data=MEMOdata, ziformula=~1,family=nbinom2)
anova(m0mem,m1mem)
m1sum.memory<-summary(m1mem)

### PLOT predicted OUTPUT ###

plotdat<-bind_rows(replicate(36,data, simplify=F)) %>%
  mutate(Observer=rep(rep(unique(MEMOdata$Observer), each=dim(data)[1]),6)) %>% ### causes strange 'subscript out of bounds' error in predict
  mutate(hour=rep(rep(c(6,8,10,12,14,16), dim(data)[1]),each=6)) %>% 
  filter(!Phase=="PrePhase1") %>%
  mutate(Before=ifelse(Phase %in% c("Phase2","Phase1"),"before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1))


plotdat %>%
  mutate(pred.num=predict(m1mem, newdat=plotdat)) %>%
  mutate(Before=ifelse(Before=="before","during LEB","post LEB")) %>%
  mutate(PhaseNum=paste("Phase",PhaseNum, sep=" ")) %>%
  group_by(PhaseNum,Treatment,Before) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  
  ggplot(aes(y=mean, x=Before)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(-1,4)) +
  facet_grid(PhaseNum~Treatment) +
  xlab("") +
  ylab("Predicted number of LTDU") +
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

ggsave("LEB_Memory_effect_byPhase_BACI.jpg", width=8, height=11)

### Quantify effect size

effsizeMEM<-plotdat %>%
  mutate(pred.num=predict(m1mem, newdat=plotdat)) %>%
  group_by(PhaseNum,Treatment,Before) %>%
  summarise(mean=mean(pred.num)) %>%
  spread(key=Before, value=mean) %>%
  mutate(reduction=((before-after)/before)*100)
fwrite(effsizeMEM,"LEB_BACI_MEMORY_effect_size_perPhase.csv")






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 8. ASSESS HABITUATION ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## assess habituation effect
### Hypothesis is that deterrence effect diminishes over time
### only done for deployment phases
## requested by Yann Rouxel on 10 Aug 2020

## exclude pre- and post-phase data
PhaseStart<-data %>% group_by(Phase) %>% summarise(start=min(day))

HABIdata <- data %>% filter(!Phase %in% c("PrePhase1","PostPhase1","PostPhase2")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  left_join(PhaseStart, by="Phase") %>%
  mutate(DAY=day-start)
unique(HABIdata$Observer)

m1hab<-glmmTMB(N~Treatment+DAY+DAY*Treatment+PhaseNum*DAY+hour+Observer+(1|PhaseNum), data=HABIdata, ziformula=~1,family=nbinom2)
m0hab<-glmmTMB(N~Treatment+DAY+PhaseNum*DAY+hour+Observer+(1|PhaseNum), data=HABIdata, ziformula=~1,family=nbinom2)
anova(m0hab,m1hab)
m1sum.habituation<-summary(m1hab)


### PLOT predicted OUTPUT ###
plotdat<-bind_rows(replicate(36,data, simplify=F)) %>%
  mutate(Observer=rep(rep(unique(HABIdata$Observer), each=dim(data)[1]),6)) %>% ### causes strange 'subscript out of bounds' error in predict
  mutate(hour=rep(rep(c(6,8,10,12,14,16), dim(data)[1]),each=6)) %>% 
  filter(!Phase %in% c("PrePhase1","PostPhase1","PostPhase2")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  left_join(PhaseStart, by="Phase") %>%
  mutate(DAY=day-start)

#plotdat<-expand.grid(Observer=unique(data$Observer),hour=as.integer(seq(6,18,1)),Treatment=unique(data$Treatment), DAY=as.numeric(seq(0:25)),PhaseNum=c(1,2))
#dim(plotdat)
plotdat  %>%
  mutate(pred.num=predict(m1hab, newdat=plotdat)) %>%
  mutate(PhaseNum=paste("Phase",PhaseNum, sep=" ")) %>%
  group_by(PhaseNum,Treatment,DAY) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  
  ggplot(aes(y=mean, x=DAY)) + geom_line(size=1, colour="firebrick")+
  geom_ribbon(aes(x=DAY, ymin=lcl,ymax=ucl),alpha=0.2)+
  scale_y_continuous(limits=c(0,2.5)) +
  facet_grid(PhaseNum~Treatment) +
  xlab("") +
  ylab("Predicted number of LTDU") +
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

ggsave("LEB_Habituation_effect_byPhase.jpg", width=8, height=11)






#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     FORMAL ANALYSIS FOR ALL SEABIRDS EXCEPT LTDU      ~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
unique(counts$Species)

### SELECT ONLY THE RECORDS RELEVANT TO THE TEST
data<- counts %>% filter(!is.na(Number)) %>% 
  filter(Species!= "Long-tailed Duck") %>%   ##"everything except LTDU
  mutate(Observer=ifelse(Observer=="Ainar,Mati","Mati,Ainar",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Ainar,Andrus,Rein,Veljo,Mati,Andres,OtherObservers","OtherObservers",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Veljo,OtherObservers","Veljo",Observer)) %>%
  group_by(Phase,Treatment,CountSeq,PeriodCode,Observer,weather,wind_dir,Temp,wind_speed,cloud,sea,vis,Start) %>%
  summarise(N=sum(Number)) %>%
  mutate(day=yday(Start)) %>%
  mutate(hour=hour(Start)) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor)

hist(data$N)
dim(data)
table(data$Observer)



##### CONDUCT PARAMETRIC BACI ANALYSIS ######

BACIdata <- data %>% mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0))

## fit models - tried all options to include more interactions and variables, but convergence problems if we add more
m1<-glmmTMB(N~Before+Treatment+Before*Treatment+Memory*Treatment +day+hour +(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)
m0<-glmmTMB(N~Before+Treatment+Memory*Treatment +day+hour +(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)

## assess significance of interaction effect
anova(m0,m1)
m1sum<-summary(m1)


### PLOT predicted OUTPUT ###

plotdat<-bind_rows(replicate(10,data, simplify=F)) %>%
  mutate(Treatment=rep(rep(unique(data$Treatment), each=dim(data)[1]),5)) %>%
  mutate(Phase=rep(rep(unique(data$Phase), each=dim(data)[1]), each=2)) %>%
  mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0)) #%>%


#### CALCULATE EFFECT SIZE ######
effsize<-plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num)) %>%
  spread(key=Treatment, value=mean) %>%
  mutate(reduction=((Control-Treatment)/Control)*100)
#fwrite(effsize,"LEB_BACI_effect_size_perPhase_seabirds.csv")

plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  
  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  #scale_y_continuous(limits=c(0,2.5)) +
  facet_wrap(~Phase, ncol=2) +
  xlab("") +
  ylab("Predicted number of seabirds other than LTDU") +
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

#ggsave("LEB_Treatment_effect_byPhase_BACI_seabirds.jpg", width=8, height=11)







#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     FORMAL ANALYSIS FOR EIDERS AND SCOTERS      ~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
unique(counts$Species)

### SELECT ONLY THE RECORDS RELEVANT TO THE TEST
data<- counts %>% filter(!is.na(Number)) %>% 
  filter(Species %in% c("Unknown Eiders","Steller's Eider","Common Eider")) %>%   ##"Unknown Scoters","Velvet Scoter","Common Scoter",
  mutate(Observer=ifelse(Observer=="Ainar,Mati","Mati,Ainar",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Ainar,Andrus,Rein,Veljo,Mati,Andres,OtherObservers","OtherObservers",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Veljo,OtherObservers","Veljo",Observer)) %>%
  group_by(Phase,Treatment,CountSeq,PeriodCode,Observer,weather,wind_dir,Temp,wind_speed,cloud,sea,vis,Start) %>%
  summarise(N=sum(Number)) %>%
  mutate(day=yday(Start)) %>%
  mutate(hour=hour(Start)) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor)

hist(data$N)
dim(data)
table(data$Observer)



##### CONDUCT PARAMETRIC BACI ANALYSIS ######

BACIdata <- data %>% mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0))

## fit models - tried all options to include more interactions and variables, but convergence problems if we add more
m1<-glmmTMB(N~Before+Treatment+Before*Treatment+Memory*Treatment +day+hour +(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)
m0<-glmmTMB(N~Before+Treatment+Memory*Treatment +day+hour +(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)

## assess significance of interaction effect
anova(m0,m1)
m1sum<-summary(m1)


### PLOT predicted OUTPUT ###

plotdat<-bind_rows(replicate(10,data, simplify=F)) %>%
  mutate(Treatment=rep(rep(unique(data$Treatment), each=dim(data)[1]),5)) %>%
  mutate(Phase=rep(rep(unique(data$Phase), each=dim(data)[1]), each=2)) %>%
  mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0)) #%>%


#### CALCULATE EFFECT SIZE ######
effsize<-plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num)) %>%
  spread(key=Treatment, value=mean) %>%
  mutate(reduction=((Control-Treatment)/Control)*100)
fwrite(effsize,"LEB_BACI_effect_size_perPhase_eiders.csv")

plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  
  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  #scale_y_continuous(limits=c(0,2.5)) +
  facet_wrap(~Phase, ncol=2) +
  xlab("") +
  ylab("Predicted number of eiders and scoters") +
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

ggsave("LEB_Treatment_effect_byPhase_BACI_eiders.jpg", width=8, height=11)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     FORMAL ANALYSIS FOR MERGANSERS      ~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
unique(counts$Species)

### SELECT ONLY THE RECORDS RELEVANT TO THE TEST
data<- counts %>% filter(!is.na(Number)) %>% 
  filter(Species %in% c("Uknown Mergansers","Red-breasted Merganser","Common Merganser","Smew")) %>%   ##"Unknown Scoters","Velvet Scoter","Common Scoter",
  mutate(Observer=ifelse(Observer=="Ainar,Mati","Mati,Ainar",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Ainar,Andrus,Rein,Veljo,Mati,Andres,OtherObservers","OtherObservers",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Veljo,OtherObservers","Veljo",Observer)) %>%
  group_by(Phase,Treatment,CountSeq,PeriodCode,Observer,weather,wind_dir,Temp,wind_speed,cloud,sea,vis,Start) %>%
  summarise(N=sum(Number)) %>%
  mutate(day=yday(Start)) %>%
  mutate(hour=hour(Start)) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor)

hist(data$N)
dim(data)
table(data$Observer)



##### CONDUCT PARAMETRIC BACI ANALYSIS ######

BACIdata <- data %>% mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0))

## fit models - tried all options to include more interactions and variables, but convergence problems if we add more
m1<-glmmTMB(N~Before+Treatment+Before*Treatment+Memory*Treatment +day+hour +(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)
m0<-glmmTMB(N~Before+Treatment+Memory*Treatment +day+hour +(1|PhaseNum), data=BACIdata, ziformula=~1,family=nbinom2)

## assess significance of interaction effect
anova(m0,m1)
m1sum<-summary(m1)


### PLOT predicted OUTPUT ###

plotdat<-bind_rows(replicate(10,data, simplify=F)) %>%
  mutate(Treatment=rep(rep(unique(data$Treatment), each=dim(data)[1]),5)) %>%
  mutate(Phase=rep(rep(unique(data$Phase), each=dim(data)[1]), each=2)) %>%
  mutate(Before=ifelse(Phase=="PrePhase1","before","after")) %>%
  mutate(PhaseNum=ifelse(Phase %in% c("Phase2","PostPhase2"),2,1)) %>%
  mutate(Memory=ifelse(Phase %in% c("PostPhase1","PostPhase2"),1,0)) #%>%


#### CALCULATE EFFECT SIZE ######
effsize<-plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num)) %>%
  spread(key=Treatment, value=mean) %>%
  mutate(reduction=((Control-Treatment)/Control)*100)
#fwrite(effsize,"LEB_BACI_effect_size_perPhase_mergansers.csv")

plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  
  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  #scale_y_continuous(limits=c(0,2.5)) +
  facet_wrap(~Phase, ncol=2) +
  xlab("") +
  ylab("Predicted number of mergansers") +
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

#ggsave("LEB_Treatment_effect_byPhase_BACI_mergansers.jpg", width=8, height=11)









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### CONDUCT PARAMETRIC BACI ANALYSIS FOR LTDU HABITUATION IN PHASE 1 ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Hypothesis is that deterrence effect diminishes over time, i.e. pre- and post-phase should be similar
### only done for Phase 1 when we have pre- and post-phase observations
## requested by Yann Rouxel on 10 Aug 2020




