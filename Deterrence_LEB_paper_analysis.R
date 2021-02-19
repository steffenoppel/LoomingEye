### ##################################################
### BALTIC SEA gillnet bycatch - test of LOOMING EYE BUOY (LEB) TO DETER DUCKS
### written by steffen.oppel@rspb.org.uk
### ##################################################

## originally created 19 April 2020
## modified on 28 April 2020 to include Yann's comments
## temporarily removed counts from Maris as they seem to be completely opposite to anyone else - but then reinserted
## updated on 3 May 2020: included sections for other species

## REDUCED AND BRANCHED ON 28 SEPT 2020 to only retain paper-relevant code. See Deterrence_LEB_analysis.R for full code

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
         weather=`Sun or Precipitation:`,wind_dir=`Wind Direction:`,Temp=`Air Temperature (°C):`,
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
#####     SUMMARISE DATA FOR RESULTS TEXT  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

summary(counts$Number)
table(counts$Observer)
range(dmy(surveys$Date))


SUMMARY<-counts %>% filter(!is.na(Number)) %>%
  filter(Species=="Long-tailed Duck") %>%
  group_by(Phase,Treatment, GlobalID,CountSeq) %>%
  summarise(N=sum(Number), n_counts=length(unique(GlobalID))) %>%
  ungroup() %>%
  group_by(Phase) %>%
  summarise(n_counts=sum(n_counts), mean=mean(N), lcl=quantile(N,0.25),ucl=quantile(N,0.975), max=max(N))
SUMMARY


ZEROCOUNTS<-counts %>% filter(!is.na(Number)) %>%
  filter(Species=="Long-tailed Duck") %>%
  group_by(GlobalID,CountSeq) %>%
  summarise(N=sum(Number), n_counts=length(unique(GlobalID))) %>%
  ungroup() %>%
  group_by(N) %>%
  summarise(n_counts=(sum(n_counts)/SUMMARY$n_counts)*100)
ZEROCOUNTS


MONTHS<-counts %>% filter(!is.na(Number)) %>%
  filter(Species=="Long-tailed Duck") %>%
  mutate(MONTH=month(Start)) %>%
  group_by(GlobalID,Treatment,CountSeq, MONTH) %>%
  summarise(N=sum(Number), n_counts=length(unique(GlobalID))) %>%
  ungroup() %>%
  group_by(MONTH) %>%
  summarise(n_counts=sum(n_counts), mean=mean(N))
MONTHS
sum(MONTHS$n_counts)

#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     FORMAL ANALYSIS OF LTDU ABUNDANCE BETWEEN CONTROL AND TREATMENT       ~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
setwd("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\LEB_Baltic_bycatch")

### SELECT ONLY THE RECORDS RELEVANT TO THE TEST
data<- counts %>% filter(!is.na(Number)) %>% 
  filter(Species=="Long-tailed Duck") %>%
  mutate(Observer=ifelse(Observer=="Ainar,Mati","Mati,Ainar",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Ainar,Andrus,Rein,Veljo,Mati,Andres,OtherObservers","OtherObservers",Observer)) %>%
  mutate(Observer=ifelse(Observer=="Veljo,OtherObservers","Veljo",Observer)) %>%
  group_by(Phase,Treatment,CountSeq,PeriodCode,Observer,weather,wind_dir,Temp,wind_speed,cloud,sea,vis,Start) %>%
  summarise(N=sum(Number)) %>%
  mutate(day=yday(Start)) %>%
  mutate(hour=hour(Start)) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor)

hist(data$N[data$Phase=="PostPhase2" & data$Treatment=="Treatment"])
dim(data)
table(data$Observer)
which(is.na(data)==T) ### no missing values



### FIT RANDOM FOREST MODEL
head(data)
RF<- randomForest(N~Phase+Treatment+day+hour+weather+wind_dir+Temp+wind_speed+cloud+sea+vis+Observer, data=data, mtry=5,ntree=500, importance=T)
RF



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

## advanced plot for publication
## NOTE THAT variable list must follow order of formula in RF call!
VAR<-importance(RF, type=1)
IMP<-data.frame(variable=c("Experimental phase",
                            "Looming eye buoy",
                           "Date",
                           "Time of day",
                           "Weather",
                           "Wind direction",
                           "Temperature",
                           "Wind speed",
                           "Cloud cover",
                           "Sea state",
                           "Visibility",
                           "Observer"), MSE=VAR) %>%      ###row.names(VAR) for automatic variable list
  arrange(desc(X.IncMSE))    ## SORTED BY increase in mean squared error

IMP$rel_imp<-round((IMP$X.IncMSE/IMP$X.IncMSE[1])*100,2)
IMP$variable <- factor(IMP$variable, levels = IMP$variable[order(IMP$X.IncMSE)])  ## this ensures correct presentation on the plot
IMP


### CREATE PLOT

ggplot(IMP, aes(x=variable, y=rel_imp)) +
  geom_bar(stat='identity', fill='lightblue') +
  geom_text(aes(label=variable), position=position_dodge(width=0.1), hjust=-0.1,size=5, color="darkgrey")+
  coord_flip()+
  ylab("Variable importance (%)") +
  xlab("Explanatory variable") +
  scale_y_continuous(limits=c(0,120), breaks=seq(0,100,20), labels=seq(0,100,20))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_blank(), 
        axis.title=element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
ggsave("Fig2_variable_importance.jpg", width=10, height=9)





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
  ungroup() %>%
  mutate(Phase=ifelse(Phase=="PrePhase1", "Before LEB",as.character(Phase))) %>%
  mutate(Phase=ifelse(Phase=="Phase1", "During LEB (1)",as.character(Phase))) %>%
  mutate(Phase=ifelse(Phase=="Phase2", "During LEB (2)",as.character(Phase))) %>%
  mutate(Phase=ifelse(Phase=="PostPhase1", "After LEB (1)",as.character(Phase))) %>%
  mutate(Phase=ifelse(Phase=="PostPhase2", "After LEB (2)",as.character(Phase))) %>%
  transform(Phase=factor(Phase,levels=c("After LEB (1)","After LEB (2)","During LEB (1)","During LEB (2)","Before LEB"))) %>% 
  
  
  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(0,2.5)) +
  facet_wrap(~Phase, ncol=2, dir="h",as.table=F) +
  xlab("") +
  ylab("Predicted number of Long-tailed Ducks") +
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

ggsave("Fig3_predicted_abundance.jpg", width=8, height=11)


########## ALTERNATIVE AND LESS CONFUSING FIGURE FOR MANUSCRIPT  ########

plotdat %>%
  mutate(pred.num=predict(m1, newdat=plotdat)) %>%
  mutate(Phase=ifelse(Phase=="PrePhase1", "Before","After")) %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  ungroup() %>%
  transform(Phase=factor(Phase,levels=c("Before","After"))) %>% 
  

  ggplot(aes(y=mean, x=Treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(0,2.5)) +
  #facet_wrap(~Phase, ncol=1, dir="h",as.table=F) +
  facet_wrap(~Phase, ncol=1) +
  xlab("") +
  ylab("Predicted number of Long-tailed Ducks") +
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

ggsave("Fig4_BACI_plot.jpg", width=8, height=11)








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


m1mem<-glmmTMB(N~Before+Treatment+Before*Treatment*PhaseNum+day+Observer+(1|PhaseNum), data=MEMOdata, ziformula=~1,family=nbinom2)
m0mem<-glmmTMB(N~Before+PhaseNum*Before+day+Observer+(1|PhaseNum), data=MEMOdata, ziformula=~1,family=nbinom2)
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
  mutate(Before=ifelse(Before=="before","During LEB","After LEB")) %>%
  transform(Before=factor(Before,levels=c("During LEB","After LEB"))) %>%
  mutate(PhaseNum=paste("Phase",PhaseNum, sep=" ")) %>%
  group_by(PhaseNum,Treatment,Before) %>%
  summarise(mean=mean(pred.num), lcl=mean(pred.num)-0.5*sd(pred.num),ucl=mean(pred.num)+0.5*sd(pred.num)) %>%
  mutate(mean=ifelse(mean<0,0,mean), ucl=ifelse(ucl<0,0,ucl), lcl=ifelse(lcl<0,0,lcl)) %>%
  
  ggplot(aes(y=mean, x=Before)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  scale_y_continuous(limits=c(0,3.5), breaks=c(0,1,2,3), labels=c(0,1,2,3)) +
  facet_grid(PhaseNum~Treatment) +
  xlab("") +
  ylab("Predicted number of Long-tailed Ducks") +
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

ggsave("FigureS1_memory_effect.jpg", width=8, height=11)

### Quantify effect size

effsizeMEM<-plotdat %>%
  mutate(pred.num=predict(m1mem, newdat=plotdat)) %>%
  group_by(PhaseNum,Treatment,Before) %>%
  summarise(mean=mean(pred.num)) %>%
  mutate(mean=ifelse(mean<0,0,mean)) %>%
  spread(key=Before, value=mean) %>%
  mutate(diff=(after-before)) %>%
  select(-after,-before) %>%
  spread(key=Treatment, value=diff) %>%
  mutate(reduction=((Treatment-Control)/Control)*100)
effsizeMEM




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

m1hab<-glmmTMB(N~Treatment+DAY+DAY*Treatment*PhaseNum+hour+Observer+(1|PhaseNum), data=HABIdata, ziformula=~1,family=nbinom2)
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
  mutate(mean=ifelse(mean<0,0,mean), ucl=ifelse(ucl<0,0,ucl), lcl=ifelse(lcl<0,0,lcl)) %>%
  
  ggplot(aes(y=mean, x=DAY)) + geom_line(size=1, colour="firebrick")+
  geom_ribbon(aes(x=DAY, ymin=lcl,ymax=ucl),alpha=0.2)+
  scale_y_continuous(limits=c(0,2.5)) +
  facet_grid(PhaseNum~Treatment) +
  xlab("Days since LEB deployment") +
  ylab("Predicted number of Long-tailed Ducks") +
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

ggsave("FigS2_Habituation_effect.jpg", width=8, height=11)



effsizeHAB<-plotdat %>%
  mutate(pred.num=predict(m1hab, newdat=plotdat)) %>%
  group_by(PhaseNum,Treatment,DAY) %>%
  summarise(mean=mean(pred.num)) %>%
  filter(DAY %in% c(0,10)) %>%
  spread(key=DAY, value=mean) %>%
  mutate(slope=abs((`10`-`0`))) %>%
  select(-`10`,-`0`) %>%
  spread(key=Treatment, value=slope) %>%
  mutate(reduction=(Treatment/Control)) %>%
  mutate(increase=(Control/Treatment))
mean(c(effsizeHAB$reduction[1],effsizeHAB$increase[2]))


