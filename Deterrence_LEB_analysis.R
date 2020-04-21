### ##################################################
### BALTIC SEA gillnet bycatch - test of LOOMING EYE BUOY (LEB) TO DETER DUCKS
### written by steffen.oppel@rspb.org.uk
### ##################################################

### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(lme4)
library(glmmTMB)
library(randomForest)
filter<-dplyr::filter
select<-dplyr::select



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


#setwd("A:\\RSPB\\Marine\\Bycatch\\GillnetBycatch")
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\RawData")

# Read the data from formatted CSV files (one for each mitigation trial)
surveys <- read_excel("Estonia_Seabird_Bycatch_Project_FINAL_DATA_YR.xlsx", sheet="Estonia_Seabird_Bycatch_Pro_0")
counts <- read_excel("Estonia_Seabird_Bycatch_Project_FINAL_DATA_YR.xlsx", sheet="Count Session bird groups SUM")
counts$GlobalID<-counts$ParentGlobalID

### FORMAT DATA (remove columns we don't need and format the rest)
names(surveys)
surveys<-surveys[,c(2,3,8:11,13:19)] %>%
  rename(Date=`Survey Date`, 
         PeriodCode=`Survey Period Code:`,Location=`Location:`,Phase=`Phase:`, Observer=`Observer(s):`,
         weather=`Sun or Precipitation:`,wind_dir=`Wind Direction:`,Temp=`Air Temperature (°C):`,
         wind_speed=`Wind Speed (km/h):`, cloud=`Cloud Cover:`, sea=`Sea Conditions:`,vis=`Visibility:`)

counts<-counts[,c(1:96)] %>% select(-c(1,3,5,which(grepl("SUM",names(counts))),which(grepl("Additional",names(counts))))) %>%
  rename(Time=`Count Time:`, CountSeq=`Count Number:`) %>%
  gather(key=spec,value=Number,-GlobalID,-Phase,-Time,-CountSeq) %>%
  separate(spec,into=c("Treatment","Species"),sep="_") %>%
  left_join(surveys, by=c("GlobalID","Phase")) %>%
  mutate(Start=dmy_hm(paste(Date,Time,sep=" ")))
head(counts)
dim(counts)
summary(counts$Number)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     PLOT SIMPLE HISTOGRAM  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

counts %>% filter(!is.na(Number)) %>% #filter(Number<25) %>%
  group_by(Phase,Treatment,GlobalID) %>%
  summarise(N=sum(Number)) %>%
ggplot() + geom_histogram(aes(x=N)) + facet_wrap(Phase~Treatment)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     PLOT SIMPLE COMPARISONS WITH ERROR BARS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


SUMMARY<-counts %>% filter(!is.na(Number)) %>% #filter(Number<25) %>%
  group_by(Phase,Treatment,GlobalID) %>%
  summarise(N=sum(Number)) %>%
  ungroup() %>%
  group_by(Phase,Treatment) %>%
  summarise(mean=mean(N), lcl=quantile(N,0.025),ucl=quantile(N,0.975))  


SUMMARY %>% filter(Phase!="PrePhase1") %>%
ggplot(aes(y=mean, x=Treatment, colour=Phase)) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03)+
  guides(colour=guide_legend(title="Phase"))+
  facet_wrap(~Phase) +
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
#ggsave("LEB_raw_data_all_species.jpg", width=8, height=8)



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
ggsave("LEB_raw_data_LTDU.jpg", width=8, height=8)
#ggsave("LEB_raw_data_allbirds_without_LTDU.jpg", width=8, height=8)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     FORMAL ANALYSIS OF LTDU ABUNDANCE BETWEEN CONTROL AND TREATMENT       ~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### SELECT ONLY THE RECORDS RELEVANT TO THE TEST
data<- counts %>% filter(!is.na(Number)) %>% 
  filter(Species=="Long-tailed Duck") %>%
  filter(Phase %in% c("Phase1","Phase2")) %>%
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
varImpPlot(RF)
partialPlot(RF,pred.datax.var="Treatment")







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
ggsave("LBE_LTDU_variable_importance.jpg", width=8, height=9)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 4. CREATE PARTIAL DEPENDENCE PLOT FOR TREATMENT ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## this is the predicted effect of the different treatments accounting for all other variables
#partialPlot(RF, data, x.var=Treatment) ## does not work for character variables










