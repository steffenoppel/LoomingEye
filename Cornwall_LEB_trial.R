### ##################################################
### CORNWALL LEB AND KITE TRIALS - test of LOOMING EYE BUOY (LEB) TO DETER BIRDS
### written by steffen.oppel@rspb.org.uk
### ##################################################

## copied from Lithuanian LEB trials
## modified 29 March 2022
# modified 1 April to include mesh size

# modified 23 April to account for sampling imbalance of mesh size (requested by Yann Rouxel)

### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
filter<-dplyr::filter
select<-dplyr::select



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\RawData")

# Read the data from formatted CSV files (one for each mitigation trial)
sets <- read_excel("Cornwall_DatabaseLEB_trials.xlsx", sheet="Sheet1")
hour(sets$Depl_Date)=hour(sets$Depl_Time)
minute(sets$Depl_Date)=minute(sets$Depl_Time)
hour(sets$Haul_Date)=hour(sets$Haul_Time)
minute(sets$Haul_Date)=minute(sets$Haul_Time)
names(sets)
dim(sets)
#str(sets)

### FORMAT DATA (remove columns we don't need and format the rest)
names(sets)
unique(sets$Mesh_Size)
sets<-sets %>% rename(bycatch=`Cormorant/Shag Bycatch`,length=`Net_Lenght (m)`,mesh=`Mesh_Size (mm)`) %>%
  #mutate(start=ymd_hms(paste(Depl_Date,Depl_Time)), end=ymd_hms(paste(Haul_Date,Haul_Time)))
  mutate(effort=as.numeric(difftime(Haul_Date,Depl_Date,'hours'))) %>%
  select (Trip_ID,Set_ID,Depl_Date, Mitigation,effort, bycatch, length, mesh) %>%
  mutate(BPUE=(bycatch/(effort*length))) %>%  ### scale number of cormorants over 5 days
  mutate(Month=month(Depl_Date)) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(bycatch_bin=ifelse(bycatch>0,1,0))

dim(sets)
str(sets)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT SIMPLE BOOTSTRAP ANALYSIS ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye")
controls<- sets %>% filter(Mitigation=="Control")
LEBs<- sets %>% filter(Mitigation=="LEB")
kites<- sets %>% filter(Mitigation=="Kite")
range(sets$BPUE)
table(sets$bycatch)
range(LEBs$BPUE)
range(controls$BPUE)

## bootstrap test
boot.samples <- matrix(sample(controls$BPUE, size = 10000 * nrow(controls), replace = TRUE),10000, nrow(controls))
boot.statistics <- apply(boot.samples, 1, mean)
RATE_control<-data.frame(treatment="control",mean=mean(boot.statistics),
                  lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

LEB.samples <- matrix(sample(LEBs$BPUE, size = 10000 * nrow(LEBs), replace = TRUE),10000, nrow(LEBs))
LEB.statistics <- apply(LEB.samples, 1, mean)
RATE_LEB<-data.frame(treatment="with LEB",mean=mean(LEB.statistics),
                         lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))

kite.samples <- matrix(sample(kites$BPUE, size = 10000 * nrow(kites), replace = TRUE),10000, nrow(kites))
kite.statistics <- apply(kite.samples, 1, mean)
RATE_kite<-data.frame(treatment="with kite",mean=mean(kite.statistics),
                     lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

RATE_DIFF<-data.frame(treatment="difference",mean=mean(LEB.statistics-boot.statistics),
                     lcl=quantile(LEB.statistics-boot.statistics,0.025),ucl=quantile(LEB.statistics-boot.statistics,0.975))
(RATE_DIFF[,2:4]/RATE_control[,2:4])*100




### PLOT predicted OUTPUT ###

bind_rows(RATE_control,RATE_LEB, RATE_kite) %>%
  ggplot(aes(y=mean*7200, x=treatment)) + geom_point(size=2, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl*7200, ymax=ucl*7200), width=.03)+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
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

ggsave("Cornwall_LEB_trial_BPUE.jpg", width=11, height=8)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT PAIRED SAMPLE BOOTSTRAP ANALYSIS ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

controls.p<- controls %>% select(Trip_ID,Depl_Date,BPUE) %>%
  rename(Control=BPUE)
LEBs.p<- LEBs %>% select(Trip_ID,BPUE) %>%
  rename(LEB=BPUE)
kites.p<- kites %>% select(Trip_ID,BPUE) %>%
  rename(Kite=BPUE)
trials<-controls.p %>% left_join(LEBs.p, by=c("Trip_ID")) %>%
  left_join(kites.p, by=c("Trip_ID")) %>%
  mutate(Diff_CL=Control-LEB,Diff_CK=Control-Kite,Diff_LK=LEB-Kite)
head(trials)






## bootstrap test
boot.samples <- matrix(sample(trials$Diff_CL[!is.na(trials$Diff_CL)], size = 10000 * nrow(trials), replace = TRUE),10000, nrow(trials))
boot.statistics <- apply(boot.samples, 1, mean)
LEB_control<-data.frame(comparison="control-LEB",mean=mean(boot.statistics),
                         lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

boot.samples <- matrix(sample(trials$Diff_CK[!is.na(trials$Diff_CK)], size = 10000 * nrow(trials), replace = TRUE),10000, nrow(trials))
boot.statistics <- apply(boot.samples, 1, mean)
KITE_control<-data.frame(comparison="control-Kite",mean=mean(boot.statistics),
                         lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

boot.samples <- matrix(sample(trials$Diff_LK[!is.na(trials$Diff_LK)], size = 10000 * nrow(trials), replace = TRUE),10000, nrow(trials))
boot.statistics <- apply(boot.samples, 1, mean)
KITE_LEB<-data.frame(comparison="LEB-Kite",mean=mean(boot.statistics),
                         lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))



### PLOT predicted OUTPUT ###

bind_rows(LEB_control,KITE_control, KITE_LEB) %>%
  ggplot(aes(y=mean*7200, x=comparison)) + 
  geom_hline(yintercept=0,colour='cornflowerblue', linetype='dashed', size=2) +
  geom_point(size=3, colour="firebrick")+
  geom_errorbar(aes(ymin=lcl*7200, ymax=ucl*7200), width=.03)+
  scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1,0.1)) +
  xlab("") +
  ylab("Difference in bycatch rate (birds / day)") +
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

ggsave("Cornwall_BPUE_comparison.jpg", width=11, height=8)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT BOOTSTRAP ANALYSIS WITH MESH SIZE ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye")
controls.s<- sets %>% filter(Mitigation=="Control") %>% filter(mesh<80)
LEBs.s<- sets %>% filter(Mitigation=="LEB") %>% filter(mesh<80)
kites.s<- sets %>% filter(Mitigation=="Kite") %>% filter(mesh<80)
controls.l<- sets %>% filter(Mitigation=="Control") %>% filter(mesh>80)
LEBs.l<- sets %>% filter(Mitigation=="LEB") %>% filter(mesh>80)
kites.l<- sets %>% filter(Mitigation=="Kite") %>% filter(mesh>80)

## bootstrap test
boot.samples <- matrix(sample(controls.s$BPUE, size = 10000 * nrow(controls.s), replace = TRUE),10000, nrow(controls.s))
boot.statistics <- apply(boot.samples, 1, mean)
RATE_control.s<-data.frame(mesh= "small (<80mm)", treatment="control",mean=mean(boot.statistics),
                         lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

LEB.samples <- matrix(sample(LEBs.s$BPUE, size = 10000 * nrow(LEBs.s), replace = TRUE),10000, nrow(LEBs.s))
LEB.statistics <- apply(LEB.samples, 1, mean)
RATE_LEB.s<-data.frame(mesh= "small (<80mm)", treatment="with LEB",mean=mean(LEB.statistics),
                     lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))

kite.samples <- matrix(sample(kites.s$BPUE, size = 10000 * nrow(kites.s), replace = TRUE),10000, nrow(kites.s))
kite.statistics <- apply(kite.samples, 1, mean)
RATE_kite.s<-data.frame(mesh= "small (<80mm)", treatment="with kite",mean=mean(kite.statistics),
                      lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))

boot.samples <- matrix(sample(controls.l$BPUE, size = 10000 * nrow(controls.l), replace = TRUE),10000, nrow(controls.l))
boot.statistics <- apply(boot.samples, 1, mean)
RATE_control.l<-data.frame(mesh= "large (>80mm)", treatment="control",mean=mean(boot.statistics),
                           lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

LEB.samples <- matrix(sample(LEBs.l$BPUE, size = 10000 * nrow(LEBs.l), replace = TRUE),10000, nrow(LEBs.l))
LEB.statistics <- apply(LEB.samples, 1, mean)
RATE_LEB.l<-data.frame(mesh= "large (>80mm)", treatment="with LEB",mean=mean(LEB.statistics),
                       lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))

kite.samples <- matrix(sample(kites.l$BPUE, size = 10000 * nrow(kites.l), replace = TRUE),10000, nrow(kites.l))
kite.statistics <- apply(kite.samples, 1, mean)
RATE_kite.l<-data.frame(mesh= "large (>80mm)", treatment="with kite",mean=mean(kite.statistics),
                        lcl=quantile(kite.statistics,0.025),ucl=quantile(kite.statistics,0.975))




### PLOT predicted OUTPUT ###

bind_rows(RATE_control.s,RATE_LEB.s, RATE_kite.s,RATE_control.l,RATE_LEB.l, RATE_kite.l) %>%
  ggplot(aes(y=mean*7200, x=treatment,colour=mesh)) + geom_point(size=2, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lcl*7200, ymax=ucl*7200, colour=mesh), width=.03, position=position_dodge(width=0.5))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  xlab("") +
  ylab("Bycatch in 300 m gillnet (birds / day)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        legend.position=c(0.2,0.85),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("Cornwall_LEB_trial_BPUE_meshsize.jpg", width=11, height=8)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  CONDUCT PAIRED SAMPLE BOOTSTRAP ANALYSIS WITH MESH SIZE ######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

controls<- controls %>% select(Trip_ID,Depl_Date,BPUE, mesh) %>%
  rename(Control=BPUE)
LEBs<- LEBs %>% select(Trip_ID,BPUE) %>%
  rename(LEB=BPUE)
kites<- kites %>% select(Trip_ID,BPUE) %>%
  rename(Kite=BPUE)
trials<-controls %>% left_join(LEBs, by=c("Trip_ID")) %>%
  left_join(kites, by=c("Trip_ID")) %>%
  mutate(Diff_CL=Control-LEB,Diff_CK=Control-Kite,Diff_LK=LEB-Kite)
head(trials)






## bootstrap test
boot.samples <- matrix(sample(trials$Diff_CL[!is.na(trials$Diff_CL) & trials$mesh>80], size = 10000 * nrow(trials[!is.na(trials$Diff_CL) & trials$mesh>80,]), replace = TRUE),10000, nrow(trials[!is.na(trials$Diff_CL) & trials$mesh>80,]))
boot.statistics <- apply(boot.samples, 1, mean)
LEB_control.l<-data.frame(mesh= "large (>80mm)", comparison="control-LEB",mean=mean(boot.statistics),
                        lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

boot.samples <- matrix(sample(trials$Diff_CK[!is.na(trials$Diff_CK) & trials$mesh>80], size = 10000 * nrow(trials[!is.na(trials$Diff_CK) & trials$mesh>80,]), replace = TRUE),10000, nrow(trials[!is.na(trials$Diff_CK) & trials$mesh>80,]))
boot.statistics <- apply(boot.samples, 1, mean)
KITE_control.l<-data.frame(mesh= "large (>80mm)", comparison="control-Kite",mean=mean(boot.statistics),
                         lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

boot.samples <- matrix(sample(trials$Diff_LK[!is.na(trials$Diff_LK) & trials$mesh>80], size = 10000 * nrow(trials[!is.na(trials$Diff_LK) & trials$mesh>80,]), replace = TRUE),10000, nrow(trials[!is.na(trials$Diff_LK) & trials$mesh>80,]))
boot.statistics <- apply(boot.samples, 1, mean)
KITE_LEB.l<-data.frame(mesh= "large (>80mm)", comparison="LEB-Kite",mean=mean(boot.statistics),
                     lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))


boot.samples <- matrix(sample(trials$Diff_CL[!is.na(trials$Diff_CL) & trials$mesh<80], size = 10000 * nrow(trials[!is.na(trials$Diff_CL) & trials$mesh<80,]), replace = TRUE),10000, nrow(trials[!is.na(trials$Diff_CL) & trials$mesh<80,]))
boot.statistics <- apply(boot.samples, 1, mean)
LEB_control.s<-data.frame(mesh= "small (<80mm)", comparison="control-LEB",mean=mean(boot.statistics),
                          lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

boot.samples <- matrix(sample(trials$Diff_CK[!is.na(trials$Diff_CK) & trials$mesh<80], size = 10000 * nrow(trials[!is.na(trials$Diff_CK) & trials$mesh<80,]), replace = TRUE),10000, nrow(trials[!is.na(trials$Diff_CK) & trials$mesh<80,]))
boot.statistics <- apply(boot.samples, 1, mean)
KITE_control.s<-data.frame(mesh= "small (<80mm)", comparison="control-Kite",mean=mean(boot.statistics),
                           lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))

boot.samples <- matrix(sample(trials$Diff_LK[!is.na(trials$Diff_LK) & trials$mesh<80], size = 10000 * nrow(trials[!is.na(trials$Diff_LK) & trials$mesh<80,]), replace = TRUE),10000, nrow(trials[!is.na(trials$Diff_LK) & trials$mesh<80,]))
boot.statistics <- apply(boot.samples, 1, mean)
KITE_LEB.s<-data.frame(mesh= "small (<80mm)", comparison="LEB-Kite",mean=mean(boot.statistics),
                       lcl=quantile(boot.statistics,0.025),ucl=quantile(boot.statistics,0.975))
### PLOT predicted OUTPUT ###

bind_rows(LEB_control.s,KITE_control.s, KITE_LEB.s, LEB_control.l,KITE_control.l, KITE_LEB.l) %>%
  ggplot(aes(y=mean*7200, x=comparison, colour=mesh)) + 
  geom_hline(yintercept=0,linetype='dashed', colour="cornflowerblue", size=2, position=position_dodge(width=0.5)) +
  geom_point(size=3, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lcl*7200, ymax=ucl*7200, colour=mesh), width=.03, position=position_dodge(width=0.5))+
  scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1,0.1)) +
  xlab("") +
  ylab("Difference in bycatch rate (birds / day)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text=element_text(size=18, color="black"),
        legend.text=element_text(size=14, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.key=element_blank(),
        legend.position=c(0.2,0.85),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("Cornwall_BPUE_comparison_by_mesh.jpg", width=11, height=8)





