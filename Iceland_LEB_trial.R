### ##################################################
### ICELAND LEB TRIALS - test of LOOMING EYE BUOY (LEB) TO REDUCE BYCATCH
### written by steffen.oppel@rspb.org.uk
### ##################################################

## requested by Yann Rouxel on 2 June 2022
## copied from Lithuanian LEB trials
## trossas are sets of nets - each trossa may have several nets
## for each LEB trial there were multiple control trossas

# Was the soak time also included in the analysis?
# Were environmental factors factored too?
# Was there a difference depending if we consider bycatch events vs bycatch intensity?
# Would it be possible to conduct the same analysis for data with observers' presence vs without?
# In the (likely) event that LEB are not effective, could we also measure the effect of a) soak time and b) fishing depths on seabird bycatch per trossas

## started 28 June 2022

# further questions to answer: investigate the influence of date (day of the year) as an explanatory variable?

## TO DO: look at the depth of the trossas and distance to shore.
## Anecdotally there seemed to be an increase in bird bycatch in shallower waters.
## need to stratify random sampling by depth!

## UPDATE 11 JULY 2022: included loop to incorporate depth matching in LEB bootstrap

## UPDATE 4 October 2022: added analysis looking at depth and bird vs fish catch rates per depth category

## UPDATE 12 Oct 2022 to address further questions raised by Yann:
# can we explore the influence of season on the different bycaught species (only for species with higher bycatch rates; e/.g. Eiders, Black & common Guillemots, etc.?).
# can we explore soak time influence on seabird bycatch rates and Lumpfish catch (to find optimal balanced soak time) 


## UPDATE 24 Oct 2022 to address further questions raised by Yann:
# what would be reduction of bycatch if fishery would be curtailed to certain depth or season?
# can we explore soak time influence on seabird bycatch rates and Lumpfish catch (to find optimal balanced soak time) 


### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(sp)
library(sf)
library(rnaturalearth)
library(raster)
filter<-dplyr::filter
select<-dplyr::select
library(randomForest)
library(pdp)


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

try(setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\RawData"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\RawData"), silent=T)

# Read the data from formatted CSV files (one for each mitigation trial)
# updated database on 5 July 2022 with new data sent by Herdis correcting some coordinates
sets <- fread("Iceland_LEB_Database Final_V4.csv")
head(sets)
unique(sets$net_height_meshes)
unique(sets$biopol_onboard)

### CONVERT LATITUDE AND ONGITUDE
sets$lat<- char2dms(paste0(sets$latitude_dms,'N'), chd='d', chm='m', chs='s') %>% as.numeric()
sets$lon<- char2dms(paste0(sets$longitude_dms,'W'), chd='d', chm='m', chs='s') %>% as.numeric()

### FORMAT DATA (remove columns we don't need and format the rest)

data<-sets %>%
  mutate(start=ymd_hm(paste(departure_date,departure_time))) %>%
  mutate(trossa_length=as.numeric(net_in_trossa*net_length_meters)) %>%
  mutate(trossa_area=as.numeric(net_mesh_size_inches*0.0254*net_height_meshes)*trossa_length) %>%  ## trossa area in sqm
  mutate(effort=trossa_area*soaking_nights) %>%
  mutate(EXP=ifelse(with_leb=="LEB","LEB","control")) %>%
  mutate(birds_per_trossa=ifelse(is.na(birds_per_trossa),0,birds_per_trossa)) %>%
  mutate(common_eider=ifelse(is.na(common_eider),0,common_eider)) %>%
  mutate(black_guillemot=ifelse(is.na(black_guillemot),0,black_guillemot)) %>%
  mutate(common_murre=ifelse(is.na(common_murre),0,common_murre)) %>%
  mutate(`long-tailed_duck`=ifelse(is.na(`long-tailed_duck`),0,`long-tailed_duck`)) %>%
  mutate(lumpfish_per_trossa=ifelse(is.na(lumpfish_per_trossa),0,lumpfish_per_trossa)) %>%
  mutate(CPUE=lumpfish_per_trossa/effort, BPUE=birds_per_trossa/effort,
         EPUE=common_eider/effort,GPUE=(black_guillemot+common_murre)/effort,LPUE=`long-tailed_duck`/effort) %>%
  mutate(observer=ifelse(biopol_onboard %in% c("herdis","halldor"),1,0)) %>%
  mutate(depth=((min_depth_fathoms+max_depth_fathoms)/2)*1.8288) %>% ## convert depth to metres
  mutate(depth=ifelse(is.na(depth),7*1.8288,depth)) %>% ## fill in one missing value
  select (boat,fishing_trip_id,trossa_id,EXP,start, effort,lat,lon,CPUE, BPUE,EPUE,GPUE,LPUE,birds_per_trossa,lumpfish_per_trossa,observer,depth,trossa_area,soaking_nights) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(bycatch_bin=ifelse(BPUE>0,1,0))

dim(data)
data %>% group_by(EXP) %>% summarise(CPUE=mean(CPUE,na.rm=T), BPUE=mean(BPUE, na.rm=T))
data %>% filter(is.na(CPUE))
data %>% filter(is.na(depth))

data %>% filter(is.na(start))

#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    CALCULATE DISTANCE TO COAST   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
### WORKS ONLY IF rnaturalearthhires is installed: https://github.com/ropensci/rnaturalearthhires/issues/2
## devtools::install_github("ropensci/rnaturalearthhires") 

iceland <- ne_countries(scale = 10, country = "Iceland", returnclass = "sf")

# convert data to sf object
d1_sf <- data %>% st_as_sf(coords = c('lon','lat')) %>% 
  st_set_crs(4326)

#transform Iceland from polygon shape to line
iceland <- st_cast(iceland, "MULTILINESTRING")

#calculation of the distance between the coast and our points
dist <- st_distance(iceland, d1_sf)

#distance with unit in meters
data$dist_coast<-as.numeric(dist)
hist(data$dist_coast)

data %>% filter(dist_coast>11000)


#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    REPORT EFFORT FOR REPORT ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
dim(data)
table(data$EXP)
range(data$start, na.rm=T)
summary(data$trossa_area)
dim(data[data$BPUE==0,])/dim(data)
table(data$EXP[data$BPUE==0])/table(data$EXP)

data %>% group_by(EXP) %>% summarise(BPUE=mean(BPUE)*100)

## number of control trossas per fishing trip

data %>% mutate(count=1) %>% group_by(fishing_trip_id,EXP) %>% summarise(N=sum(count)) %>% ungroup() %>%
  group_by(EXP) %>% summarise(min=min(N), max=max(N))






#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SUMMARISE BYCATCH DATA ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

try(setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Output"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Output"), silent=T)



totals<- sets[,c(21:35)] %>% gather(key="Species", value="Catch") %>%
  mutate(Catch=ifelse(is.na(Catch),0,Catch)) %>%
  group_by(Species) %>%
  summarise(N=sum(Catch)) %>%
  arrange(desc(N))

sets[,c(2,8,21:35)] %>% gather(key="Species", value="Catch",-fishing_trip_id,-trossa_id) %>%
  mutate(Catch=ifelse(is.na(Catch),0,Catch)) %>%
  left_join(data, by=c('fishing_trip_id','trossa_id')) %>%
  mutate(BPUE=Catch/effort) %>%
  #group_by(Species,EXP) %>%
  #summarise(mean=median(BPUE),
            #lcl=quantile(BPUE,0.025),ucl=quantile(BPUE,0.975)) %>%
  #          lcl=min(BPUE),ucl=max(BPUE)) %>%
  left_join(totals, by="Species") %>%
  filter(N>2) %>%
  mutate(Species= paste(Species, " (n=",N,")", sep="")) %>%

  ggplot(aes(y=BPUE*2000, x=EXP)) +
  geom_point(size=2, colour="firebrick",position=position_jitter(width = 0.15, height = 0))+
  #geom_violin(colour="firebrick")+
  #geom_errorbar(aes(ymin=lcl*2000, ymax=ucl*2000), width=.03)+
  #scale_y_continuous(limits=c(-1.5,1.5), breaks=seq(-1.5,1.5,0.5)) +
  geom_hline(aes(yintercept=0), colour="darkgrey", linetype=2) +
  facet_wrap(~Species, ncol=3, scales='free_y') +
  xlab("") +
  ylab("Bycatch per unit effort (trossa day)") +
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

#ggsave("Iceland_LEB_raw_bycatch_data.jpg", width=11, height=9)
  



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    CONDUCT MATCHED DEPTH BOOTSTRAP ANALYSIS          ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### REMOVED SIMPLE BOOTSTRAP ON 12 JULY 2022 AFTER IMPROVED BOOTSTRAP WITH MATCHING DEPTH BELOW
# 
# #### NEED TO RUN THIS CODE TWICE: WITH AND WITHOUT OBSERVER DATA LIMITATION
# 
# setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye")

try(setwd("C:\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Marine\\Bycatch\\GillnetBycatch\\Analysis\\LoomingEye"), silent=T)

controls<- data %>% filter(EXP=="control") %>% filter(observer==1)
LEBs<- data %>% filter(EXP=="LEB") %>% filter(observer==1)

## ~~~~~~~~ commented out because it takes 2 hrs to run ~~~~~~~~~~~  ##
# ## FORMERLY CALLED APPROACH 2 - calculate difference per fishing trip and then bootstrap over difference
# ## because we have multiple control nets per LEB, we have two layers of random sampling
# ## we ensure that a sample is taken from each fishing trip by looping over trips
# # abandoned on 27 June as it is incredibly slow
# # resurrected on 11 July 2022 to include the depth matching of LEB and control nets
# # no longer calculates difference per fishing trip, but matches sampling from same trips
# # because of very long loop, calculate all matrices in the same loop
# 
# bootstraps<-10000
# # one execution takes 0.06983399 sec, so calculate length of duration as
# (round(0.06983399,2) * bootstraps * nrow(LEBs))/3600  ## in hours
# 
# boot.samples <- matrix(sample(LEBs$fishing_trip_id, size = bootstraps * nrow(LEBs), replace = TRUE),bootstraps, nrow(LEBs))
# LEB.samples <- array(NA,dim=c(bootstraps, nrow(LEBs),5),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE")))
# control.samples <- array(NA,dim=c(bootstraps, nrow(LEBs),5),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE")))
# 
# boot.samples.obs <- matrix(sample(LEBs$fishing_trip_id[LEBs$observer==1], size = bootstraps * nrow(LEBs), replace = TRUE),bootstraps, nrow(LEBs[LEBs$observer==1,]))
# LEB.samples.obs <- array(NA,dim=c(bootstraps, nrow(LEBs[LEBs$observer==1,]),5),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE")))
# control.samples.obs <- array(NA,dim=c(bootstraps, nrow(LEBs[LEBs$observer==1,]),5),dimnames=list(NULL,NULL,c("BPUE","EPUE","GPUE","LPUE","CPUE")))
# 
# for(row in 1:bootstraps){
#   for (col in 1:nrow(LEBs)){
#     #start_time <- Sys.time()
#     xd<- data %>% filter(fishing_trip_id==boot.samples[row,col])
#     mean_depth<-mean(xd$depth[xd$EXP=="LEB"])  ## take mean for the odd trip with 2 LEBs
#     target_depth<-rnorm(1,mean_depth,5)
#     LEB.samples[row,col,1:5] <- xd %>% filter(EXP=="LEB") %>%
#       sample_n(1) %>%
#       select(BPUE,EPUE,GPUE,LPUE,CPUE) %>%
#       unlist()
#     
#     control.samples[row,col,1:5] <-xd %>% filter(EXP=="control") %>%
#                                         mutate(target=target_depth) %>%
#                                         mutate(diff=abs(depth-target_depth)) %>%
#                                         filter(diff==min(diff)) %>%
#                                         sample_n(1) %>%
#                                         select(BPUE,EPUE,GPUE,LPUE,CPUE) %>%
#                                         unlist()
#     
#     ### use only observer data ###
#     if(col <= nrow(LEBs[LEBs$observer==1,])){
#       xdo<- data %>% filter(fishing_trip_id==boot.samples.obs[row,col])
#       mean_depth<-mean(xdo$depth[xdo$EXP=="LEB"])  ## take mean for the odd trip with 2 LEBs
#       target_depth<-rnorm(1,mean_depth,5)
#       LEB.samples.obs[row,col,1:5] <- xdo %>% filter(EXP=="LEB") %>%
#         sample_n(1) %>%
#         select(BPUE,EPUE,GPUE,LPUE,CPUE) %>%
#         unlist()
#       
#       control.samples.obs[row,col,1:5] <-xdo %>% filter(EXP=="control") %>%
#         mutate(target=target_depth) %>%
#         mutate(diff=abs(depth-target_depth)) %>%
#         filter(diff==min(diff)) %>%
#         sample_n(1) %>%
#         select(BPUE,EPUE,GPUE,LPUE,CPUE) %>%
#         unlist()
#     } ## close if loop for observer data
#     #end_time <- Sys.time()
#     #end_time - start_time
#   } ## close loop over row of fishing trips
#   print(sprintf("finished bootstrap %i",row))
# } ## close loop over bootstrap samples
# 
# 
# 
# ### SUMMARISE ALL BOOTSTRAP SAMPLES
# metrics<-c("all birds","common eider","black and common guillemots","long-tailed duck","fish catch")
# allout<-data.frame()
# rawout<-data.frame()
# for(m in 1:length(metrics)) {
#   LEB.statistics <- apply(LEB.samples[,,m], 1, mean)
#   RATE_LEB<-data.frame(treatment="with LEB",mean=median(LEB.statistics),
#                        lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))
#   control.statistics <- apply(control.samples[,,m], 1, mean)
#   RATE_control<-data.frame(treatment="control",mean=median(control.statistics, na.rm=T),
#                            lcl=quantile(control.statistics,0.025, na.rm=T),ucl=quantile(control.statistics,0.975, na.rm=T))
#   
#   ## OUTPUT FOR REPORT
#   rawout<-bind_rows(RATE_LEB,RATE_control) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="all data") %>%
#     bind_rows(rawout)
#   
#   ## summarise for plot
#   allout<-data.frame(mean=quantile(LEB.statistics-control.statistics,0.5)*2000, lcl=quantile(LEB.statistics-control.statistics,0.025)*2000,ucl=quantile(LEB.statistics-control.statistics,0.975)*2000) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="all data") %>%
#     bind_rows(allout)
# }
# 
# for(m in 1:length(metrics)) {
#   LEB.statistics <- apply(LEB.samples.obs[,,m], 1, mean)
#   RATE_LEB<-data.frame(treatment="with LEB",mean=median(LEB.statistics),
#                        lcl=quantile(LEB.statistics,0.025),ucl=quantile(LEB.statistics,0.975))
#   control.statistics <- apply(control.samples.obs[,,m], 1, mean)
#   RATE_control<-data.frame(treatment="control",mean=median(control.statistics, na.rm=T),
#                            lcl=quantile(control.statistics,0.025, na.rm=T),ucl=quantile(control.statistics,0.975, na.rm=T))
#   
#   ## OUTPUT FOR REPORT
#   rawout<-bind_rows(RATE_LEB,RATE_control) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="only observer data") %>%
#     bind_rows(rawout)
#   
#   ## summarise for plot
#   allout<-data.frame(mean=quantile(LEB.statistics-control.statistics,0.5)*2000, lcl=quantile(LEB.statistics-control.statistics,0.025)*2000,ucl=quantile(LEB.statistics-control.statistics,0.975)*2000) %>%
#     mutate(metric=metrics[m]) %>% mutate(obs="only observer data") %>%
#     bind_rows(allout)
# }
# 
# ### SAVE OUTPUT TO CSV
# ## first convert to mean trossa day
# rawout<-rawout %>%
#   mutate(mean=mean*2000,lcl=lcl*2000,ucl=ucl*2000) %>%
#   select(obs,metric,treatment,mean,lcl,ucl) %>%
#   arrange(obs,metric,treatment)
# 
# allout<-allout %>%
#   select(obs,metric,mean,lcl,ucl)%>%
#   arrange(obs,metric)
# 
# #fwrite(allout,"Iceland_LEB_bootstrap_differences.csv")
# #fwrite(rawout,"Iceland_LEB_bootstrap_bycatch_rates.csv")
# 
# ### CREATE SUMMARY PLOT OF ALL TARGET GROUPS
# allout %>% filter(metric != "fish catch") %>%
#   group_by(obs) %>%
#   
#   ggplot(aes(y=mean, x=metric,colour=obs)) + geom_point(size=2, position=position_dodge(width=0.2))+
#   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.03, position=position_dodge(width=0.2))+
#   labs(colour='Data source:') +
#   scale_y_continuous(limits=c(-0.5,0.5), breaks=seq(-0.5,0.5,0.1)) +
#   geom_hline(aes(yintercept=0), colour="darkgrey", linetype=2) +
#   xlab("") +
#   ylab("Bycatch difference in trossa per day with LEB") +
#   theme(panel.background=element_rect(fill="white", colour="black"), 
#         axis.text=element_text(size=16, color="black"), 
#         axis.title=element_text(size=18), 
#         strip.text=element_text(size=18, color="black"),
#         legend.text=element_text(size=14, color="black"),
#         legend.title=element_text(size=18, color="black"),
#         legend.key=element_blank(),
#         legend.position=c(0.2,0.1),
#         strip.background=element_rect(fill="white", colour="black"), 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_blank())
# 
# #ggsave("Iceland_LEB_trial_bycatch_difference.jpg", width=11, height=8)




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     EXPLORATORY ANALYSIS OF BIRD BYCATCH BETWEEN LEB and no LEB           ~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### FIT RANDOM FOREST MODELS FOR VARIOUS SUBSETS OF DATA AND RESPONSE VARIABLES
head(data)
data$jday<-yday(data$start)  ### to account for changing fishing behaviour over season

### BINARY BYCATCH YES/NO IS POORLY EXPLAINED BY EXPLANATORY VARIABLES
RFbinary<- randomForest(as.factor(bycatch_bin)~EXP+soaking_nights+trossa_area+fishing_trip_id+observer+depth+dist_coast+jday,
                        data=data, mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFbinary
varImpPlot(RFbinary)



### BINARY BYCATCH YES/NO IS EVEN MORE POORLY EXPLAINED BY EXPLANATORY VARIABLES
RFbinaryobs<- randomForest(as.factor(bycatch_bin)~EXP+soaking_nights+trossa_area+fishing_trip_id+depth+dist_coast+jday,
                        data=data[data$observer==1,], mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFbinaryobs
varImpPlot(RFbinaryobs)


### BIRD BYCATCH NUMERIC IS POORLY EXPLAINED BY EXPLANATORY VARIABLES
RFnum<- randomForest(birds_per_trossa~EXP+soaking_nights+trossa_area+fishing_trip_id+observer+depth+dist_coast+jday,
                        data=data, mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFnum
varImpPlot(RFnum)


### BIRD BYCATCH NUMERIC IS POORLY EXPLAINED BY EXPLANATORY VARIABLES
RFnumobs<- randomForest(birds_per_trossa~EXP+soaking_nights+trossa_area+fishing_trip_id+depth+dist_coast+jday,
                     data=data[data$observer==1,], mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFnumobs
varImpPlot(RFnumobs)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  PLOT VARIABLE IMPORTANCE FOR ALL FOUR MODELS ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## advanced plot for publication
VARbinary<-importance(RFbinary, type=1)
VARbinaryobs<-importance(RFbinaryobs, type=1)
VARnum<-importance(RFnum, type=1)
VARnumobs<-importance(RFnumobs, type=1)


IMP<-bind_rows(VARbinary[,1],VARbinaryobs[,1],VARnum[,1],VARnumobs[,1]) %>%
  mutate(observer=ifelse(is.na(observer),0,observer)) %>%
  mutate(MAX = do.call(pmax, (.))) %>%
  mutate(model=c("occurrence","occurrence (observer data)","abundance","abundance (observer data)")) %>%
  gather(key=variable, value=MSE,-model,-MAX) %>%
  mutate(IMP=(MSE/MAX)*100) %>%
  arrange(model,desc(IMP))
    
    
ggplot(IMP, aes(x=variable, y=IMP)) +
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
#ggsave("LEB_Iceland_variable_importance.jpg", width=10, height=8)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  PARTIAL PLOTS FOR DISTANCE AND DEPTH ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## NEED TO REMOVE NAs from data frame before plot works
#jpeg("LEB_Iceland_partial_plots.jpg", width=9, height=12, units="in", res=100)
par(mfrow=c(3,2), oma=c(1,3,0,0))
### depth ####
partialPlot(x=RFbinaryobs, pred.data=as.data.frame(data[!is.na(data$depth),]),
            x.var=depth, which.class="1", xlab="sea depth (m)",
            ylab="", lwd=3, col="firebrick", main="")
mtext("Occurrence",side=3,cex=1.5,line=0)



### depth on abundance ####
partialPlot(x=RFnumobs, pred.data=as.data.frame(data[!is.na(data$depth),]),
            x.var=depth, xlab="sea depth (m)",
            ylab="", lwd=3, col="firebrick", main="")
mtext("Abundance",side=3,cex=1.5,line=0)

range(data$jday, na.rm=T)
### distance ####
# removed on 28 June as Yann thinks it is flawed
# range(data$dist_coast)
# hist(data$dist_coast)
# 
# data$dist_coast/1000
# partialPlot(x=RFbinaryobs, pred.data=as.data.frame(data[!is.na(data$dist_coast),]),
#             x.var=dist_coast, which.class="1", xlab="distance from coast (m)",
#             ylab="Partial contribution to predict bycatch events", lwd=3, col="firebrick", main="")

### soaking time ####
partialPlot(x=RFbinaryobs, pred.data=as.data.frame(data[!is.na(data$soaking_nights),]),
            x.var=soaking_nights, which.class="1", xlab="number of soaking nights",
            ylab="", lwd=3, col="firebrick", main="")


### soaking time on abundance ####
partialPlot(x=RFnumobs, pred.data=as.data.frame(data[!is.na(data$soaking_nights),]),
            x.var=soaking_nights, xlab="number of soaking nights",
            ylab="", lwd=3, col="firebrick", main="")

### time of year ####
partialPlot(x=RFbinaryobs, pred.data=as.data.frame(data[!is.na(data$jday),]),
            x.var=jday, which.class="1", xlab="day of the year",xlim=c(80,140),xaxt="n",
            ylab="", lwd=3, col="firebrick", main="")
axis(side=1,at=seq(80,140,10),
     labels=format(seq(from=ymd("2022-03-21"), to=ymd("2022-05-20"),by="10 days"),format("%d-%b")))


### time of year on abundance ####
partialPlot(x=RFnumobs, pred.data=as.data.frame(data[!is.na(data$jday),]),
            x.var=jday, which.class="1", xlab="day of the year",xlim=c(80,140),xaxt="n",
            ylab="", lwd=3, col="firebrick", main="")
axis(side=1,at=seq(80,140,10),
labels=format(seq(from=ymd("2022-03-21"), to=ymd("2022-05-20"),by="10 days"),format("%d-%b")))

mtext("Partial contribution to predict bycatch",side=2,outer=T,cex=1.7,line=1)

#dev.off()


#fwrite(data,"Iceland_LEB_trial_data_used.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  PREDICT CONDITIONS WITH NO BYCATCH ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
RFpredict<- randomForest(as.factor(bycatch_bin)~EXP+soaking_nights+trossa_area+depth+dist_coast+jday,
                        data=data, mtry=5,ntree=1500, importance=F,na.action=na.omit)
RFpredict
newdat<-expand.grid(EXP=unique(data$EXP),
            soaking_nights=c(1,2,4),
            trossa_area=mean(data$trossa_area),
            depth=seq(0,100,10),
            dist_coast=c(50,5000,10000,15000),
            jday=c(90,110,130))

newdat$BYCATCH<-predict(RFpredict,newdat,type="prob")[,2]

### summarise the conditions when bycatch is predicted to be ZERO ###
### includes ALL conditions!!
newdat %>% #filter(BYCATCH>0.9) %>%
  filter(EXP=="control") %>%
  mutate(dist_coast=dist_coast/1000) %>%
  mutate(Season=ifelse(jday==90,"March",ifelse(jday==110,"April","May"))) %>%  #select(-EXP,-trossa_area,-BYCATCH) %>%
  #gather(key="variable",value="size") %>%
  
  #ggplot(aes(x=variable, y=size)) + 
  #geom_violin()

  ggplot(aes(x=depth,y=BYCATCH,colour=Season,size=soaking_nights)) +
  geom_point(position=position_dodge(width=1)) +
  facet_wrap(~dist_coast, ncol=2) +
  ylab("Bycatch probability") +
  xlab("Water depth (m)") +
  #scale_y_continuous(limits=c(-20,115), breaks=seq(0,100,20), labels=seq(0,100,20))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Iceland_bycatch_predictions.jpg", width=10, height=8)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####     EXPLORATORY ANALYSIS OF FISH CATCH BETWEEN LEB and no LEB           ~~~~~~~~~~~########
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

### FIT RANDOM FOREST MODELS FOR VARIOUS SUBSETS OF DATA AND RESPONSE VARIABLES
head(data)
data$jday<-yday(data$start)  ### to account for changing fishing behaviour over season


### INCLUDING ALL DATA
RFnum_fish<- randomForest(lumpfish_per_trossa~EXP+soaking_nights+trossa_area+fishing_trip_id+observer+depth+dist_coast+jday,
                     data=data, mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFnum_fish
varImpPlot(RFnum_fish)


### ONLY INCLUDING DATA WITH INDEPENDENT OBSERVER ON BOARD
RFnumobs_fish<- randomForest(lumpfish_per_trossa~EXP+soaking_nights+trossa_area+fishing_trip_id+depth+dist_coast+jday,
                        data=data[data$observer==1,], mtry=5,ntree=1500, importance=T,na.action=na.omit)
RFnumobs_fish
varImpPlot(RFnumobs_fish)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  PLOT VARIABLE IMPORTANCE FOR BOTH MODELS ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## advanced plot for publication
VARnum_fish<-importance(RFnum_fish, type=1)
VARnumobs_fish<-importance(RFnumobs_fish, type=1)


IMP<-bind_rows(VARnum_fish[,1],VARnumobs_fish[,1]) %>%
  mutate(observer=ifelse(is.na(observer),0,observer)) %>%
  mutate(MAX = do.call(pmax, (.))) %>%
  mutate(model=c("fish catch","fish catch (observer data)")) %>%
  gather(key=variable, value=MSE,-model,-MAX) %>%
  mutate(IMP=(MSE/MAX)*100) %>%
  arrange(model,desc(IMP))


ggplot(IMP, aes(x=variable, y=IMP)) +
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
#ggsave("Iceland_fish_catch_variable_importance.jpg", width=10, height=8)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  PARTIAL PLOTS FOR DISTANCE AND DATE ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## NEED TO REMOVE NAs from data frame before plot works
#jpeg("Iceland_fish_catch_partial_plots.jpg", width=9, height=12, units="in", res=100)
par(mfrow=c(2,2), oma=c(1,3,0,0))
### depth ####
partialPlot(x=RFnum_fish, pred.data=as.data.frame(data[!is.na(data$depth),]),
            x.var=depth, which.class="1", xlab="sea depth (m)",
            ylab="", lwd=3, col="firebrick", main="")
mtext("Fish catch",side=3,cex=1.5,line=0)



### depth on abundance ####
partialPlot(x=RFnumobs_fish, pred.data=as.data.frame(data[!is.na(data$depth),]),
            x.var=depth, xlab="sea depth (m)",
            ylab="", lwd=3, col="firebrick", main="")
mtext("Fish catch (observer data)",side=3,cex=1.5,line=0)

### time of year ####
partialPlot(x=RFnum_fish, pred.data=as.data.frame(data[!is.na(data$jday),]),
            x.var=jday, which.class="1", xlab="day of the year",xlim=c(80,140),xaxt="n",
            ylab="", lwd=3, col="firebrick", main="")
axis(side=1,at=seq(80,140,10),
     labels=format(seq(from=ymd("2022-03-21"), to=ymd("2022-05-20"),by="10 days"),format("%d-%b")))


### time of year on abundance ####
partialPlot(x=RFnumobs_fish, pred.data=as.data.frame(data[!is.na(data$jday),]),
            x.var=jday, which.class="1", xlab="day of the year",xlim=c(80,140),xaxt="n",
            ylab="", lwd=3, col="firebrick", main="")
axis(side=1,at=seq(80,140,10),
     labels=format(seq(from=ymd("2022-03-21"), to=ymd("2022-05-20"),by="10 days"),format("%d-%b")))

mtext("Partial contribution to predict fish catch",side=2,outer=T,cex=1.7,line=1)

#dev.off()




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY DEPTH AT WHICH SPECIES ARE CAUGHT ###   ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
names(sets)
depthcaptures<-sets[,c(17,18,21:35)] %>% 
  mutate(depth=((min_depth_fathoms+max_depth_fathoms)/2)*1.8288) %>% ## convert depth to metres
  mutate(depth=ifelse(is.na(depth),7*1.8288,depth)) %>% ## fill in one missing value
  dplyr::select(-min_depth_fathoms,-max_depth_fathoms) %>%
  gather(key="Species", value="Catch",-depth) %>%
  mutate(Catch=ifelse(is.na(Catch),0,Catch)) %>%
  filter(Catch>0)

alldepthcaptures <- data.frame()
for(x in 1:dim(depthcaptures)[1]){
  l<-depthcaptures[x,]
  out_l<-data.frame(Species=l$Species,depth=l$depth,Catch=rep(1,l$Catch))
  alldepthcaptures <- bind_rows(alldepthcaptures, out_l)
}
dim(depthcaptures)
dim(alldepthcaptures)

depthmeans<-alldepthcaptures %>% group_by(Species) %>%
  summarise(depth=mean(depth)) %>%
  arrange(depth)

alldepthcaptures %>%
  left_join(totals, by="Species") %>%
  filter(N>2) %>%
  mutate(Species=as.factor(Species)) %>%
  mutate(Species=factor(Species, levels=depthmeans$Species)) %>%
  
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

#ggsave("LEB_Iceland_mean_catch_depth.jpg", width=10, height=8)






#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY CATCH AND BYCATCH RATES PER DEPTH CATEGORY TO INFORM MANAGEMENT  ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

## FIRST COMPILE THE DATA WITH THE DEPTH INFO FOR EACH TROSSA

head(data)
names(sets)
depthdata<-data %>% left_join(sets[,c(2,8,17,18)], by=c("fishing_trip_id","trossa_id")) %>% 
  mutate(depth=((min_depth_fathoms+max_depth_fathoms)/2)*1.8288) %>% ## convert depth to metres
  mutate(depth=ifelse(is.na(depth),7*1.8288,depth)) %>% ## fill in one missing value
  dplyr::select(-min_depth_fathoms,-max_depth_fathoms) %>%
  mutate(Month=month(start))


## SUMMARISE CAPTURE NUMBERS for waters more/less than 40 m depth

meanCPUE<-depthdata  %>%
  dplyr::select(fishing_trip_id, depth, CPUE, BPUE) %>%
  gather(key="type",value="catch",-fishing_trip_id,-depth) %>%
  mutate(type=ifelse(type=="CPUE","fish","birds")) %>%
  mutate(water=ifelse(depth>40,60,20)) %>%
  group_by(type,water) %>%
  summarise(annotation=mean(catch), yaxis=max(catch)) %>%
  mutate(annotation=sprintf("%s %s per trossa",round(annotation*mean(data$effort),2),type))


## ACCOUNT FOR EFFORT AND NET SIZE IN CAPTURE NUMBERS

depthdata  %>%
  dplyr::select(fishing_trip_id, depth, CPUE, BPUE,jday,dist_coast, Month) %>%
  gather(key="type",value="catch",-fishing_trip_id,-depth,-Month,-jday,-dist_coast) %>%
  mutate(type=ifelse(type=="CPUE","fish","birds")) %>%
  mutate(water=ifelse(depth>40,60,20)) %>%
  left_join(meanCPUE, by=c("water","type")) %>%
  select(-yaxis) %>%
  left_join((meanCPUE %>% group_by(type) %>% summarise(yaxis=max(yaxis))), by=c("type")) %>%
#filter(type=="birds")
  
  ggplot(aes(y=catch, x=depth, color=type)) +
  geom_point(size=0.5) +
  geom_smooth(method = "gam") +
  geom_text(aes(x=water,y=yaxis,label=annotation),size=4) +
  geom_vline(aes(xintercept=40), colour="grey45", linetype="dashed") +
  facet_grid(type~Month, scales="free_y") +
  #facet_wrap(~type, scales="free_y") +
  ylab("Mean catch per unit effort") +
  xlab("Water depth (m)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Iceland_catch_by_depth.jpg", width=10, height=8)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY CATCH AND BYCATCH RATES PER DAY TO INFORM MANAGEMENT  ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


## ACCOUNT FOR EFFORT AND NET SIZE IN CAPTURE NUMBERS

depthdata  %>%
  dplyr::select(fishing_trip_id, depth, CPUE, BPUE,start,dist_coast) %>%
  gather(key="type",value="catch",-fishing_trip_id,-depth,-start,-dist_coast) %>%
  mutate(type=ifelse(type=="CPUE","fish","birds")) %>%

  ggplot(aes(y=catch, x=start, color=type)) +
  geom_point(size=0.5) +
  geom_smooth(method = "gam") +
  facet_wrap(~type, scales="free_y") +
  ylab("Mean catch per unit effort") +
  xlab("Day of the season") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Iceland_catch_by_date.jpg", width=10, height=8)






#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY CATCH AND BYCATCH RATES PER DISTANCE TO COAST  ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


## ACCOUNT FOR EFFORT AND NET SIZE IN CAPTURE NUMBERS

depthdata  %>%
  dplyr::select(fishing_trip_id, depth, CPUE, BPUE,start,dist_coast) %>%
  gather(key="type",value="catch",-fishing_trip_id,-depth,-start,-dist_coast) %>%
  mutate(type=ifelse(type=="CPUE","fish","birds")) %>%
  
  ggplot(aes(y=catch, x=dist_coast, color=type)) +
  geom_point(size=0.5) +
  geom_smooth(method = "gam") +
  facet_wrap(~type, scales="free_y") +
  ylab("Mean catch per unit effort") +
  xlab("Distance from coast (m)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Iceland_catch_by_distance.jpg", width=10, height=8)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY PORPOISE CATCH RATES PER DEPTH ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

## FIRST COMPILE THE DATA WITH THE DEPTH INFO FOR EACH TROSSA

head(data)
names(sets)
porpdepthdata<-data %>% left_join(sets[,c(2,8,17,18,35)], by=c("fishing_trip_id","trossa_id")) %>%
  mutate(harbour_porpoise=ifelse(is.na(harbour_porpoise),0,harbour_porpoise)) %>%
  mutate(depth=((min_depth_fathoms+max_depth_fathoms)/2)*1.8288) %>% ## convert depth to metres
  mutate(depth=ifelse(is.na(depth),7*1.8288,depth)) %>% ## fill in one missing value
  mutate(BPUE=harbour_porpoise/effort) %>%
  dplyr::select(-min_depth_fathoms,-max_depth_fathoms) %>%
  mutate(Month=month(start))


## SUMMARISE CAPTURE NUMBERS for waters more/less than 40 m depth

porpmeanCPUE<-porpdepthdata  %>%
  dplyr::select(fishing_trip_id, depth, CPUE, BPUE) %>%
  gather(key="type",value="catch",-fishing_trip_id,-depth) %>%
  mutate(type=ifelse(type=="CPUE","fish","harbour porpoise")) %>%
  mutate(water=ifelse(depth>40,60,20)) %>%
  group_by(type,water) %>%
  summarise(annotation=mean(catch), yaxis=max(catch)) %>%
  mutate(annotation=sprintf("%s %s per trossa",round(annotation*mean(data$effort),2),type))


## ACCOUNT FOR EFFORT AND NET SIZE IN CAPTURE NUMBERS

porpdepthdata  %>%
  dplyr::select(fishing_trip_id, depth, CPUE, BPUE,jday,dist_coast, Month) %>%
  gather(key="type",value="catch",-fishing_trip_id,-depth,-Month,-jday,-dist_coast) %>%
  mutate(type=ifelse(type=="CPUE","fish","harbour porpoise")) %>%
  mutate(water=ifelse(depth>40,60,20)) %>%
  left_join(porpmeanCPUE, by=c("water","type")) %>%
  select(-yaxis) %>%
  left_join((porpmeanCPUE %>% group_by(type) %>% summarise(yaxis=max(yaxis))), by=c("type")) %>%
  mutate(psize=ifelse(type!="fish" & catch>0,1,0.5)) %>%

  ggplot(aes(y=catch, x=depth, color=type, size=psize)) +
  geom_point() +
  geom_smooth(method = "gam") +
  geom_text(aes(x=water,y=yaxis,label=annotation),size=4) +
  geom_vline(aes(xintercept=40), colour="grey45", linetype="dashed") +
  #facet_grid(type~Month, scales="free_y") +
  facet_wrap(~type, scales="free_y") +
  ylab("Mean catch per unit effort") +
  xlab("Water depth (m)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Iceland_porpoise_catch_by_depth.jpg", width=10, height=8)





#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY EIDER AND GUILLEMOT CATCH RATES PER DAY ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

head(data)

data  %>%
  dplyr::select(fishing_trip_id, jday, EPUE, GPUE,LPUE,start) %>%
  gather(key="type",value="catch",-fishing_trip_id,-jday,-start) %>%
  mutate(type=ifelse(type=="EPUE","eiders",ifelse(type=="GPUE","guillemots","long-tailed ducks"))) %>%
  
  ggplot(aes(y=catch, x=start, color=type)) +
  geom_point(size=1) +
  geom_smooth(method = "gam") +
  facet_wrap(~type, scales="free_y") +
  ylab("Mean catch per unit effort") +
  xlab("Day of the season") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Iceland_bycatch_by_date.jpg", width=16, height=8)







#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY FISH AND BIRD CATCH PER SOAK TIME ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

head(data)
table(data$soaking_nights)

data  %>%
  dplyr::select(fishing_trip_id, soaking_nights, BPUE, CPUE,start) %>%
  gather(key="type",value="catch",-fishing_trip_id,-start,-soaking_nights) %>%
  mutate(type=ifelse(type=="CPUE","fish","birds")) %>%
  mutate(Month=month(start)) %>%
  
  ggplot(aes(y=catch, x=soaking_nights, color=type)) +
  geom_point(size=1) +
  geom_smooth(method = "loess") +
  facet_wrap(~type, scales="free_y") +
  #facet_grid(type~Month, scales="free_y") +
  ylab("Mean catch per unit effort") +
  xlab("Number of soaking nights") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"),
        axis.title=element_text(size=20), 
        strip.text=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#ggsave("Iceland_bycatch_by_soak_time.jpg", width=12, height=8)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    QUANTIFY CHANGE IN BYCATCH UNDER M<ANAGEMENT SCENARIOS  ~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

## added on 24 Oct 2022
# I think the data "distance from coast" are a bit challenging to work around, since most fishing occurs within fjords. A 5 km net ban from shore could literally mean a fishery closure. 
# Thus depth-based restriction might be more manageable with the local conditions in my opinion
# 
# What would be the seabird bycatch % reduction if a 40 m set depth limit is implemented, compared to the currently observed bycatch levels?
# And in the case of a 10m, 20m, or 30m limit?

# We know that a total of 174 boats actively participated in the Icelandic lumpfish fishery the previous season (2021)
# Annually, over 8,000 seabirds are bycaught in this fishery (Christensen-Dalsgaard et al. 2019).

# In 2021, each boat had a 40 days long fishing season (max consecutive fishing days allowed, starting from first fishing day).
# In 2022, the season was only 25 days.
# 
# Each boat is allowed to deploy a total of 7500 meters of nets and to leave them unattended for a maximum of 3 days under the current regulation (165/2020).
# Gillnets are generally being deployed between 5-50 meters deep (MFRI 2022). 
# 
# In this report from 2014-2017, from which the 8,000 bycatch figure is extracted, we have calculated the depth and temporal distribution of fishing effort.
# There is also a bycatch rate shown, but CPUE is per trip.


## APPROACH:
# estimate bycatch rate per depth category
# estimate fish catch rate per depth category
# use landings data per depth category to estimate proportion of national fishing effort per depth category
# extrapolate current country-wide bycatch number (hoping it will be close to the ~8000)
# simulate redistribution of fishing effort from <40 m to deeper depths and re-calculate bycatch and fish catch
# this ignores potential availability of water at that depth with sufficient fish ressource


## landings data
depthlandings<-data.frame(depth=c(0,10,20,30,40,50),landings=c(0.13,0.34,0.18,0.13,0.1,0.12))
seasonlandings<-data.frame(month=c(3,4,5,6,7,8),landings=c(0.07,0.45,0.30,0.12,0.05,0.01))
landings<-data.frame(year=seq(2014,2021),tons=c(4074,6474,5504,4565,4516,5044,5315,7601))

## catch and bycatch per depth category
depthrates<-depthdata  %>% ungroup() %>%
  mutate(depth=ifelse(depth<10,0,ifelse(depth<20,10,ifelse(depth<30,20,ifelse(depth<40,30,ifelse(depth<50,40,50)))))) %>%
  dplyr::select(depth, CPUE, BPUE) %>%
  gather(key="type",value="catch",-depth) %>%
  mutate(type=ifelse(type=="CPUE","fish","birds")) %>% #filter(water==20 & type=="birds" & catch>0) %>% ggplot() + geom_histogram(aes(x=catch))
  group_by(type,depth) %>%
  summarise(mean=mean(catch),lcl=stats::quantile(catch,0.025),ucl=stats::quantile(catch,0.975))

## apportion the tons of fish to depth categories in each year to calculate effort
deptheffort<-bind_rows(landings,landings,landings,landings,landings,landings) %>%
  mutate(depth=rep(depthlandings$depth,each=length(landings$year)), prop=rep(depthlandings$landings,each=length(landings$year))) %>%
  mutate(depthcatch=prop*tons) %>%
  arrange(year,depth) %>%
  left_join(depthrates %>% filter(type=="fish"), by="depth") %>%
  mutate(effort=(depthcatch/0.0055)/mean, effort.lcl=(depthcatch/0.0055)/lcl,effort.ucl=(depthcatch/0.0055)/ucl)   ## assuming that one fish weighs 5.5 kg or 0.0055 tons

## extrapolate the estimated effort at depth categories to bycatch rates to estimate total bycatch per depth
depthbycatch<-deptheffort %>%
  select(year, tons,depth, depthcatch,effort,effort.lcl,effort.ucl) %>%
  left_join(depthrates %>% filter(type=="birds"), by="depth") %>%
  mutate(bycatch=mean*effort, bycatch.ucl=ucl*effort.ucl)   ## assuming that one fish weighs 5.5 kg or 0.0055 tons

BYCATCH_TOTAL_BASELINE<-depthbycatch %>% group_by(year) %>%
  summarise(fish=sum(depthcatch),birds=sum(bycatch),birds.ucl=sum(bycatch.ucl))

fwrite(depthbycatch,"Iceland_total_bycatch_per_depth.csv")
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  SIMULATE FISHING CLOSURE AT CERTAIN DEPTH AND REALLOCATION OF EFFORT TO DEEPER WATER ###########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## assume fishing closed in waters <40m depth
## all fishing effort would go into water >40 m depth

closure_depth_simulation<-data.frame()

for(d in c(10, 20, 30, 40, 50)) {
  for (y in unique(BYCATCH_TOTAL_BASELINE$year)) {
    yeff <- deptheffort %>% filter(year == y) %>%
      mutate(IN = if_else(depth >= d, 1, 0))
    
    ### summarise effort in closed waters and redistribute to open waters
    mean.eff.add <-
      sum(yeff$effort[yeff$IN == 0]) / length(yeff$effort[yeff$IN == 1])
    max.eff.add <-
      sum(yeff$effort.ucl[yeff$IN == 0]) / length(yeff$effort[yeff$IN == 1])
    
    out <- yeff %>% filter(IN == 1) %>%
      mutate(effort = effort + mean.eff.add,
             effort.ucl = effort.ucl + max.eff.add) %>%
      mutate(depthcatch = mean * effort * 0.0055) %>%    ### calculating fish catch when effort moved to deeper water
      select(year, tons, depth, depthcatch, effort, effort.ucl) %>%
      left_join(depthrates %>% filter(type == "birds"), by = "depth") %>%
      mutate(bycatch = mean * effort,
             bycatch.ucl = ucl * effort.ucl)  %>% group_by(year, tons) %>%
      summarise(
        landings = sum(depthcatch),
        bycatch = sum(bycatch),
        bycatch.ucl = sum(bycatch.ucl)
      ) %>%
      mutate(closure_depth = d)
    
    closure_depth_simulation <- bind_rows(closure_depth_simulation, out)
  }
}



#### CALCULATING RELATIVE CHANGE IN FISH CATCH AND CHANGE IN BIRD BYCATCH

closure_depth_simulation %>% left_join(BYCATCH_TOTAL_BASELINE, by="year") %>%
  mutate(lumpfish=((landings-fish)/fish)*100,
         seabirds=((bycatch-birds)/birds)*100,
         bird_max_change=((bycatch.ucl-birds.ucl)/birds.ucl)*100) %>%
  select(year,closure_depth,lumpfish,seabirds) %>%
  gather(key="Species",value="change",-year,-closure_depth) %>%
  mutate(Species=ifelse(Species=="seabirds","seabird bycatch","target lumpfish catch")) %>%
  rename(Category=Species) %>%
  filter(year==2021) %>%   ## all years have the same proportional change in reduction
  
  ggplot() +
  geom_bar(aes(y = change, x = closure_depth, fill = Category), stat="identity",position="dodge")+
  geom_hline(aes(yintercept=0), linetype="dashed", size=1)+
  ylab("Change in total catch per year (in %)") +
  xlab("Closure depth for fishing (m)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18),
        legend.text=element_text(size=16, color="black"),
        legend.title=element_text(size=18, color="black"),
        legend.position=c(0.15,0.1),
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("Iceland_bycatch_reduction_depth_closure.jpg", width=12, height=8)

