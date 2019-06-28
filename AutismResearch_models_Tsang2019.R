### Author: Tawny Tsang
### Last edited: 11/2018
### Accompanying publication: Tsang, T., Johnson, S., Jeste, S., & Dapretto, M. (2019). Social complexity and the early social environment affect visual social attention to faces. Autism Research, 12(3), 445-457.

### This script shows the models reported in the paper and plots the data for some visualization

packages = c("tidyverse", "data.table", "readxl", "dplyr", "ggplot2", "nmle")
lapply(packages, require, character.only = TRUE)

## read in data file:
CBSST_data <- read_excel("~/Desktop/Projects/ETdataFiles/spreadsheets/__10-13-17_CBSST_ET_data_concat_FINAL.xlsx", sheet = "all")
CBSST_all <- as.data.frame(CBSST_data)
### subset data with good eye-tracking calibration accuracy)
CBSST<- as.data.frame(subset(CBSST_all, poor_cal==1))

#### assign numeric factors with meaningful labels:
CBSST$rx_name <- factor(CBSST$RX, levels = c(0,1), labels = c("Low Risk ASD", "High Risk ASD"))
CBSST$SRS_name <- factor(CBSST$median_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))
CBSST$StimType_name <- factor(CBSST$StimType, levels=c(0,1), labels = c("Charlie Brown", "Sesame Street"))
CBSST$SubjectID <-as.character(CBSST$SubjectID)

### Data are longitudinal; individual datapoints at different ages (~3, 6, 9, 12 months) are nested within each subject. Each subject also saw two types of clips: Charlie Brown (cartoon) and Sesame Street (live-action). 
###### Dependent variables are onscreen attention (%Fixation = on-screen dwell time/total clip length), relative attention to characters' faces (dwell time faces/onscreen onscreen dwell time)
###### Independent variables: infant age, autism rx status, parent SRS score 

################################################
######## A)  Analyze relative onscreen attention (%Fixation)
### 1A. Calculate ICC with an unconditional model
### First step is to see whether stimulua type (StimType) is an informative level for analysis
### For overall onscreen attention (dependent variable is % fixation)
### lmer calculates variance and we want to calculate amount of variance explained by StimTyp (as a level), within factors (age), and between factors (subj)

fm.empty_percent_fixation <- lmer(Percent_fix ~ age_centered + (age_centered | SubjectID/StimType), 
                                  data=CBSST_clean)
summary(fm.empty_percent_fixation)
#### Model outputs 
### subject variance = 118.7398
### StimType variance = 29.005
### Time (age) variance = 258.3809

######### ICC for subject (between):
ICC_subj_perfix <- 118.7398/(29.0050+118.7398+258.3809)
# 0.292372

######### ICC for StimType:
ICC_StimType_perfix<-29.005/(29.0050+118.7398+258.3809)
# 0.07141878
## this is a very little amount of variance explained by StimType (7.14%). We won't use that as a separate level in our linear mixed model

######### ICC for Time (within):
ICC_time_perfix <- 258.3809/(29.0050+118.7398+258.3809)
# 0.6362092

### 2A. Analyze developmental change in onscreen attention as a function of infant risk status and parental SRS score
fm.stim_SRS_RX_fix <- lme(Percent_fix ~ age_centered*mean_centered_SRS*RX,
                          random= ~age_centered|SubjectID, ## random effects of age 
                          data=CBSST)
summary(fm.stim_SRS_RX_fix)

################################################
######## B)  Analyze relative attention to faces by stimulus type, infant risk status and parental SRS score
### 1B. Calculate ICC with an unconditional model
### First step is to see whether stimulua type (StimType) is an informative level for analysis
### For overall onscreen attention (dependent variable is % fixation)

### lmer calculates variance and we want to calculate amount of variance explained by StimTyp (as a level), within factors (age), and between factors (subj)
fm.empty_face <- lmer(Percent_face ~ age_centered + (age_centered|SubjectID/StimType), data=CBSST)
summary(fm.empty_face)

#### Model outputs 
### subject variance = 45.3864
### StimType variance = 182.1779
### Time (age) variance = 104.2095

ICC_subj_face <- 45.3864/ (182.1779+45.3864+104.2095)
#0.1367
ICC_stim_face <- 182.1779/(182.1779+45.3864+104.2095)
#0.5491027; 54.91% of variance in attention to faces is accounted for by stimulus type. Let's include this as a level in the model!
ICC_time_face <- 104.2095/(182.1779+45.3864+104.2095)
#0.314098

### 2B. Analyze developmental change in attention to faces as a function of infant risk status, parental SRS score, and stimulus type
fm.stim_SRS_RX_face <- lme(Percent_face ~ age_centered*mean_centered_SRS*StimType +
                      age_centered*RX*StimType + age:RX:mean_centered_SRS, 
                      random= ~age_centered|SubjectID/StimType, ##random effects of age nested w/in stim w/in subj
                      data=CBSST)
summary(fm.stim_SRS_RX_face)

### there was a 3-way interaction with stimulus type, age, and familial SRS score. Let's break it up for easier interpretation by subsetting by stimulus type 
##### Just eye-tracking data from Charlie Brown 
CB_only <- subset(CBSST, StimType==0)
fm.SRS_CB_face <- lme(Percent_face ~ age_centered*mean_centered_SRS,
                 random= ~age_centered|SubjectID,
                 data=CB_only)
summary(fm.SRS_CB_face)

##### Just eye-tracking data from Sesame Street 
SST_only <- subset(CBSST, StimType==1)
fm.SRS_SST_face <- lme(Percent_face ~ age_centered*mean_centered_SRS,
                      random= ~age_centered|SubjectID,
                      data=SST_only)
summary(fm.SRS_SST_face)

############################################
###### Some Plots ##########################
############################################
workingDirName <- "~/Desktop/Projects/ETdataFiles/figs"
#### Plotting Eye-tracking data with age as X axis, % looking Faces as Y axis by infant Risk status
# Facet by Stimulus Type (Charlie Brown and Sesame Street)
##### FIGURE 1 in paper:
ggplot(CBSST,aes(x=age, y=Percent_face, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~StimType_name) + ylab("Percent On-Screen Looking") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=FALSE) + 
  geom_point(alpha=0.3, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=rx_name) +
  scale_colour_manual(breaks=c("Low Family Affectedness", "High Family Affectedness"),values=c("chartreuse4","darkmagenta")) + 
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=SRS_name,colour=SRS_name),method="lm",size=0.7, se=FALSE) +
  stat_smooth(aes(group=SRS_name, colour=SRS_name),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.08, show.legend=FALSE) +
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))+
  ggsave("Percent_face_SRS_mean.pdf",path=workingDirName,width=5,height=3,units="in")


#### Plotting Eye-tracking data with age as X axis, % looking Faces as Y axis by infant Risk status
# Facet by Stimulus Type (Charlie Brown and Sesame Street)

ggplot(CBSST,aes(x=age, y=Percent_face, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~StimType_name) + ylab("Percent Looking, Faces") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=TRUE) + 
  geom_point(alpha=0.3, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=rx_name) +
  scale_colour_manual(breaks=c("Low Risk","High Risk"),values=c("blue","red")) + 
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=rx_name,colour=rx_name),method="lm",size=0.7, se=FALSE) +
  stat_smooth(aes(group=rx_name, colour=rx_name),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.1, show.legend=FALSE) +
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12")) +
  ggsave("Percent_face_RX.pdf",path=workingDirName,width=5,height=3,units="in")


### density plots of SRS scores
### taking a look if SRS scores differ by parent and risk status
partChar_data <- read_excel("~/Desktop/Projects/Dissertation/CBSST/spreadsheets/r_plots_srs.xlsx", sheet = "Sheet1")
partChar <- as.data.frame(partChar_data)

partChar$rx_name <- factor(partChar$RX, levels = c(0,1), labels = c("Low Risk", "High Risk"))
partChar$SRS_resp <- factor(partChar$Group, levels = c(0,1,2), labels = c("Mom SRS", "Dad SRS", "Average SRS"))
CBSST$SRS_name <- factor(CBSST$median_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))


ggplot(partChar,aes(x=average_SRS, color=rx_name)) +
  geom_density(aes(x=MomSRS),show.legend=FALSE, stat="density") + 
  geom_density(aes(x=DadSRS), linetype="dotted",show.legend=FALSE) +
  scale_colour_manual(breaks=c("Low Risk", "High Risk"),values=c("blue","red")) + 
  xlab("Raw SRS score") + theme_bw() + 
  ggsave("SRS_dist.pdf",path=workingDirName,width=5,height=3,units="in")

p <- ggplot(partChar, aes(SRS_resp,SRS))
p + geom_boxplot(aes(fill = rx_name, alpha=0.1), outlier.shape = NA, width=0.3, show.legend=FALSE,position=position_dodge(0.4))+
  scale_fill_manual(breaks=c("Low Risk", "High Risk"),values=c("blue","red")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("")+
  ggsave("SRS_boxplot.pdf",path=workingDirName,width=5,height=3,units="in")
