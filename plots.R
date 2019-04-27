library(ggplot2)
library(extrafont)
library(ggthemes)

workingDirName<-"~/Desktop/Projects/ETdataFiles/figs" #Used for finding where .txt is saved and for deciding where to output .png of graph
CBSST_data <- read_excel("~/Desktop/Projects/ETdataFiles/spreadsheets/__10-13-17_CBSST_ET_data_concat.xlsx", sheet = "Sheet1")
CBSST_all <- as.data.frame(CBSST_data)

CBSST_clean <- subset(CBSST_all, poor_cal==1)
CBSST <- subset(CBSST_clean, include==1)
CBSST_plot <- as.data.frame(CBSST_clean)
CBSST_nosibs <-subset(CBSST, sib==0)
CBSST <-as.data.frame(CBSST_nosibs)


#### plotting the 3-way interaction. Decomposted by age, SRS and StimType (maybe look both at mean centered and median split)
SST_only <- subset(CBSST, StimType==1)
CB_only <- subset(CBSST, StimType==0)

CB_only$SubjectID<-as.character(CB_only$SubjectID)
CB_only$SRS_name_mean <- factor(CB_only$mean_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))
CB_only$SRS_name_median <- factor(CB_only$median_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))
CB_only$RX <- factor(CB_only$RX, levels=c(0,1), labels = c("Low-Risk", "High Risk"))

SST_only$SubjectID<-as.character(SST_only$SubjectID)
SST_only$SRS_name_mean <- factor(SST_only$mean_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))
SST_only$SRS_name_median <- factor(SST_only$median_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))
SST_only$RX <- factor(SST_only$RX, levels=c(0,1), labels = c("Low-Risk", "High Risk"))

CBSST$SubjectID <- as.character(CBSST$SubjectID)
CBSST$SRS_name_mean <- factor(CBSST$mean_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))
CBSST$SRS_name_median <- factor(CBSST$median_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))


ggplot(CB_only,aes(x=age, y=Percent_face, color=SubjectID, linetype="dashed")) + 
   ylab("Percent Looking, Face") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=FALSE) + 
  geom_point(alpha=0.5, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=RX) + coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() +
  scale_colour_manual(breaks=c("Low Risk","High Risk"),values=c("blue","red")) + 
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))+
  ggsave("Percent_face_CB_RX_scatter.pdf",path=workingDirName,width=5,height=3,units="in")



ggplot(SST_only,aes(x=age, y=Percent_face, color=SubjectID)) +
  ylab("Percent Looking, Face") + 
  geom_smooth(aes(group=SRS_name_median,colour=SRS_name_median),method="lm",size=0.7, se=FALSE, show.legend=FALSE) +
stat_smooth(aes(group=SRS_name_median, colour=SRS_name_median),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.08, show.legend=FALSE) +
  scale_colour_manual(breaks=c("Low Family Affectedness", "High Family Affectedness"),values=c("chartreuse4","darkmagenta")) + 
  xlab("Age, Months") +
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12")) +
  ggsave("Percent_face_SST_SRS_fit.pdf",path=workingDirName,width=5,height=3,units="in")
  
  #Actually does the coloring based on diagnosis, with red and blue
  
  
   + 
  geom_smooth(aes(group=SRS_name_median,colour=SRS_name_median),method="lm",size=0.7, se=FALSE) +
  stat_smooth(aes(group=SRS_name_median, colour=SRS_name_median),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.08, show.legend=FALSE) +
  
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))
+
  ggsave("Percent_fixation_SRS_mean.pdf",path=workingDirName,width=5,height=3,units="in")




CBSSTave_data <- read_excel("~/Desktop/Projects/Dissertation/CBSST/spreadsheets/__10-13-17_CBSST_ET_data_concat.xlsx", sheet = "short")
CBSSTave_data <- as.data.frame(CBSSTave_data)
CBSSTave_data$rx_name <- factor(CBSSTave_data$RX, levels = c(0,1), labels = c("Low Risk", "High Risk"))
CBSSTave_data$SubjectID <-as.character(CBSSTave_data$SubjectID)

ggplot(CBSSTave_data,aes(x=age, y=Percent_face, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~ rx_name) + ylab("Percent Looking, Face") + 
  geom_line(aes(group=SubjectID, linetype="dashed")) + geom_point(aes(group=SubjectID)) + guides(color = "none", linetype="none") +
  coord_cartesian(xlim=c(2,14), ylim=c(0,100)) +theme_bw()


ggplot(CBSSTave_data,aes(x=age, y=Percent_face, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~rx_name) + ylab("Percent Looking, Faces") + 
  geom_line(aes(group=SubjectID),linetype="dashed", size=0.3, show.legend=FALSE) + 
  geom_point(size=1.5) + guides(color = "none", linetype="none") + 
xlab("Age, Months") + coord_cartesian(ylim=c(0,100))+ scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12")) + theme_bw()+
  ggsave("Percent_faces_all.pdf",path=workingDirName,width=5,height=3,units="in")
  
  
  #Actually does the coloring based on diagnosis, with red and blue
  scale_colour_manual(breaks=c("Low Risk","High Risk"),values=c("blue","red")) + 
  
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=rx_name,colour=rx_name),method="lm",formula = y ~ x + I(x^2),size=0.7, se=FALSE) +
  stat_smooth(aes(group=rx_name, colour=rx_name),size=0.5,method="lm",formula = y ~ x + I(x^2), linetype=2,geom="ribbon", alpha=0.1, show.legend=FALSE) +
  
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))+


SN_dat <- read_excel("~/Desktop/Projects/ETdataFiles/spreadsheets/__10-13-17_CBSST_ET_data_concat.xlsx", sheet = "SN_dat")
SN_dat <- as.data.frame(SN_dat)
CBSST_plot$rx_name <- factor(CBSST_plot$RX, levels = c(0,1), labels = c("Low Risk", "High Risk"))
CBSST_plot$SRS_name_mean <- factor(CBSST_plot$mean_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))
CBSST_plot$SRS_name_median <- factor(CBSST_plot$median_split_SRS, levels=c(0,1), labels = c("Low Family Affectedness", "High Family Affectedness"))

CBSST_plot$StimType_name <- factor(CBSST_plot$StimType, levels=c(0,1), labels = c("Charlie Brown", "Sesame Street"))
CBSST_plot$SubjectID <-as.character(CBSST_plot$SubjectID)

ggplot(CBSST_plot,aes(x=age, y=Percent_face_dyad, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~StimType_name) + ylab("Percent On-Screen Looking") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=FALSE) + 
  geom_point(alpha=0.3, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=rx_name) +
  
  #Actually does the coloring based on diagnosis, with red and blue
  scale_colour_manual(breaks=c("Low Risk","High Risk"),values=c("blue","red")) + 
  
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=rx_name,colour=rx_name),method="lm",formula = y ~ x + I(x^2),size=0.7, se=FALSE) +
  stat_smooth(aes(group=rx_name, colour=rx_name),size=0.5,method="lm",formula = y ~ x + I(x^2), linetype=2,geom="ribbon", alpha=0.1, show.legend=FALSE) +
  
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))+
 ggsave("Percent_fix_faces_quad.pdf",path=workingDirName,width=5,height=3,units="in")
  
  

ggplot(CBSST_plot,aes(x=age, y=Percent_face, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~StimType_name) + ylab("Percent Looking, Faces") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=TRUE) + 
  geom_point(alpha=0.3, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=rx_name) +
  
  #Actually does the coloring based on diagnosis, with red and blue
  scale_colour_manual(breaks=c("Low Risk","High Risk"),values=c("blue","red")) + 
  
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=rx_name,colour=rx_name),method="lm",size=0.7, se=FALSE) +
  stat_smooth(aes(group=rx_name, colour=rx_name),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.1, show.legend=FALSE) +
  
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))
+
  
  ggsave("Percent_face_RX.pdf",path=workingDirName,width=5,height=3,units="in")



#### by SRS
ggplot(CBSST_plot,aes(x=age, y=Percent_fix, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~StimType_name) + ylab("Percent On-Screen Looking") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=FALSE) + 
  geom_point(alpha=0.3, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=SRS_name_mean) +
  
  #Actually does the coloring based on diagnosis, with red and blue
  scale_colour_manual(breaks=c("Low Family Affectedness", "High Family Affectedness"),values=c("chartreuse4","darkmagenta")) + 
  
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=SRS_name_mean,colour=SRS_name_mean),method="lm",size=0.7, se=FALSE) +
  stat_smooth(aes(group=SRS_name_mean, colour=SRS_name_mean),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.08, show.legend=FALSE) +
  
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))+
  ggsave("Percent_fixation_SRS_mean.pdf",path=workingDirName,width=5,height=3,units="in")


ggplot(CBSST_plot,aes(x=age, y=Percent_dyad_face, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~StimType_name) + ylab("Percent Looking, Face for Salience") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=FALSE) + 
  geom_point(alpha=0.3, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=SRS_name_mean) +
  
  #Actually does the coloring based on diagnosis, with red and blue
  scale_colour_manual(breaks=c("Low Family Affectedness", "High Family Affectedness"),values=c("dark green","purple")) + 
  
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=SRS_name_mean,colour=SRS_name_mean),method="lm",size=0.7, se=FALSE) +
  stat_smooth(aes(group=SRS_name_mean, colour=SRS_name_mean),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.1, show.legend=FALSE) +
  
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))+
  ggsave("Percent_fix_facessal_SRS_mean.pdf",path=workingDirName,width=5,height=3,units="in")



ggplot(CBSST,aes(x=age, y=Percent_fix, color=SubjectID, linetype="dashed")) + 
  facet_grid(.~StimType_name) + ylab("Percent Fixation, Faces") + 
  geom_line(aes(group=SubjectID),linetype="dashed", alpha=0.35, size=0.1, show.legend=FALSE) + 
  geom_point(alpha=0.3, size=0.5) + guides(color = "none", linetype="none") +
  aes(colour=SRS_name) +
  
  #Actually does the coloring based on diagnosis, with red and blue
  scale_colour_manual(breaks=c("Low Family Affectedness", "High Family Affectedness"),values=c("dark green","purple")) + 
  
  coord_cartesian(xlim=c(2.5,13), ylim=c(0,100)) +theme_bw() + 
  geom_smooth(aes(group=SRS_name,colour=SRS_name),method="lm",size=0.7, se=FALSE) +
  stat_smooth(aes(group=SRS_name, colour=SRS_name),size=0.5,method="lm",linetype=2,geom="ribbon", alpha=0.1, show.legend=FALSE) +
  
  xlab("Age, Months") +
  scale_x_continuous(limits=c(2.5,13),breaks=c(3,6,9,12),labels=c("3", "6","9","12"))+
  ggsave("Percent_fix_SRS.pdf",path=workingDirName,width=5,height=3,units="in")



### density plots of SRS scores
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


ggplot(CBSST,aes(x=age, y=entropy, color=SubjectID, linetype="dashed")) + 
  facet_grid(StimType_name ~ rx_name) + ylab("Entropy") + 
  geom_line(aes(group=SubjectID, linetype="dashed")) + geom_point(aes(group=SubjectID)) + guides(color = "none", alpha="none", linetype="none") +
  coord_cartesian(xlim=c(2,14), ylim=c(10,15)) +theme_bw()

ggplot(CBSST,aes(x=age, y=Percent_salience, color=SubjectID, linetype="dashed")) + 
  facet_grid(StimType_name ~ rx_name) + ylab("Percent Fixation, Salience") + 
  geom_line(aes(group=SubjectID, linetype="dashed")) + geom_point(aes(group=SubjectID)) + guides(color = "none", linetype="none") +
  coord_cartesian(xlim=c(2,14), ylim=c(0,100)) +theme_bw()

ggplot(CBSST,aes(x=age, y=Mean_Fix_MS, color=SubjectID, linetype="dashed")) + 
  facet_grid(StimType_name ~ rx_name) + ylab("Mean Fixation, MS") + 
  geom_line(aes(group=SubjectID, linetype="dashed")) + geom_point(aes(group=SubjectID, alpha=0.8)) + guides(color = "none", alpha="none", linetype="none") 
#+
# coord_cartesian(xlim=c(2,14), ylim=c(0,100)) 

ggplot(CBSST,aes(x=age, y=Congruent_Face_Sal, color=SubjectID, linetype="dashed")) + 
  facet_grid(StimType_name ~ rx_name) + ylab("Percent Fixation, Congruent Salience & Face") + 
  geom_line(aes(group=SubjectID, linetype="dashed")) + geom_point(aes(group=SubjectID)) + guides(color = "none", linetype="none") +
  coord_cartesian(xlim=c(2,14), ylim=c(0,75)) + theme_bw()

ggplot(CBSST,aes(x=age, y=Face_over_Sal, color=SubjectID, linetype="dashed")) + 
  facet_grid(StimType_name ~ rx_name) + ylab("Percent Fixation, Face over Salient") + 
  geom_line(aes(group=SubjectID, linetype="dashed")) + geom_point(aes(group=SubjectID)) + guides(color = "none", linetype="none") +
  coord_cartesian(xlim=c(2,14), ylim=c(0,50)) +theme_bw()

ggplot(CBSST,aes(x=age, y=Sal_over_Face, color=SubjectID, linetype="dashed")) + 
  facet_grid(StimType_name ~ rx_name) + ylab("Percent Fixation, Salient over Face") + 
  geom_line(aes(group=SubjectID, linetype="dashed")) + geom_point(aes(group=SubjectID)) + guides(color = "none", linetype="none") +
  coord_cartesian(xlim=c(2,14), ylim=c(0,50)) +theme_bw()
