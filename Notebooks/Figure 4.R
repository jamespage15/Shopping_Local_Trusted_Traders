
#### Packages ####

library(datplot)
library(dplyr)
library(ggplot2)
library(patchwork)

### Plot Theme ###
Plot_Theme<-theme(panel.background =element_blank(), panel.grid.major =element_line("dimgray",size=0.5, linetype="dashed"), panel.grid.minor=element_line("grey",size=0.5, linetype="dotted"), panel.border = element_rect(colour = "black", fill=NA, size=1))
Plot_Scale<-scale_x_continuous(breaks=seq(from=-75,to=250,by=25), limits=c(-75,250),name="") 

#### Import Data ####
northernstamps <- read.csv("northernstamps.csv")
unstampedNITS <- read.csv("unstampedNITS.csv")

#### Subset Clusters ####

### Assign Cluster to Sites (Stamped) ###
northernstamps <- northernstamps %>%
  mutate(cluster = case_when(
    site %in% c("Ascona", "Angera", "Locarno", "Turin", "Cremona", "Milan", "Ivrea", "Giubiasco", "Ornavasso") ~ "cluster1",
    site %in% c("Aquileia", "Altino", "Concordia Sagittaria", "Padua", "Bologna", "Modena", "Adria", "Concordia", "Russi", "Rimini", "Faenza", "Polesine", "Tortona", "Valli Gr. Veronesi", "Budrio") ~ "cluster2",
    TRUE ~ NA_character_  
  ))

### Assign Cluster to Sites (Unstamped) ###
unstampedNITS <- unstampedNITS %>%
  mutate(cluster = case_when(
    Location_Specific %in% c("Milan", "Alba", "Chieri", "Bedriacum", "Cremona", "Acqui Terme", "Ivrea", "Augusta Bagiennorum", "Como", "Cividate Camunnorum", "Brescia") ~ "cluster1",
    Location_Specific %in% c("Aquileia", "Altinum", "Julia Concordia", "Padua", "Bologna", "Modena", "Adria", "Forlì", "Verona", "Ariminum", "Trento", "Tortona",  "Ravenna") ~ "cluster2",
    TRUE ~ NA_character_  
  ))

#### Aoristic Analysis ####

#### Stamped Sites ####

### Make production start and end dates numeric ###
northernstamps$DAT_min <- as.numeric(northernstamps$lower_date)
northernstamps$DAT_max <- as.numeric(northernstamps$upper_date)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
northernstamps_steps<-northernstamps %>% 
  select(id, cluster, DAT_min, DAT_max) %>% 
  datsteps(stepsize=5)

### Addition of weights to assign equal importance to all dated forms ###
northernstamps_steps<-scaleweight(northernstamps_steps,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(northernstamps_steps)$stepsize 
scalevalue<-get.histogramscale(northernstamps_steps,binwidth=binwidth) 

#### Plotting the Frequency ####
p1 <- ggplot(northernstamps_steps,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Cluster", labels = c("Cluster 1", "Cluster 2"), values = c("indianred2", "sandybrown")) + labs(y="Frequency", legend="Cluster", title="", subtitle = "A - Stamped NITS Assemblages", attributes(northernstamps)$source,sep="") 

#### Unstamped Sites ####

### Make production start and end dates numeric ###
unstampedNITS$DAT_min <- as.numeric(unstampedNITS$Lower_Date_Form)
unstampedNITS$DAT_max <- as.numeric(unstampedNITS$Upper_Date_Form)

### Creation of dating steps (multiplies each object into a number of observations dependent on the object’s dating range)
unstampedNITS_steps<-unstampedNITS %>% 
  select(REFINI_ID, cluster, DAT_min, DAT_max) %>% 
  datsteps(stepsize=5)

### Addition of weights to assign equal importance to all dated forms ###
unstampedNITS_steps<-scaleweight(unstampedNITS_steps,var="all")

### Breakdown of Distribution ###
binwidth<-attributes(unstampedNITS_steps)$stepsize 
scalevalue<-get.histogramscale(unstampedNITS_steps,binwidth=binwidth) 

#### Plotting the Frequency ####
p2 <- ggplot(unstampedNITS_steps,aes(x=DAT_step, colour=variable)) + geom_density(bw=5, aes(y = after_stat(count)), size=1.0, alpha = 0.25,) + Plot_Theme + Plot_Scale + scale_colour_manual(name  ="Cluster", labels = c("Cluster 1", "Cluster 2"), values = c("indianred2", "sandybrown")) + labs(y="Frequency", legend="Cluster", title="", subtitle = "B - Unstamped NITS Assemblages", attributes(unstampedNITS)$source,sep="") 

Aoristic  <- (p1)/(p2)
Aoristic 
