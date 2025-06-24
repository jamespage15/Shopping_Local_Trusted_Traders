#### Heatmaps for Assemblage Similarity ####

#### Packages ####
library(dplyr)
library(readxl)
library(reshape2)
library(vegan)
library(ggplot2)
library(ggdendro)
library(scales)
brks <- c(0, 0.25, 0.5, 0.75, 1)


#### Import data ####
sigillata <- read_csv("sigillata.csv")


#### All Stamps ####

### Creating a presence/absence table for all stamps ###
sigillataMatrix <- acast(sigillata, site~pottername, value.var="pottername", fill=0)

### Morisita-Horn Similarity Coefficient Analysis (all stamps) ###
stampDistance <- vegdist(sigillataMatrix, method="horn")
stampDistance
stampDistanceMatrix <- as.matrix(stampDistance)

### Plotting as a heatmap ###
get_lower_tri<-function(stampDistanceMatrix){stampDistanceMatrix[lower.tri(stampDistanceMatrix)] <- NA
return(stampDistanceMatrix)}
lower_tri <- get_lower_tri(stampDistanceMatrix)
lower_tri
stamps <- melt(lower_tri, na.rm = TRUE)
ggplot(data = stamps, aes(Var1, Var2, fill = value, label=round(value, 2))) + geom_tile(color = "white") + scale_fill_gradientn(colours=c("indianred2", "cornsilk", "steelblue2"), 
                       space = "Lab", 
                       name="Horn-Morisita Index\nDistance Score") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ labs(title="", x="", y="") + geom_text(color="black", size=1)


#### Northern Italic Stamps ####

### Creating a dataframe for NITS stamps ###
categories1 <- c("Po Valley", "Po Valley?", "Bologna?", "Cremona", "Faenza", "Vercelli")
northernstamps <- sigillata[sigillata$production %in% categories1,]

### Removing sites with less than 10 NITS stamps ###
northernstamps <- northernstamps[northernstamps$site != "Alba", ]
northernstamps <- northernstamps[northernstamps$site != "Bene Vagienna", ]
northernstamps <- northernstamps[northernstamps$site != "Cassana", ]
northernstamps <- northernstamps[northernstamps$site != "Como", ]
northernstamps <- northernstamps[northernstamps$site != "Forlì", ]
northernstamps <- northernstamps[northernstamps$site != "Libarna", ]
northernstamps <- northernstamps[northernstamps$site != "Luni", ]
northernstamps <- northernstamps[northernstamps$site != "Novara", ]
northernstamps <- northernstamps[northernstamps$site != "Pollentia (2)", ]
northernstamps <- northernstamps[northernstamps$site != "Romprezzano", ]
northernstamps <- northernstamps[northernstamps$site != "Spilamberto", ]
northernstamps <- northernstamps[northernstamps$site != "Trento", ]
northernstamps <- northernstamps[northernstamps$site != "Velleia", ]
northernstamps <- northernstamps[northernstamps$site != "Vercelli", ]
northernstamps <- northernstamps[northernstamps$site != "Verona", ]
northernstamps <- northernstamps[northernstamps$site != "Vicenza", ]

### Creating a presence/absence table for NITS stamps ###
northernMatrix <- acast(northernstamps, site~pottername, value.var="pottername", fill=0)

### Morisita-Horn Similarity Coefficient Analysis (NITS stamps) ###
northernstampDistance <- vegdist(northernMatrix, method="horn")
northernstampDistance
northernstampDistanceMatrix <- as.matrix(northernstampDistance)

### Plotting as a heatmap ###
get_lower_tri<-function(northernstampDistanceMatrix){northernstampDistanceMatrix[lower.tri(northernstampDistanceMatrix)] <- NA
return(northernstampDistanceMatrix)}
lower_tri <- get_lower_tri(northernstampDistanceMatrix)
lower_tri
northernstamps2 <- melt(lower_tri, na.rm = TRUE)
ggplot(data = northernstamps2, aes(Var1, Var2, fill = value, label=round(value, 2)))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colours=c("indianred2", "cornsilk", "steelblue2"), 
                       space = "Lab", 
                       name="Horn-Morisita Index\nDistance Score") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ labs(title="", x="", y="") + geom_text(color="black", size=1)


#### Arretine and Central Italian Stamps ####

### Creating a dataframe for stamps from the rest of Italy ###
categories2 <- c("Arezzo", "Arezzo?", "Arezzo/Pisa", "Central Italy", "Central Italy?", "Etruria", "Etruria?", "Pisa", "Pisa?", "Pozzuoli", "Pozzuoli?", "Puetonana", "Scoppieto", "Torrita di Siena", "Vasanello")
centralstamps <- sigillata[sigillata$production %in% categories2,]

#### Removing Sites with less than 10 Italian stamps ####
centralstamps <- centralstamps[centralstamps$site != "Adria", ]
centralstamps <- centralstamps[centralstamps$site != "Angera", ]
centralstamps <- centralstamps[centralstamps$site != "Ascona", ]
centralstamps <- centralstamps[centralstamps$site != "Como", ]
centralstamps <- centralstamps[centralstamps$site != "Concordia", ]
centralstamps <- centralstamps[centralstamps$site != "Forlì", ]
centralstamps <- centralstamps[centralstamps$site != "Polesine", ]
centralstamps <- centralstamps[centralstamps$site != "Pollentia (2)", ]
centralstamps <- centralstamps[centralstamps$site != "Romprezzano", ]
centralstamps <- centralstamps[centralstamps$site != "Spilamberto", ]
centralstamps <- centralstamps[centralstamps$site != "Trento", ]
centralstamps <- centralstamps[centralstamps$site != "Valli Gr. Veronesi", ]
centralstamps <- centralstamps[centralstamps$site != "Verona", ]
centralstamps <- centralstamps[centralstamps$site != "Vicenza", ]

#### Creating a presence/absence table for Italian stamps ####
centralMatrix <- acast(centralstamps, site~pottername, value.var="pottername", fill=0)

#### Morisita-Horn Similarity Coefficient Analysis (Italian stamps) ####
centralstampDistance <- vegdist(centralMatrix, method="horn")
centralstampDistance
centralstampDistanceMatrix <- as.matrix(centralstampDistance)

### Plotting as a heatmap ###
get_lower_tri<-function(centralstampDistanceMatrix){centralstampDistanceMatrix[lower.tri(centralstampDistanceMatrix)] <- NA
return(centralstampDistanceMatrix)}
lower_tri <- get_lower_tri(centralstampDistanceMatrix)
lower_tri
centralstamps2 <- melt(lower_tri, na.rm = TRUE)
ggplot(data = centralstamps2, aes(Var1, Var2, fill = value, label=round(value, 2)))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colours=c("indianred2", "cornsilk", "steelblue2"), 
                       space = "Lab", 
                       name="Horn-Morisita Index\nDistance Score") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ labs(title="", x="", y="") + geom_text(color="black", size=1)



