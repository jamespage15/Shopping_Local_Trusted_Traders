#### Figure 2 ####


#### Packages ####
library(dendextend)
library(dplyr)
library(ggplot2)
library(ggdendro)
library(readxl)
library(reshape2)
library(scales)
library(vegan)
brks <- c(0, 0.25, 0.5, 0.75, 1)


#### Import data ####
sigillata <- read_csv("sigillata.csv")

#### Creating a dataframe for NITS stamps ####
categories <- c("Po Valley", "Po Valley?", "Bologna?", "Cremona", "Faenza", "Vercelli")
northernstamps <- sigillata[sigillata$production %in% categories,]

#### Removing sites with less than 10 NITS stamps ####
northernstamps <- northernstamps[northernstamps$site != "Alba", ]
northernstamps <- northernstamps[northernstamps$site != "Bene Vagienna", ]
northernstamps <- northernstamps[northernstamps$site != "Cassana", ]
northernstamps <- northernstamps[northernstamps$site != "Como", ]
northernstamps <- northernstamps[northernstamps$site != "Forlì", ]
northernstamps <- northernstamps[northernstamps$site != "Libarna", ]
northernstamps <- northernstamps[northernstamps$site != "Luni", ]
northernstamps <- northernstamps[northernstamps$site != "Novara", ]
northernstamps <- northernstamps[northernstamps$site != "Pollentia", ]
northernstamps <- northernstamps[northernstamps$site != "Romprezzano", ]
northernstamps <- northernstamps[northernstamps$site != "Spilamberto", ]
northernstamps <- northernstamps[northernstamps$site != "Trento", ]
northernstamps <- northernstamps[northernstamps$site != "Velleia", ]
northernstamps <- northernstamps[northernstamps$site != "Vercelli", ]
northernstamps <- northernstamps[northernstamps$site != "Verona", ]
northernstamps <- northernstamps[northernstamps$site != "Vicenza", ]

#### Creating a presence/absence table for NITS stamps ####
northernMatrix <- acast(northernstamps, site~pottername, value.var="pottername", fill=0)
View(northernMatrix)

#### Morisita-Horn Similarity Coefficient Analysis (NITS stamps) ####
stampDistance <- vegdist(northernMatrix, method="horn")
stampDistance
stampDistanceMatrix <- as.matrix(stampDistance)
stamps <- melt(stampDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")
ggplot(stamps, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) + geom_raster() + labs(title="Morisita-Horn Overlap Coefficient", x = NULL, y = NULL, fill = "Distance") + geom_raster() + geom_text(color="white")
morisitaHClust <- hclust(stampDistance, method="average")
dend <- morisitaHClust %>% as.dendrogram %>%
  set("branches_k_color", value = c("#cb7f00", "#bbbe50", "#9b3d3d", "#5d5dd3"), k=4) %>% 
  set("labels_cex", 0.8) %>% 
  set("branches_lwd", 0.8)
plot(dend)
ggd1 <- as.ggdend(dend)
ggplot(ggd1, horiz = TRUE) + 
  labs(title="", x="", y="Distance between assemblages") +  
  theme_bw() + 
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(), 
        plot.margin = margin(t = 10, r = 10, b = 10, l = 20)) 