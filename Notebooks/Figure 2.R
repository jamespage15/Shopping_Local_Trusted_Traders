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
northernstamps <- read_csv("northernstamps.csv")

### Creating a presence/absence table for NITS stamps ###
northernMatrix <- acast(northernstamps, site~pottername, value.var="pottername", fill=0)

### Morisita-Horn Similarity Coefficient Analysis (NITS stamps) ###
northernstampDistance <- vegdist(northernMatrix, method="horn")
northernstampDistance
northernstampDistanceMatrix <- as.matrix(northernstampDistance)

### Plotting Distance Scores as a Heatmap ###
get_lower_tri<-function(northernstampDistanceMatrix){northernstampDistanceMatrix[lower.tri(northernstampDistanceMatrix)] <- NA
return(northernstampDistanceMatrix)}
lower_tri <- get_lower_tri(northernstampDistanceMatrix)
lower_tri
northernstamps2 <- melt(lower_tri, na.rm = TRUE)
ggplot(data = northernstamps2, aes(Var1, Var2, fill = value, label=round(value, 2)))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colours=c("indianred2", "cornsilk", "steelblue2"), 
                       space = "Lab", 
                       name="Morisita-Horn Index\nDistance Score") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ labs(title="", x="", y="") + geom_text(color="black", size=1)

