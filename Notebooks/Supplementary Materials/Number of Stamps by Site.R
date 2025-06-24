#### Quantities of Sigillata Stamps By Site ####

#### Packages ####
library(ggplot2)


#### Import data ####
sigillata <- read_csv("sigillata.csv")


#### All Stamps ####

#### Displaying Number of Stamps by Site ####
ggplot(sigillata, aes(x=site)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x="", y="Number of Stamps") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


#### Northern Italic Stamps ####

#### Creating a dataframe for NITS stamps ####
categories <- c("Po Valley", "Po Valley?", "Bologna?", "Cremona", "Faenza", "Vercelli")
northernstamps <- sigillata[sigillata3$production %in% categories,]

#### Displaying Number of Stamps by Site ####
ggplot(northernstamps, aes(x=site)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x="", y="Number of Stamps") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


#### Arretine and Central Italic Stamps ####

#### Creating a dataframe for stamps from Central Italy ####
categories4 <- c("Arezzo", "Arezzo?", "Central Italy", "Central Italy?", "Etruria", "Etruria?", "Pisa", "Pisa?", "Pozzuoli", "Pozzuoli?", "Puetonana", "Scoppieto", "Torrita di Siena", "Vasanello")
centralstamps <- sigillata[sigillata3$production %in% categories4,]

#### Displaying Number of Stamps by Site ####
ggplot(centralstamps, aes(x=site)) + geom_bar(stat="count") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x="", y="Number of Stamps") + theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
