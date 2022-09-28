# Coding Club Workshop 1 - R Basics
# Ecostats Meetings Sept 21 & 28th

#installing packages for this tutorial
install.packages("dplyr")
library(dplyr)

#setting the working directory to my folder
setwd("~/Desktop/Ecostats")

#reading in the data for this tutorial
edidiv <- read.csv("~/Dropbox/Mac/Desktop/Ecostats/CC-RBasics-master/edidiv.csv")

head(edidiv)
tail(edidiv)
str(edidiv)

#You can pull out a specific column by using the $ sign 
head(edidiv$taxonGroup)
class(edidiv$taxonGroup)
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)

class(edidiv$taxonGroup)

#Dim shows you the dimensions of your data (but you can also see this under the Environment tab)
dim(edidiv)

summary(edidiv)
summary(edidiv$taxonGroup)
unique(edidiv$taxonGroup)
table(edidiv$taxonGroup)
table(edidiv$organisationName)

#calculating species richness 
Beetle <- filter(edidiv, taxonGroup == "Beetle")
Bird <- filter(edidiv, taxonGroup == "Bird")

beetle_richness <- length(unique(Beetle$taxonName))
beetle_richness
bird_richness <- length(unique(Bird$taxonName))
bird_richness

biodiv <- c(beetle_richness, bird_richness)
biodiv
names(biodiv) <- c("Beetle", "Bird")
biodiv

#making a barplot of the biodiversity data and saving the plot to your computer
#control or command shift c will comment out multiple lines of code at once if they are highlighted
barplot(biodiv)
# png(filename = "biodivbarplot.png", width = 600, height = 600)
# barplot(biodiv, xlab = "Taxa", ylab = "Species Richness")
# dev.off()

#creating our own dataframe 
taxa <- c("Beetle", "Bird")
taxa_f <- factor(taxa)
richness <- c(beetle_richness, bird_richness)
biodata <- data.frame(taxa_f, richness)
biodata
#saving your dataframe as a csv
write.csv(biodata, file="biodata.csv")

barplot(biodata$richness, names.arg = c("Beetle", "Bird"))

#trying the challenge!

bird_sp <- c("sparrow", "kingfisher", "eagle", "hummingbird", 
             "sparrow", "kingfisher", "eagle", "hummingbird",
             "sparrow", "kingfisher", "eagle", "hummingbird")
wingspan <- c(22, 26, 195, 8, 24, 23, 201, 9, 21, 25, 185, 9)
bird_wingspan <- data.frame(bird_sp, wingspan)
Sparrow <- filter(bird_wingspan, bird_sp =="sparrow")
Kingfisher <- filter(bird_wingspan, bird_sp =="kingfisher")
Eagle <- filter(bird_wingspan, bird_sp == "eagle")
Hummingbird <- filter(bird_wingspan, bird_sp =="hummingbird")

mean_sparrow <- mean(Sparrow$wingspan)
mean_kingfisher <- mean(Kingfisher$wingspan)
mean_eagle <- mean(Eagle$wingspan)
mean_hummingbird <- mean(Hummingbird$wingspan)

mean_birds <- c(mean_sparrow, mean_kingfisher, mean_eagle, mean_hummingbird)
barplot(mean_birds, names.arg = c("Sparrow", "Kingfisher", "Eagle", "Hummingbird"))
