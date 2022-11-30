library(here)
library(ggplot2)
trees_bicuar<- read.csv(here::here("Nov_2_FunLoops/CC", "trees_bicuar.csv"))
trees_mlunguya<- read.csv(here::here("Nov_2_FunLoops/CC", "trees_mlunguya.csv"))
LPI <- read.csv(here::here("Nov_2_FunLoops/CC", "LPI_data_loops.csv"))

head(trees_bicuar)
str(trees_bicuar)

example.fn <- function(x, y) {
  x + y
  
}

example.fn(x = 1, y = 2)

basal.area <- function(x) {
  (pi*(x)^2)/40000
  
}

basal.area(x=5)
trees_bicuar$ba <-basal.area(x = trees_bicuar$diam)
trees_mlunguya$ba <- basal.area(x=trees_mlunguya$diam)

trees <- list("trees_bicuar"= trees_bicuar, "trees_mlunguya" = trees_mlunguya)

trees[[1]] #pulling out one data frame or one element in a list

for(i in 1:length(trees)) {
  
  trees[[i]]$ba_loop <- basal.area(x = trees[[i]]$diam)
  
}

trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year)

mean_ba_list <- list()

for (i in 1:length(trees_mlunguya_list)) {
 
  ba <- basal.area(trees_mlunguya_list[[i]]$diam)
  mean_ba <- mean(ba)
  year <- mean(trees_mlunguya_list[[i]]$year)
  dat <- data.frame(year,mean_ba)
  mean_ba_list[[i]] <- dat
}

# for the year line (line 43) you could also use names(trees_mlunguya_list)[i]

#making a new fxn using the fxn we made above
ba.mean.year <- function (dbh, year) {
  data.frame(
    mean_ba = mean(basal.area(dbh)),
    year = mean(year)
  )
  
}

ba.mean.year(5,2006) #testing the function we just made 


mean_ba_list2 <- list()

for (i in 1:length(trees_mlunguya_list)) {
  mean_ba_list2[[i]] <- ba.mean.year(
   dbh = trees_mlunguya_list[[i]]$diam,
   year = trees_mlunguya_list[[i]]$year
  )
  
}

bicuar_mean <- mean_ba_list2[[1]]
trees_means <- data.frame()

for(i in 1:length(mean_ba_list2)) {
  trees_means <- rbind(trees_means, mean_ba_list2[[i]])
  
}

# FUNCTIONS AND LOOPS PART 2 

lapply(trees_mlunguya_list, function(x){ba.mean.year(dbh= x$diam, year = x$year)}) 

bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family)
lapply(bicuar_height_list, mean, na.rm = TRUE)

#use sapply to output to a vector. "s" stands for simple 
sapply(bicuar_height_list, mean, na.rm = TRUE)

# Conditional statements  

stick.adj.lorey <- function(height, method, ba) {
  height_adj <- ifelse(method == "stick", height + 1, round(height, digits = 1))
  lorey_height <- sum(height_adj * ba, na.rm = TRUE)/ sum(ba, na.rm = TRUE)
  return(lorey_height)
}

trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode)

lapply(trees_bicuar_list, function(x) {
  stick.adj.lorey(height = x$height, method = x$height_method, ba =x$ba)
})

diam.summ <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean == TRUE, mean(dbh), NA)
  median_dbh <- ifelse(median == TRUE, median(dbh), NA)
  mean_ba <- ifelse(ba ==TRUE, mean(basal.area(dbh)), NA)
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}

# without specifying, it will return all with the assumption of being TRUE 
diam.summ(dbh = trees_bicuar$diam)
# but you can specify that certain aspects are FALSE 
diam.summ(dbh = trees_bicuar$diam, ba = FALSE)

#adding an ! before the ba tells it to treat it as FALSE (contrary to what is specified in the first line)
diam.summ.peter <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean, mean(dbh), NA)
  median_dbh <- ifelse(median, median(dbh), NA)
  mean_ba <- ifelse(!ba, mean(basal.area(dbh)), NA)
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}

diam.summ.peter(dbh = trees_bicuar$diam)

diam.summ.peter(dbh = trees_bicuar$diam, ba = FALSE)

# Plotting with loops - vulture data 

vulture <- dplyr::filter(LPI, LPI$Common.Name == "Griffon vulture / Eurasian griffon")
vultureITCR <- dplyr::filter(vulture, Country.list == c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, color = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list))+                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                # Removing the background grid lines                
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),           # Adding a 0.5cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),              # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position = c(0.9, 0.9)))               # Setting the position for the legend - 0 is left/bottom, 1 is top/right


#creating your own theme that you can call on in plots 
theme.my.own <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}

(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, color = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list))+                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear") +
    theme.my.own())


LPI.UK <- filter(LPI, Country.list == "United Kingdom")
house.sparrow <- filter(LPI.UK, Common.Name == "House sparrow")
ggplot(house.sparrow, aes(x = year, y= abundance)) +
  geom_point() + geom_smooth(method = "lm")

great.tit <- filter(LPI.UK, Common.Name == "Great tit")
corn.bunting <- filter(LPI.UK, Common.Name == "Corn bunting")
reed.bunting <- filter(LPI.UK, Common.Name == "Reed bunting")
meadow.pipit <- filter(LPI.UK, Common.Name == "Meadow pipit")

sp_list <- list(house.sparrow,great.tit, corn.bunting, meadow.pipit)

for (i in 1:length(sp_list)) { 
  data <- as.data.frame(sp_list[i])
  sp.name <- unique(data$Common.Name)
  plot<- ggplot(data, aes(x = year, y = abundance)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = sp.name)
  ggsave(plot, file = here::here("Nov_2_Funloops", paste(sp.name, ".png", sep = "")), scale = 2)
  print(plot)
}



#instead of creating a list first, what if we just loop through all the available
#species in the UK data set
unique(LPI.UK$Common.Name)


for (i in 1:length(unique(LPI.UK$Common.Name))) { 
  data <- filter(LPI.UK, Common.Name == unique(LPI.UK$Common.Name)[i])
  sp.name <- unique(data$Common.Name)
  plot<- ggplot(data, aes(x = year, y = abundance)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = sp.name) +
    theme_classic()
  ggsave(plot, file = here::here("Nov_2_Funloops", paste(sp.name, ".png", sep = "")), scale = 2)
  print(plot)
}