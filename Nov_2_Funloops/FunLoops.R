library(here)
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