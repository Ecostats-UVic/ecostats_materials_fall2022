library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

LPI <- read.csv(here::here("Nov_23_ggplot/CC", "LPIdata_CC.csv"))

LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)
#parse_number gets rid of the X that is before each date 
LPI2$year <- parse_number(LPI2$year)

LPI2$abundance <- as.numeric(LPI2$abundance)
unique(LPI2$Common.Name)

vulture <- filter(LPI2, Common.Name == "Griffon vulture / Eurasian griffon")
head(vulture)

vulture <- na.omit(vulture)

#ready to plot! 

hist(vulture$abundance)

(vulture_hist <- ggplot(vulture, aes(x = abundance))+
    geom_histogram(binwidth = 250, color = "brown", fill= "orange") +
    geom_vline(aes(xintercept = mean(abundance)), color = "red", linetype = "dashed", size = 1) +
    theme_bw() +
    labs(y = "Count\n", x = "\nGriffon vulture abundance") +
    theme(axis.text = element_text(size = 12),                          # Changing font size of axis labels and title
          axis.title = element_text(size = 14, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))


#install.packages("colourpicker")

#whereever your curser is, when you click colour picker colours and press DONE it will add the hex codes 
c("#FF7F00", "#00FA9A", "#E066FF")

vultureITCR <- filter(vulture, Country.list %in% c("Croatia", "Italy"))

plot(vultureITCR$year, vultureITCR$abundance, col = c("#00FA9A", "#E066FF"))

(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    scale_fill_manual(values = c("#00FA9A", "#E066FF")) +                # Adding custom colours for solid geoms (ribbon)
    scale_colour_manual(values = c("#00FA9A", "#E066FF"),                # Adding custom colours for lines and points
                        labels = c("Croatia", "Italy")) +                # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = c(0.9, 0.9)))                                 # Setting legend position - 0 is left/bottom, 1 is top/right



(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance)) +
    geom_boxplot(aes(fill = Country.list)) +
    theme_classic() +
    scale_fill_manual(values = c("#00FA9A", "#E066FF")) +              
    scale_colour_manual(values = c("#00FA9A", "#E066FF")) +
    xlab("\nCountry") + ylab("Griffon Vulture Abundance\n"))

richness <- LPI2 %>% filter(Country.list %in% c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>%
  group_by(Country.list) %>% mutate(Richness = length(unique(Common.Name)))
  
(richness_barplot <- ggplot(richness, aes(Country.list, Richness)) +
    geom_bar(position = position_dodge(), stat = "identity", color = "black", fill = ("#9AC0CD")) +
    xlab("\nCountry") + ylab("Richness\n") +
   theme_classic())

#this plot is very ugly- add a facet wrap to be able to see all the countries 
(vulture_scatter_all <- ggplot(vulture, aes(year, abundance, color = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    theme_classic() +
    xlab("\nYear") + ylab("Griffon Vulture Abundance\n"))  

#use a facet wrap to create a plot for each country
(vulture_scatter_facet <- ggplot(vulture, aes(year, abundance, color = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    theme_classic() +
    xlab("\nYear") + ylab("Griffon Vulture Abundance\n") +
    facet_wrap(~Country.list, scales = "free_y"))  


grid.arrange(vulture_hist, vulture_scatter, vulture_boxplot, ncol = 1) 

(panel <- grid.arrange(
  vulture_hist + ggtitle("A") + ylab("Count") + xlab("Abundance") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), unit = "cm")),
  vulture_boxplot + ggtitle("B") + ylab("Abundance") + xlab("Country") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), unit = "cm")),
  vulture_scatter + ggtitle("C") + ylab("Abundance") + xlab("Year") + 
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), unit = "cm")) +
    theme(legend.text = element_text(size = 12, face = "italic"),     
          legend.title = element_blank(),                                   
          legend.position = c(0.85, 0.85)), # changing the legend position so that it fits within the panel
  
  ncol = 1)) # ncol determines how many columns you have)

#saving your plot as a file on your computer 
ggsave(panel, file = here::here("Nov_23_ggplot", "vultureplot.png"), width = 5, height = 12)
