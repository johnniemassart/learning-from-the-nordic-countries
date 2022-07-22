#New Package to Install
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggrepel")

#Import Libraries
library("readxl")
library(tidyverse)
library(ggplot2)
library(ggrepel)

#Create World Happiness Variable from Excel data
World_Happiness <- read_excel("/Users/johnniemassart/Documents/World Happiness Report 2022.xlsx")

#Attach columns to variable
attach(World_Happiness)

#Multiple Linear Regression
#Model
summary(lm(`Happiness score` ~ `Explained by: GDP per capita` + 
             `Explained by: Social support` + `Explained by: Healthy life expectancy` +
             `Explained by: Freedom to make life choices` + `Explained by: Generosity` +
             `Explained by: Perceptions of corruption`))

#Nordic Country Filter
finland <- World_Happiness %>%
  filter(Country=="Finland")
sweden <- World_Happiness %>%
  filter(Country=="Sweden")
denmark <- World_Happiness %>%
  filter(Country=="Denmark")
norway <- World_Happiness %>%
  filter(Country=="Norway")
iceland <- World_Happiness %>%
  filter(Country=="Iceland")

#USA Filter
usa <- World_Happiness %>% 
  filter(Country=="United States")

#Plot 1 - Perception of Corruption
ggplot(World_Happiness, aes(`Explained by: Perceptions of corruption`,
                            `Happiness score`)) +
  geom_point(color="grey") +
  geom_point(data=finland, color="red") +
  geom_label_repel(data=subset(World_Happiness, 
                               Country=="Finland"),
                   aes(label=Country,
                       nudge_x = 0.67, nudge_y = 6.5)) +
  geom_point(data=sweden, color="red") +
  geom_point(data=denmark, color="red") +
  geom_point(data=norway, color="red") +
  geom_point(data=iceland, color="red") +
  geom_point(data=usa, color = "blue") +
  geom_label_repel(data=subset(World_Happiness, 
                               Country=="United States"),
                   aes(label=Country,
                       nudge_x = 0.177, nudge_y = 6.2)) +
  theme_classic() +
  ggtitle("Happiness Score vs. Perception of Corruption") +
  xlab("Perception of Corruption") + 
  ylab("Happiness Score") +
  geom_smooth(method = "lm", se = FALSE, col = "black")


#Plot 2 - GDP Per Capita
ggplot(World_Happiness, aes(`Explained by: GDP per capita`,
                            `Happiness score`)) +
  geom_point(color="grey") +
  geom_point(data=finland, color="red") +
  geom_label_repel(data=subset(World_Happiness, 
                               Country=="Finland"),
                   aes(label=Country,
                       nudge_x = 1.5, nudge_y = 7.821)) +
  geom_point(data=sweden, color="red") +
  geom_point(data=denmark, color="red") +
  geom_point(data=norway, color="red") +
  geom_point(data=iceland, color="red") +
  geom_point(data=usa, color = "blue") +
  geom_label_repel(data=subset(World_Happiness, 
                               Country=="United States"),
                   aes(label=Country,
                       nudge_x = 1.982, nudge_y = 6.2)) +
  theme_classic() +
  ggtitle("Happiness Score vs. GDP Per Capita") +
  xlab("GDP Per Capita") + 
  ylab("Happiness Score") +
  geom_smooth(method = "lm", se = FALSE, col = "black")


#Plot 3 - Generosity
ggplot(World_Happiness, aes(`Explained by: Generosity`,
                            `Happiness score`)) +
  geom_point(color="grey") +
  geom_point(data=finland, color="red") +
  geom_label_repel(data=subset(World_Happiness, 
                               Country=="Finland"),
                   aes(label=Country,
                       nudge_x = 0.05, nudge_y = 7.821)) +
  geom_point(data=sweden, color="red") +
  geom_point(data=denmark, color="red") +
  geom_point(data=norway, color="red") +
  geom_point(data=iceland, color="red") +
  geom_point(data=usa, color = "blue") +
  geom_label_repel(data=subset(World_Happiness, 
                               Country=="United States"),
                   aes(label=Country,
                       nudge_x = 0.22, nudge_y = 6.3)) +
  theme_classic() +
  ggtitle("Happiness Score vs. Generosity") +
  xlab("Generosity") + 
  ylab("Happiness Score") +
  geom_smooth(method = "lm", se = FALSE, col = "black")





