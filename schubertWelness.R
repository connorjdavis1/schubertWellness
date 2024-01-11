#---
setwd("/Users/connordavis/Documents/GitHub/schubertWellness/")
schubert_data <- read.csv("schubertData.csv", header=T)
library(tidyverse)
library(GGally)
library(effects) #for plotting parameter effects
#---

#This data set is musical features from the Schubert melodies throughout
#his lifetime along with information pertaining to his wellness and unwellness.
#The aim of this project is to see if there is a difference in his musical output
#in the different unwell periods and what musical features are present if a
#difference exists

#ClEANING#

str(schubert_data)

schubertDataClean <- schubert_data %>% 
  filter(!str_detect(wellness, "uncertain")) %>% #get rid of pieces we aren't certain about his health
  select(-NOTES..to.be.deleted., #remove unecessary columns
         -completion_date,
         -deutsche_num,
         -filename) %>% 
  filter(!str_detect(start_date, "UNKNOWN")) %>% #remove unknowns from the dates
  drop_na() 

schubertDataClean$wellness <- if_else(schubertDataClean$wellness == "well", 1, 0)
schubertDataClean$start_date <- as.integer(schubertDataClean$start_date)


###Checking correlations and normality

schubertDataClean %>% #looks like normal disctributions and degree_entropy and intervallic_entropy are positively correlated.
  select(degree_entropy, intervallic_entropy, npvi, wellness) %>% 
  ggpairs() 


###GLM Test for Significant variables
glmTest <- glm(formula = wellness ~ degree_entropy + #npvi and degree_entropy are significant
                 intervallic_entropy + 
                 npvi,
               data = schubertDataClean, 
               family = binomial(link = "logit"))

summary(glmTest) #npvi and degree_entropy are significant at predicting wellness

plot(allEffects(glmTest)) ##to visualize the effect we can see the positive and negative influences of our two significant variables

###Testing Fit and Evaluating Parameters

glmSchubert <- glm(formula = wellness ~ degree_entropy + #npvi and degree_entropy are significant
                               intervallic_entropy,
                             data = schubertDataClean, 
                             family = binomial(link = "logit"))

anova(glmTest, glmSchubert, test ="Chisq") #evaluating test against model without npvi. Model without npvi is better, so we will proceed with that. Note chi-squared.

glmTest$aic
glmSchubert$aic #just to further check, and glmSchubert is definitely better.

ggplot(schubertDataClean, aes(x = degree_entropy+npvi, y = wellness)) + 
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family=binomial), se = TRUE)

##plotting our two variables for an elbow, which we can see beginning at the unwell period starting in 1822 when he developed syphilis.

ggplot(schubertDataClean, aes(start_date, degree_entropy, wellness)) +
  geom_rect(aes(NULL, NULL,
                xmin=start_date - .5 , xmax=start_date +.5,
                ymin=min(degree_entropy), ymax=max(degree_entropy),
                fill=wellness
                )) +
    geom_point() + 
    geom_smooth()

ggplot(schubertDataClean, aes(start_date, npvi, wellness)) +
  geom_rect(aes(NULL, NULL,
                xmin=start_date - .5 ,xmax=start_date +.5,
                ymin=min(npvi), ymax=max(npvi),
                fill=wellness)) +
  geom_point() + 
  geom_smooth()

#it seems there is a difference in at least melodic output when a composer is unwell.

