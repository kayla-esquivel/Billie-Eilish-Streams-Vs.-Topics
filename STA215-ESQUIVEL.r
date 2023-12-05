## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################

# EXAMINE QUANT_VAR1
table(data$seconds_of_lyrics)
mean(data$seconds_of_lyrics)
sd(data$seconds_of_lyrics)
summary(data$seconds_of_lyrics)

# EXAMINE QUANT_VAR2
table(data$number_of_streams)
mean(data$number_of_streams)
sd(data$number_of_streams)
summary(data$number_of_streams)

# EXAMINE QUAL_VAR1
table(data$controversial_topics)

# EXAMINE QUAL_VAR2
table(data$personal_rating)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################

table(data$controversial_topics, data$personal_rating)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################

chisq.test(data$controversial_topics, data$personal_rating)

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################

anova_adapted <- aov(number_of_streams ~ controversial_topics, data = data)

summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################

cor(data$seconds_of_lyrics, data$number_of_streams)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################

linear_relationship <- lm(data$number_of_streams ~ data$seconds_of_lyrics, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################

linear_plot <- plot(data$seconds_of_lyrics, data$number_of_streams)
print(linear_plot)

abline(linear_relationship, col= 'magenta')

x_mean=mean(data$seconds_of_lyrics)
abline(v=x_mean, col="blue")

y_mean=mean(data$number_of_streams)
abline(h=y_mean, col="turquoise")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################

plot(data$seconds_of_lyrics, residuals(linear_relationship))

abline(h=y_mean, col="orange")

