install.packages("tidyverse")

library(ggplot2)
library(dplyr)

dat <- read.csv("Team-YOLO/Data/Obese_Lean_Microbiome_Data_2.csv")

dat$obesitycat[which(dat$obesitycat=='Obese')] = 'Overweight'
dat$obesitycat[which(dat$obesitycat=='NA')] = 'Mother'

# zygosity
g <- ggplot(dat,aes(zygosity)) + scale_fill_brewer(palette = 'Spectral')
g + geom_bar(aes(fill=factor(dat$obesitycat)),
             #bins=20,
             col="black",
             size=.1) +
    labs(title = 'Obesity by zygosity', fill = 'Groups') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

chisq.test(dat$obesitycat,dat$zygosity)

# Ancestry
g <- ggplot(dat,aes(ancestry)) + scale_fill_brewer(palette = 'Spectral')
g + geom_bar(aes(fill=factor(dat$obesitycat)),
             #bins=20,
             col="black",
             size=.1) +
    labs(title = 'Obesity by ancestry \n p < 0.001', fill = 'Groups') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

chisq.test(dat$ancestry,dat$obesitycat)

# twin_mother
g <- ggplot(dat,aes(twin_mother)) + scale_fill_brewer(palette = 'Spectral')
g + geom_bar(aes(fill=factor(dat$obesitycat)),
             #bins=20,
             col="black",
             size=.1) +
    labs(title = 'Obesity by  \n p < 0.05', fill = 'Groups') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))

chisq.test(dat$twin_mother,dat$obesitycat)


