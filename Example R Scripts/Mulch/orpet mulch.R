# Data and analysis modified from Orpet et al. 2019
# https://doi.org/10.1371/journal.pone.0216424

# Version notes:
# New analysis run using R version 4.0.0 with libraries accessed on March 17 2023

# Libraries ####
library(lme4)
library(piecewiseSEM)
library(tidyverse)
library(janitor)

# Data ####
# load data
data_aphids <- read.csv("./Example R Scripts/Mulch/pone.0216424.s003.csv")
# encode dummy variables
data_aphids$Sandy <- ifelse(data_aphids$Medium == "Sandy",1,0)
data_aphids$Chips <- ifelse(data_aphids$Mulch == "Chips",1,0)
data_aphids$Slurry <- ifelse(data_aphids$Mulch == "Slurry",1,0)

# A priori path model ####
# The goal of the study was to test whether colonies and damage (galls) of woolly apple aphid to apple trees could be reduced based on soil and mulch treatments that restrict above- to below-ground movement of this species The experiment tested effects of two potting media (sandy and plain) and three mulches (none, chips, and slurry) on woolly apple aphid colony and gall formation on roots of potted apple trees. Path analysis was used because of treatments could also have affected covariates (root dry weight, aerial woolly apple aphid colonies) in the analysis, violating an assumption of conventional ANCOVA. As per Orpet et al. 2019, we hypothesized:
# (H1) sandy media reduces  root colonies and galls
# (H2) lower root dry weight reduces the number of root woolly apple aphid colonies and galls because of less available space
# (H3) a greater number of aerial colonies results in more root colonies and galls
# (H4) chips and slurry would reduce the number of root galls, but not the number of root colonies
# (H5) root dry weight would be affected by potting media.

#CONSTRUCT MODELS
#(1) a linear model of root colonies distributed by potting media treatment, root dry weight, and aerial colonies
model_rootcolonies <- lm(Root.aphid.colonies ~ Medium + Root.dry.weight..g. + Aerial.aphid.colonies, data = data_aphids)
#(2) a generalized linear model with a Poisson distribution and log link function of root galls distributed by potting media treatment, mulch treatment, root dry weight, and aerial colonies
model_rootgalls <- glm(Root.galls ~ Medium + Mulch + Root.dry.weight..g. + Aerial.aphid.colonies, family = poisson(link = "log"), data = data_aphids)
#(3) a linear model of root dry weight distributed by potting media treatment.
model_rootweight <- lm(Root.dry.weight..g. ~ Medium, data = data_aphids)

# Create path model object
sem_aphids <- psem(model_rootcolonies, model_rootgalls, model_rootweight)

# Evaluate a priori model
summary(sem_aphids, standardize="none", conserve=TRUE)


# Model list
# model 1 addresses effects on woolly apple aphid root colonies (H1, H2, H3)
m1 <- lm(Root.aphid.colonies ~ Sandy + Chips + Slurry + Root.dry.weight..g. + Aerial.aphid.colonies, data = data_aphids) 
# model 2 addresses effects on woolly apple aphid root galls (H1, H2, H3, H4)
m2 <- glm(Root.galls ~ Sandy + Chips + Slurry + Root.dry.weight..g. + Aerial.aphid.colonies, family = poisson(link = "log"), data = data_aphids)
#model 3 addresses effects on apple tree root dry weight (H5)
m3 <- lm(Root.dry.weight..g. ~ Sandy, data = data_aphids)

# Create path model object
sem_aphids <- psem(m1, m2, m3)

# Evaluate a priori model
summary(sem_aphids, standardize="none", conserve=TRUE)
