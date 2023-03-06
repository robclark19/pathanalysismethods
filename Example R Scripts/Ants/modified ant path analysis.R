# Data and analysis modified from Clark et al. 2016 in Ecology
# https://doi.org/10.1002/ecy.1571

# Version notes:
# New analysis run using R version 4.2.2 with libraries accessed on March 6 2023

# Libraries ####
library(lme4)
library(piecewiseSEM)
library(tidyverse)
library(janitor)

# Data ####
ants_dat <- read.csv("./Example R Scripts/Ants/RC2012PlantDamage.csv") %>%
  clean_names() %>%
  subset(., sampling_period == "S2") %>% # Sampling period two only since leaf damage is across both sampling periods
  mutate(ant_treatment = ifelse(ant_treatment == "tanglefoot", 1, 0)) # Encode predator manipulation as 0,1


# A priori path model ####
# Leaf damage is impacted by tanglefoot, caterpillars, and sap-feeding Hemiptera
# Ant abundance is impacted by sap-feeders, tanglefoot
# Caterpillar abundance is impacted by ants
# Sampling location and host plant species are treated as random effects in all models

# Model list
mod_1 <- lmer(leaf_area_lost ~ cumulative_cat_count +
                (1|host_plant) + 
                (1|site:block), data=ants_dat)

mod_2 <- glmer.nb(cumulative_cat_count ~ total_ants +
                    (1|host_plant) + 
                    (1|site:block), data=ants_dat)

mod_3 <- glmer.nb(total_ants ~ s1_s2_sap_presence + ant_treatment +
                    (1|host_plant) + 
                    (1|site:block),
                    data=ants_dat)

# Create path model object
ants_sem <- psem(mod_1, mod_2, mod_3)

# Evaluate a prior model
summary(ants_sem, standardize="none", conserve=TRUE)


# Add paths ####
# Add significant paths reported from tests of directed separation
# Sap-feeder presence added to model 1
mod_1 <- lmer(leaf_area_lost ~ cumulative_cat_count + s1_s2_sap_presence +
                (1|host_plant) + 
                (1|site:block), data=ants_dat)

# Update path model object
ants_sem <- psem(mod_1, mod_2, mod_3)

# Evaluate updated model
summary(ants_sem, standardize="none", conserve=TRUE)



# Remove paths ####
mod_1 <- lmer(leaf_area_lost ~ s1_s2_sap_presence +
                (1|host_plant) + 
                (1|site:block), data=ants_dat)


# Update path model object
ants_sem <- psem(mod_1, mod_2, mod_3)

# Evaluate updated model
summary(ants_sem, standardize="none", conserve=TRUE)

# AIC criteria ####
# Note ant treatment effect on leaf area lost have P = 0.06 in test of directed separation
# Adding this term fo the path model is advisable, as it increases AIC by at least 2

# Accepted a posteriori model ####
# Added ant treatment to mod_1 to improve AIC > 2
mod_1 <- lmer(leaf_area_lost ~ s1_s2_sap_presence + ant_treatment +
                (1|host_plant) + 
                (1|site:block), data=ants_dat)

mod_2 <- glmer.nb(cumulative_cat_count ~ total_ants + 
                    (1|host_plant) + 
                    (1|site:block), data=ants_dat)

mod_3 <- glmer.nb(total_ants ~ s1_s2_sap_presence + ant_treatment +
                    (1|host_plant) + 
                    (1|site:block),
                  data=ants_dat)

ants_sem <- psem(mod_1, mod_2, mod_3)

summary(ants_sem, standardize="none", conserve=TRUE) # Standardizing path coefficients not applied in this example

