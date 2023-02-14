 
library(lme4)
library(piecewiseSEM)
library(car)
library(tidyverse)
library(janitor)

ants_dat <- read.csv("./Example R Scripts/Ants/RC2012PlantDamage.csv") %>%
  clean_names() 

ants_dat <- subset(ants_dat, sampling_period == "S2") %>% #leaf damage is across both sampling periods
  mutate(ant_treatment = ifelse(ant_treatment == "tanglefoot", 1, 0)) # encode predator manipulation as 0,1


# description of paths
# leaf damage is impacted by tanglefoot, caterpillars, sap-feeding Hemiptera
# carps are impacted by saps, tanglefoot
# cats are impacted by carps, tanglefoot

# a priori model

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

ants_sem <- psem(mod_1, mod_2, mod_3)

summary(ants_sem, standardize="none", conserve=TRUE)

# a posteriori model

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

mod_4 <- lmer(s1_s2_sap_presence)

ants_sem <- psem(mod_1, mod_2, mod_3)

summary(ants_sem, standardize="none", conserve=TRUE)

