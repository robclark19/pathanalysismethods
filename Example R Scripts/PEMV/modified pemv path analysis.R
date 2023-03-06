# Data and analysis modified from Clark et al. 2019 in Ecology.
# 


# Libraries ####
library(lme4)
library(tidyverse)
library(piecewiseSEM)
library(janitor)

# Data ####

# Full PEMV data from greenhouse experiment
pemv_dat <- read.csv(file = "full page thunderdorm data no height.csv",
                     stringsAsFactors = F)

# Create total aphid count values
pemv_summary <- pemv_dat %>%
  clean_names() %>%
  group_by(dorm, plant_number, column) %>%
  summarize(sum_adults = sum(total_adults))

# Calculate distance by plant rows
pemv_source <- pemv_summary %>%
  # Create separate cols from 'column'
  spread(key = column, value = sum_adults) %>%
  # Then set B = A when plant_number = S
  mutate(B = if_else(condition = plant_number == "S", true = A, false = B)) %>%
  # Then recombine into single col
  gather(key = column, value = sum_adults, A:B) %>%
  # Create a plant distance value
  mutate(
    plant_weight = case_when(
      plant_number != "S" ~ plant_number,
      plant_number == "S" ~ "0"),
    plant_weight = as.numeric(plant_weight)
  )

# Calculate a weighted distance by plant rows
distance_calc <- pemv_source %>%
  mutate(weighted_row_col = plant_weight * sum_adults) %>%
  group_by(dorm, column) %>%
  summarize(weighted_dorm_col_sum = sum(weighted_row_col),
            unweighted_dorm_col_sum = sum(sum_adults)) %>%
  mutate(distance_w_source = weighted_dorm_col_sum / unweighted_dorm_col_sum,
         distance_w_source = replace_na(data = distance_w_source, replace = 0))

# Import sorted data summarized by insect dorm
sem_dat <- read.csv("new.SEM.csv")

# Log transform total aphid counts
sem_dat$log.all.aphids <- log(sem_dat$Total.Aphids+1)

# Log transform nymph aphid counts
sem_dat$log.nymphs <- log(sem_dat$Total.Nymphs+1)

# Find total adult counts
sem_dat$adults <- sem_dat$Total.Aphids - sem_dat$Total.Nymphs

# Merge distance calcs with sem_dat
sem_dat <- inner_join(x=sem_dat, y=dplyr::select(distance_calc, dorm,column,distance_w_source), by=c("Dorm"="dorm","Column"="column"))



# A priori model #####








# A posterori model ####

#SEMs ###########
# rerun with distance average that now includes source plant totals
# distance of adults
model.a5 <- lme(distance_w_source ~ vetch.num + ladybird.num, random=~1|Dorm, data=SEM.dat)
Anova(model.a5)

#nymph abundance
model.b <- lme(log.nymphs ~ ladybird.num + vetch.num, random=~1|Dorm, data=SEM.dat)
summary(model.b)
Anova(model.b)

#pemv infection
model.c <- glmer(PEMV.num ~ log.nymphs*weevil.num + (1|Dorm), family=binomial, data=SEM.dat, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(model.c)

# sem fit
sem.model <- psem(model.a5,model.b,model.c, data=SEM.dat)
summary(sem.model, standardize="none", conserve=TRUE)
#add correlated error for movement by distance

#Table S1 #####
summary(update(sem.model, log.nymphs %~~% distance_w_source), standardize="scale")







