
# Libraries

library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("multcompView")
library("tidyverse")
library("janitor")
library("piecewiseSEM")
library("effects")
library("nlme")
library("MASS")


#add distance_new from distance_calc object to end of SEM.dat
#added PEMV data from PEMV reads
bug.2 <- read.csv(file = "./Example R Scripts/PEMV/full page thunderdorm data no height.csv",
                  stringsAsFactors = F)

thunder_summary <- bug.2 %>%
  clean_names() %>%
  group_by(dorm, plant_number, column) %>%
  summarize(sum_adults = sum(total_adults))

thunder_source_fix <- thunder_summary %>%
  # Create separate cols from 'column'
  spread(key = column, value = sum_adults) %>%
  # Then set B = A when plant_number = S
  mutate(B = if_else(condition = plant_number == "S", true = A, false = B)) %>%
  # Then recombine into single col
  gather(key = column, value = sum_adults, A:B) %>%
  # Create a plant weight col for doing math
  mutate(
    plant_weight = case_when(
      plant_number != "S" ~ plant_number,
      plant_number == "S" ~ "0"),
    plant_weight = as.numeric(plant_weight)
  )

distance_calc <- thunder_source_fix %>%
  mutate(weighted_row_col = plant_weight * sum_adults) %>%
  group_by(dorm, column) %>%
  summarize(weighted_dorm_col_sum = sum(weighted_row_col),
            unweighted_dorm_col_sum = sum(sum_adults)) %>%
  mutate(distance_w_source = weighted_dorm_col_sum / unweighted_dorm_col_sum,
         distance_w_source = replace_na(data = distance_w_source, replace = 0))




# SEM dat is a lightly edited file from full page thunderdorm data no height (primary file)

SEM.dat <- read.csv("./Example R Scripts/PEMV/new.SEM.csv")
SEM.dat$log.all.aphids <- log(SEM.dat$Total.Aphids+1)
SEM.dat$log.nymphs <- log(SEM.dat$Total.Nymphs+1)
SEM.dat$adults <- SEM.dat$Total.Aphids - SEM.dat$Total.Nymphs
SEM.dat <- inner_join(x=SEM.dat, y=dplyr::select(distance_calc, dorm,column,distance_w_source), by=c("Dorm"="dorm","Column"="column"))

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

