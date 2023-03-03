# R code for Clark et al. 2019 "Tri-trophic interactions mediate the spread of a vector-borne plant pathogen" in Ecology.

#Packages ####

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


#Fig 2 ####
#Data organization for path analysis

#added PEMV data from PEMV reads
bug.2 <- read.csv(file = "full page thunderdorm data no height.csv",
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

#add distance_new from distance_calc object to end of SEM.dat
# SEM dat is a lightly edited file from full page thunderdorm data no height (primary file)

SEM.dat <- read.csv("new.SEM.csv")
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







# Fig S3 #### 
# Main treatment effects
# below are figures which show basic tests of direct treatment effects

#1 predation on abundance
#2 vetch on abundnace
#3 weevil on abundance

main.1.glm <- lmer(log.nymphs ~ Ladybird + Source.Species + Weevil + (1|Dorm), data=SEM.dat)
Anova(main.1.glm)

main.1.cld <- cld(emmeans(main.1.glm, ~Ladybird, adjust="none"))
main.1.cld

ggplot(main.1.cld, aes(x=Ladybird, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Predators") +
  ylab("Log Nymph Abundance")

main.2.cld <- cld(emmeans(main.1.glm, ~Source.Species, adjust="none"))
main.2.cld

ggplot(main.2.cld, aes(x=Source.Species, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Starting Host") +
  ylab("Log Nymph Abundance")

main.3.cld <- cld(emmeans(main.1.glm, ~Weevil, adjust="none"))
main.3.cld

ggplot(main.3.cld, aes(x=Weevil, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Pea Leaf Weevils") +
  ylab("Log Nymph Abundance")


#4 predation on movement
#5 vetch on movement
#6 weevil on movement

main.2.glm <- glmer(Occupancy ~ Ladybird + Source.Species + Weevil + (1|Dorm), family=poisson, data = SEM.dat)
Anova(main.2.glm)

main.4.cld <- cld(emmeans(main.2.glm, ~Ladybird, adjust="none"))
main.4.cld

ggplot(main.4.cld, aes(Ladybird, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Predators") +
  ylab("Occupancy")

main.5.cld <- cld(emmeans(main.2.glm, ~Source.Species, adjust="none"))
main.5.cld

ggplot(main.5.cld, aes(x=Source.Species, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Starting Host") +
  ylab("Occupancy")

main.6.cld <- cld(emmeans(main.2.glm, ~Weevil, adjust="none"))
main.6.cld

ggplot(main.6.cld, aes(x=Weevil, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Pea Leaf Weevils") +
  ylab("Occupancy")

#4a predation on avg. distance
#5a vetch on avg. distance
#6a weevil on avg. distance
str(SEM.dat)

main.2a.glm <- lmer(distance_w_source ~ Ladybird + Source.Species + Weevil + (1|Dorm), data = SEM.dat)
Anova(main.2a.glm)

main.4a.cld <- cld(emmeans(main.2a.glm, ~Ladybird, adjust="none"))
main.4a.cld

ggplot(main.4a.cld, aes(Ladybird, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Predators") +
  ylab("Average Distance")

main.5a.cld <- cld(emmeans(main.2a.glm, ~Source.Species, adjust="none"))
main.5a.cld

ggplot(main.5a.cld, aes(x=Source.Species, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Starting Host") +
  ylab("Average Distance")

main.6a.cld <- cld(emmeans(main.2a.glm, ~Weevil, adjust="none"))
main.6a.cld

ggplot(main.6a.cld, aes(x=Weevil, y=emmean)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Pea Leaf Weevils") +
  ylab("Average Distance")

#7 predation on virus
#8 vetch on virus
#9 weevil on virus

main.3.glm <- glmer(PEMV.num ~ Ladybird + Source.Species + Weevil + (1|Dorm), family=binomial, data=SEM.dat)
Anova(main.3.glm)


main.7.cld <- cld(emmeans(main.3.glm, ~Ladybird, adjust="none", type="response"))
main.7.cld

ggplot(main.7.cld, aes(Ladybird, y=prob)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=prob-(SE), ymax=prob+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Predators") +
  ylab("PEMV infection")

main.8.cld <- cld(emmeans(main.3.glm, ~Source.Species, adjust="none", type="response"))
main.8.cld

ggplot(main.8.cld, aes(x=Source.Species, y=prob)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=prob-(SE), ymax=prob+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Starting Host") +
  ylab("PEMV infection")

main.9.cld <- cld(emmeans(main.3.glm, ~Weevil, adjust="none", type="response"))
main.9.cld

ggplot(main.9.cld, aes(x=Weevil, y=prob)) +
  geom_point(size=4.5) +
  theme_bw(base_size = 20) +
  geom_errorbar(aes(ymin=prob-(SE), ymax=prob+(SE)), position=position_dodge(0.5), width=0.2) +
  xlab("Pea Leaf Weevils") +
  ylab("PEMV infection")







#Fig 3 PLW*log.nymphs#####
model.weevil.plot <- glmer(PEMV ~ log.nymphs*Weevil + (1|Dorm), family=binomial(link=probit), data=SEM.dat, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(model.weevil.plot)
Anova(model.weevil.plot)

# below attempt to make a nice ggplot that illustrates the relationship between weevils and aphid population density

#The Interaction
ac.interaction.raw <- effect('log.nymphs*Weevil', model.weevil.plot, se=TRUE, xlevels=100)
#Data Frame
ac.interaction <-as.data.frame(ac.interaction.raw)



#Create plot
Plot.ac <-ggplot(data=ac.interaction, aes(x=log.nymphs, y=fit, group=Weevil))+
  geom_line(size=2, aes(color=Weevil))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=Weevil),alpha=.2)+
  #ggtitle("Interaction Plot for Weevil Treatment by Nymph Production")+
  theme_bw() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(text = element_text(size=16),
        legend.text = element_text(size=16),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top") +
  xlab("Number of nymphs (Log abundance on day 7)") +
  ylab("Probability of virus presence") +
  geom_point(data=SEM.dat, aes(x=log.nymphs, y=PEMV.num, colour=Weevil), position=position_jitter(w=0, h=0.05))
#geom_smooth(method='lm',formula=y~x, aes(color=Weevil),level=0.95)
Plot.ac

plot(ac.interaction.raw, multiline = TRUE) 









# Table S2 #####
# Model specification and significance tests for GLMM changes to aphid abundance over time






#Fig S7 #####






#Table S3 ####
# Model specification for distance of aphids on Day 7...
#... for source plant (vetch/pea) and predators (ladybird/control).






# Fig S5 #####








# Fig S6 #####

