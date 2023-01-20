
#LOAD PACKAGES
library(piecewiseSEM)
library("multcompView")

#LOAD DATA
Data_mulch <- read.csv("pone.0216424.s003.csv", header = TRUE)

#CONSTRUCT MODELS
#(1) a linear model of root colonies distributed by potting media treatment, root dry weight, and aerial colonies
model_rootcolonies <- lm(Root.aphid.colonies ~ Medium + Root.dry.weight..g. + Aerial.aphid.colonies, data = Data_mulch)
#(2) a generalized linear model with a Poisson distribution and log link function of root galls distributed by potting media treatment, mulch treatment, root dry weight, and aerial colonies
model_rootgalls <- glm(Root.galls ~ Medium + Mulch + Root.dry.weight..g. + Aerial.aphid.colonies, family = poisson(link = "log"), data = Data_mulch)
#(3) a linear model of root dry weight distributed by potting media treatment.
model_rootweight <- lm(Root.dry.weight..g. ~ Medium, data = Data_mulch)

#STRUCTURAL EQUATION MODEL
SEM_mulch <- psem(model_rootcolonies,model_rootgalls,model_rootweight)
summary(SEM_mulch)

summary(model_rootcolonies)

summary(model_rootweight)
