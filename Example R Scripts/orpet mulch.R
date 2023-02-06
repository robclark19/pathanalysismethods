
#LOAD PACKAGES
library("piecewiseSEM")
library("multcompView")
library("tidyverse")
library("janitor")
library("MASS")

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



# New models created by R Clark in 2023

mulch_dat <- read.csv("./Example R Scripts/pone.0216424.s003.csv", header = TRUE) %>%
  clean_names()

# Make predictor variables 0/1 for models

# One-hot-encode the column "medium"
mulch_ <- as.data.frame(model.matrix(~ medium - 1, data = mulch_dat))

# Make column names to select later on
colnames(df_encoded) <- c("medium_Plain", "medium_Sandy")

# Select and rename the column
df_encoded_short <- df_encoded %>% select(medium_Sandy) %>% rename(medium_sandy = medium_Sandy)

# Merge just the medium
mulch_merged <- cbind(mulch_dat, df_encoded_short)

# One-hot-encode the column "mulch" and repeat
df_encoded <- as.data.frame(model.matrix(~ mulch - 1, data = mulch_dat))
colnames(df_encoded) <- c("mulch_chips", "mulch_control", "mulch_slurry")
mulch_final <- cbind(mulch_merged, df_encoded)



# model 1
mod_1 <- lm(root_aphid_colonies ~ medium_sandy + root_dry_weight_g + aerial_aphid_colonies, data= mulch_final)

# model 2
# Poisson not used due to error in fit, using nb
# mulch slurry has singularity so dropped from SEM, it had no effects on the other variables

mod_2 <- glm.nb(root_galls ~ medium_sandy + mulch_slurry + mulch_chips + root_dry_weight_g + aerial_aphid_colonies, data=mulch_final)

# non-normal distribution suggests nb fit using MASS package
hist(mulch_final$root_galls)

# model 3
mod_3 <- lm(root_dry_weight_g ~ medium_sandy, data= mulch_final)


# path analysis

mulch_sem <- psem(mod_1, mod_2, mod_3)
summary(mulch_sem, standardize="none", conserve=TRUE)
