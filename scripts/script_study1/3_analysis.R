## ------------------------------------------------------------------------
## Project: Study4_ambiguous
##
## Script: Preregisterd Analysis
## ------------------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(here)
library(lmerTest)
library(effects)
library(influence.ME)
library(broom)
library(broom.mixed)
library(emmeans)

# Environment -------------------------------------------------------------

emm_options(lmerTest.limit = 10000) # for t-test in emmeans

# Data --------------------------------------------------------------------

dat <- read_rds("data/study1/data_no_outlier.rds")

# Modelling ---------------------------------------------------------------

# Effect coding

contrasts(dat$group) <- c(0.5, -0.5)
contrasts(dat$valence) <- c(0.5, -0.5)
contrasts(dat$cue) <- c(0.5, -0.5)

# Data for models

dat_exp <- dat %>% filter(cond == "exp")
dat_val_arr <- dat %>% filter(cond == "val_arr")

# Preregistred models

fit_exp <- lmer(exprating ~ group * cue + (cue|workerId), 
                data = dat_exp, 
                na.action = na.fail)

fit_val <- lmer(valrating ~ group * cue * valence + (valence|workerId), 
                data = dat_val_arr,  
                na.action = na.fail)


MuMIn::r.squaredGLMM(fit_exp, fit_exp0)
MuMIn::r.squaredGLMM(fit_exp)

mods <- list(
    fit_exp = fit_exp,
    fit_val = fit_val
)

# Post-Hoc Contrast -------------------------------------------------------

post_fit_exp <- get_contrast_and_effect_size(fit_exp, term = "cue|group")
post_fit_val <- get_contrast_and_effect_size(fit_val, term = "cue|valence|group")

post_hoc_effsize <- list(
    post_fit_exp = post_fit_exp,
    post_fit_val = post_fit_val
)

#  Confidence Intervals ---------------------------------------------------

confint_mods <- map(mods, function(mod) confint(mod, level = 0.95, method = "Wald"))

# Anova -------------------------------------------------------------------

anova_models <- map(mods, tidy_anova)

# R2 ----------------------------------------------------------------------

r2_models <- map(mods, MuMIn::r.squaredGLMM)

# Saving ------------------------------------------------------------------

prereg_list <- list(
    mods = mods,
    anova_models = anova_models,
    post_hoc_effsize = post_hoc_effsize,
    confint_mods = confint_mods,
    r2_models = r2_models
)

saveRDS(prereg_list, file = "objects/study1/prereg_list.rds")