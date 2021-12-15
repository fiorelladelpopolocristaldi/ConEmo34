## ------------------------------------------------------------------------
## Project: ConEmo34 - Experiment 1
##
## Script: Exploratory Analysis
## ------------------------------------------------------------------------

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(emmeans)
library(broom)
library(broom.mixed)

# Environment -------------------------------------------------------------

study = "study1"

# Functions ---------------------------------------------------------------

set_sum_contrast <- function(fac){
  contr.sum(length(levels(fac)))
}

# Data --------------------------------------------------------------------

dat <- read_rds(here("data", study, "data_no_outlier.rds"))

# Modelling ---------------------------------------------------------------

# Effect coding

contrasts(dat$group) <- set_sum_contrast(dat$group)
contrasts(dat$s1_color) <- set_sum_contrast(dat$s1_color)

# Data for models

dat_exp <- dat %>% filter(cond == "exp")
dat_val_arr <- dat %>% filter(cond == "val_arr")

# Cue -----------------------------------------------------------------------

fit_arr <- lmer(arrating ~ group * cue * valence + (valence|workerId), 
                data = dat_val_arr,  
                na.action = na.fail)

# S1 color -----------------------------------------------------------------------

fit_exp_color <- lmer(exprating ~ group * s1_color + (s1_color|workerId), 
                data = dat_exp,  
                na.action = na.fail)


# Model List --------------------------------------------------------------

mods <- list(
  fit_arr = fit_arr,
  fit_exp_color = fit_exp_color
)

post_hoc_effsize <- list(fit_exp_color = get_contrast_and_effect_size(fit_exp_color, term = "s1_color|group"))

# Anova --------------------------------------------------------------

anova_models <- map(mods, tidy_anova)


# R2 ----------------------------------------------------------------------

r2_models <- map(mods, MuMIn::r.squaredGLMM)

#  Confidence Intervals ---------------------------------------------------

confint_mods <- map(mods, function(mod) confint(mod, level = 0.95, method = "Wald"))

# Saving ------------------------------------------------------------------

expl_list <- list(
  mods = mods,
  anova_models = anova_models,
  post_hoc_effsize = post_hoc_effsize,
  confint_mods = confint_mods,
  r2_models = r2_models
)

# Saving ------------------------------------------------------------------

saveRDS(expl_list, file = here("objects", study, "expl_list.rds"))