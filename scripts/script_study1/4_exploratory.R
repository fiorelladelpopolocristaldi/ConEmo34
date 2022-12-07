## ------------------------------------------------------------------------
## Project: Study4_ambiguous
##
## Script: Exploratory Analysis
## ------------------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(emmeans)
library(broom)
library(broom.mixed)
library(faux)

devtools::load_all()

# Data --------------------------------------------------------------------

dat <- read_rds("data/study1/data_no_outlier.rds")

# Modelling ---------------------------------------------------------------

# Data for models

dat_exp <- dat %>% filter(cond == "exp")
dat_val_arr <- dat %>% filter(cond == "val_arr")

# anova-like coding for better parameters 
# with more than 2 levels 
# see https://debruine.github.io/faux/articles/contrasts.html#anova

dat_exp <- dat_exp %>% 
  add_contrast(., "group", "anova") %>% 
  add_contrast(., "s1_color", "anova")

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

saveRDS(expl_list, file = "objects/study1/expl_list.rds")