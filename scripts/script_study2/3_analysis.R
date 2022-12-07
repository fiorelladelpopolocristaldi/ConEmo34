## ------------------------------------------------------------------------
## Project: ConEmo34 - Experiment 1
##
## Script: Preregistered Analysis
## ------------------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(broom.mixed)
library(emmeans)
library(here)

devtools::load_all()

# Environment -------------------------------------------------------------

emm_options(lmerTest.limit = 10000) # for t-test in emmeans

# Data --------------------------------------------------------------------

dat <- read_rds("data/study2/data_no_outlier.rds")

# Modelling ---------------------------------------------------------------

# Data for Reaction Time

dat_learn_RT <- dat %>% 
    filter(cond == "learning",
           RT %in% 100:1500, # trimming RTs
           correct == "1") %>%  # keep only correct trials
    mutate(logIES = log(IES)) # log-transform IES

# Data for Accuracy

dat_learn_acc <- dat %>% 
    filter(cond == "learning",
           RT %in% (100:1500)) %>%  # trimming RTs
    mutate(logIES=log(IES)) # log-transform IES

# Data for Ratings

dat_exp <- dat %>% filter(cond == "exp")
dat_val_arr <- dat %>% filter(cond == "val_arr")

# Preregistred models

fit_IES <- lmer(logIES ~ group + (1|workerId),
                data = dat_learn_RT,
                na.action = na.fail)

fit_acc <- glmer(correct ~ group + (1|workerId),
                 data = dat_learn_acc,
                 na.action = na.fail,
                 family = binomial)

fit_exp <- lmer(exprating ~ group * s1_color + (s1_color|workerId), 
                data = dat_exp, 
                na.action = na.fail)

fit_val <- lmer(valrating ~ group * valence + (valence|workerId), 
                data = dat_val_arr,  
                na.action = na.fail)

fit_arr <- lmer(arrating ~ group * valence + (valence|workerId), 
                data = dat_val_arr,  
                na.action = na.fail)

mods <- list(
    fit_IES = fit_IES,
    fit_acc = fit_acc,
    fit_exp = fit_exp,
    fit_val = fit_val,
    fit_arr = fit_arr
)

# Post-Hoc Contrast -------------------------------------------------------

# effect size are not valid for binomial model, just using the same
# function for easiness

post_fit_IES <- get_contrast_and_effect_size(mods$fit_IES, term = "group")
post_fit_acc <- get_contrast_and_effect_size(mods$fit_acc, term = "group")
post_fit_exp <- get_contrast_and_effect_size(mods$fit_exp, term = "s1_color|group")
post_fit_val <- get_contrast_and_effect_size(mods$fit_val, term = "group|valence")
post_fit_arr <- get_contrast_and_effect_size(mods$fit_arr, term = "group|valence")

post_hoc_effsize <- list(
    post_fit_IES = post_fit_IES,
    post_fit_acc = post_fit_acc,
    post_fit_exp = post_fit_exp,
    post_fit_val = post_fit_val,
    post_fit_arr = post_fit_arr
)

# Anova -------------------------------------------------------------------

anova_models <- map(mods, function(x) broom.mixed::tidy(anova(x)))

#  Confidence Intervals ---------------------------------------------------

confint_mods <- map(mods, function(mod) confint(mod, level = 0.95, method = "Wald"))

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

saveRDS(prereg_list, file = here("objects", study, "prereg_list.rds"))