## ------------------------------------------------------------------------
## Project: ConEmo34 - Experiment 1
##
## Script: Figures and Tables
## ------------------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)
library(broom.mixed)
library(ggeffects)
library(effects)
library(cowplot)
library(ggthemes)
library(flextable)
library(officer)
library(here)
library(emmeans)
library(ftExtra)

devtools::load_all()

# Functions ----------------------------------------------------------------

save_table <- function(tab, prop, name){
    save_as_docx(tab, path = name, pr_section = prop)
}

# Loading -----------------------------------------------------------------

prereg_list <- readRDS("objects/study1/prereg_list.rds")
expl_list <- readRDS("objects/study1/expl_list.rds")

dat <- readRDS("data/study1/data_no_outlier.rds")
dat_exp <- dat %>% filter(cond == "exp")
dat_val_arr <- dat %>% filter(cond == "val_arr")

# Creating custom lists

mod_list <- c(
    fit_exp = prereg_list$mods$fit_exp,
    fit_val = prereg_list$mods$fit_val,
    fit_arr = expl_list$mods$fit_arr,
    fit_exp_color = expl_list$mods$fit_exp_color
)

all_mods <- list(mod_list = mod_list)

all_table <- tibble(
    models = all_mods,
    names = names(all_mods)
)

# Tables -------------------------------------------------------------------

sect_properties <- prop_section(
    page_size = page_size(orient = "landscape",
                          width = 8.3, height = 11.7),
    type = "continuous",
    page_margins = page_mar()
)

# Models

all_table_mod <- all_table %>%
    mutate(tidy = map_depth(models, tidy_fit, .depth = 2),
           tidy = map_depth(tidy, prep_names_model, .depth = 2),
           tidy = map(tidy, bind_rows, .id = "mod"))

# Paper Table

all_table_mod %>% 
  select(tidy) %>% 
  unnest(tidy) %>% 
  #filter(mod %in% c("fit_exp", "fit_val", "fit_exp_color")) %>% 
  model_table() %>% 
  save_table(sect_properties, "tables/study1/table_mod_paper.docx")

# Anova

all_table_anova <- all_table %>%
    mutate(tidy = map_depth(models, tidy_anova, .depth = 2))

# Saving

saveRDS(all_table_anova, file = here("objects", study, "anova_paper.rds"))
saveRDS(all_table_mod, file = here("objects", study, "mod_paper.rds"))

# Figures -----------------------------------------------------------------

eff_exp <- get_effects(all_table$models$mod_list$fit_exp,
                           y = exprating, workerId, group, cue)

eff_val <- get_effects(all_table$models$mod_list$fit_val,
                           y = valrating, workerId, group, cue, valence)
eff_arr <- get_effects(all_table$models$mod_list$fit_arr,
                           y = arrating, workerId, group, cue, valence)

dat_plot <- bind_rows(eff_exp, eff_val, eff_arr) %>%
    clean_names_plot(study) %>%
    unite(cond, valence, cue, sep = "")

plot_effects <- box_plot(dat_plot, cond) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Saving

plot_name <- here::here(file.path("figures", study, "plot_paper.png"))

cowplot::save_plot(plot_name, plot_effects, base_height = 6)

saveRDS(plot_effects, file = here("objects", study, "plot_paper.rds"))
