## ------------------------------------------------------------------------
## Project: Study3_implicit
##
## Script: Figures and Tables
## ------------------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)
library(broom.mixed)
library(cowplot)
library(ggthemes)
library(flextable)
library(officer)
library(here)

devtools::load_all()

# Functions ----------------------------------------------------------------

save_table <- function(tab, prop, name){
    save_as_docx(tab, path = name, pr_section = prop)
}

# Loading -----------------------------------------------------------------

prereg_list <- readRDS("objects/study2/prereg_list.rds")
dat <- readRDS("data/study2/data_no_outlier.rds")

dat_learn_RT <- dat %>% 
    filter(cond == "learning",
           RT %in% (100:1500), # trimming RTs
           correct=="1") %>%  # keep only correct trials
    mutate(logIES=log(IES)) # log-transform IES

dat_exp <- dat %>% dplyr::filter(cond == "exp")
dat_val_arr <- dat %>% dplyr::filter(cond == "val_arr")

# Creating custom lists

mod_list <- c(
    fit_IES = prereg_list$mods$fit_IES,
    fit_acc = prereg_list$mods$fit_acc,
    fit_exp = prereg_list$mods$fit_exp,
    fit_val = prereg_list$mods$fit_val,
    fit_arr = prereg_list$mods$fit_arr
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
           tidy = map(tidy, bind_rows, .id = "mod"),
           table = map(tidy, model_table),
           save_names = str_remove(names, "mod_list_"),
           save_names = file.path("tables/study2/mod_table_paper.docx"))

# Anova

all_table_anova <- all_table %>% 
    mutate(tidy = map_depth(models, tidy_anova, .depth = 2))
          
# Saving

save_table(all_table_mod$table$mod_list, sect_properties, all_table_mod$save_names)

saveRDS(all_table_anova, file = "objects/study2/anova_paper.rds")
saveRDS(all_table_mod, file = "objects/study2/mod_paper.rds")

# Figures -----------------------------------------------------------------

# Preregistration

eff_exp <- get_effects(all_table$models$mod_list$fit_exp,
                       y = exprating, workerId, group, s1_color)
eff_val <- get_effects(all_table$models$mod_list$fit_val,
                       y = valrating, workerId, group, valence)
eff_arr <- get_effects(all_table$models$mod_list$fit_arr,
                       y = arrating, workerId, group, valence)

dat_plot <- bind_rows(eff_exp, eff_val, eff_arr) %>%
    clean_names_plot(., study = "study2") %>%
    unite(cond, valence, s1_color, sep = "") %>% 
    mutate(cond = str_remove_all(cond, "NA"))

plot_effects <- box_plot(dat_plot, cond)

# Saving

plot_name <- file.path("figures/study2/plot_paper.png")

cowplot::save_plot(plot_name, plot_effects, base_height = 6)

saveRDS(plot_effects, file = "objects/study2/plot_paper.rds")