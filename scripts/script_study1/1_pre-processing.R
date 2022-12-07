## ------------------------------------------------------------------------
## Project: Study4_ambiguous
##
## Script: Pre-processing
## ------------------------------------------------------------------------

rm(list = ls())

# Environment -------------------------------------------------------------

library(tidyverse)
devtools::load_all()

# Importing Data ----------------------------------------------------------

dat_clean <- read_rds("data/study1/raw/data_raw.rds")

# Creating Trial indexes --------------------------------------------------

dat_clean <- dat_clean %>% 
  group_by(workerId) %>% 
  mutate(trial_order = 1:n(), # this is the trial order in temporal terms
         block = ifelse(trial_order <= 40, 1, 2), # this is the block = 1 first 40 trials, block = 2 41 to 80 trial
         cond = ifelse(EXP == "X", "exp", "val_arr")) %>% # better cond variable
  group_by(workerId, cond) %>% 
  mutate(trial_cond = 1:n()) %>% # create trials from 1 to 20 for each condition (valrating, arrating) * valence
  ungroup()

# Color Variable ----------------------------------------------------------

dat_clean <- dat_clean %>% 
  mutate(s1_color = case_when(Proc == "A" & S1 == "red" ~ "red", # color variable
                              Proc == "A" & S1 == "blue" ~ "blue",
                              Proc == "A" & S1 == "coral" ~ "coral",
                              Proc == "A" & S1 == "turquoise" ~ "turquoise",
                              Proc == "B" & S1 == "red" ~ "blue",
                              Proc == "B" & S1 == "blue" ~ "red",
                              Proc == "B" & S1 == "coral" ~ "turquoise",
                              Proc == "B" & S1 == "turquoise" ~ "coral",
                              Proc == "C" & S1 == "red" ~ "red", 
                              Proc == "C" & S1 == "blue" ~ "blue",
                              Proc == "C" & S1 == "coral" ~ "coral",
                              Proc == "C" & S1 == "turquoise" ~ "turquoise",
                              Proc == "D" & S1 == "red" ~ "blue",
                              Proc == "D" & S1 == "blue" ~ "red",
                              Proc == "D" & S1 == "coral" ~ "turquoise",
                              Proc == "D" & S1 == "turquoise" ~ "coral",
                              TRUE ~ S1),
         s1_val = case_when(Proc == "A" & S1 == "red" ~ "neg", # color variable
                            Proc == "A" & S1 == "blue" ~ "neu",
                            Proc == "A" & S1 == "coral" ~ "neg",
                            Proc == "A" & S1 == "turquoise" ~ "neu",
                            Proc == "B" & S1 == "red" ~ "neu",
                            Proc == "B" & S1 == "blue" ~ "neg",
                            Proc == "B" & S1 == "coral" ~ "neu",
                            Proc == "B" & S1 == "turquoise" ~ "neg",
                            Proc == "C" & S1 == "red" ~ "neg", 
                            Proc == "C" & S1 == "blue" ~ "neu",
                            Proc == "C" & S1 == "coral" ~ "neg", 
                            Proc == "C" & S1 == "turquoise" ~ "neu",
                            Proc == "D" & S1 == "red" ~ "neu",
                            Proc == "D" & S1 == "blue" ~ "neg",
                            Proc == "D" & S1 == "coral" ~ "neu",
                            Proc == "D" & S1 == "turquoise" ~ "neg",
                            TRUE ~ S1))

# Final Cleaning ----------------------------------------------------------

dat_clean_final <- dat_clean %>% 
  rename("valence" = S2_val,
         "group" = GROUP,
         "valrating" = samvalrating,
         "arrating" = samarrating,
         "cue" = CUE) %>%
  mutate(s1_color = factor(s1_color),
         group = factor(group),
         valence = factor(valence),
         workerId = factor(workerId),
         Cong = factor(Cong),
         block = factor(block),
         sex = factor(sex),
         sex = recode_factor(sex,
                             "Male" = "M",
                             "Female" = "F",
                             "Prefer not to say" = "NA"),
         cue = factor(cue),
         cond = ifelse(EXP == "X", "exp", "val_arr")) %>% 
  
  # select relevant variables
  
  select(workerId, group, cond, valence, exprating, valrating, 
         arrating, block, trial_order, trial_cond, s1_color, Cong, age, sex, cue)

# Saving ------------------------------------------------------------------

saveRDS(dat_clean_final, "data/study1/data_cleaned.rds")
write.csv(dat_clean_final, "data/study1/data_cleaned.csv", row.names = FALSE)