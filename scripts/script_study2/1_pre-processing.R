## ------------------------------------------------------------------------
## Project: Study3_implicit
##
## Script: Pre-processing
## ------------------------------------------------------------------------

rm(list = ls())

# Environment -------------------------------------------------------------

library(tidyverse)

# Importing Data ----------------------------------------------------------

dat_clean <- read_rds("data/study2/raw/data_raw.rds")

# Creating Trial indexes --------------------------------------------------

# Phase: L = learning, T = Test
# Block 1/2 only for test

dat_clean <- dat_clean %>% 
  group_by(workerId) %>% 
  mutate(trial_order = 1:n(),
         block = case_when(
           # 40-80 is the first block
           # 80-120 is the second block
           trial_order <= 80 & phase == "T" ~ 1,
           trial_order > 80 & phase == "T" ~ 2,
           phase == "L" ~ 0),
         cond = case_when(
           EXP == "X" & phase == "T" ~ "exp",
           EXP == "O" & phase == "T" ~ "val_arr",
           phase == "L" ~ "learning")) %>% 
  group_by(workerId, cond) %>% 
  mutate(trial_cond = 1:n()) %>% # create trials from 1 to 20 for each condition (valrating, arrating) * valence
  ungroup()

# Color Variable ----------------------------------------------------------

# red = negative
# blue = neutral

dat_clean <- dat_clean %>% 
  mutate(s1_color = case_when(Proc == "A" & S1 == "red" ~ "neg",
                              Proc == "A" & S1 == "blue" ~ "neu",
                              Proc == "B" & S1 == "red" ~ "neu",
                              Proc == "B" & S1 == "blue" ~ "neg",
                              Proc == "C" & S1 == "red" ~ "neg", 
                              Proc == "C" & S1 == "blue" ~ "neu",
                              Proc == "D" & S1 == "red" ~ "neu",
                              Proc == "D" & S1 == "blue" ~ "neg",
                              TRUE ~ S1))

# IES calculation ---------------------------------------------------------

a <- dat_clean %>% 
  filter(cond=="learning") %>% 
  group_by(workerId, GROUP) %>% 
  summarise(acc_mean=mean(correct)) %>% # calculate mean accuracy by id and group
  mutate(PE=1-(acc_mean)) # calculate proportion of error (PE)

dat_clean <- dat_clean %>% 
  left_join(., a, by=c("workerId", "GROUP")) # adding PE to main dataset

dat_clean <- dat_clean %>% 
  mutate(IES=avg_rt/(1-PE)) # calculate IES

# Final Cleaning ----------------------------------------------------------

dat_clean_final <- dat_clean %>% 
  rename("valence" = S2_val,
         "group" = GROUP,
         "RT" = avg_rt,
         "valrating" = samvalrating,
         "arrating" = samarrating) %>%
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
                             "DATA EXPIRED" = "NA")) %>% 
  
  # select relevant variables
  
  select(workerId, group, cond, valence, exprating, valrating, arrating, correct, 
         RT, block, trial_order, trial_cond, s1_color, Cong, age, sex, IES)

# Saving ------------------------------------------------------------------

saveRDS(dat_clean_final, "data/study2/data_cleaned.rds")