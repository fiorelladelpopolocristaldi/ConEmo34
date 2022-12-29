# --- Post processing functions

# These functions are useful for working with fitted models and creating tidy
# summaries and tables

# tidy_fit ----------------------------------------------------------------

# create a tidy version of a fitted model removing unused parameters

tidy_fit <- function(fit){
    fit %>%
        tidy(conf.int = T) %>%
        select(-group, -effect) %>%
        dplyr::filter(!startsWith(term, "cor")) # removing the correlation parameter
}

# tidy_anova --------------------------------------------------------------

# create a tidy version of an anova table for a  fitted model

tidy_anova <- function(fit){
  if(lme4::isGLMM(fit)){
    car::Anova(fit) %>% tidy()
  }else{
    anova(fit) %>% tidy()
  }
}

# prep_names_model --------------------------------------------------------

# clean the parameters name for tables

prep_names_model <- function(tidy_mod){
    tidy_mod %>%
        mutate(
            term = case_when(term == "(Intercept)" ~ "Intercept",
                             
                             # Main effects
                             
                             term == "group1" ~ "UG - CG",
                             term == "cue1" ~ "Amb - Unamb",
                             term == "valence1" ~ "neg - neu",
                             term == "s1_color1" ~ "cue~neg~ - cue~neu~",
                             
                             # 2 way interactions
                             
                             term == "group1:cue1" ~ "group x cue",
                             term == "group1:valence1" ~ "valence x group",
                             term == "group1:s1_color1" ~ "cue x group",
                             term == "cue1:valence1" ~ "cue x valence",
                             
                             # 3 way interactions
                             
                             term == "group1:cue1:valence1" ~ "group x cue x valence",
                             
                             # variance
                             
                             term == "sd__(Intercept)" ~ paste0("\u03C3", " ID"),
                             term == "sd__Observation" ~ paste0("\u03C3", " residual"),
                             term == "sd__valence1" ~ paste0("\u03C3", " valence"),
                             term == "sd__cue1" ~ paste0("\u03C3", " cue"),
                             term == "sd__s1_color1" ~ paste0("\u03C3", " cue"),
                             
                             # anova-like contrasts
                             
                             term == "group.UG-CG" ~ "UG - CG",
                             term == "s1_color.coral-blue" ~ "Coral - Blue",
                             term == "s1_color.red-blue" ~ "Red - Blue",
                             term == "s1_color.turquoise-blue" ~ "Turquoise - Blue",
                             term == "group.UG-CG:s1_color.coral-blue" ~ "UG - CG x Coral - Blue",
                             term == "group.UG-CG:s1_color.red-blue" ~ "UG - CG x Red - Blue",
                             term == "group.UG-CG:s1_color.turquoise-blue" ~ "UG - CG x Turquoise - Blue",
                             term == "sd__s1_color.coral-blue" ~ paste("\u03C3", "Coral - Blue"),
                             term == "sd__s1_color.red-blue" ~ paste("\u03C3", "Red - Blue"),
                             term == "sd__s1_color.turquoise-blue" ~ paste("\u03C3", "Turquoise - Blue"),
                             
                             TRUE ~ term),
            p.value = ifelse(p.value < 0.001, "< 0.001", as.character(round(p.value, 3)))) %>%
        rename(
            "Parameter" = term,
            "Estimate" = estimate,
            "SE" = std.error,
            "Statistic" = statistic,
            "p" = p.value)
}

# prep_names_anova --------------------------------------------------------

# clean the parameters name for tables

prep_names_anova <- function(tidy_fit){
    tidy_fit %>%
        mutate(
            term = case_when(
                term == "group" ~ "Group",
                term == "valence" ~ "Valence",
                term == "s1_color" ~ "Color",
                term == "group:s1_color" ~ "Group x Color",
                term == "group:valence" ~ "Group x Valence",
                term == "group:Cong" ~ "Group x Congruency",
                term == "valence:Cong" ~ "Valence x Congruency",
                term == "group:valence:Cong" ~ "Group x Valence x Congruency",
                TRUE ~ term),
            p.value = ifelse(p.value < 0.001,
                             "< 0.001",
                             as.character(round(p.value, 3)))) %>%

        select(-meansq) %>%
        rename("Effect" = term,
               "SS" = sumsq,
               "p" = p.value,
               "F" = statistic)
}

# model_table -------------------------------------------------------------

# create a model table using the flextable package

model_table <- function(data){
    data %>%
        mutate(across(where(is.numeric), round, 2),
               mod = case_when(mod == "fit_arr" ~ "Arousal",
                               mod == "fit_exp" ~ "Expectancy",
                               mod == "fit_val" ~ "Valence",
                               mod == "fit_acc" ~ "Accuracy",
                               mod == "fit_IES" ~ "IES",
                               mod == "fit_exp_color" ~ "Expectancy (exploratory)")) %>%
        flextable() %>%
        flextable::merge_v(j = 1) %>%
        flextable::theme_vanilla() %>%
        set_header_labels(
            values = list(mod = "Model",
                          Parameter = "Parameter",
                          Estimate = "Estimate",
                          SE = "SE",
                          t = "t",
                          df = "df",
                          p = "p",
                          conf.low = "95% CI",
                          conf.high = "95% CI")) %>%
        merge_h(part = "header") %>%
        align(align = "center", part = "all") %>%
        autofit() %>%
        flextable::fontsize(part = "all", size = 8) %>% 
      ftExtra::colformat_md(part = "all")
}

# anova_table -------------------------------------------------------------

# create an anova table using the flextable package

anova_table <- function(data){
    data %>%
        mutate(across(where(is.numeric), round, 2),
               mod = case_when(mod == "fit_arr" ~ "Arousal",
                               mod == "fit_exp" ~ "Expectancy",
                               mod == "fit_val" ~ "Valence",
                               mod == "fit_exp_color" ~ "Expectancy (Color)")) %>%
        flextable() %>%
        autofit() %>%
        flextable::compose(part = "header", j = "NumDF",
                value = as_paragraph("Df", as_sub("num"))) %>%
        flextable::compose(part = "header", j = "DenDF",
                value = as_paragraph("Df", as_sub("den"))) %>%
        align(part = "all", align = "center") %>%
        merge_v(j = "mod") %>%
        theme_vanilla() %>%
        set_header_labels(values = list(mod = "Model"))
}

# get_effects -------------------------------------------------------------

# get model effects from a fitted model for plotting purposes. Use
# lazy evaluation

get_effects <- function(fit, y,...){
    #eff_fit <- effects::allEffects(fit)[[1]] %>% data.frame()
    data <- fit@frame
    dots <- rlang::enexprs(...)
    dots_join <- dots[dots %in% c("group", "s1_color", "valence", "block", "Cong", "cue")]
    y <- rlang::enexpr(y)
    data %>%
        group_by(!!!dots) %>%
        summarise(.mean = mean(!!y)) %>%
        #left_join(., eff_fit, by = sapply(dots_join, as.character)) %>%
        ungroup() %>%
        mutate(resp = as.character(quote(!!y)))
}

# r2 ----------------------------------------------------------------------

# get R2 from a fitted model. Is a simple wrapper of MuMIn::r.squaredGLMM

r2 <- function(fit, which = c("marg", "cond")){
    index <- ifelse(which == "marg", 1, 2)
    MuMIn::r.squaredGLMM(fit)[[index]]
}

# get_formula -------------------------------------------------------------

# get the formula from a fitted model as string

get_formula <- function(fit){
    as.character(fit@call)[2]
}

# table_emmeans -----------------------------------------------------------

# create a tidy emmeans object

table_emmeans <- function(emm_list){

    emm <- emm_list$contrasts %>%
        tidy(conf.int = TRUE)
    eff <- emm_list$eff_size %>%
        data.frame() %>%
        tibble()
    colnames(eff) <- paste0(colnames(eff), "_es")
    out <- cbind(emm, eff)
}


# clean_model_names -------------------------------------------------------

# fix the name of fitted models with the response variable

clean_mod_names <- function(x, column){
    column <- rlang::enexpr(column)
    x %>%
        mutate(!!column := case_when(!!column == "fit_arr" ~ "Arousal",
                                     !!column == "fit_val" ~ "Valence",
                                     !!column == "fit_exp" ~ "Expectancy"))
}


# get_contrast_and_effect_size --------------------------------------------

# This function is a simple wrapper for emmeans::emmeans() that also return
# the effect size.
# Importantly, the effect size is calculated using the residual SD of the model
# and residual DF. This somehow mimics the get_eff_size_lmer(only_residual = TRUE)
# when the fixed effect correspond to a mean difference

get_contrast_and_effect_size <- function(mod, term, sd = "total", df = "satterthwaite"){
  if(sd == "total"){
    sd_to_use <- get_overall_sigma(mod)
  }else{
    sd_to_use <- sigma(mod)
  }

  out <- emmeans(mod, as.formula(paste0("pairwise ~", term)), lmer.df = df)
  out$eff_size <- eff_size(out, sigma = sd_to_use, edf = Inf)
  return(out)
}

# get_overall_sigma -------------------------------------------------------

# Return the total estimated variability

get_overall_sigma <- function(fit){
  sigma_fit <- data.frame(VarCorr(fit))
  sigma_fit <- sigma_fit[is.na(sigma_fit$var1) | is.na(sigma_fit$var2), "sdcor"]
  out <- sqrt(sum(sigma_fit^2))
  return(out)
}

# models_utils ------------------------------------------------------------

# Check for convergency problems

conv_problem <- function(mod){
  any(grepl("failed to converge",mod@optinfo$conv$lme4$messages))
}

# Check for model singularity probelm using grepl

singular_problem <- function(mod){
  any(grepl("?isSingular", mod@optinfo$conv$lme4$messages))
}

# Return a dataset given a model

get_model_data <- function(mod){
  out <- mod@frame
  return(out)
}

# Wrapper for getting the model formula

get_call <- function(mod){
  return(mod@call)
}

# Wrapper to clean the model formula keeping only the lmer call

clean_formula <- function(call){
  paste(as.character(call[2])) # take the central part
}


# get_cook_table --------------------------------------------------------

# from a fitted model get cook distances in a tidy way. A wrapper
# of influence.ME::influence()

get_cook_table <- function(model, group_factor){

  infl <- suppressMessages(influence.ME::influence(model, group = group_factor)) # get influence analysis

  if(isSingular(model)){
    warning("The model has some fitting problems, results could be unreliable")
  }

  dat <- model@frame # get data from models

  # Cook Distance

  cook_dat <- influence.ME::cooks.distance.estex(infl) # get cook distance

  cook_dat <- tibble(id = rownames(cook_dat),
                     cook = as.vector(cook_dat)) # create tibble

  colnames(cook_dat)[1] <- group_factor # rename with grouping factor

  # Combine

  return(cook_dat)
}