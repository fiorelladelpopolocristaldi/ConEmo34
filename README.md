
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Repository

This is the repository for the analysis script of the paper *The role of
implicit learning and cue ambiguity on the subjective experience of
affective predictions: a follow-up behavioral investigation*. This
folder allows to fully reproduce the analysis reported in the paper, and
producing tables, figures and the supplementary materials.

# Folder’s organization

The folder is organized as using `.Rprojects`, and all functions within
the `R/` folder are automatically loaded in the global environment.

-   `data/`: contains raw data for each experiment, both in `.rds` and
    `.csv` format
-   `figures/`: contains all figures
-   `tables/`: contains all tables
-   `objects/`: contains all objects created by analysis scripts
-   `R/`: contains all custom functions used in the project
-   `script_visual/auditory`: contains all script to reproduce the
    analysis:
    -   `*_outliers`: detect and remove relevant outliers
    -   `*_analysis`: the main analysis reported in the paper
    -   `*_exploratory`: extra analysis included in the supplementary
        materials
    -   `*_paper_figures_tables`: create figures and tables
-   `renv/`: contains the R environment with all packages version in
    order to reproduce the analysis
-   `main_script.R`: is the main script that call each sub-script to run
    the full analysis

## Suggested workflow

1.  Make sure that *R* and *Rstudio* are up-to-date. Under the *Session
    Info* section is reported the R version used for this project.
2.  Open the `analysis.Rproj` file. This file automatically load all
    functions and the `renv` environment. If `renv` is not loaded
    automatically, use `renv::restore()` and then restart the R session.
3.  Run the `main_script.R` file. This script can be launched directly
    `source("main_script.R")` or each analysis step separately using
    `run_script()`.

# Dataset description

## Study 1

-   `workerId`: unique identifier for each subject
-   `group`: between-subject variable that identifies the experimental
    group (UG = uncertain group and CG = certain group)
-   `cond`: relevant response required in each trial: exp = Expectancy
    rating, val_arr = Valence/Arousal rating
-   `S1_color`:
-   `cue`:
-   `valence`: the valence of the S2 stimulus (Negative and Neutral)
-   `exprating/valrating/arrating`: response variables: exprating =
    expectancy rating, valrating = valence rating, arrating = arousal
    rating

The dataset is organized in long format, where each line is a trial. The
`cond` variable indicate which response variable is relevant for that
specific trial.

## Study 2

-   `workerId`: unique identifier for each subject
-   `group`: between-subject variable that identifies the experimental
    group (UG = uncertain group and CG = certain group)
-   `cond`: relevant response required in each trial: exp = Expectancy
    rating, val_arr = Valence/Arousal rating
-   `S1_color`:
-   `valence`: the valence of the S2 stimulus (Negative and Neutral)
-   `exprating/valrating/arrating`: response variables: exprating =
    expectancy rating, valrating = valence rating, arrating = arousal
    rating
-   `RT`:
-   `IUS`:

The dataset is organized in long format, where each line is a trial. The
`cond` variable indicate which response variable is relevant for that
specific trial.

# Packages

-   `devtools`
-   `rlang`
-   `car`
-   `dplyr`
-   `effects`
-   `flextable`
-   `influence.ME`
-   `lme4`
-   `MuMIn`
-   `cli`
-   `here`
-   `renv`
-   `tibble`
-   `rmarkdown`
-   `magrittr`
-   `knitr`
-   `lmerTest`
-   `Routliers`
-   `tidyverse`
-   `broom`
-   `broom.mixed`
-   `emmeans`
-   `cowplot`
-   `ggeffects`
-   `ggthemes`
-   `officer`

# Session Info

``` r
session_info()
#> # A tibble: 3 × 2
#>   Info      Value                       
#>   <chr>     <chr>                       
#> 1 R version R version 4.1.1 (2021-08-10)
#> 2 Platform  x86_64-pc-linux-gnu (64-bit)
#> 3 OS        Pop!_OS 21.10
```
