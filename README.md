
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
-   `scripts/script_study1-study2`: contains all script to reproduce the
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

# Suggested workflow

1.  Make sure that *R* and *Rstudio* are up-to-date. Under the *Session
    Info* section is reported the R version used for this project.
2.  Install the
    [`renv`](https://rstudio.github.io/renv/articles/renv.html) package
    for managing dependencies
3.  Open the `ConEmo34.Rproj` file
4.  Use `renv::init()` in order to initialize the project with `renv`
5.  Use `renv::restore()` in order to install packages from the
    `renv.lock` file
6.  Run the `main_script.R` file. This script can be launched directly
    `source("main_script.R")` or each analysis step separately using
    `run_script()`.

# Dataset description

## Study 1

-   `workerId`: unique identifier for each subject
-   `group`: between-subject variable that identifies the experimental
    group (UG = uncertain group and CG = certain group)
-   `cond`: relevant response required in each trial: exp = Expectancy
    rating, val_arr = Valence/Arousal rating
-   `S1_color`: The color of the S1 stimulus (red, blue, coral,
    turquoise)
-   `cue`: Ambuguitiy of S1 stimulus (Ambiguous, Unambiguous)
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
-   `S1_color`: The color of S1 stimulus (red and blue)
-   `valence`: the valence of the S2 stimulus (Negative and Neutral)
-   `exprating/valrating/arrating`: response variables: exprating =
    expectancy rating, valrating = valence rating, arrating = arousal
    rating
-   `RT`: Reaction times to the *Parity Judgement Task*
-   `IES`: *Inverse Efficency Score* to the *Parity Judgement Task*
-   `correct`: Accuracy (0 and 1) to the *Parity Judgement Task*

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
#> # A tibble: 3 x 2
#>   Info      Value                          
#>   <chr>     <chr>                          
#> 1 R version R version 4.1.2 (2021-11-01)   
#> 2 Platform  x86_64-w64-mingw32/x64 (64-bit)
#> 3 OS        Windows 10 x64 (build 19044)
```
