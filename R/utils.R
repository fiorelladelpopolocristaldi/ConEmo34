# --- Utils

# these functions are used for the internal project organization and for running
# each step of the analysis

# get_all_packages --------------------------------------------------------

# This function retrieve all packages used in the project using renv

get_all_packages <- function(){
  options(renv.verbose = FALSE)
  pkgs <- renv::dependencies()$Package
  pkgs <- unique(pkgs)
  options(renv.verbose = TRUE)
  return(pkgs)
}

# put_packages_readme -----------------------------------------------------

# This function format the packages for the README file creating a bullet list

format_pkgs_readme <- function(){
  
  all_pkgs <- get_all_packages()
  all_pkgs <- paste0("`", all_pkgs, "`")
  all_pkgs <- paste("-", all_pkgs)
  
  return(all_pkgs)
  
}

# run_script --------------------------------------------------------------

# This function run a single analysis step and clean the environment at the end

run_script <- function(file, analysis = c("study1", "study2")){
  
  fun_name <- deparse(substitute(file))
  
  analysis = match.arg(analysis)
  
  suppressMessages(
    suppressWarnings(
      source(here::here("scripts", paste0("script_", analysis), file))
      ))
  
  clean_env() # clean everything
  
  cli::cli_alert_success(paste(fun_name, "finished!"))
  
}

# clean_env ---------------------------------------------------------------

# wrapper for cleaning the global environment when calling a function

clean_env <- function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

# session_info ------------------------------------------------------------

# return a nice tibble with useful information about the machine where the
# project has been created and maintained

session_info <- function(){
  session <- sessionInfo()
  tibble::tibble(
    Info = c("R version", "Platform", "OS"),
    Value = c(
      session$R.version$version.string,
      session$platform,
      session$running
    )
  )
  
}