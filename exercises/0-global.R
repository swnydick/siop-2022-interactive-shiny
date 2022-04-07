#########################################
# Setting Up                            #
# Source this before running the apps   # 
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

# Set wd to avoid confusion between what's run within project versus app
# get the project directory
project_dir    <- here::here()
analyses_dir   <- file.path(project_dir, "exercises")
data_dir       <- file.path(project_dir, "data")

# set the path to excercises
setwd(analyses_dir)

# Libraries 
library(ggplot2)
library(dplyr)
library(shiny)
library(shinyjs)
library(thematic)
library(bslib)
library(shinyWidgets)
library(tools)
library(htmltools)

# Load data 
data <- readRDS(file.path(data_dir, "garments_worker_productivity_cleaned.rds"))

# create lists to feed the control widgets
department_list <- setNames(nm = sort(unique(data$department)))
team_list       <- setNames(nm = sort(unique(data$team)))

# set starting selection for department and team
dept_starting_selection <- department_list[2]
team_starting_selection <- team_list[1:2]

# text labels for action button for showing/hiding reporting
reporting_button_text   <- c("Hide Reporting", "Show Reporting")

##################
# INCENTIVE PLOT #
##################

incentive_plot <- function(data, dept, team = NULL, trendline = FALSE){
  
  trendline_txt <- c("", "Trendline drawn")[trendline + 1]
  
  # One color if no teams selected 
  if(is.null(team)){
    g <- ggplot(data    = data,
                mapping = aes(x     = incentive, 
                              y     = actual_productivity)) + 
      geom_point(color = '#006550')
  } else{
    g <- ggplot(data    = data,
                mapping = aes(x     = incentive, 
                              y     = actual_productivity,
                              color = team)) + 
      geom_point(alpha = 0.7)
  }
  
  g <- g + 
    labs(title    = paste("Department:", dept), 
         caption  = trendline_txt,
         subtitle = "Incentive vs Actual Productivity")
  
  if (trendline) {
    g <- g + geom_smooth(formula = y ~ x, method = lm, alpha = 0.15)
  }
  
  g
}

data_team_subset <- function(data, dept, team = NULL){
  
  dd <- data[data$department %in% dept, ]
  
  if (!is.null(team)) {
    dd <- dd[dd$team %in% team, ]
  }
  
  return(dd)
}

##################
# INTRO DISPLAYR #
##################

# Function to extract the name of the file - where intro and file names match
extract_appname <- 
  function(){
    file_path_sans_ext(basename(rstudioapi::getActiveDocumentContext()$path))
  }

# Function to read in and report html text in the viewer
# Assumes you are in the exercises directory - the app location
intro_displayr <- 
  function(apppath       = NULL, 
           file_location = 'intros'){
    
    # get app path if null
    if(is.null(apppath)){
      apppath <- extract_appname()
    }
    
    # get intro file
    intropath <- file.path(file_location, 
                           paste0(apppath, '.txt'))
    intro     <- readLines(intropath)
    
    # Display html intro
    html_print(
      div(
        HTML(
          intro
        )
      )
    )
  } # End intro displayr

################
# LADDA BUTTON #
################

# copied from 8-external-js-scripts-1ladda.R (see that script for comments)

# indicate the directory to install included stuff
assets_dir    <- "assets"
ladda_dir     <- file.path(assets_dir, "ladda")

# we need to tell our application where to look for ladda
laddaDependency <- function(){
  htmltools::htmlDependency(
    name    = "ladda",
    version = "1.0.6",
    src     = file.path(analyses_dir, ladda_dir),
    script     = c("js/spin.min.js",
                   "js/ladda.min.js"),
    stylesheet = c("css/ladda.min.css"),
  )
}

# first create a visual!
laddaButton <- function(inputId,
                        label){
  
  value <- restoreInput(id      = inputId,
                        default = NULL)
  
  data_style    <- "contract"
  data_color    <- "mint"
  data_size     <- "m"
  spinner_size  <- NULL
  spinner_lines <- 12
  spinner_color <- "#ffffff"
  
  tagButton     <- htmltools::tags$button(
    id                   = inputId,
    type                 = "button",
    class                = "ladda-button",
    class                = "btn btn-default action-button",
    `data-val`           = value,
    `data-style`         = data_style,
    `data-color`         = data_color,
    `data-size`          = data_size,
    `data-spinner-color` = spinner_color,
    `data-spinner-lines` = spinner_lines,
    htmltools::tags$span(class = "ladda-label", label)
  )
  
  htmltools::attachDependencies(
    x      = tagButton,
    value  = laddaDependency(),
    append = TRUE
  )
}

# function to get query to run in JS
laddaQuery <- function(id, method, ...){
  
  method <- match.arg(arg     = method,
                      choices = c("start", "stop", "toggle", "setProgress"))
  
  args   <- paste(..., sep = ", ")
  
  script <- paste0("Ladda.create(document.querySelector('\\#", id, "'))")
  
  paste0(script, ".", method, "(", args, ")")
}

# function to run query in JS
laddaRun <- function(...){
  shinyjs::runjs(laddaQuery(...))
}

# create Ladda based on ID that has all of the relevant methods
Ladda    <- function(id){
  
  gen_ladda_method <- function(method){
    function(...)
      laddaRun(id     = id,
               method = method,
               ...)
  }
  
  list(start       = gen_ladda_method("start"),
       stop        = gen_ladda_method("stop"),
       toggle      = gen_ladda_method("toggle"),
       setProgress = gen_ladda_method("setProgress"))
}
