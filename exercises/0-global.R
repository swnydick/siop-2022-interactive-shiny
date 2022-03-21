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
function_dir   <- file.path(project_dir, "exercises", "R")

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

# ################
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
  }
