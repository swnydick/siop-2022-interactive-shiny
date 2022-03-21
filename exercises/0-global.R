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

# Source a few custom r functions 
source(file.path(function_dir, 'incentive_plot.R'), 
       local = FALSE)
source(file.path(function_dir, 'intro_displayr.R'), 
       local = FALSE)
