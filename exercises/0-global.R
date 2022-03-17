#########################################
# Setting Up                            #
# Source this before running the apps   # 
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

# Load data 
data <- readRDS(file.path("data", "garments_worker_productivity_cleaned.rds"))

# create lists to feed the control widgets
department_list <- setNames(nm = sort(unique(data$department)))
team_list       <- setNames(nm = sort(unique(data$team)))

# set starting selection for department and team
dept_starting_selection <- department_list[2]
team_starting_selection <- team_list[1:2]

# text labels for action button for showing/hiding reporting
reporting_button_text   <- c("Hide Reporting", "Show Reporting")

# Source a few custom r functions 
source(file.path('exercises', 'R', 'incentive_plot.R'))
source(file.path('exercises', 'R', 'intro_displayr.R'))
