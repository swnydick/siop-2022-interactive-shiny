#########################################
# App 2: User Controls                  #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

##############
# SETTING UP #
##############

# Set wd to avoid confusion between what's run within project versus app
# get the project directory
project_dir    <- here::here()
analyses_dir   <- file.path(project_dir, "exercises")
data_dir       <- file.path(project_dir, "data")

# set the path to excercises
setwd(analyses_dir)

# To clean things up - running what's needed for all apps
source('0-global.R')

# INTRO # 
intro_displayr()

###########################
# NEW GLOBAL ITEMS : DATA #
###########################

# DATA #
# Note that RStudio will run the app.R in its own working directory - so it's 
# necessary in this case to navigate 'up' to the data folder.
# For info on how the data was cleaned, see the clean_data.R file in the data folder
# data <- readRDS(file.path(data_dir, 'garments_worker_productivity_cleaned.rds'))

# Explain Data Here
View(head(data))

# FIND UI LISTS # 
# The ui will need to have lists of choices for the buttons and check box.
department_list <- setNames(nm = sort(unique(data$department)))
team_list       <- setNames(nm = sort(unique(data$team)))

# set starting selection for department and team
dept_starting_selection <- department_list[2]
team_starting_selection <- team_list[1:2]

# After this app - we will load this information in a setting up file
# within each app. 
# This is what can be loaded right before each app.
# source('0-global.R')

######
# UI #
######

# PAGE # 
ui <- fluidPage(
  
  # TITLE #
  titlePanel("App 2: User Controls"),
  
  # SIDEBAR # 
  sidebarLayout(
    sidebarPanel(
      
      ## RADIO BUTTON FOR DEPARTMENT SELECTION ## 
      # always need an inputId - (server function(s) will operate on this id)
      # label will produce a title on the page for the control
      # choices are what the user can choose - we defined this earlier
      # selected is the initial choice
      h4("Select these:"),
      radioButtons(inputId  = "radio", 
                   label    = h5("Department"),
                   choices  = department_list, #finishing or sewing
                   selected = dept_starting_selection), # starts with sewing
      
      ## SELECTING TEAM BY A LIST ##
      # selectInput creates a select list that can choose from a list of values
      # a new option is the option to use multiple = TRUE to choose multiple selections
      selectInput(inputId  = "select", 
                  label    = h5("Teams"), 
                  choices  = team_list,
                  multiple = TRUE,
                  selected = team_starting_selection),
      
      # BREAK # 
      hr(),
      
      # DATA SOURCE # 
      h4("Source Data"),
      p("The ", 
        a("Productivity Prediction Garment Employees Dataset", 
          href = "http://archive.ics.uci.edu/ml/datasets/Productivity+Prediction+of+Garment+Employees"),
        "dataset courtesy of the UCI Machine Learning Repository. 
              Please visit the link for source and data dictionary."
      )
    ), #End side bar 
    
    # MAIN # 
    mainPanel(
      
      ## OUTPUT FROM SELECTIONS ##
      # Shiny's version of the 'print' function to render text 
      # The outputId is created after the server operations
      "Make selections in the side panel and watch the outputs react!",
      verbatimTextOutput(outputId = "department_value"),
      verbatimTextOutput(outputId = "team_value"),
    )
  )
)

##########
# SERVER #
##########

server <- function(input, output) {
  
  ## OUTPUT ##
  # Render functions determine what type of output to produce
  # The result is stored in an output id that feeds back to the UI
  # renderPrint is equivalent to 'print' for printing the results of an expr 
  # to the console. 
  output$department_value <- renderPrint(expr = input$radio) # sewing or finishing
  output$team_value       <- renderPrint(expr = input$select) # teams
  
}

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
