#########################################
# Plot Introduction                     #
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

# set the path to excercises
setwd(analyses_dir)

# To clean things up - running what's needed for all apps
source('0-global.R')

# Note - need some new items - but these are sourced in global 
# library(ggplot2)
# library(dplyr)

# INTRO # 
intro_displayr()

######
# UI #
######

# PAGE # 
ui <- fluidPage(
  
  # TITLE #
  titlePanel("App 3: Plotting Introduction"),
  
  # SIDEBAR #
  sidebarLayout(
    sidebarPanel(
      
      # RADIO BUTTON FOR DEPARTMENT SELECTION #
      h4("Select these:"),
      radioButtons(inputId  = "radio", 
                   label    = h5("Department"),
                   choices  = department_list, #finishing or sewing
                   selected = dept_starting_selection), # starts with sewing
      
      # SELECTING TEAM BY A LIST # 
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
        " dataset courtesy of the UCI Machine Learning Repository. 
              Please visit the link for source and data dictionary."
      ) # End paragraph
      
    ), # End side bar
    
    # MAIN #
    mainPanel(
      
      # OUTPUT FROM SELECTIONS #
      "Make selections in the side panel and watch the outputs react!",
      verbatimTextOutput(outputId = "department_value"),
      verbatimTextOutput(outputId = "team_value"),
      
      ## PLOT OUTPUT ##
      # This takes the plotting instructions from the server 
      # plot is the outputId
      plotOutput(outputId = "plot")
      
    ) # End mainPanel
  ) # End sideBarLayout
) # End UI

##########
# SERVER #
##########

server <- function(input, output) {
  
  ## PRINTING SELECTIONS ## 
  output$department_value <- renderPrint(expr = input$radio)
  output$team_value       <- renderPrint(expr = input$select)
  
  ## STORE THE PLOT OUTPUT ## 
  # This tells the ui that a plot should be displayed (and prepares it)
  output$plot <- renderPlot(expr = data_team_subset(data = data, 
                                                    dept = input$radio, 
                                                    team = input$select) %>%
                              incentive_plot(data = ., 
                                             dept = input$radio, 
                                             team = input$select, 
                                             trendline = FALSE),
                            res = 96
  ) # End renderPlot
  
} # End server

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
