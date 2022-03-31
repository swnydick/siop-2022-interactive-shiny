#########################################
# Introduction to Reactivity            #
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

# INTRO # 
intro_displayr()

######
# UI #
######

# PAGE # 
ui <- fluidPage(
  
  # TITLE #
  titlePanel("App 4: Introduction to Reactivity"),
  
  # SIDEBAR #
  sidebarLayout(
    sidebarPanel(
      
      # DEPARTMENT SELECTIONS # 
      h4("Select these:"),
      radioButtons(inputId  = "radio", 
                   label    = h5("Department"),
                   choices  = department_list, #finishing or sewing
                   selected = dept_starting_selection), # starts with sewing
      
      # TEAM SELECTIONS # 
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
      
    ), # End SIDEBAR PANEL
    
    # MAIN # 
    mainPanel(
      
      # PRINT RENDERED TEXT #
      "Make selections in the side panel and watch the outputs react!",
      verbatimTextOutput("department_value"),
      verbatimTextOutput("team_value"),
      
      # DISPLAY RENDERED PLOT # 
      plotOutput("plot")
      
    ) # End mainPanel
  ) # End sideBarLayout
) # End UI

##########
# SERVER #
##########

server <- function(input, output) {
  
  ## REACTIVE OUTPUTS ##
  # Instead of calling input$radio twice, reactive values are used
  # Reactive values are created to store user selections
  # They only change when something triggers it - otherwise they use they store 
  # the previous result. 
  # This is part of what makes shiny 'imperative' versus 'declarative'.
  dept_selected <- reactive(input$radio)
  team_selected <- reactive(input$select)
  
  ## RENDER PRINT ##
  # Notice the notation - reactives are functions 
  # TODO maybe we could show a recursive loop example as an extra here
  output$department_value <- renderPrint(dept_selected())
  output$team_value       <- renderPrint(team_selected())
  
  ## REACTIVE DATA ## 
  # Here, we make the data reactive - so that we can better call if to other 
  # functions later. 
  plot_data <- reactive(x = data_team_subset(data = data, 
                                             dept = dept_selected(), 
                                             team = team_selected())) 
  
  # RENDER PLOT #
  output$plot <- renderPlot(
    incentive_plot(data = plot_data(), 
                   dept = dept_selected(), 
                   team = team_selected()), 
    res = 96
  ) 
  
} # End Server

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
