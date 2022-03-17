##############
# SETTING UP #
##############

# NEW ITEMS INTO GLOBAL ENVIRONMENT : PLOTTING FUNCTIONS and LIBRARY # 
library(ggplot2)
source("../R/incentive_plot.R",
       local = TRUE)

##################################
# INTRO RUN BEFORE UI AND SERVER #
##################################

# INTRO # 
intro_displayr('app4')

######
# UI #
######

# PAGE # 
ui <- fluidPage(
  
  # TITLE #
  titlePanel("App 4: Our First Plot!"),
  
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
      verbatimTextOutput("department_value"),
      verbatimTextOutput("team_value"),
      
      ## PLOT OUTPUT ##
      # This takes the plotting instructions from the server 
      # plot is the outputId
      plotOutput("plot")
      
    ) # End mainPanel
  ) # End sideBarLayout
) # End UI

##########
# SERVER #
##########

server <- function(input, output) {
  
  ## PRINTING SELECTIONS ## 
  # Notice here that we're calling input$radio twice: 
  # here and in the renderPlot function: not efficient!
  # We'll learn how to make this more efficient in the next app
  output$department_value <- renderPrint(x = input$radio)
  output$team_value       <- renderPrint(x = input$select)
  
  ## STORE THE PLOT OUTPUT ## 
  # This tells the ui that a plot should be displayed (and prepares it)
  # TODO can add this to front 
  output$plot <- renderPlot(
    incentive_plot(data = data, 
                   dept = input$radio, 
                   team = input$select),
    res = 96
  ) # End renderPlot
  
} # End server

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
