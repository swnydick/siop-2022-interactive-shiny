############################################
# Additional Features and Reactivity con't #
#                                          #
# Korn Ferry Institute: Automation Team    #
# 2022-04-29                               #
############################################

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
  titlePanel("App 5: Additional Features and Reactivity con't"),
  
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
      
      # TRENDLINE # 
      # Here, we add a checkboxInput to ask if we should plot trendlines for each 
      # team on the plot. 
      h4("Additional controls:"),
      checkboxInput(inputId = "checkbox", 
                    label   = "Plot regression line for each team?",
                    value   = FALSE),
      
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
      
    ), # End sidebarPanel
    
    # MAIN # 
    mainPanel(
      
      # REPORTING OUTPUTS # 
      # Stores the ids for the reporting text and reporting table
      textOutput(outputId  = "actual_productivity_statement"),
      tableOutput(outputId = "actual_productivity_week"),
      
      # PLOT OUTPUTS # 
      # Stores the ids for the plot and the click-action
      plotOutput(outputId = "plot", 
                 click    = "plot_click"),
      
      # TABLE OUTPUTS # 
      # Stores the ids for the reactive table
      "Click somewhere on the plot to see data near it.",
      tableOutput(outputId = "data_at_clickpoint")
      
    ) # End mainPanel
  ) # End sidebarLayout
) # End ui

##########
# SERVER #
##########

server <- function(input, output) {
  
  # REACTIVE OUTPUTS # 
  # Adding a trendline - and introducing a new control widget - checkbox 
  dept_selected <- reactive(input$radio)
  team_selected <- reactive(input$select)
  trendline     <- reactive(input$checkbox)
  
  # REACTIVE DATA # 
  plot_data <- reactive(x = data_team_subset(data = data, 
                                             dept = dept_selected(), 
                                             team = team_selected())) 
  
  # PLOT # 
  output$plot <- renderPlot(expr = incentive_plot(data      = plot_data(), 
                                                  dept      = dept_selected(), 
                                                  team      = team_selected(), 
                                                  trendline = trendline()),
                            res = 96)
  
  ## ADDING PLOT FUNCTIONALITY : nearPoints ## 
  # This will allow the user to click near a point and see the exact data near 
  # that point.
  
  # reactiveVal is used to store the data from the clicked point
  click_point_data <- reactiveVal(NULL)
  
  # observeEvent - bases actions on whether a new click has occurred
  # req - makes sure that a click actually exists
  # click_point_data - this reactive gets filled with the data from the clicked point
  observeEvent(input$plot_click, {
    req(input$plot_click)
    click_point_data(nearPoints(df        = plot_data(), 
                                coordinfo = input$plot_click))
  })
  
  # renders the data stored in reactiveVal to the table
  output$data_at_clickpoint <- renderTable({
    click_point_data()
  })
  
  # GENERATE PRODUCTIVITY STATEMENT # 
  output$actual_productivity_statement <- renderText({
    
    # This generates the text to display the team(s) selected 
    # Example : Team 1, Team 2 have
    team_txt <- ifelse(is.null(team_selected()), 
                       "all teams have ", 
                       paste(
                         paste0(team_selected(), collapse = ", ") ,
                         ifelse(length(team_selected()) < 2, "has ", "have ")
                       )
    )
    
    # This combines the team and department text with their productivity value
    # Example : 'sewing's Team 1, Team 2 have an average actual_productivity of 0.79
    paste0(dept_selected(),
           "'s ",
           team_txt,
           "an average actual productivity of ",
           round(mean(plot_data()$actual_productivity),2)) 

  }) # End productivity statement generation
  
  ## PRODUCTIVITY TABLE ## 
  # renderTable is the function is the print(df) function for shiny 
  output$actual_productivity_week <- renderTable({
    act_prod_wk <- aggregate.data.frame(x   = plot_data()$actual_productivity, 
                                        by  = list(plot_data()$day), 
                                        FUN = mean)
    setNames(object = act_prod_wk, 
             nm     = c('day', 'average actual productivity'))
  }) # End productivity table
  
} # End server

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
