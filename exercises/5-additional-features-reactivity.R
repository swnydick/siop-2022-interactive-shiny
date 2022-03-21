# To do this, let's modify the plotting function to split it into two functions: 
# one for plotting, and 
# a second to return a reactive dataset
# since we will be re-using this reactive dataset in the table output
#
# As example, compare the table outputs when you click on 
# (department = sweing) incentive = 113
# between no teams are selected (notice that teams using incentive 113 = teams 1,2,3,10)
# while after you select on teams 1 and 2, 
# the table output subsets itself appropriately.
#
# Notice data$date has been coerced from Date to as.character as Shiny doesn't
# seem to like display as.Date as a character....
#
# 
# [B] Let's also swap the initial department and team selection with something
# more 'reporting-like': a sentence describing the average actual productivity,
# and a table showing the average actual productivity by day of week.
#
# [C]: let's also add the ability to toggle a regression line  
#
# Continue to talk about reactivity and reuse....

##############
# SETTING UP #
##############

# DATE DISPLAY ISSUE # 
data$date <- as.character(data$date)

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
      textOutput(outputId  = "actual_productivity_statement"),
      tableOutput(outputId = "actual_productivity_week"),
      
      # PLOT OUTPUTS # 
      plotOutput(outputId = "plot", 
                 click    = "plot_click"),
      
      # TABLE OUTPUTS # 
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
  dept_selected <- reactive(x = input$radio)
  team_selected <- reactive(x = input$select)
  trendline     <- reactive(x = input$checkbox)
  
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
  # click_point_data - this reactive gets filled with the data from the clickec point
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
    
    # This generates the text to display about the teams productivity # 
    # Example : Team 1, Team 2 have
    team_txt <- ifelse(is.null(team_selected()), 
                       "all teams have ", 
                       paste(
                         paste0(team_selected(), collapse = ", ") ,
                         ifelse(length(team_selected()) < 2, "has ", "have ")
                       )
    ) 
    # This combines the team and department text 
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
    setNames(object = act_prod_wk, nm = c('day', 'average actual productivity'))
  }) # End productivity table
  
} # End server

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
