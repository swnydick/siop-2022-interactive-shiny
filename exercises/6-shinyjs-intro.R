#########################################
# Adding Javascript Components          #
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
  
  # SHINYJS # 
  # Need this to use this in ui to use the package # 
  useShinyjs(),
  
  # TITLE # 
  titlePanel("App 6: Adding Javascript Components"),
  
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
      h4("Additional controls:"),
      checkboxInput(inputId = "checkbox", 
                    label   = "Plot regression line for each team?",
                    value   = FALSE),
      
      ## ACTION BUTTON TO SHOW/HIDE REPORTING ##
      # the class attribute provides some color via Bootstrap
      # see https://getbootstrap.com/docs/4.0/components/buttons/
      # See also the related observeEvent(input$button, ...)
      # for the change in text and color when this button is clicked
      actionButton(inputId = "button", 
                   label   = reporting_button_text[1], 
                   class   = "btn-warning"),
      
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
      
    ), # End sidebarpanel
    
    # MAIN # 
    mainPanel(
      
      ## REPORTING BUTTON ## 
      # set the reporting section as its own div section and call it 'Reporting'
      # div produces a tag name for the button
      div(
          id = 'Reporting',
          textOutput("actual_productivity_statement"),
          tableOutput("actual_productivity_week"),
      ),
      
      # PLOT OUTPUTS # 
      plotOutput(outputId = "plot", 
                 click    = "plot_click"),
      
      # TABLE OUTPUTS # 
      "Click somewhere on the plot to see data near it.",
      tableOutput("data_at_clickpoint")
      
    ) # End mainPanel
  ) # End sidebarLayout
) # End ui

##########
# SERVER #
##########

server <- function(input, output) {
  
  ## REACTIVE OUTPUTS ##
  # Note the addition of the reporting button - provided as a state
  dept_selected          <- reactive(input$radio)
  team_selected          <- reactive(input$select)
  trendline              <- reactive(input$checkbox)
  reporting_button_state <- reactive(input$button)
  
  # MAKE PLOT DATA # 
  plot_data  <- reactive(x = data_team_subset(data = data, 
                                             dept = dept_selected(), 
                                             team = team_selected()))
  
  # PLOT # 
  output$plot <- renderPlot(expr = incentive_plot(data      = plot_data(), 
                                                  dept      = dept_selected(), 
                                                  team      = team_selected(), 
                                                  trendline = trendline()),
                            res = 96)
  
  ## ADDING PLOT FUNCTIONALITY : nearPoints ## 
  click_point_data <- reactiveVal(NULL)

  observeEvent(input$plot_click, {
    req(input$plot_click)
    click_point_data(nearPoints(df        = plot_data(), 
                                coordinfo = input$plot_click))
  })
  
  output$data_at_clickpoint <- renderTable({
    click_point_data()
  })
  
  ## REPORTING STATE ## 
  # observeEvent in this case is triggered with the reporting button state changes
  # it then either shows or hides the reporting options 
  observeEvent(reporting_button_state(), {
    
    toggle(id   = "Reporting", 
           anim = TRUE) # hide or show
    toggleClass(id    = "button", 
                class = "btn-danger") # toggled button color
    
    # recall that input$button starts with value 0 and increases by 1 
    # every time you click on it.
    # use even/odd state to toggle between the two states of hide/show
    html("button", ifelse((reporting_button_state() %% 2) == 0 , 
                           reporting_button_text[1] , 
                           reporting_button_text[2]))
  }) 
  
  # GENERATE PRODUCTIVITY STATEMENT # 
  output$actual_productivity_statement <- renderText({
    
    # This generates the text to display the team(s) selected 
    team_txt <- ifelse(is.null(team_selected()), 
                       "all teams have ",
                       paste(
                         paste0(team_selected(), collapse = ", ") ,
                         ifelse(length(team_selected()) < 2, "has ", "have ")
                       )
    ) 
    
    # This combines the team and department text with their productivity value
    paste0(
        dept_selected(),
        "'s ",
        team_txt,
        "an average actual_productivity of ",
        round(mean(plot_data()$actual_productivity),2)
      )
    
  }) # End productivity statement generation
  
  # PRODUCTIVITY TABLE # 
  # renderTable is the function is the print(df) function for shiny - we need 
  # an HTML table. 
  output$actual_productivity_week <- renderTable({
    act_prod_wk <- aggregate.data.frame(plot_data()$actual_productivity, 
                                        by = list(plot_data()$day), 
                                        mean)
    setNames(object = act_prod_wk, nm = c('day', 'average actual productivity'))
  })
  
} #End server

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
