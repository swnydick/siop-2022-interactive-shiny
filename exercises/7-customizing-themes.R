#########################################
# Customization of Themes               #
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

## New libraries for customising your own themes
library(thematic)
library(bslib)
# customise pretty colours for the ggplots bit: you can use R colours here
# note the default discrete scale okabe_ito() has only 8 colours: 
# anything more than that defaults to ggplot2
thematic_shiny(bg = "gray10", 
               fg = 'slategray3')
theme_set(theme_bw()) ## setting the ggplot theme here globally instead of within the plotting function previously

######
# UI #
######

# PAGE #
ui <- fluidPage(
  
  # NECESSARY CALL TO USE SHINYJS # 
  useShinyjs(),
  
  ## CREATE A BOTTSTRAP THEME ##
  # Use this to set requirements for a bootstrap theme - there are a lot more options 
  # available, such as setting more font options.
  theme = bs_theme(bg        = "#345678",
                   fg        = "white",
                   primary   = "tomato", # the link is picking up the primary colour
                   warning   = "gold", # bootstrap colour applied to the show/hide reporting button
                   danger    = "lightcyan",
                   base_font = font_google("Redressed")),
  
  # TITLE #
  titlePanel("App 7: Customizing Themes"),
  
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
      
      # TRENDLINE # 
      h4("Additional controls:"),
      checkboxInput(inputId = "checkbox", 
                    label   = "Plot regression line for each team?",
                    value   = FALSE),
      
      # ACTION BUTTON TO SHOW/HIDE REPORTING # 
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
              Please visit the link for source and data dictionary.")
    ), # End sidebarpanel
    
    # MAIN #
    mainPanel(
      
      # REPORTING BUTTON #
      div(id = 'Reporting',
          textOutput("actual_productivity_statement"),
          tableOutput("actual_productivity_week"),
      ),
      
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
  dept_selected          <- reactive(input$radio)
  team_selected          <- reactive(input$select)
  trendline              <- reactive(input$checkbox)
  reporting_button_state <- reactive(input$button)
  
  # MAKE PLOT DATA #
  plot_data <- reactive(x = data_team_subset(data = data, 
                                             dept = dept_selected(), 
                                             team = team_selected())) # End plot data
  
  # PLOT # 
  output$plot <- renderPlot(expr = incentive_plot(data      = plot_data(), 
                                                  dept      = dept_selected(), 
                                                  team      = team_selected(), 
                                                  trendline = trendline()),
                            res = 96)
  
  # CLICK-POINT FUNCTIONALITY # 
  click_point_data <- reactiveVal(NULL)

  observeEvent(input$plot_click, {
    req(input$plot_click)
    click_point_data(nearPoints(plot_data(), input$plot_click))
  })
  
  output$data_at_clickpoint <- renderTable({
    click_point_data()
  })
  
  # PRODUCTIVITY REPORTING OUTPUTS # 
  
  ## toggles reporting section based on button input
  observeEvent(reporting_button_state(), {
    toggle(id   = "Reporting", 
           anim = TRUE) 
    toggleClass(id    = "button", 
                class = "btn-danger") 
    html("button", ifelse((reporting_button_state() %% 2) == 0 , 
                          reporting_button_text[1] , 
                          reporting_button_text[2]))
  }) 
  
  # GENERATE PRODUCTIVITY STATEMENT # 
  output$actual_productivity_statement <- renderText({
    
    # This generates the text to display about the teams productivity # 
    team_txt <- ifelse(is.null(team_selected()), 
                       "all teams have ", # need this and the last clarifier if all teams
                       paste(
                         paste0(team_selected(), collapse = ", ") ,
                         ifelse(length(team_selected()) < 2, "has ", "have ")
                       )
    ) 
    
    # This combines the team and department text
    paste0(
      dept_selected(),
      "'s ",
      team_txt,
      "an average actual_productivity of ",
      round(mean(plot_data()$actual_productivity),2)
    )
  })
  
  output$actual_productivity_week <- renderTable({
    act_prod_wk <- aggregate.data.frame(plot_data()$actual_productivity, 
                                        by = list(plot_data()$day), 
                                        mean)
    setNames(object = act_prod_wk, nm = c('day', 'average actual productivity'))
  }) # End productivity statement generation

} # End server 

#######
# RUN #
#######

# The theme seems to carry over to other plots - turn this off if you want to reset the theme.
# thematic_off()
shinyApp(ui = ui, server = server)
