#########################################
# Using External JS in Shiny: App       #
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

# set the path to exercises
setwd(analyses_dir)

# To clean things up - running what's needed for all apps
source('0-global.R')

#########
# INTRO #
#########

intro_displayr()

# 1. Application ===============================================================
thematic_shiny(bg = "gray10",
               fg = 'slategray3')
theme_set(theme_bw())

ui <- fluidPage(
  
  # NECESSARY CALL TO USE SHINYJS # 
  useShinyjs(),
  
  # CREATE A BOTTSTRAP THEME #
  # theme = bs_theme(bg        = "#345678",
  #                  fg        = "white",
  #                  primary   = "tomato",
  #                  warning   = "gold",
  #                  danger    = "lightcyan",
  #                  base_font = font_google("Redressed")),
  
  # TITLE #
  titlePanel("App 8: JS Integration"),
  
  # SIDEBAR #
  sidebarLayout(
    sidebarPanel(
      
      # RADIO BUTTON FOR DEPARTMENT SELECTION #
      h4("Select these:"),
      radioButtons(inputId  = "radio",
                   label    = h5("Department"),
                   choices  = department_list, 
                   selected = dept_starting_selection),
      
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
      
      # LADDA BUTTON #
      # add ladda button for plot refresh
      laddaButton(inputId = "refresh",
                  label   = "Refresh"),
      hr(),
      
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
              Please visit the link for source and data dictionary."
      )
      
    ),
    
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
      tableOutput("data_at_clickpoint")
      
    )
  )
)


server <- function(input, output) {
  
  # indicate ladda button for ID
  ladda                  <- Ladda("refresh")
  
  # REACTIVE OUTPUTS #
  trendline              <- reactive(input$checkbox)
  reporting_button_state <- reactive(input$button)
  
  # turn reactives into reactiveVal to update ONLY when button press
  dept_selected          <- reactiveVal()
  team_selected          <- reactiveVal()
  plot_data              <- reactiveVal()
  plot_gg                <- reactiveVal()
  
  # MAKE PLOT DATA AND PLOT OBJECT #
  
  # update plot ONLY when refresh
  observeEvent(
    eventExpr   = input$refresh,
    handlerExpr = {
      
      # only run the button if it's clicked!
      if(!is.null(input$refresh) && input$refresh > 0){
        
        # start ladda
        ladda$start()
    
        # stop ladda when leaving reactive
        on.exit(ladda$stop())
    
        # update progress
        for(p in seq(0, 1, by = .1)){
          Sys.sleep(.1)
          ladda$setProgress(p)
        }
      }
    
      # update department/team
      dept_selected(input$radio)
      team_selected(input$select)
    
      # update plot data and plot
      plot_data(
        data_team_subset(data = data,
                         dept = dept_selected(),
                         team = team_selected())
      )
      plot_gg(
        incentive_plot(data      = plot_data(),
                       dept      = dept_selected(),
                       team      = team_selected(),
                       trendline = trendline())
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
  # PLOT #
  
  # make ONLY if we've updated the plot
  output$plot <- renderPlot(
    expr = {
      if(is.null(plot_gg())){
        return(NULL)
      } else{
        return(plot_gg())
      }
    },
    res = 96
  )
  
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
  observeEvent(reporting_button_state(), {
    toggle(id   = "Reporting",
           anim = TRUE) 
    toggleClass("button", "btn-danger") 
    html("button",
         reporting_button_text[((reporting_button_state() %% 2) == 0) + 1])
  }) 
  
  # GENERATE PRODUCTIVITY STATEMENT # 
  
  # make text ONLY if we've updated the text
  output$actual_productivity_statement <- renderText({
    req(plot_data())
    teams    <- team_selected()
    n_teams  <- length(teams)
    team_txt <- paste(teams, collapse = ", ")
    
    if(n_teams == 0){
      team_txt <- "all teams have"
    } else{
      team_txt <- paste(team_txt,
                        c("has ", "have ")[(n_teams < 2) + 1])
    }
    
    return(
      paste0(
        dept_selected(),
        "'s ",
        team_txt,
        " an average actual_productivity of ",
        round(mean(plot_data()$actual_productivity), 2)
      )
    )
  })
  
  # make table ONLY if we've updated the table
  output$actual_productivity_week <- renderTable({
    req(plot_data())
    aggregate.data.frame(x   = plot_data()$actual_productivity, 
                         by  = list(plot_data()$day), 
                         FUN = mean) |>
    setNames(c("day", "average actual productivity"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#######
# RUN #
#######

# turn this off if you want to reset the theme.
# thematic_off()
shinyApp(ui = ui, server = server)
