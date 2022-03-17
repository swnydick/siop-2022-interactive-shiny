# Step 7 Hide the 'more reporting' with shinyjs
# 
# Now let's suppose you want to give users the choice to hide or show the
# reporting section
#
# This can be done using javascript via shinyjs().
#

##############
# SETTING UP #
##############

# New Library # 
library(shinyjs)

# text labels for action button for showing/hiding reporting
reporting_button_text <- c("Hide Reporting", "Show Reporting")

######
# UI #
######

# PAGE #
ui <- fluidPage(
  
  # SHINYJS # 
  # Need this to use this in ui to use the package # 
  useShinyjs(),
  
  # TITLE # 
  titlePanel("App 7: Adding Javascript components"),
  
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
      
      # ACTION BUTTON #
      ## the class attribute provides some colour via Bootstrap
      ## see https://getbootstrap.com/docs/4.0/components/buttons/
      ## See also the related observeEvent(input$button, ...)
      ## for the change in text and colour when this button is clicked
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
      
    ), # sidebarPanel
    
    # MAIN # 
    mainPanel(
      
      # REPORTING BUTTON # 
      # set the reporting section as its own div section and call it 'Reporting'
      # div produces a tag name for the button
      div(
          id = 'Reporting',
          textOutput("actual_productivity_statement"),
          tableOutput("actual_productivity_week"),
      ),
      
      # PLOT OUTPUTS # 
      plotOutput("plot", click = "plot_click"),
      
      # TABLE OUTPUTS # 
      "Click somewhere on the plot to see data near it.",
      tableOutput("data_at_clickpoint")
      
    )
  )
)


server <- function(input, output) {
  
  # REACTIVE OUTPUTS #
  # Note the addition of the reporting button - provided as a state
  dept_selected <- reactive(x = input$radio)
  team_selected <- reactive(x = input$select)
  trendline     <- reactive(x = input$checkbox)
  reporting_button_state <- reactive(input$button)
  
  plot_data <- reactive(x = data_team_subset(data = data, 
                                             dept = dept_selected(), 
                                             team = team_selected())) # End plot data
  
  # PLOT # 
  output$plot <- renderPlot(expr = incentive_plot(data    = plot_data(), 
                                                  dept    = dept_selected(), 
                                                  team    = team_selected(), 
                                                  show_lm = trendline()),
                            res = 96)
  
  # POINT - CLICK : FUNCTIONALITY # 
  output$data_at_clickpoint <- renderTable({
    
    req(input$plot_click)
    nearPoints(df        = plot_data(), 
               coordinfo = input$plot_click)
  })
  
  ## REPORTING STATE ## 
  # observeEvent in this case is triggered with the reporting button state changes
  # it then either shows or hides the reporting options 
  observeEvent(reporting_button_state(), {
    toggle(id = "Reporting", anim = TRUE) # hide or show
    toggleClass("button", "btn-danger") # toggled button colour
    # recall that input$button starts with value 0 increments by 1 everytime you click on it.
    # use even/odd state to toggle between the two states of hide/show
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
    return(
      paste0(
        dept_selected(),
        "'s ",
        team_txt,
        "an average actual_productivity of ",
        round(mean(plot_data()$actual_productivity),2)
      )
    )
  }) # End productivity statement generation
  
  # PRODUCTIVITY TABLE # 
  # renderTable is the function is the print(df) function for shiny - we need 
  # an HTML table. 
  output$actual_productivity_week <- renderTable({
    act_prod_wk <- aggregate.data.frame(plot_data()$actual_productivity, 
                                        by = list(plot_data()$day), 
                                        mean)
    colnames(act_prod_wk) <- c("day", "average actual productivity")
    return(act_prod_wk)
  })
  
}

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
