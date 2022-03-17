# Step 8 prettify using thematic
#
# More HTML stuff!

##############
# SETTING UP #
##############

## libraries for customising your own themes
library(thematic)
library(bslib)
# customise pretty colours for the ggplots bit: you can use R colours here
# note the default discrete scale okabe_ito() has only 8 colours: 
# anything more than that defaults to ggplot2

thematic_shiny(bg = "gray10", fg = 'slategray3')
theme_set(theme_bw()) ## setting the ggplot theme here globally instead of within the plotting function previously

ui <- fluidPage(
  
  useShinyjs(),
  
  # customise pretty colours for the html bits: note that you have to use HTML colours
  theme = bs_theme(bg        = "#345678",
                   fg        = "white",
                   primary   = "tomato", # the link is picking up the primary colour
                   warning   = "gold", # bootstrap colour applied to the show/hide reporting button
                   danger    = "lightcyan",
                   base_font = font_google("Redressed")),
  
  # Dashboard Title
  titlePanel("App 8: Prettify"),
  
  sidebarLayout(
    sidebarPanel(
      
      # control widgets
      h4("Select these:"),
      
      radioButtons("radio", label = h5("Department"),
                   choices = department_list, 
                   selected = dept_starting_selection),
      
      # multiple team selection enabled!
      selectInput("select", label = h5("Teams"), 
                  choices = team_list,
                  multiple = TRUE,
                  selected = team_starting_selection),
      
      h4("Additional controls:"),
      
      # TRUE/FALSE control for plotting regression line in plot
      checkboxInput("checkbox", label = "Plot regression line for each team?",
                    value = FALSE),
      
      # Button to show or hide reporting section
      actionButton("button", label = reporting_button_text[1], 
                   class = "btn-warning"),
      
      hr(),
      
      # Descriptions for the data
      h4("Source Data"),
      p("The ", 
        a("Productivity Prediction Garment Employees Dataset", 
          href = "http://archive.ics.uci.edu/ml/datasets/Productivity+Prediction+of+Garment+Employees"),
        " dataset courtesy of the UCI Machine Learning Repository. 
              Please visit the link for source and data dictionary."
      )
      
    ),
    
    mainPanel(
      
      # set the reporting section as its own div section and call it 'reporting'
      div(id = 'Reporting',
          textOutput("actual_productivity_statement"),
          tableOutput("actual_productivity_week"),
      ),
      
      # render plot output
      plotOutput("plot", click = "plot_click"),
      
      "Click somewhere on the plot to see data near it.",
      tableOutput("data_at_clickpoint")
      
    )
  )
)


server <- function(input, output) {
  
  ## reactive outputs ----
  dept_selected <- reactive(input$radio)
  team_selected <- reactive(input$select)
  trendline <- reactive(input$checkbox)
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
  
  output$data_at_clickpoint <- renderTable({
    req(input$plot_click)
    nearPoints(plot_data(), input$plot_click)
  })
  
  ## actual_productivity reporting outputs ----
  
  ## toggles reporting section based on button input
  observeEvent(reporting_button_state(), {
    toggle(id = "Reporting", anim = TRUE) 
    toggleClass("button", "btn-danger") 
    html("button", ifelse((reporting_button_state() %% 2) == 0 , 
                          reporting_button_text[1] , 
                          reporting_button_text[2]))
  }) 
  
  output$actual_productivity_statement <- renderText({
    team_txt <- ifelse(is.null(team_selected()), 
                       "all teams have ",
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
  })
  
  output$actual_productivity_week <- renderTable({
    act_prod_wk <- aggregate.data.frame(plot_data()$actual_productivity, 
                                        by = list(plot_data()$day), 
                                        mean)
    colnames(act_prod_wk) <- c("day", "average actual productivity")
    return(act_prod_wk)
  })
  
  
  
}

## mustn't turn this off otherwise ggplot won't pick up the desired colour schemes
# thematic_off()

# Run the application 
shinyApp(ui = ui, server = server)
