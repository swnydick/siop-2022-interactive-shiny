# Step 4
# Let's plot a incentive vs actual_productivity scatter plot
# with team on legend (different colours).
#
# To do this, let's create a function that takes in the user selections
# to draw a ggplot,  
# create an output using using renderPlot() in server()
# and show render the output using plotOutput() ui().
#
# Notice that input$radio and input$checkGroup are called twice in server()
# i.e. making the server check each inputs twice 
# even when the user hasn't changed the selections made. 
# .... so it's time to go to App 5: Reactivity.



# data pre-processing ----

## use this when loading directly from within R for checking etc.
# data <- read.csv(file.path("data", "garments_worker_productivity.csv"))

## use this for 'Run App'.
data <- read.csv(file.path("..","data", "garments_worker_productivity.csv"))

data$date <- as.Date(data$date, tryFormats = c("%m/%d/%Y", "%m/%d/%y"))
data$team <- paste0("team_", ifelse(data$team < 10, paste0(0, data$team), data$team))
data$day <- factor(data$day, levels = c("Saturday",
                                        "Sunday",
                                        "Monday",
                                        "Tuesday",
                                        "Wednesday",
                                        "Thursday",
                                        "Friday"))

# create lists to feed the control widgets
department_list <- sapply(sort(unique(trimws(data$department))), list)
team_list <- sapply(sort(unique(data$team)), list)

# functions ----
## function to plot incentive vs actual_productivity scatter plot
makeplot1 <- function(dat, dept, tm = NULL){
  
  # subset data based on user selections
  plot_dat <- dat[dat$department %in% dept, ]
  if (!is.null(tm)) {
    plot_dat <- plot_dat[plot_dat$team %in% tm, ]
  }
  
  # draw a ggplot
  ggplot(plot_dat, aes(x = incentive, 
                       y = actual_productivity,
                       color = team
  )
  ) + 
    geom_point() + 
    theme_bw() +
    labs(title = paste("Department:", dept), 
         subtitle = "Incentive vs Actual Productivity") 
}


# R Shiny app ----

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  # Dashboard Title
  titlePanel("App 4: Our First Plot!"),
  
  sidebarLayout(
    sidebarPanel(
      
      # control widgets
      h4("Select these:"),
      
      radioButtons("radio", label = h5("Department"),
                   choices = department_list, 
                   selected = "sweing"),
      
      checkboxGroupInput("checkGroup", label = h5("Teams"), 
                         choices = team_list,
                         selected = NULL),
      
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
      
      # render interaction outputs
      "Change the selections in the side panel and watch the outputs react!",
      verbatimTextOutput("department_value"),
      verbatimTextOutput("team_value"),
      
      # render plot output
      plotOutput("plot")
      
    )
  )
)


server <- function(input, output) {
  
  # Collect the user's input, and return it to the ui()
  # Notice here that we're calling input$radio twice: 
  # here and in the renderPlot function: not efficient!
  output$department_value <- renderPrint(input$radio)
  output$team_value <- renderPrint(input$checkGroup)
  
  ## plot output ----
  output$plot <- renderPlot(
    makeplot1(data, input$radio, input$checkGroup),
    res = 96
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
