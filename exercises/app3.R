# Step 3.
# Now let's load the data into R.
# 
# For the purpose of this demo, let's visualise
# relationship between incentives and productivity 
# of the two departments and their teams.
# 
# Note that RStudio will run the app.R in its own working directory
# so the file.path("..", "data", ...) is needed for the app to find
# its way to the data folder
#
# The data needs some pre-processing.
# (a) change the date from character to Date format,
# (b) change the team column from integer to character:
# let's prefix the integers with a 0 if the integer is less than 10
# and then prefix the numbers with a 'team_',
# (c) trim unwanted white-space using trimws() from the department column. 
# (d) order the weekdays logically
#
# Let's include on the user interface the ability 
# to toggle between the 2 departments and select teams at will.
# 
# To do so let's create some control widgets for the user to play with.
# In ui(),
# (a) create radio buttons selectors for department (1 selection only)
# (b) create checkbox group for team (can select multiple; note the default chosen is NULL)
#
# In server(), create the necessary output items to 'collect' user selections
# for display back into ui()'s mainPanel.
# 
# Here, let's use the most primitive 'verbatimTextOutput': 
# just like printing to console.
#
# Note that the control widgets require us to feed 
# a list in function input "choices"
# to tell it what to render for the choices, so let's do so in 
# data prep.



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

# R Shiny app ----

library(shiny)

ui <- fluidPage(
  
  # Dashboard Title
  titlePanel("App 3: Look Ma, it moves!"),
  
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
      
    )
  )
)


server <- function(input, output) {
  
  # collect the user's input, and return it to the ui()
  output$department_value <- renderPrint(input$radio)
  output$team_value <- renderPrint(input$checkGroup)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
