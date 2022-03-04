# Step 2. 
# Let's add some standard text to the app.
# Notice I've changed the title text.
#
# Now let's include information regarding the dataset 
# in the side panel.
#
# Notice that we're writing in html here for formatting control:
# h4() corresponds to header levels,
# p() corresponds to <p></p>,
# a(, href) corresponds to <a href></a> etc.
#
# The downloaded .csv is placed in a folder
# alongside the exercise folder



library(shiny)

ui <- fluidPage(
  
  # Dashboard Title
  titlePanel("App 2: Introducing static text"),
  
  sidebarLayout(
    sidebarPanel(
      
      p("hello"),
      
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
      
      "world"
      
    )
  )
)


server <- function(input, output) {
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
