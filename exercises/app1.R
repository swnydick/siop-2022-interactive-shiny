# Step 1: get started.  
#
# The default app.R file from RStudio contains an interactive Old Faithful histogram.
#
# We'll stick with the sidebar layout in the User Interface: 
# so let's retain titlePanel() and sidebarLayout() in ui()
# Delete (mostly) everything in ui() and server().
# Add simple outputs in the form of text.
# 
# We don't need anything in the server() just yet.
#
# Press Run App on RStudio.



library(shiny)

ui <- fluidPage(
  
  # Dashboard Title
  titlePanel("App 1: Hello World"),
  
  # Simple content
  sidebarLayout(
    sidebarPanel(
      
      "hello"
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
