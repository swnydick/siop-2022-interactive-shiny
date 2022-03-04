# Step 7 Hide the 'more reporting' with shinyjs
# 
# Now let's suppose you want to give users the choice to hide or show the
# reporting section
#
# This can be done using javascript via shinyjs().
#

# data pre-processing ----

## use this when loading directly from within R for checking etc.
# data <- read.csv(file.path("data", "garments_worker_productivity.csv"))

## use this for 'Run App'.
data <- read.csv(file.path("..","data", "garments_worker_productivity.csv"))

data$date <- as.character(as.Date(data$date, tryFormats = c("%m/%d/%Y", "%m/%d/%y")))
data$team <- paste0("Team", ifelse(data$team < 10, paste0(0, data$team), data$team))
data$day <- factor(data$day, levels = c("Saturday",
                                        "Sunday",
                                        "Monday",
                                        "Tuesday",
                                        "Wednesday",
                                        "Thursday",
                                        "Friday"))

# remove white spaces from  department names
data$department <- trimws(data$department)
# replace wrong spelling
data$department[data$department %in% 'sweing'] <- 'sewing'

# create lists to feed the control widgets
department_list <- sapply(sort(unique(data$department)), list)
team_list <- sapply(sort(unique(data$team)), list)

# set starting selection for department and team
dept_starting_selection <- department_list[[2]]
team_starting_selection <- c(team_list[[1]], team_list[[2]])

# text labels for action button for showing/hiding reporting
reporting_button_text <- c("Hide Reporting", "Show Reporting")

# functions ----
## function to plot incentive vs actual_productivity scatter plot
makeplot2 <- function(dat, dept, trendline){
  
  trendline_txt <- ifelse(trendline, 
                          "Trendline drawn", 
                          "")
  
  g <- ggplot(dat, aes(x = incentive, 
                       y = actual_productivity,
                       color = team)) + 
    geom_point(alpha = 0.7) + 
    theme_bw() + 
    labs(title = paste("Department:", dept), 
         caption = trendline_txt,
         subtitle = "Incentive vs Actual Productivity")
  
  if (trendline) {
    g <- g + geom_smooth(formula = y ~ x, method = lm, alpha = 0.15)
  }
  
  g
  
}

## function to return reactive data subset based on the department and team(s) selected
makeplotdata <- function(dat, dept, tm = NULL){
  
  dd <- dat[dat$department %in% dept, ]
  if (!is.null(tm)) {
    dd <- dd[dd$team %in% tm, ]
  }
  
  return(dd)
}

# R Shiny app ----

library(shiny)
library(shinyjs)
library(ggplot2)

ui <- fluidPage(
  
  useShinyjs(),
  
  # Dashboard Title
  titlePanel("App 7: Adding Javascript components"),
  
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
      ## the class attribute provides some colour via Bootstrap
      ## see https://getbootstrap.com/docs/4.0/components/buttons/
      ## See also the related observeEvent(input$button, ...)
      ## for the change in text and colour when this button is clicked
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
      
      # set the reporting section as its own div section and call it 'Reporting'
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
  
  plot_data <- reactive(
    makeplotdata(data, dept_selected(), team_selected())
  )
  
  ## plot output ----
  output$plot <- renderPlot( 
    makeplot2(plot_data(), dept_selected(), trendline()),
    res = 96
  )
  
  output$data_at_clickpoint <- renderTable({
    req(input$plot_click)
    nearPoints(plot_data(), input$plot_click)
  })
  
  ## actual_productivity reporting outputs ----
  
  observeEvent(reporting_button_state(), {
    toggle(id = "Reporting", anim = TRUE) # hide or show
    toggleClass("button", "btn-danger") # toggled button colour
    # recall that input$button starts with value 0 increments by 1 everytime you click on it.
    # use even/odd state to toggle between the two states of hide/show
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

# Run the application 
shinyApp(ui = ui, server = server)
