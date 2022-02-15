# Step 7 talking point to be determined
# 
# TODO: perhaps to make an optional "colourblind palette picker" panel and 
# TODO: make the the n_col_buffer variable reactive?
# TODO: use this to introduce an action button?
# TODO: or use this to talk about extensions?
#
# The R library colorspace is also used in the ggplot to 
# improve UI the experience for colorblindness
# 


# data pre-processing ----

## use this when loading directly from within R for checking etc.
# data <- read.csv(file.path("data", "garments_worker_productivity.csv"))

## use this for 'Run App'.
data <- read.csv(file.path("..","data", "garments_worker_productivity.csv"))

data$date <- as.character(as.Date(data$date, tryFormats = c("%m/%d/%Y", "%m/%d/%y")))
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
makeplot2 <- function(dat, dept){
  
  ## Managing colorspace::scale_color_discrete_sequential colours:
  # as the discrete_sequential colour palette's
  # lightest colour can be perceived as 'too light'
  # as is the case where in the current dataset when
  # all 12 teams are plotted, 
  # let's create the ability to add additional 'lightest' colours which we 
  # then do not use to 'buffer' the lightest colour
  
  n_col_buffer <- 1 # additional 'lightest colour'
  unique_colours <- length(unique(dat$team)) + n_col_buffer
  
  ggplot(dat, aes(x = incentive, 
                  y = actual_productivity,
                  color = team)
  ) + 
    # geom_point replaced by translucent jittered points for better visibility
    # geom_jitter(alpha = 0.7) + 
    geom_point(alpha = 0.7) +
    # color scheme used for vision diversity support
    scale_color_discrete_sequential(palette = "batlow", 
                                    nmax = unique_colours, 
                                    order = (1 + n_col_buffer):unique_colours
    ) + 
    theme_bw() + 
    labs(title = paste("Department:", dept), 
         # caption = "Points are jittered slightly and made translucent for better visibility",
         subtitle = "Incentive vs Actual Productivity") 
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
library(ggplot2)
library(colorspace)

ui <- fluidPage(
  
  # Dashboard Title
  titlePanel("App 7: ?"),
  
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
      
      # render reporting outputs
      textOutput("actual_productivity_statement"),
      tableOutput("actual_productivity_week"),
      
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
  team_selected <- reactive(input$checkGroup)
  
  plot_data <- reactive(
    makeplotdata(data, dept_selected(), team_selected())
  )
  
  ## plot output ----
  output$plot <- renderPlot(
    makeplot2(plot_data(), dept_selected()), 
    res = 96
  )
  
  # table output to show points near user click as a table
  # note: because there are now more than 1 line in the render* function, 
  # the curly brackets are now needed
  output$data_at_clickpoint <- renderTable({
    req(input$plot_click)
    nearPoints(plot_data(), input$plot_click)
  })
  
  ## actual_productivity reporting outputs ----
  output$actual_productivity_statement <- renderText({
    # technically we can stick the team_txt into the paste0itself,
    # but that would make the statement harder to read!
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
