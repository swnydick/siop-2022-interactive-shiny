# Step 8 prettify using thematic
#
# More HTML stuff!

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
    # theme_bw() + ## moving this theme setting into the global space
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
  theme = bs_theme(bg = "#345678",
                   fg = "white",
                   primary = "tomato", # the link is picking up the primary colour
                   warning = "gold", # bootstrap colour applied to the show/hide reporting button
                   danger = "lightcyan",
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
