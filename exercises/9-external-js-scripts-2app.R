#########################################
# Using External JS in Shiny: App       #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

# 1. Read/Clean Data ===========================================================
require(dplyr)
require(stringr)
require(ggplot2)

# read data and fix columns
data <- read.csv(file.path("..", "data", "garments_worker_productivity.csv")) 
data <- mutate(
  .data = data,
  date       = as.Date(x          = date,
                       tryFormats = c("%m/%d/%Y", "%m/%d/%y")) |>
               as.character(),
  team       = str_pad(string = team,
                       width  = 2,
                       side   = "left") %>%
               paste0("Team", .),
  day        = factor(x   = day,
                      levels = c("Saturday", "Sunday",
                                 "Monday", "Tuesday",
                                 "Wednesday", "Thursday", "Friday")),
  department = str_trim(department) %>%
               ifelse(. %in% "sweing", "sewing", .)
  
  
)

# create lists to feed the control widgets
department_list <- setNames(nm = sort(unique(data$department)))
team_list       <- setNames(nm = sort(unique(data$team)))

# set starting selection for department and team
dept_starting_selection <- department_list[2]
team_starting_selection <- team_list[1:2]

# text labels for action button for showing/hiding reporting
reporting_button_text   <- c("Hide Reporting", "Show Reporting")

# 2. Functions =================================================================

makeplot2 <- function(dat, dept, trendline){
  
  trendline_txt <- c("", "Trendline drawn")[trendline + 1]
  
  g <- ggplot(data    = dat,
              mapping = aes(x     = incentive, 
                            y     = actual_productivity,
                            color = team)) + 
      geom_point(alpha = 0.7) + 
      labs(title    = paste("Department:", dept), 
           caption  = trendline_txt,
           subtitle = "Incentive vs Actual Productivity")
  
  if (trendline) {
    g <- g + geom_smooth(formula = y ~ x, method = lm, alpha = 0.15)
  }
  
  g
}

makeplotdata <- function(dat, dept, tm = NULL){
  
  dd <- dat[dat$department %in% dept, ]
  
  if (!is.null(tm)) {
    dd <- dd[dd$team %in% tm, ]
  }
  
  return(dd)
}

# 3. Application ===============================================================

library(shiny)
library(shinyjs)
library(ggplot2)
library(thematic)
library(bslib)

thematic_shiny(bg = "gray10",
               fg = 'slategray3')
theme_set(theme_bw())

ui <- fluidPage(
  
  useShinyjs(),
  
  theme = bs_theme(bg        = "#345678",
                   fg        = "white",
                   primary   = "tomato",
                   warning   = "gold", 
                   danger    = "lightcyan",
                   base_font = font_google("Redressed")),
  
  titlePanel("App 9: Integration JS"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select these:"),
      radioButtons(inputId  = "radio",
                   label    = h5("Department"),
                   choices  = department_list, 
                   selected = dept_starting_selection),
      selectInput(inputId  = "select",
                  label    = h5("Teams"), 
                  choices  = team_list,
                  multiple = TRUE,
                  selected = team_starting_selection),
      
      h4("Additional controls:"),
      checkboxInput(inputId = "checkbox",
                    label   = "Plot regression line for each team?",
                    value   = FALSE),
      # add ladda button for plot refresh
      laddaButton(inputId = "refresh",
                  label   = "Refresh"),
      hr(),
      actionButton(inputId = "button",
                   label   = reporting_button_text[1], 
                   class   = "btn-warning"),
      
      hr(),
      
      h4("Source Data"),
      p("The ", 
        a("Productivity Prediction Garment Employees Dataset", 
          href = "http://archive.ics.uci.edu/ml/datasets/Productivity+Prediction+of+Garment+Employees"),
        " dataset courtesy of the UCI Machine Learning Repository. 
              Please visit the link for source and data dictionary."
      )
      
    ),
    
    mainPanel(
      div(id = 'Reporting',
          textOutput("actual_productivity_statement"),
          tableOutput("actual_productivity_week"),
      ),
      plotOutput(outputId = "plot",
                 click    = "plot_click"),
      "Click somewhere on the plot to see data near it.",
      tableOutput("data_at_clickpoint")
      
    )
  )
)


server <- function(input, output) {
  
  # indicate ladda button for ID
  ladda                  <- Ladda("refresh")
  
  trendline              <- reactive(input$checkbox)
  reporting_button_state <- reactive(input$button)
  
  # turn reactives into reactiveVal to update ONLY when button press
  dept_selected          <- reactiveVal()
  team_selected          <- reactiveVal()
  plot_data              <- reactiveVal()
  plot_gg                <- reactiveVal()
  
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
      plot_data(makeplotdata(data, dept_selected(), team_selected()))
      plot_gg(makeplot2(plot_data(), dept_selected(), trendline()))
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
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
  
  output$data_at_clickpoint <- renderTable({
    req(input$plot_click)
    nearPoints(plot_data(), input$plot_click)
  })
  
  observeEvent(reporting_button_state(), {
    toggle(id   = "Reporting",
           anim = TRUE) 
    toggleClass("button", "btn-danger") 
    html("button",
         reporting_button_text[((reporting_button_state() %% 2) == 0) + 1])
  }) 
  
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

