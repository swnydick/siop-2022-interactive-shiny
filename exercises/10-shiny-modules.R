#########################################
# Shiny Modules                         #
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

# 3. Create Modules (NEW!!!) ===================================================

library(shiny)

# need two functions: UI and Server
productivityDisplayUI <- function(id){
  
  # can add OTHER arguments if you want, but only ID is required
  
  # NEED to create a namespace function to update the IDs
  ns     <- NS(id)
  
  # - make sure that every id is surrounded by the namespace function (ns)
  # - make sure everything is wrapped within a tagList to return
  tagList(
    div(id = ns("Reporting"),
        textOutput(ns("actual_productivity_statement")),
        tableOutput(ns("actual_productivity_week"))),
    plotOutput(outputId = ns("plot"),
               click    = ns("plot_click")),
    "Click somewhere on the plot to see data near it.",
    tableOutput(ns("data_at_clickpoint"))
  )
}

productivityDisplayServer <- function(id, data,
                                      dept      = "sewing",
                                      tm        = NULL,
                                      trendline = TRUE,
                                      reporting = TRUE){
  
  # WRAP the code in moduleServer function
  # - pass the ID into moduleServer
  # - create module function with input/output/server arguments
  # - generally no need for NS UNLESS updating outside of input/output
  moduleServer(
    id     = id,
    module = function(input, output, session){
      
      # not likely to be needed, but JUST IN CASE
      ns        <- session$ns

      # - fixed  stuff are passed as simple arguments
      # - change stuff are passed as reactives that need to be evaluated
      plot_data <- reactive({
        makeplotdata(
          dat   = data,   # data is a fixed data.frame
          dept  = dept,   # dept is a fixed char for a given panel
          tm    = tm()    # tm are the teams that can change
        )
      })

      # - fixed  stuff are passed as simple arguments
      # - change stuff are passed as reactives that need to be evaluated
      plot_gg   <- reactive({
        makeplot2(
          dat       = plot_data(), # plot data changes above!
          dept      = dept,        # dept is a fixed char for a given panel
          trendline = trendline()  # trendline is the linear line that can change
        )
      })

      # can add plot to output (NO NEED TO USE NS) ... SAME AS BEFORE
      output$plot <- renderPlot(
        expr = {
          if(is.null(plot_gg())){
            return(NULL)
          } else{
            return(plot_gg())
          }
        },
        res  = 96
      )

      # can add clickpoint data (NO NEED TO USE NS) ... SAMEISH AS BEFORE
      click_point_data <- reactiveVal(NULL)
      
      observeEvent(input$plot_click, {
        req(input$plot_click)
        click_point_data(nearPoints(plot_data(), input$plot_click))
      })
      
      output$data_at_clickpoint <- renderTable({
        click_point_data()
      })

      # can update report button
      observeEvent(reporting(), { # passed as a reactive value and evaluated

        # shinyjs doesn't need id surrounded by NS, although some things do
        toggle(id   = "Reporting",
               anim = TRUE)

      })

      # can update text (NO NEED TO USE NS) ... SAME AS BEFORE
      output$actual_productivity_statement <- renderText({
        req(plot_data())

        # tm is passed as reactive value, everything else the same
        teams    <- tm()
        n_teams  <- length(teams)
        team_txt <- paste(teams, collapse = ", ")

        if(n_teams == 0){
          team_txt <- "all teams have"
        } else{
          team_txt <- paste0(team_txt,
                             c("has", "have")[(n_teams < 2) + 1])
        }


        # same as before, but dept is now FIXED and not reactive
        return(
          paste0(
            dept,
            "'s ",
            team_txt,
            " an average actual_productivity of ",
            round(mean(plot_data()$actual_productivity), 2)
          )
        )
      })

      # same as before, as everything is already in this function
      output$actual_productivity_week <- renderTable({
        req(plot_data())
        aggregate.data.frame(x   = plot_data()$actual_productivity,
                             by  = list(plot_data()$day),
                             FUN = mean) |>
        setNames(c("day", "average actual productivity"))
      })

      # can pass something back to the calling function as a
      # -NO parentheses, not evaluated, just reactive
      return(plot_data)
    }
  )
  
}

# 3. Application ===============================================================

library(shinyjs)
library(ggplot2)
library(thematic)
library(bslib)

thematic_shiny(bg = "gray10",
               fg = 'slategray3')
theme_set(theme_bw())

### A. Create Panels for EACH Group --------------------------------------------

# - create tagList for each finishing/sewing id
# - turn each list element into a tabPanel
prod_ids        <- sort(unique(data$department))
prod_panels_ind <- lapply(
  X   = prod_ids,
  FUN = function(id){
    tabPanel(title = id,
             productivityDisplayUI(id))
  }
)
 # - do.call to create tabsetPanel from this list as arguments
prod_panels_all <- do.call(what = tabsetPanel,
                           args = prod_panels_ind)


### B. Put Everything in UI/Server ---------------------------------------------
ui <- fluidPage(
  
  useShinyjs(),
  
  theme = bs_theme(bg        = "#345678",
                   fg        = "white",
                   primary   = "tomato",
                   warning   = "gold", 
                   danger    = "lightcyan",
                   base_font = font_google("Redressed")),
  
  titlePanel("App 9: Shiny Modules"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select these:"),
      # get rid of radio, as this will be done in panels
      selectInput(inputId  = "select",
                  label    = h5("Teams"), 
                  choices  = team_list,
                  multiple = TRUE,
                  selected = team_starting_selection),
      
      h4("Additional controls:"),
      checkboxInput(inputId = "checkbox",
                    label   = "Plot regression line for each team?",
                    value   = FALSE),
      hr(),
      actionButton(inputId = "button",
                   label   = reporting_button_text[1], 
                   class   = "btn-warning"),
      
      # add download button to download the data
      downloadButton("download_data"),
      
      hr(),
      h4("Source Data"),
      p("The ", 
        a("Productivity Prediction Garment Employees Dataset", 
          href = "http://archive.ics.uci.edu/ml/datasets/Productivity+Prediction+of+Garment+Employees"),
        " dataset courtesy of the UCI Machine Learning Repository. 
              Please visit the link for source and data dictionary."
      )
      
    ),
    
    # already created this earlier
    mainPanel(prod_panels_all),
  )
)


server <- function(input, output, session) {
  
  trendline              <- reactive(input$checkbox)
  reporting_button_state <- reactive(input$button)
  
  # get rid of dept_selected, as this will be fixed
  team_selected          <- reactive(input$select)
  
  # GET RID OF ALL PLOTTING/DATA/TABLES (NOW IN MODULE)
  
  # keep reporting button, as this is out of module, but ONLY toggle the class
  observeEvent(reporting_button_state(), {
    toggleClass("button", "btn-danger") 
    html("button",
         reporting_button_text[((reporting_button_state() %% 2) == 0) + 1])
  }) 
  
  # GET RID OF ALL PLOTTING/DATA/TABLES (NOW IN MODULE)
  
  # add module information (Map/lapply works better than for loops)
  plot_data  <- lapply(
    X   = setNames(nm = prod_ids),
    FUN = function(id){
      productivityDisplayServer(
        id        = id,                    # fixed in a given tab
        data      = data,                  # fixed for all tabs
        dept      = id,                    # fixed for a given tab
        tm        = team_selected,         # pass REACTIVE
        trendline = trendline,             # pass REACTIVE
        reporting = reporting_button_state # pass REACTIVE
      )
    }
  )
  
  # add download handler to download the data
  # - need filename argument (how to create filename)
  # - need content argument (what to DO with file)
  output$download_data <- downloadHandler(
    filename = function(){
      paste0("productivity_data-", Sys.Date(), ".xlsx")
    },
    content  = function(file){
      # make sure to evaluate the reactive expression
      all_data <- lapply(plot_data, function(r) r())
      
      # write the data to a file
      writexl::write_xlsx(x    = all_data,
                          path = file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
