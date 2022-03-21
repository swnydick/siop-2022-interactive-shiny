#########################################
# Shiny Modules                         #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

##############
# SETTING UP #
##############

# Set wd to avoid confusion between what's run within project versus app
# get the project directory
project_dir    <- here::here()
analyses_dir   <- file.path(project_dir, "exercises")

# set the path to exercises
setwd(analyses_dir)

# To clean things up - running what's needed for all apps
source('0-global.R')

#########
# INTRO #
#########

# intro_displayr()

# 1. Create Modules (NEW!!!) ===================================================

library(shiny)

# need two functions: UI and Server
productivityDisplayUI <- function(id){
  
  # can add OTHER arguments if you want, but only ID is required
  
  # NEED to create a namespace function to update the IDs
  ns     <- NS(id)
  
  # - make sure that every id is surrounded by the namespace function (ns)
  # - make sure everything is wrapped within a tagList to return
  tagList(
    
    # REPORTING BUTTON #
    div(id = ns("Reporting"),
        textOutput(ns("actual_productivity_statement")),
        tableOutput(ns("actual_productivity_week"))),
    
    # PLOT OUTPUTS #
    plotOutput(outputId = ns("plot"),
               click    = ns("plot_click")),
    
    # TABLE OUTPUTS #
    "Click somewhere on the plot to see data near it.",
    tableOutput(ns("data_at_clickpoint"))
  )
}

productivityDisplayServer <- function(id, data,
                                      dept      = "sewing",
                                      team      = NULL,
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

      # MAKE PLOT DATA #
      
      # - fixed  stuff are passed as simple arguments
      # - change stuff are passed as reactives that need to be evaluated
      plot_data <- reactive({
        data_team_subset(
          dat   = data,   # data is a fixed data.frame
          dept  = dept,   # dept is a fixed char for a given panel
          team  = team()  # team are the teams that can change
        )
      })

      # - fixed  stuff are passed as simple arguments
      # - change stuff are passed as reactives that need to be evaluated
      plot_gg   <- reactive({
        incentive_plot(
          dat       = plot_data(), # plot data changes above!
          dept      = dept,        # dept is a fixed char for a given panel
          team      = team(),      # team are the teams that can change
          trendline = trendline()  # trendline is the linear line that can change
        )
      })
      
      # PLOT #
      
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
      
      # CLICK-POINT FUNCTIONALITY #

      # can add clickpoint data (NO NEED TO USE NS)
      click_point_data <- reactiveVal(NULL)
      
      observeEvent(input$plot_click, {
        req(input$plot_click)
        click_point_data(nearPoints(plot_data(), input$plot_click))
      })
      
      output$data_at_clickpoint <- renderTable({
        click_point_data()
      })

      # PRODUCTIVITY REPORTING OUTPUTS # 
      
      # can update report button
      observeEvent(reporting(), { # passed as a reactive value and evaluated

        # shinyjs doesn't need id surrounded by NS, although some things do
        toggle(id   = "Reporting",
               anim = TRUE)

      })

      # GENERATE PRODUCTIVITY STATEMENT # 
      
      # can update text (NO NEED TO USE NS)
      output$actual_productivity_statement <- renderText({
        req(plot_data())

        # team is passed as reactive value, everything else the same
        teams    <- team()
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

      # can pass something back to the calling function as a reactive
      # - NO parentheses, not evaluated, just reactive
      return(plot_data)
    }
  )
  
}

# 2. Application ===============================================================

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
  
  # NECESSARY CALL TO USE SHINYJS #
  useShinyjs(),
  
  # CREATE A BOTTSTRAP THEME #
  theme = bs_theme(bg        = "#345678",
                   fg        = "white",
                   primary   = "tomato",
                   warning   = "gold", 
                   danger    = "lightcyan",
                   base_font = font_google("Redressed")),
  
  # TITLE #
  titlePanel("App 10: Shiny Modules"),
  
  # SIDEBAR #
  sidebarLayout(
    sidebarPanel(
      h4("Select these:"),
      
      # get rid of radio, as this will be done in panels
      
      # SELECTING TEAM BY A LIST # 
      selectInput(inputId  = "select",
                  label    = h5("Teams"), 
                  choices  = team_list,
                  multiple = TRUE,
                  selected = team_starting_selection),
      
      # TRENDLINE # 
      h4("Additional controls:"),
      checkboxInput(inputId = "checkbox",
                    label   = "Plot regression line for each team?",
                    value   = FALSE),
      
      # ACTION BUTTON TO SHOW/HIDE REPORTING #
      actionButton(inputId = "button",
                   label   = reporting_button_text[1], 
                   class   = "btn-warning"),
      
      # DOWNLOAD BUTTON #
      # add download button to download the data
      downloadButton("download_data"),
      
      # BREAK #
      hr(),
      
      # DATA SOURCE #
      h4("Source Data"),
      p("The ", 
        a("Productivity Prediction Garment Employees Dataset", 
          href = "http://archive.ics.uci.edu/ml/datasets/Productivity+Prediction+of+Garment+Employees"),
        " dataset courtesy of the UCI Machine Learning Repository. 
              Please visit the link for source and data dictionary."
      )
      
    ),
    
    # MAIN #
    # already created this earlier
    mainPanel(prod_panels_all),
  )
)


server <- function(input, output, session) {
  
  # REACTIVE OUTPUTS #
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
  
  # MODULE CALL #
  # add module information (Map/lapply works better than for loops)
  plot_data  <- lapply(
    X   = setNames(nm = prod_ids),
    FUN = function(id){
      productivityDisplayServer(
        id        = id,                    # fixed in a given tab
        data      = data,                  # fixed for all tabs
        dept      = id,                    # fixed for a given tab
        team      = team_selected,         # pass REACTIVE
        trendline = trendline,             # pass REACTIVE
        reporting = reporting_button_state # pass REACTIVE
      )
    }
  )
  
  # DOWNLOAD HANDLER #
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
