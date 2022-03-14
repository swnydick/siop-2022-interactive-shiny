#########################################
# Reactive Programming                  #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

# The following are a brief set of "gotchas" when trying to use reactive
# programming. There are many more pieces of advice when programming with
# shiny and many more tips/tricks to make it do what you want. See the Mastering
# reactivity section of https://mastering-shiny.org/ for more information.

# There are many ways of debugging R scripts. We typically use browser statements
# to step into files!

library(shiny)
library(shinyWidgets)

# 1. Reactive Function NEEDS Reactive Context ==================================

# this often happens when you're trying to print something to the console on setup
ui    <- fluidPage(
  radioButtons(inputId  = "numbers",
               label    = "How many numbers should we select?",
               choices  = c(50, 100, 1000),
               selected = 100),
  radioButtons(inputId  = "statistic",
               label    = "What statistic should we calculate?",
               choices  = c("mean", "median", "sd"),
               selected = "mean"),
  textOutput("statistic")
)

server <- function(input, output, session){
  
  # generate data and store in a reactive object
  data <- reactive({
    rnorm(n = input$numbers)
  })
  
  # print to the console the FIRST of stuff (WON'T WORK)
  message("Processed the initial ", input$numbers, " numbers. Everything OK!")
  
  # update the statistic
  output$statistic <- renderText({
    stat      <- input$statistic
    calc_stat <- get(stat)
    paste0("The ", stat, " of the ", input$numbers, " numbers is: ", calc_stat(data()))
  })
}

shinyApp(ui, server)

# SOLUTION
# 1. create an observeEvent with once set to TRUE
# 2. run the code through the observeEvent

# note: if something reactive, evalulate ONLY in reactive.
#       also applies to: reactive, reactiveVal, reactiveValues (like input)

# 2. Static Object DOESN'T Trigger Updates =====================================

# this often comes about when trying to use modules with input variables
genUI <- function(id){
  ns <- NS(id)
  
  # a silly example of generating random numbers
  tagList(
    numericInput(inputId = ns("number"),
                 label   = "Generate random uniform deviates",
                 value   = 1,
                 min     = 1,
                 max     = 10,
                 step    = 1),
    verbatimTextOutput(ns("sample"))
  )
}

genServer <- function(id,
                      reset){
  
  # applying the silly example in shiny module ... see RESET
  moduleServer(
    id     = id,
    module = function(input, output, session){
      output$sample <- renderText({
        runif(input$number)
      })
  
      # will not work because reset needs to be evaluated to trigger reactivity
      observeEvent(reset, {
        updateNumericInput(session = session,
                           inputId = "number",
                           value   = 1)
      })
    }
  )
}

# creating UI and server
ui    <- fluidPage(
  actionButton(inputId = "reset_all",
               label   = "Reset"),
  genUI("gen_one"),
  genUI("gen_two")
)

server <- function(input, output, session){
  
  # will not work because reset_all is a constant, not reactive anymore!!!
  genServer(id    = "gen_one",
            reset = input$reset_all)
  genServer(id    = "gen_two",
            reset = input$reset_all)
}

shinyApp(ui, server)

# SOLUTION
# 1. create separate reactive that stores input$reset_all: reset <- reactive({input$reset_all})
# 2. send the unevaulated reactive into genServer: reset = reset
# 3. update genServer reset observer to evaluate reset: observeEvent(reset(), ...)

# note: if something static CAN change, put it in a reactive!

# 3. Beware of Infinite Loops ==================================================

# this happens most often when you have objects updating input and input updating
# the same objects (although can happen in many situations)
ui     <- fluidPage(
  div(
    # the entire set of stuff will be 200px wide (otherwise would take up whole screen)
    style = "width: 300px;",
    div(
      
      # the font will be 14px so make it not too big
      style = "font-size: 14px;",
      
      # add two columns - one for the switch and one for the number!
      column(
        div(style = "overflow: auto;",
            prettySwitch(inputId = "add",
                         label   = "Item",
                         value   = FALSE,
                         status  = "success",
                         fill    = TRUE)),
        width = 8
      ),
      column(
        numericInput(inputId = "value",
                     label   = NULL,
                     value   = 0,
                     min     = 0,
                     step    = 1,
                     width   = "80px"),
        width = 4
      )
    )
  )
)

server <- function(input, output, session){
  
  # add one if we flip the switch to positive
  observe({
    if(isTRUE(input$add)){
      updateNumericInput(inputId = "value",
                         value   = input$value + 1)
    } else{
      updateNumericInput(inputId = "value",
                         value   = 0)
    }
  })
  
  # - flip the switch to negative if we have 0
  # - flip the switch to positive if we have at least one
  observe({
    if(input$value == 0){
      updatePrettySwitch(inputId = "add",
                         value   = FALSE)
    } else{
      updatePrettySwitch(inputId = "add",
                         value   = TRUE)
    }
  })
}

shinyApp(ui, server)

# SOLUTION (some ideas) ...
# 1. Use observeEvent(input$add, ...) RATHER than observe to ONLY update value
#    if input$add is TRUE
# 2. Use isolate(input$value) to make sure the observer doesn't update when
#    you do not want it to.
# 3. Don't be clever ... just set value to the value you want!
