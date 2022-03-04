#########################################
# Using External JS in Shiny            #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

# 1. Setup =====================================================================

require(shiny)
require(shinyjs)

# we are going to integrate Ladda buttons into an application
# Github Page: https://github.com/hakimel/Ladda
# Examples/Docs: https://lab.hakim.se/ladda/

# get the project directory
project_dir    <- here::here()
analyses_dir   <- file.path(project_dir, "exercises")

# set the path to excercises
setwd(analyses_dir)

# indicate the directory to install included stuff
assets_dir    <- "assets"
ladda_dir     <- file.path(assets_dir, "ladda")
ladda_css_dir <- file.path(ladda_dir, "css")
ladda_js_dir  <- file.path(ladda_dir, "js")

# create the directories (if they don't exist)
dir.create(path         = ladda_css_dir,
           showWarnings = FALSE,
           recursive    = TRUE)
dir.create(path         = ladda_js_dir,
           showWarnings = FALSE,
           recursive    = TRUE)

# 2. Download Files ============================================================

# need the following files to make this work:
# - CSS: ladda.min.css
# - JS:  spin.min.js
# - JS:  ladda.min.js

# let's download all of those files into the appropriate directory
ladda_files <- c(css = "https://cdnjs.cloudflare.com/ajax/libs/Ladda/1.0.6/ladda.min.css",
                 js  = "https://cdnjs.cloudflare.com/ajax/libs/Ladda/1.0.6/spin.min.js",
                 js  = "https://cdnjs.cloudflare.com/ajax/libs/Ladda/1.0.6/ladda.min.js")

lapply(
  X   = seq_along(ladda_files),
  FUN = function(i){
    
    # pull out the file
    src      <- ladda_files[[i]]
    
    # figure out the directory where we want to put the file
    dest_dir <- switch(EXPR  = names(ladda_files)[i],
                       css   = ladda_css_dir,
                       js    = ladda_js_dir)
    
    # add the file name to the directory
    dest     <- file.path(dest_dir, basename(src))
    
    # download the file
    download.file(url      = src,
                  destfile = dest)
  }
)

# 3. Add Dependency ============================================================

# we need to tell our application where to look for ladda
# note: we don't need to make this a function, but it looks cleaner to me!
# note: we can put this directly in the laddaButton function, but it's easier
#       to see what's going on if we separate the code
laddaDependency <- function(){
  htmltools::htmlDependency(
    # the name of the asset folder AND version number OF ladda
    # (can be anything, but must not match another asset/version combo)
    name    = "ladda",
    version = "1.0.6",
    
    # the directory of the files (relative or absolute)
    src     = file.path(analyses_dir, ladda_dir),
    
    # javascript/css files (RELATIVE to the ladda_files directory)
    # note: spin.min.js needs to come BEFORE ladda.min.js due to dependencies
    script     = c("js/spin.min.js",
                   "js/ladda.min.js"),
    stylesheet = c("css/ladda.min.css"),
  )
}

# 4. Create Visual =============================================================

## A. Code ---------------------------------------------------------------------

# first create a visual! shiny::actionButton as a reference
# see https://github.com/hakimel/Ladda button attributes
laddaButton <- function(inputId,
                        label){
  
  # see actionButton
  value <- restoreInput(id      = inputId,
                        default = NULL)
  
  # although see github for additional options (CAN add arguments)
  data_style    <- "contract"
  data_color    <- "mint"
  data_size     <- "m"
  spinner_size  <- NULL
  spinner_lines <- 12
  spinner_color <- "#ffffff"
  
  # creating button
  tagButton     <- htmltools::tags$button(
    # the id/type (see actionButton)
    id                   = inputId,
    type                 = "button",
    
    # the class of the ladda button (see https://github.com/hakimel/Ladda)
    class                = "ladda-button",
    
    # the class of a normal shiny button (see actionButton)
    class                = "btn btn-default action-button",
    
    # standard shiny input code (see restoreInput)
    `data-val`           = value,
    
    # all of the ladda attributes attached to the button
    `data-style`         = data_style,
    `data-color`         = data_color,
    `data-size`          = data_size,
    `data-spinner-color` = spinner_color,
    `data-spinner-lines` = spinner_lines,
    
    # adding the label to the button
    htmltools::tags$span(class = "ladda-label", label)
  )
  
  # adding the dependency to the button so that it knows where to get the css/js
  htmltools::attachDependencies(
    x      = tagButton,
    value  = laddaDependency(),
    append = TRUE
  )
}

## B. Example ------------------------------------------------------------------

# creating a button
ladda_ui_button <- laddaButton(inputId = "click",
                               label   = "Click")

# let's see what is in the button
ladda_ui_button
attributes(ladda_ui_button)

# let's create a simply ui to run the code
shinyApp(ui     = fluidPage(ladda_ui_button),
         server = function(...) {})

# To see if everything is there:
# -> Right click --> Inspect Element
# -> Sources --> 127.0.1 --> ladda-1.0.6 shows up!

# To see the button work (in Inspect Element)
# -> Console
# -> ladda = Ladda.create(document.querySelector("#click"))
# -> Try the different methods!!
#    - ladda.start()
#    - ladda.stop()
#    - ladda.toggle()
#    - ladda.setProgress(.5)

# It works ... but how to we get this into R??

# 5. Create Functional =========================================================

## A. Code ---------------------------------------------------------------------

# to make this somewhat simple, use shinyjs::runjs!

# function to get query to run in JS
laddaQuery <- function(id, method, ...){
  
  # make sure method is OK
  method <- match.arg(arg     = method,
                      choices = c("start", "stop", "toggle", "setProgress"))
  
  # paste ... to make arguments (NO checks)
  args   <- paste(..., sep = ", ")
  
  # find ID in JS
  script <- paste0("Ladda.create(document.querySelector('\\#", id, "'))")
  
  # add method and arguments
  paste0(script, ".", method, "(", args, ")")
}

# function to run query in JS
laddaRun <- function(...){
  shinyjs::runjs(laddaQuery(...))
}

# create Ladda based on ID that has all of the relevant methods
Ladda    <- function(id){
  
  # want to create a function factory!
  gen_ladda_method <- function(method){
    function(...)
      laddaRun(id     = id,
               method = method,
               ...)
  }
  
  # create individual functions
  list(start       = gen_ladda_method("start"),
       stop        = gen_ladda_method("stop"),
       toggle      = gen_ladda_method("toggle"),
       setProgress = gen_ladda_method("setProgress"))
}

## B. Example ------------------------------------------------------------------

# creating a shiny application given query
ui        <- fluidPage(
  # need to load the shinyjs dependency
  useShinyjs(),
  
  # simple UI buttons
  laddaButton(inputId = "click",
              label   = "Start!"),
  actionButton(inputId = "stop",
               label   = "Stop!"),
  actionButton(inputId = "toggle",
               label   = "Toggle!"),
  numericInput(inputId = "add_progress",
               label   = "Add Progress!",
               value   = 0,
               min     = 0,
               max     = 100)
)

server   <- function(session, input, output){
  
  # create the ladda button with all of the methods
  ladda          <- Ladda("click")
  
  # start, stop, toggle, set progress, ...
  observeEvent(input$click, {
    ladda$start()
  })
  observeEvent(input$stop, {
    ladda$stop()
  })
  observeEvent(input$toggle, {
    ladda$toggle()
  })
  observeEvent(input$add_progress, {
    ladda$setProgress(input$add_progress / 100)
  })
}

# let's create a simply ui to run the code
shinyApp(ui     = ui,
         server = server)
