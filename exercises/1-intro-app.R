#########################################
# 1 - Introduction to shiny             #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################

##############
# SETTING UP #
##############

# Run what's needed for all files 

# Set wd to avoid confusion between what's run within project versus app
# get the project directory
project_dir    <- here::here()
analyses_dir   <- file.path(project_dir, "exercises")

# set the path to excercises
setwd(analyses_dir)

# To clean things up - running what's needed for all apps
source('0-global.R')

# INTRO # 
intro_displayr()

######
# UI #
######

# PAGE # 
ui <- fluidPage(
  
  # TITLE # 
  titlePanel("App 1: Introduction to Shiny"),
  
  # SIDEBAR # 
  sidebarLayout(
    sidebarPanel(
      
      ## PRINT HELLO ## 
      # htmltools are exported into shiny - there are several 
      # useful functions to create html tags using an r language.  
      # p() is an tag that corresponds to <p></p> (paragraph)
      p("hello"),
      
      # hr() breaks to a new style
      hr(),
      
      ## DATA SOURCE ## 
      # h4() is an tag that corresponds to header levels (the size of the header)
      # a(, href) corresponds to <a href></a> etc. to create reference to a link.
      h4("Source Data"),
      p("The ", 
        a("Productivity Prediction Garment Employees Dataset", 
          href = "http://archive.ics.uci.edu/ml/datasets/Productivity+Prediction+of+Garment+Employees"),
        " dataset courtesy of the UCI Machine Learning Repository. 
              Please visit the link for source and data dictionary.")
    ), # end sidebar panel
    
    # MAIN # 
    mainPanel(
      "world"
    ) # end main
  ) # end sidebar
) # end ui

##########
# SERVER #
##########

# Since we are just printing text to the page, none of the inputs need 
# to be transformed - so this is currently empty 
# input - operates on the variables from ui
# output - returns output that may feed back to the ui
server <- function(input, output) {
  
}

#######
# RUN #
#######

shinyApp(ui = ui, server = server)
