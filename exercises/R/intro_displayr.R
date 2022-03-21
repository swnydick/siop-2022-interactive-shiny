library(tools)
library(htmltools)


# Function to extract the name of the file - where intro and file names match
extract_appname <- 
  function(){
    file_path_sans_ext(basename(rstudioapi::getActiveDocumentContext()$path))
  }

# Function to read in and report html text in the viewer
# Assumes you are in the exercises directory - the app location
intro_displayr <- 
  function(apppath       = NULL, 
           file_location = 'intros'){
    
    # get app path if null
    if(is.null(apppath)){
      apppath <- extract_appname()
    }
    
    # get intro file
    intropath <- file.path(file_location, 
                           paste0(apppath, '.txt'))
    intro     <- readLines(intropath)
    
    # Display html intro
    html_print(
      div(
        HTML(
          intro
        )
      )
    )
  }

