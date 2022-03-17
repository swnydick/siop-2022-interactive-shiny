#########################################
# Cleaning Data                         #
# Produces a clean data file            # 
#                                       #
# Korn Ferry Institute: Automation Team #
# 2022-04-29                            #
#########################################


# libraries 
require(dplyr)
require(stringr)
require(ggplot2)

# read data and fix columns
data <- read.csv(file.path("data", "garments_worker_productivity.csv"))
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

# Write to data folder for use in applications 
# rds will preserve our factor levels
# saveRDS(data, 
#         file.path("data", "garments_worker_productivity_cleaned.rds"))
