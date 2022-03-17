##################
# INCENTIVE PLOT #
##################

incentive_plot <- function(data, dept, team, trendline){
  
  trendline_txt <- c("", "Trendline drawn")[trendline + 1]
  
  g <- ggplot(data    = data,
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

data_team_subset <- function(data, dept, team = NULL){
  
  dd <- data[data$department %in% dept, ]
  
  if (!is.null(tm)) {
    dd <- dd[dd$team %in% team, ]
  }
  
  return(dd)
}
# # FUNCTIONS TO FILTER AND GENERATE INCENTIVE PLOT # 
# data_team_subset <- function(data, 
#                              dept, 
#                              team     = NULL){
#   # Find the department selection 
#   ind <- data$department %in% dept 
#   
#   # Find the team selection if it exists 
#   if(!is.null(team)){
#     where_team <- data$team %in% team
#     ind        <- ind*where_team
#   }
#   
#   # return data at the right indices 
#   data[as.logical(ind), ]
# }

# # INCENTIVE PLOT FUNCTION #
# # After this app - you can find this in the 'examples/R' folder 
# incentive_plot <- function(data, 
#                            dept, 
#                            team    = NULL, 
#                            show_lm = FALSE){
#   
#   # NEW DATA # 
#   # TODO - added this from the beginning - but should just have called it
#   data <- data_team_subset(data = data, 
#                            dept = dept, 
#                            team = team)
#   
#   # PLOT # 
#   
#   # create initial plot 
#   # Add team color if it exists 
#   if(!is.null(team)){
#     p <- ggplot(data, aes(x         = incentive, 
#                           y         = actual_productivity, 
#                           color     = team))
#   } else{
#     p <- ggplot(data, aes(x     = incentive, 
#                           y     = actual_productivity), 
#                 color = '#006550')
#   }
#   
#   # Add trendline if asked 
#   if (show_lm) {
#     p <- p + geom_smooth(formula = y ~ x, 
#                          method  = lm, 
#                          alpha   = 0.15)
#   }
#   
#   p +
#     geom_point() + 
#     theme_bw() +
#     labs(title    = paste("Department:", dept), 
#          subtitle = "Incentive vs Actual Productivity") 
# }
