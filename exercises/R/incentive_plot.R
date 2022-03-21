##################
# INCENTIVE PLOT #
##################

incentive_plot <- function(data, dept, team = NULL, trendline = FALSE){
  
  trendline_txt <- c("", "Trendline drawn")[trendline + 1]
  
  # One color if no teams selected 
  if(is.null(team)){
    g <- ggplot(data    = data,
                mapping = aes(x     = incentive, 
                              y     = actual_productivity)) + 
      geom_point(color = '#006550')
  } else{
    g <- ggplot(data    = data,
                mapping = aes(x     = incentive, 
                              y     = actual_productivity,
                              color = team)) + 
      geom_point(alpha = 0.7)
  }

  g <- g + 
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
  
  if (!is.null(team)) {
    dd <- dd[dd$team %in% team, ]
  }
  
  return(dd)
}
