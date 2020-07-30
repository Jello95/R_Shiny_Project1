library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

function(input, output){
  
  output$frontier <- renderPlot(
    table %>%
      ggplot(aes(x = stdev, y = expreturn, colour = weight)) + 
      geom_point() +
      labs(title = 'Efficient Frontier', 
           x = 'Standard Deviation',
           y = 'Expected Return') +
      theme_bw()
  )
  
  output$bargraph <- renderPlot(
    allocation %>%
      ggplot() +
      geom_bar(aes(x = Year, y = newstockval, fill = 'stocks'),
             stat = 'identity', width = 0.3, position = 'dodge') +
      geom_bar(aes(x = Year, y = newbondval, fill = 'bonds'),
            stat = 'identity', width = 0.3, position = position_nudge(x = 0.3)) +
      labs(title = 'Historical Performance',
           y = 'Returns') +
      theme_bw()
  )
  
  output$statistics <- renderTable(newalloc)
  output$allnumbers <- renderTable(table)
}