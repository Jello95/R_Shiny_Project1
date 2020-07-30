library(shiny)

img1 <- 'https://upload.wikimedia.org/wikipedia/commons/f/f5/Photos_NewYork1_032.jpg'
img2 <- 'https://upload.wikimedia.org/wikipedia/commons/3/3e/NYSE127.jpg'
img3 <- 'https://upload.wikimedia.org/wikipedia/commons/9/98/NYSE_opening_bell.jpg'
vid1 <- 'https://www.youtube.com/embed/_-Zqz75r9PQ'

fluidPage(
  titlePanel(
    'Efficient Frontier and Asset Allocation'
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('Wall Street', img(src = img1, width = '100%')),
      tabPanel('Trading Floor', img(src = img2, width = '100%')),
      tabPanel('Opening Bell', img(src = img3, width = '100%'))),
    h3('Purpose and Summary'),
    p('In modern portfolio theory, the efficient frontier (or portfolio frontier) is 
        an investment portfolio which occupies the efficient parts of the risk-return spectrum. 
        Formally, it is the set of portfolios which satisfy the condition that no other 
        portfolio exists with a higher expected return but with the same standard deviation of 
       return (i.e., the risk). The efficient frontier was first formulated by Harry Markowitz 
       in 1952. (Wikipedia)'),
    p(),
    p(),
    p(),
    selectizeInput(inputID = 'A',
                   label = 'Index1',
                   choices = c(1:6)),
    selectizeInput(inputID = 'B',
                   label = 'Index2',
                   choices = c(1:6)),
    plotOutput('frontier', width = '100%'),
    plotOutput('bargraph', width = '100%'),
    h3('Additional Resources'),
    tags$iframe(src = vid1 , width = '560', height = '315')
  )
)
