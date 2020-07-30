library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(rsconnect)

allocation <- fread(file = 'returns.csv')

allocation <- allocation %>%
              filter(Year %in% c(1993:2019))

converter <- function(x){
  x = gsub('%', '', x)
  x = as.numeric(x)
  x = x/100
  return (x)
}

allocation <- allocation %>% 
              mutate(newstockval = converter(allocation$`US Stock Market`), 
                     newbondval = converter(allocation$`Total US Bond Market`),
                     newEUROval = converter(allocation$`European Stocks`),
                     newASIAval = converter(allocation$`Pacific Stocks`),
                     newIGval = converter(allocation$`Long-Term Corporate Bonds`),
                     newHYval = converter(allocation$`High Yield Corporate Bonds`)
                     ) %>%
              select(Year, newstockval, newbondval, newEUROval, newASIAval,
                     newIGval, newHYval)

newalloc <- allocation %>%
              summarise(ID = c('USstocks', 'USbonds',
                               'EUstocks', 'APstocks',
                               'IGbonds', 'HYbonds'),
                        ER = c(mean(newstockval), mean(newbondval),
                               mean(newEUROval), mean(newASIAval),
                               mean(newIGval), mean(newHYval)),
                        SD = c(sd(newstockval), sd(newbondval),
                             sd(newEUROval), sd(newASIAval),
                             sd(newIGval), sd(newHYval)))

table <- as.data.frame(seq(1:101))
colnames(table) <- 'weight'
table <- table %>% mutate(weight = (weight - 1)/100)
corr <- -0.8

standard <- function (w, corr, sd1, sd2){
  a = (w * sd1)^2
  b = ((1 - w)*sd2)^2
  c = 2 * w * (1 - w) * sd1*sd2*corr
  value = (a + b + c)^(1/2)
  return (value)
}

expected <- function (w, er1, er2){
  a = w * er1
  b = (1 - w) * er2
  return (a+b)
}

x = input$A
y = input$B

table <- table %>% mutate(stdev = standard(weight, corr, 
                                           newalloc$SD[x], newalloc$SD[y]), 
                          expreturn = expected(weight,
                                               newalloc$ER[x], newalloc$ER[y]))

table <- table %>% mutate(weight = weight*100, stdev = stdev*100,
                          expreturn = expreturn*100)

newalloc <- newalloc %>% mutate(ER = ER * 100, SD = SD * 100)

allocation

allocation <- allocation %>% mutate(newstockval = newstockval * 100,
                                    newbondval = newbondval * 100,
                                    newEUROval = newEUROval * 100,
                                    newASIAval = newASIAval * 100,
                                    newIGval = newIGval * 100,
                                    newHYval = newHYval *100)
