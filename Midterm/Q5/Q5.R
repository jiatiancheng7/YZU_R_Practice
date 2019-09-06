library(dplyr)
library(plotly)
data = read.csv("Q5.csv",header=T,sep=",")
data[,1] = ordered(data[,1], levels = data[,1])
p = plot_ly(data, x = ~Date, y = ~data[,2], type = 'scatter', mode = 'lines',name = 'West Texas') %>%
  add_trace(y = ~data[,3], name = 'Dubai', mode = 'lines') %>%
  
  add_trace(y = ~data[,4], name = 'Brent Crude', mode = 'lines')%>%
  layout(title = "",
         xaxis = list(title = ""),
         yaxis = list (title = ""))

p


