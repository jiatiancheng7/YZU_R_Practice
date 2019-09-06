plot(iris[,3:4],main="Main title", xlab="Title of x-axis", ylab="Title of y-axis")
library(plotly)
p = plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width,type ="scatter",mode = 'markers')
p

data = read.csv("exchangerate.csv",header = T,sep=",")
p = plot_ly(data, x = ~date, y = ~rate, type = 'scatter', mode = 'lines')
p

library(dplyr)
data = read.csv("name_english.csv",sep=",", header=F)
colnames(data) = c("sex", "year", "name", "freq")
x = data %>% 
  filter(year==2012) %>%
  group_by(name) %>%
  summarise(Total=sum(freq))
barplot(x$Total,names.arg = x$name)


x = sample(1:6, 150, replace=T, prob=c(0.1, 0.1, 0.2, 0.2, 0.2, 0.2))
A = table(x)
B = cumsum(A)
C = A/150
D = cumsum(C)
out = cbind(A,B,C,D)
colnames(out) = c("frequency","cumulative frequency","relative frequency(%)","cumulative relative frequency(%)")
out
barplot(A)
pie(A)



x = read.csv("Taiwan.csv")
barplot(x[,2],names.arg = x[,1])
barplot(x[,3],names.arg = x[,1])
pie(x[,2],labels = x[,1])
pie(x[,3],labels = x[,1])


p = plot_ly( data = x, x = x[,1], y = x[,2], name = "Taiwan", type = "bar")
p

p = plot_ly( data = x, x = x[,1], y = x[,3],name = "Taiwan",type = "bar")
p

p = plot_ly( data = x, labels  = x[,1], values = x[,2], type = "pie") %>%
     layout(title = 'Taiwan',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

p = plot_ly( data = x, labels  = x[,1], values = x[,3], type = "pie") %>%
  layout(title = 'Taiwan',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

boxplot(iris[,1:4])

library(rowr)
x = read.csv("Taipei_LAND_A.csv")
dist = unique(x[,1]) # A ~ L
out = NULL
for (i in 1:length(dist))
{
  tmp = x[which(x[,1]==dist[i]),2]
  out = cbind.fill(out,tmp, fill = NA)
}
out = out[,-1] # delete 1st column
colnames(out)= dist
boxplot(out)

#plotly ..
x = read.csv("Taipei_LAND_A.csv")
dist = unique(x[,1])
tmp = x[which(x[,1]==dist[1]),2]
p = plot_ly(y = tmp, type = "box", name=dist[1]) 
for (i in 2:length(dist))
{
  tmp = x[which(x[,1]==dist[i]),2]
  p = p %>% add_trace(y = tmp, name=dist[i]) 
}
p