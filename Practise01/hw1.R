x=read.csv("finalscore_exercise.csv",sep=",",header=T)
library(plotly)
p=plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width,type ="scatter",mode = 'markers')


data = read.csv("name_english.csv",sep=",", header=F)
colnames(data) = c("sex", "year", "name", "freq")
data = data %>%  
  group_by(name) %>%  
  summarise(Freqsum = sum(freq, na.rm=TRUE))
barplot(data$Freqsum, names.arg=data$name)

cc=c("1","2","3","4","5","6")
x = sample(1:6, 150, replace=T, prob=c(0.1, 0.1, 0.2, 0.2, 0.2, 0.2))
x=table(x)
rownames(x)=c("num","freq")
barplot(data$Freqsum, names.arg=data$name)
pie(x,labels=cc)


x=read.csv("Taiwan.csv",sep=",")
x=t(x)
cbind(x[1,])
x=x %>%
  transform()
boxplot(x)




library(rowr)
x=read.csv("Taipei_LAND_A.csv")
dis=unique(x[,1])
out=NULL
for (i in 1:length(dis)) {
  tmp = x[which(x[,1]==dis[i]),2]
  out = cbind.fill(out,tmp,fill = NA)
  
  
}

out = out[,-1]
colnames(out) = dis
boxplot(out)


  