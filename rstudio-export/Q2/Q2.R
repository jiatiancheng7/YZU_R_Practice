library(dplyr)
dat = read.table('1.txt',header = FALSE,sep = ' ')
dat2 = read.table('2.txt',header = FALSE,sep = ' ')
dat3 = read.table('3.txt',header = FALSE,sep = ' ')
dat4 = read.table('4.txt',header = FALSE,sep = ' ')
dat5 = read.table('5.txt',header = FALSE,sep = ' ')
dat6 = read.table('6.txt',header = FALSE,sep = ' ')
data=t(cbind(dat,dat2,dat3,dat4,dat5,dat6))
type=unique(data)



x=NULL

for (j in 1:length(type)) {
  count=0
  for (i in 1:length(data)) {
    
   
    if(type[j]==data[i])
      count=count+1
    
  }
  x[j]=count
}
cbind(type,x)
