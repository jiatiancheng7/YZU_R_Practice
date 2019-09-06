x=read.csv("AirPollution.csv",header=T)
# init
rn = unique(x[,2])
cn = unique(x[,3])
out = array(0,c(length(rn),length(cn)))
#set name
colnames(out) = cn
rownames(out) = rn
#calculate
for (i in 1:length(rn))
{
  for (j in 1:length(cn))
  {
    num = x[x[,2]==rn[i]&x[,3]==cn[j],4:27]
    out[i,j] = mean(as.vector(as.numeric(t(num))),na.rm = T)
  }
}

library(xlsx)
filelist = list.files(path = "AQData")
for (i in 1:length(filelist))
{
  fn = paste("AQData/",filelist[i],sep="")
  out = substring(filelist[i],1,nchar(filelist[i])-5)
  out =paste(out,".csv",sep="")
  x = read.xlsx(fn,sheetName="data")
  write.table(x,file=out,col.names = T,row.names = F,sep=",")
}

#Q1
data = read.csv("311.csv",header = T)
out = data %>%
  mutate(Freq=1) %>%
  group_by(Complaint.Type) %>%
  summarise(Total=sum(Freq))
barplot(out$Total,names.arg = out$Complaint.Type)

table(data$Complaint.Type)

#Q2
out = data %>%
  mutate(Freq=1) %>%
  filter(Complaint.Type=="Blocked Driveway") %>% 
  group_by(Borough) %>%
  summarise(Total=sum(Freq))
barplot(out$Total,names.arg = out$Borough)
  
data %>% 
  filter(Complaint.Type=="Blocked Driveway") %>% 
  select(Borough) %>% 
  table()


