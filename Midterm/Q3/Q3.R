student = read.csv("student.csv",header = FALSE,sep = ",")
class = read.csv("class.csv",header = FALSE,sep = ",")
score = read.csv("score.csv",header = FALSE,sep = ",")
names(student)=c("studentId","sex","email")
names(class)=c("course","credits")
names(score)=c("studentId","course","score")



y=data.frame(Freq = c(1))
student = cbind(student,y)
student %>%  
  group_by(sex)  %>%
  summarise(Sexsum = sum(Freq, na.rm=TRUE))



new = merge(x=student,y=score, by="studentId",all=TRUE)
new %>%  
  filter(course=="AA101")   %>%
  select(Freq) %>% 
  sum()



new %>%  
  filter(course=="AA101" & score<60)   %>%
  select(Freq) %>% 
  sum()



s1000001score = score %>%
  filter(studentId=="s1000001")
s1000001class = merge(x=s1000001score,y=class, by="course",all=TRUE)
s1000001class %>%  
  filter(score>60) %>%
  select(credits) %>% 
  sum()


new %>% 
  group_by(course, sex) %>% 
  summarise(fs = sum(Freq))