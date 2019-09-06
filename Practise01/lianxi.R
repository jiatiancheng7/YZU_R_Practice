library("dplyr")


Name = c("Date","Hour","AQI","O3(8h)","O3(1h)","PM2.5","PM10","CO","SO2","NO2")

#剔除out of range

data = read.csv("hw1_case1.csv",sep=",",header=T)
datas = data[1:14,]
X13 <- as.numeric(as.character(datas[,17]))
X16 <- as.numeric(as.character(datas[,20]))
X18 <- as.numeric(as.character(datas[,22]))
datas1 = datas[,1:16]
datas2 = datas[,18:19]
datas3 = datas[,21]
datas4 = datas[,23:27]
ap = cbind(datas1,X13,datas2,X16,datas3,X18,datas4)

for (m in 1:14) {
  for (n in 4:27) {
    if(!is.na(ap[m,n])){
      if(ap[m,n] > 300){
        ap[m,n] = NA
      }
    }
    
  }
}





#填充
tcfun=function(xa,ya,xb,yb,x){
  y=ya+(yb-ya)*((x-xa)/(xb-xa))
  return(y)
}










for (i in 1:14) {
  nn=NULL
  nn=which(is.na(ap[i,]))
  
  for (j in nn) {
    
    xxd=min(nn)-1
    dd=max(nn)+1
    
    ap[i,j]=tcfun(xxd,ap[i,xxd],dd,ap[i,dd],j)
    
  }
  
}






#定义函数（ifin：当前时间；pjxs:取多长时间的平均；type1：种类）########
qqpj=function(ifin,pjxs,type1){
  
  mean_co=ap %>% filter(TYPE==type1)
  x=NULL 
  if(ifin < pjxs-2){
    
    for (i in 1:pjxs-ifin-1) {
      x[i]=mean_co[1,(28-i)]
    }
    i=i+1
    for (j in 0:ifin) {
      x[i+j]=mean_co[2,(4+j)]
    }
    return(mean(x))
    
  }else{
    for (j in 0:ifin) {
      i=1
      x[i+j]=mean_co[2,(4+j)]
    }
    return(mean(x))
  }
}


################求取平均值############################
O3mean=NULL
for (ii in 0:23) {
  O3mean[ii+1]=qqpj(ii,8,"O3")
}

COmean=NULL
for (ii in 0:23) {
  COmean[ii+1]=qqpj(ii,8,"CO")
}

PM2.5mean=NULL
for (ii in 0:23) {
  PM2.5mean[ii+1]=qqpj(ii,24,"PM2.5")
}

PM10mean=NULL
for (ii in 0:23) {
  PM10mean[ii+1]=qqpj(ii,24,"PM10")
}

NO2mean = t(ap[10,4:length(ap[10,])])

SO2mean = t(ap[14,4:length(ap[10,])])


O31mean=NULL
mean_tem=NULL
mean_tem=ap %>% filter(Date=="2018/01/02" & TYPE=="O3")
for (jj in 1:24) {
  O31mean[jj]=mean_tem[1,3+jj]
}

#####################定义IAQI函数#####################################


IAQI=function(IAH,IAL,BPH,BPL,CP){
  IAQIp=((IAH-IAL)/(BPH-BPL))*(CP-BPL)+IAL
  return(IAQIp)
}


####################计算相应的IAQI############################################
O3AQ=NULL
for (i in 1:24) {
  O3AQ[i]=IAQI(50,0,0.054,0.000,(O3mean[i]*0.001))
}

O31AQ=0
#for (i in 1:24) {
#  O31AQ[i]=IAQI(50,0,0.054,0.000,(O31mean[i]*0.001))
#}

PM2.5AQ=NULL
for (i in 1:24) {
  if(PM2.5mean[i]<15.4){
    PM2.5AQ[i]=IAQI(50,0,15.4,0.0,PM2.5mean[i])
  }else{
    PM2.5AQ[i]=IAQI(100,51,35.4,15.5,PM2.5mean[i])
  }
}

PM10AQ=NULL
for (i in 1:24) {
  if(PM10mean[i]<55){
    PM10AQ[i]=IAQI(50,0,54,0,PM10mean[i])
  }else{
    PM10AQ[i]=IAQI(100,51,125,55,PM10mean[i])
  }
}

COAQ=NULL
for (i in 1:24) {
  COAQ[i]=IAQI(50,0,4.4,0.0,COmean[i])
}

SO2AQ=NULL
for (i in 1:24) {
  SO2AQ[i]=IAQI(50,0,35,0,SO2mean[i])
}

NO2AQ=NULL
for (i in 1:24) {
  NO2AQ[i]=IAQI(50,0,53,0,NO2mean[i])
}

###################################################################


IAQIM=cbind(O3AQ,O31AQ,PM2.5AQ,PM10AQ,COAQ,SO2AQ,NO2AQ)


AQI=NULL
for (i in 1:24) {
  AQI[i]=max(IAQIM[i,])
}
Date=array(data="2018/01/02",c(24,1))
Hour=c(00:23)
AQIM=cbind(Date,Hour,AQI,IAQIM)
AQIM[,5]="-"
colnames(AQIM)=paste(Name)



#####################################################################


write.table(AQIM, file="out.csv", row.names=F, col.names=T, sep=",")

