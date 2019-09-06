library(dplyr)
library(leaflet)

data = read.csv("Q4.csv",header = T,sep = ",")
data = data %>%
  select("Complaint.Type","Resolution.Description","Latitude","Longitude")
icon1 <- makeIcon(
  iconUrl = "red-dot.png",
  iconWidth = 30, iconHeight = 30,
)

icon2 <- makeIcon(
  iconUrl = "green-dot.png",
  iconWidth = 30, iconHeight = 30,
)
icon3 <- makeIcon(
  iconUrl = "yellow-dot.png",
  iconWidth = 30, iconHeight = 30,
)
icon4 <- makeIcon(
  iconUrl = "blue-dot.png",
  iconWidth = 30, iconHeight = 30,
)

UserIcon <- icons(
  iconUrl = ifelse(data$Complaint.Type == "Blocked Driveway",
                   "red-dot.png",
                   "blue-dot.png"
  ),
  iconWidth = 30, iconHeight = 30
)

m = leaflet(data = data ) %>% 
  addTiles()%>%
  addMarkers(data=data,~Longitude, ~Latitude, icon = 
               icon4,popup=paste("Resolution Description",data$Resolution.Description,sep=":")) %>%
  addMarkers(data=data[data$Complaint.Type=="Blocked Driveway",],~Longitude, ~Latitude, icon = 
               icon1,popup=paste("Resolution Description",data$Resolution.Description,sep=":"))%>%
  addMarkers(data=data[data$Complaint.Type=="Street Condition",],~Longitude, ~Latitude, icon = 
               icon2,popup=paste("Resolution Description",data$Resolution.Description,sep=":"))%>%
  addMarkers(data=data[data$Complaint.Type=="Illegal Parking",],~Longitude, ~Latitude, icon = 
               icon3,popup=paste("Resolution Description",data$Resolution.Description,sep=":"))


m