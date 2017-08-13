library(dplyr)
library(unbalanced)
library(randomForest)
library(caret)
library(tidyr)

library(rjson)
library(RCurl)
library(jsonlite)
library(ggmap)
library(readr)
library(plyr)
library(pander)
library(ggplot2) 
library(LICORS) 
library(foreach) 
library(mosaic) 
library(gridExtra) 
library(wordcloud)
library(data.table)
library(randomForest)

accident <- read_csv("https://raw.githubusercontent.com/andi228/testproj/master/bigaccidents.csv")
############## Get rid of columns with tons of missing values
accident$MEDIAN_TYPE <- NULL
accident$MEDIAN_WIDTH<- NULL
accident$NO_PASSING<- NULL
accident$RUMBLE_STRIPS<- NULL
accident$SHIDER_WIDTH_INSIDE<- NULL
accident$DEGREE_OF_CURVE<-NULL
accident$ROAD_SURFTYPE_OTHER<-NULL
accident$DAY_OF_ACCIDENT<-NULL
accident$ACC_CLASS_OTHER_OBJECT<-NULL
accident$COLLISION_W_OTHER_VEH_FHE<-NULL
accident$COLLISION_W_OTHER_VEH_MHE<-NULL
accident$COLLISION_W_VEH_OTHER<-NULL
accident$FIXED_OBJECT_TYPE_FHE<-NULL
accident$FIXED_OBJECT_TYPE_MHE<-NULL
accident$FIXED_OBJECT_OTHER<-NULL
accident$ACCIDENT_LOCATION_OTHER<-NULL
accident$INTERSECTION_TYPE<-NULL
accident$ROUNDABOUT_NBR<-NULL
accident$WEATHER_COND_OTHER<-NULL
accident$ANIMAL_TYPE<-NULL
accident$SHLDR_WIDTH_INSIDE<-NULL
accident$DAY_ARRIVED<-NULL

######### Find out the location
address <- accident[,24:25]

accident$ORIG_ROUTE_PREFIX <- revalue(accident$ORIG_ROUTE_PREFIX,
                                      c("I"="Interstate", "U"="US", "K"="Kansas"))

accident$addID <-paste(accident$ORIG_ROUTE_PREFIX, accident$ORIG_ROUTE_NBR, sep = " ")

### Define a function to find the longtitude and latitude
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}


accident$lat<-NULL
accident$lon<-NULL
accident$geographyadd<-NULL
accident$long<-NULL
new_acc=na.omit(accident)
new_ac<-accident[!(length(accident$addID)== 0),]

new_ac$lat = ""
new_ac$lon = ""

for (i in 1:length(new_ac$addID[length(new_ac$addID) > 0])){
 
    if(i %% 5 == 0) Sys.sleep(5)
 
      add1<-geocodeAdddress(new_ac$addID[i])
      new_ac$lat[i] <- add1[1]
      new_ac$lon[i] <- add1[2]
 
}

View(new_ac)

## Clustering without transpose

### Assign numberical values to character string column
accident$ACCIDENT_SEVERITY <- revalue(accident$ACCIDENT_SEVERITY,
                                      c("F"="2", "I"="1", "N"="0"))
### Create a new table to use Clustering
cl1 = subset(accident, select = c(NBR_OF_TRAFFIC_UNITS,NBR_OF_VEHICLES,NBR_OF_OCCUPANTS,NBR_OF_PEDESTRIANS,NBR_OF_FATALITIES,NBR_OF_DISABLED,ANNUAL_ADT,PRCNT_HEAVY_CMMRCL,ON_ROAD_SPEED_LIMIT))
cl1_new = scale(cl1, center=TRUE, scale=TRUE)
# Get rid of the missing values
cl1_new <- na.omit(cl1_new)


## K means clustering
list= rep(NA, dim(cl1_new)[2]-1)
list2= rep(NA, dim(cl1_new)[2]-1)
set.seed(1)
for ( i in 2:dim(cl1_new)[2]){
  list[i-1]=kmeans(cl1_new, i , 
                   nstart = 50)$betweenss/ kmeans(cl1_new, i , 
                                                  nstart = 50)$tot.withinss 
  (dim(cl1_new)[1]-i)/(i-1)
  list2[i-1]=kmeans(cl1_new,i , 
                    nstart = 50)$tot.withinss
}
par(mfrow=c(1,2))
plot(list ~ c(2:9), type='b', xlab = 'number of K', ylab = 'CH(K)')
plot(list2 ~ c(2:9), type='b',xlab='number of K',ylab='W(k)')

#There are 9 variables in the cl1_new dataset, 
#CH(K) (Calinski-Harabasz criterion) and W(K) (total within-cluster sum of squares) at different k ranging from k=2 to k=9
# while holding the numebr of centers to be 50. It is found that the maximum CH is at k=9 and the minimum W is at k=9.
# In the following steps, we tried to answer this question by subjectively evaluating categories contained within each cluster.

#K means for k=2
set.seed(1)
kmeans_cl2<- kmeans(cl1_new, 2, nstart = 50)
print(apply(kmeans_cl2$centers,1,function(x) colnames(cl1_new)[order(x, decreasing=TRUE)[1:9]]))
names(kmeans_cl2)
kmeans_cl2$size

#K means for k=3
set.seed(1)
kmeans_cl3<- kmeans(cl1_new, 3, nstart = 50)
print(apply(kmeans_cl3$centers,1,function(x) colnames(cl1_new)[order(x, decreasing=TRUE)[1:9]]))
names(kmeans_cl1)
kmeans_cl3$size
#    1                               2                      3                     
#[1,] "ON_ROAD_SPEED_LIMIT"  "NBR_OF_FATALITIES"    "NBR_OF_VEHICLES"     
#[2,] "PRCNT_HEAVY_CMMRCL"   "NBR_OF_PEDESTRIANS"   "NBR_OF_TRAFFIC_UNITS"
#[3,] "NBR_OF_DISABLED"      "NBR_OF_DISABLED"      "NBR_OF_OCCUPANTS"    
#[4,] "NBR_OF_PEDESTRIANS"   "NBR_OF_TRAFFIC_UNITS" "ANNUAL_ADT"          
#[5,] "NBR_OF_FATALITIES"    "PRCNT_HEAVY_CMMRCL"   "NBR_OF_DISABLED"     
#[6,] "ANNUAL_ADT"           "NBR_OF_VEHICLES"      "NBR_OF_PEDESTRIANS"  
#[7,] "NBR_OF_OCCUPANTS"     "ON_ROAD_SPEED_LIMIT"  "NBR_OF_FATALITIES"   
#[8,] "NBR_OF_VEHICLES"      "NBR_OF_OCCUPANTS"     "ON_ROAD_SPEED_LIMIT" 
#[9,] "NBR_OF_TRAFFIC_UNITS" "ANNUAL_ADT"           "PRCNT_HEAVY_CMMRCL"  

cl_freq=cl1_new/rowSums(cl1_new)
require("RColorBrewer")
#### plot them
# cluster1
wordcloud(colnames(cl_freq),
          kmeans_cl3$centers[1,],min.freq=0,
          max.words=10, 
          colors = rainbow(9),ordered.colors = TRUE) 
# cluster2
wordcloud(colnames(cl_freq),
         kmeans_cl3$centers[2,],min.freq=0,
         max.words=10, 
         colors = rainbow(9),ordered.colors = TRUE) 
# cluster3
wordcloud(colnames(cl_freq),
          kmeans_cl3$centers[3,],min.freq=0,
          max.words=10, 
          colors = rainbow(9),ordered.colors = TRUE) 


#######Clustering after transposing to find out the relationships
transposed_cl<-t(cl1_new)
View(transposed_cl)

# k means for k=2
set.seed(1)
kmeans_tc2<- kmeans(transposed_cl, 2, nstart = 50)
print(apply(kmeans_tc2$centers,1,function(x) colnames(transposed_cl)[order(x, decreasing=TRUE)[1:9]]))
names(kmeans_tc2)
kmeans_tc2$size
kmeans_tc2$cluster

# k means for k=3
set.seed(1)
kmeans_tc3<- kmeans(transposed_cl, 3, nstart = 50)
print(apply(kmeans_tc2$centers,1,function(x) colnames(transposed_cl)[order(x, decreasing=TRUE)[1:9]]))
names(kmeans_tc3)
kmeans_tc3$size


#### conclusion: transposing doesn't work


###### ignore
getmap <- get_googlemap(center = c(lon =-98.42,lat = 37.63),zoom=11,maptype = "roadmap")


ggmap(getmap,extent = "device",ylab = "lat",xlab = "lon",maprange=FALSE) +
  geom_point(data = rdata3,colour = "darkred", pch=16, cex= 1.5,alpha = 1) +
  stat_density2d(data = rdata3, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),size = 0.01, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.05, 0.15))  +
  theme(legend.position = "none") 
#### ignore above

### There are about 129,133 accidents in which fatalities are not involved.So we are careful about
### getting the balanced traning data from sampling.

cl1_nn=na.omit(cl1)
View(cl1_nn)

set.seed(8)
train = sample(1:nrow(cl1_nn), nrow(cl1_nn)/2) 
cl1nn.train = cl1_nn[train, ]
cl1nn.test = cl1_nn[-train, ] 

set.seed(8)
rf.cl1nn=randomForest(NBR_OF_FATALITIES~.,data=cl1_nn,subset=cl1nn.train,mtry=3,importance=TRUE)
yhat.rf=predict(rf.cl1nn,newdata=cl1_nn[-train, ])
mean((yhat.rf-cl1nn.test)^2)
importance(rf.cl1nn)
varImpPlot(rf.cl1nn)
