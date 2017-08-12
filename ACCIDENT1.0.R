library(readr)
library(plyr)
library(pander)
library(ggplot2) 
library(LICORS) 
library(foreach) 
library(mosaic) 
library(gridExtra) 
library(wordcloud)
accident <- read_csv("https://raw.githubusercontent.com/andi228/testproj/master/bigaccidents.csv")
### Get rid of columns with tons of missing values
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
kmeans_cl1<- kmeans(cl1_new, 2, nstart = 50)
print(apply(kmeans_cl1$centers,1,function(x) colnames(cl1_new)[order(x, decreasing=TRUE)[1:9]]))
names(kmeans_cl1)
kmeans_cl1$size

#K means for k=3
set.seed(1)
kmeans_cl3<- kmeans(cl1_new, 3, nstart = 50)
print(apply(kmeans_cl3$centers,1,function(x) colnames(cl1_new)[order(x, decreasing=TRUE)[1:9]]))
names(kmeans_cl1)
kmeans_cl3$size
#1                               2                      3                     
#[1,] "ON_ROAD_SPEED_LIMIT"  "NBR_OF_FATALITIES"    "NBR_OF_VEHICLES"     
#[2,] "PRCNT_HEAVY_CMMRCL"   "NBR_OF_PEDESTRIANS"   "NBR_OF_TRAFFIC_UNITS"
#[3,] "NBR_OF_DISABLED"      "NBR_OF_DISABLED"      "NBR_OF_OCCUPANTS"    
#[4,] "NBR_OF_PEDESTRIANS"   "NBR_OF_TRAFFIC_UNITS" "ANNUAL_ADT"          
#[5,] "NBR_OF_FATALITIES"    "PRCNT_HEAVY_CMMRCL"   "NBR_OF_DISABLED"     
#[6,] "ANNUAL_ADT"           "NBR_OF_VEHICLES"      "NBR_OF_PEDESTRIANS"  
#[7,] "NBR_OF_OCCUPANTS"     "ON_ROAD_SPEED_LIMIT"  "NBR_OF_FATALITIES"   
#[8,] "NBR_OF_VEHICLES"      "NBR_OF_OCCUPANTS"     "ON_ROAD_SPEED_LIMIT" 
#[9,] "NBR_OF_TRAFFIC_UNITS" "ANNUAL_ADT"           "PRCNT_HEAVY_CMMRCL"  
