
#library(data.table)
ifelse("ggplot2" %in% rownames(installed.packages()),library("ggplot2"), install.packages("ggplot2"))
ifelse("dplyr" %in% rownames(installed.packages()),library("dplyr"), install.packages("dplyr"))
ifelse("gridExtra" %in% rownames(installed.packages()),library("gridExtra"), install.packages("gridExtra"))
ifelse("randomForest" %in% rownames(installed.packages()),library("randomForest"),
       install.packages("randomForest"))
ifelse("ggmap" %in% rownames(installed.packages()),library("ggmap"), install.packages("ggmap"))
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1555/R", getOption("repos"))))
library(h2o)


#Programmatically download and load into your favorite analytical tool the trip data for September 2015.
setwd('path')
ptm <- proc.time()
green <- read.csv("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv")
proc.time() - ptm
cat("Downloadin the data took 7.5 mins")

#Report how many rows and columns of data you have loaded.
cat('there are', nrow(green), 'rows and there are',ncol(green),'columns')


#Plot a histogram of the number of the trip distance ("Trip Distance").
ggplot(green, aes(x=Trip_distance)) + 
  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#the graph doesn't help much in infering information of the trip distance

ggplot(green, aes(x=Trip_distance)) + 
  geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  xlim(0, 15)

#limiting the x-axis gives us better information about the distribution of trip distance
#the trip distance has a right skewed distribution (positive skewed distribution) with mean trip distance of
#2.93 miles and median of 2 miles


#Report mean and median trip distance grouped by hour of day

#extracting hour of the day from pickup datetime
green$hours <- as.numeric(format(strptime(green$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"),"%H"))

hourly_trip_distance <- data.frame(green%>%
                                     group_by(hours)%>%
                                     summarise(mean_trip_dist = mean(Trip_distance, na.rm = TRUE),
                                               median_trip_dist = median(Trip_distance, na.rm = TRUE)))

p1 <- ggplot(hourly_trip_distance, aes(x=hours, y=mean_trip_dist)) + geom_bar(stat = "identity") 
p2 <- ggplot(hourly_trip_distance, aes(x=hours, y=median_trip_dist)) + geom_bar(stat = "identity") 
grid.arrange(p1, p2, ncol=1, nrow =2)

#Long distances are usually taken in the early hours of the day around 5am, 6am

#finding which cabs dropped off at JFK. Using the latitude and longitude of jfk to find out
#which ones attributed to JFK

green$jfk_drop_airtport <- ifelse(green$Dropoff_latitude < 40.660 & green$Dropoff_latitude > 40.635 
                                  & green$Dropoff_longitude < -73.775 & green$Dropoff_longitude > -73.815,
                                  1,0)

queens <- get_map(location = "Queens", zoom = 11, maptype = "roadmap")
ggmap(queens)+ geom_point(data = green[green$jfk_drop_airtport==1,], aes(x = Dropoff_longitude, y = Dropoff_latitude), 
                          colour = 'red', size = 1) + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) 

cat('There are',nrow(green[green$jfk_drop_airtport==1,]),'drop offs at JFK airport')

green$time_travelled <- as.numeric(difftime(strptime(green$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"), 
                                            strptime(green$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"), 
                                            units = "mins"))
green$weekday <- weekdays(as.Date(green$lpep_pickup_datetime))

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data.frame(green%>%group_by(jfk_drop_airtport)%>%
             summarise(avg_Fare = mean(Fare_amount),
                       busy_hours = mode(hours),
                       busy_week = mode(weekday),
                       Tip_amount = mean(Tip_amount)))


#Build a derived variable for tip as a percentage of the total fare.
green$tip_prc <- ifelse(green$Tip_amount==0.00 | green$Total_amount==0.00 , 0.00,
                        round(green$Tip_amount/green$Total_amount,4))
green <- green[!is.na(green$tip_prc),]
plot(green$Total_amount,green$Tip_amount)

cat("Average tip percentage", round((sum(green$Tip_amount, na.rm=T)/sum(green$Total_amount,na.rm=T)),5)*100,"%")
hist(green$tip_prc)

#Build a predictive model for tip as a percentage of the total fare. 
#Use as much of the data as you like (or all of it). We will validate a sample.

  
cor(green[,c(10,11,12,13,14,15,16,18,19)])

str(green)
green$VendorID <- factor(green$VendorID)
green$RateCodeID <- factor(green$RateCodeID)
green$Payment_type <- factor(green$Payment_type)
green$Trip_type <- factor(green$Trip_type)
green$jfk_drop_airtport <- factor(green$jfk_drop_airtport)
green$weekday <- factor(green$weekday)

################preprocessing##########

#checking for missing values
green <- green[!complete.cases(green),]
green <- green[!is.na(green$Trip_type),]
green <- green[!green$RateCodeID==99,]

#mahalanobis distance to find outlier
green_out <- green[,c(10,11,12,13,14,15,16,18,19,24,26)]
m.dist.order <- order(mahalanobis(green_out, colMeans(green_out), cov(green_out)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(green_out))
is.outlier[m.dist.order[1:2]] <- TRUE # Mark as outliers the 2 most extreme points
col <- is.outlier + 1
green$outlier <- col
green <- green[green$outlier==1,]
green$outlier<-NULL
rm(green_out,col,is.outlier,m.dist.order)

#tip amount, toll amount, total_amount being less than 0
green <- green[!green$Tip_amount < 0,]
green <- green[!green$Tolls_amount < 0,]
green <- green[!green$Total_amount < 0,]

#deleting data when there is fare amount but no travel time
green <- green[!(green$time_travelled == 0 & green$Fare_amount != 0),]
#deleting data when there is no fare amount charged but there is time travelled
green <- green[!(green$time_travelled != 0 & green$Fare_amount == 0),]

#deleting ambigious trip  distances
green <- green[!green$Trip_distance %in% c(112.60,603.10),]
#delete where tip amount is greater than fare amount
green <- green[green$Tip_amount < green$Fare_amount,]

#deleting the fault outliers from time travelled 
head(green[green$time_travelled > 350,])
green <- green[green$time_travelled < 350,]
cat("Initial number of records are 1494926, and after cleaning there are", nrow(green),"records")


#creating training and testing for linear model
smp_size <- floor(0.8* nrow(green))
set.seed(123)
train_ind <- sample(seq_len(nrow(green)), size = smp_size)
lm_train <- green[train_ind, ]
lm_test <- green[-train_ind, ]

lm_model <- lm(tip_prc ~ VendorID+RateCodeID+Passenger_count+
                 Extra+Tolls_amount+
                 Total_amount+Payment_type+Trip_type+hours+jfk_drop_airtport+time_travelled+weekday, 
               data = lm_train)
summary(lm_model) #Adjusted R-squared:  0.6845 
lm_model_predictions <- predict(lm_model,lm_test)
lm_model_rmse <- sqrt(mean((lm_model_predictions-lm_test$tip_prc)^2)) 
lm_model_rmse #0.04743373
lm_model_mae <- mean(abs(lm_test$tip_prc-lm_model_predictions)) 
lm_model_mae #0.02671113


#randomForest
#creating training and testing data sets
rf_train<- lm_train[,c("VendorID","Store_and_fwd_flag","RateCodeID","Passenger_count",
                       "Extra","Tolls_amount","improvement_surcharge",
                       "Total_amount","Payment_type","Trip_type","hours","jfk_drop_airtport",
                       "time_travelled","weekday","tip_prc")]
rf_test<- lm_test[,c("VendorID","Store_and_fwd_flag","RateCodeID","Passenger_count",
                     "Extra","Tolls_amount","improvement_surcharge",
                     "Total_amount","Payment_type","Trip_type","hours","jfk_drop_airtport",
                     "time_travelled","weekday","tip_prc")]


h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "8G")    ## specify the memory size for the H2O cloud
h2o.removeAll()

rf_train.hex <- as.h2o(rf_train)
rf_test.hex <- as.h2o(rf_test)

rf1 <- h2o.randomForest(training_frame = rf_train.hex,
                        x=1:14,y=15,                      
                        ntrees = 200,                  
                        score_each_iteration = T)
summary(rf1) 
rf_model_predictions <- predict(rf1,rf_test.hex)
rf_model_rmse<-sqrt(mean((as.data.frame(rf_model_predictions)$predict-as.data.frame(rf_test.hex)$tip_prc)^2)) 
rf_model_rmse #0.03210149
rf1@model$variable_importances

#Xgboost
gbm <- h2o.gbm(training_frame = rf_train.hex,
               x=1:14,y=15,                      
               ntrees = 200, learn_rate = 0.2, max_depth = 8)

summary(gbm) 
gbm_model_predictions <- predict(gbm,rf_test.hex)
gbm_model_rmse<-sqrt(mean((as.data.frame(gbm_model_predictions)$predict-as.data.frame(rf_test.hex)$tip_prc)^2)) 
gbm_model_rmse #0.03119293
gbm@model$variable_importances


#Option A: Distributions
green$speed <- ifelse(green$Trip_distance==0.00 | green$time_travelled==0.00 ,0.00,
                      round((green$Trip_distance/green$time_travelled)*60,4))

green$day <- as.numeric(format(strptime(green$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"),"%d"))
green$week_num <- ifelse(green$day <=7,1,
                         ifelse(green$day <= 14,2,
                                ifelse(green$day <=21,3,4)))
#labour day on 5th september

data.frame(green%>%group_by(week_num)%>%
             summarise(average_speed = mean(speed)))

speed_time <- data.frame(green%>%group_by(hours)%>%
                           summarise(average_speed = mean(speed)))

ggplot(data=speed_time, aes(x=hours,y=average_speed))+ geom_line()+
  scale_x_continuous(breaks = seq(0,23,2))



