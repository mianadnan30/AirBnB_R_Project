library(dplyr)
library(tidyverse)
library(rpart)
library(caret)
library(e1071)
library("ggplot2")
#READING THE DATA INTO THE CSV FILEs
data <- read.csv("/Users/macuser/Downloads/listings.csv")
######################  DATA PRE-PROCESSING  ##################
##Using DataExplorer plot_bar function to check frequency

##Check missing data
plot_missing(data)

##Check on correlation of the data.
plot_correlation(data)

summary(data$price)

#Describe the distribution of room_type. 
#Which is the most popular
plot_bar(data)

#Is there a difference in price across room types?
ggplot(data = data) +
  geom_boxplot(mapping =aes( fill=room_type, y=price))
#Is there a difference in price across different neighbourhood groups?
ggplot(data = data) +
  geom_boxplot(mapping = aes(x=neighbourhood_group, y=price))
#Describe the distribution of minimum_nights.
#Is there a relationship between price and 
#minimum_nights? If there is, describe it. 
#If there isn't, explain how you know.

summary(data$minimum_nights)
ggplot(data = data) +
  geom_point(mapping = aes(x=price, y=minimum_nights))

#Describe the distribution of number_of_reviews.
#Is there a relationship between price and 
#number_of_reviews? If there is, describe it. 
#If there isn't, explain how you know.
summary(data$number_of_reviews)
ggplot(data = data) +
  geom_point(mapping = aes(x=price, y=number_of_reviews))
##Is there a relationship between minimum_nights and neighbourhood_group?
##If there is, describe it. If there isn't, explain how you know.
##Is there a relationship between minimum_nights and room_type? 
#If there is, describe it. If there isn't, explain how you know.
summary(data$number_of_reviews)
ggplot(data = data) +
  geom_boxplot(mapping = aes(x=neighbourhood_group, y=minimum_nights))
ggplot(data = data) +
  geom_boxplot(mapping = aes(x=room_type, y=minimum_nights))


#DROPPING THE UNNECESSARY COLOUMNS which have no or fewer 
#impact on the data
data <- select(data,-c("id","name","host_name","last_review","reviews_per_month"))
#CONVERTING THE STRING DATA TO NUMERIC DATA
data$neighbourhood_group <- as.numeric(factor(data$neighbourhood_group))
data$neighbourhood <- as.numeric(factor(data$neighbourhood))
data$room_type <- as.numeric(factor(data$room_type))
#SPLITTING THE DATA INTO TRIANING AND TEST SET
set.seed(100)
XX <- sample(nrow(data), 0.8*nrow(data))
train.dfXX  <- data[XX,]
valid.dfXX <- data[-XX,]
#NORMALIZING THE DATA
norm.values <- preProcess(train.dfXX, method="range")
train.norm.dfXX <- predict(norm.values, train.dfXX)
valid.norm.dfXX <- predict(norm.values, valid.dfXX)

head(train.norm.dfXX)
#FINDING THE TOTAL NULL VALUES IN THE DATA
sum(is.na(data))
#FINDING COORELATION
cor(data$price,data$room_type, method = c("pearson", "kendall", "spearman"))

#Model1 BUILDING A LINEAR REGRESSION MODEL
Model <- train(price ~ neighbourhood_group+latitude+longitude+room_type+
                 minimum_nights+number_of_reviews+calculated_host_listings_count+
                 availability_365, method= 'lm',data = train.dfXX)
predModel <- predict(Model,
                     valid.dfXX)
actuals_preds <- data.frame(cbind(actuals=valid.dfXX$price, 
                                  predicteds=predModel))
head(actuals_preds)
ModelAccuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
ModelAccuracy

#Model2 BUILDING A MODEL FOR DECISION TREE
Model <- rpart(price ~ neighbourhood_group+neighbourhood+latitude+longitude+room_type+
                 minimum_nights+number_of_reviews+calculated_host_listings_count+
                 availability_365,data = train.dfXX)
predModel <- predict(Model,
                     valid.dfXX)
actuals_preds <- data.frame(cbind(actuals=valid.dfXX$price, 
                                  predicteds=predModel))
head(actuals_preds)
ModelAccuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
ModelAccuracy

#Model3 BUILDING A MODEL FOR SVM
Model <- svm(price ~ neighbourhood_group+neighbourhood+latitude+longitude+room_type+
               minimum_nights+number_of_reviews+calculated_host_listings_count+
               availability_365,data = train.dfXX)
predModel <- predict(Model,
                     valid.dfXX)
actuals_preds <- data.frame(cbind(actuals=valid.dfXX$price, 
                                  predicteds=predModel))
head(actuals_preds)
ModelAccuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
ModelAccuracy

q0 <- data %>%
  filter(!is.na(room_type)) %>%
  filter(price <= 500)
#Plotting Price Against the neighbourhood_group
ggplot(data=q0, aes(x=reorder(neighbourhood_group, price, FUN=median), y=price)) +
  geom_point(fill="grey50")+
  geom_boxplot(fill="royalblue3", alpha=0.5)+
  theme_minimal() +
  theme( axis.title.x = element_text(size=10, face="bold"),
         axis.title.y = element_text(size=10, face="bold"),
         axis.text.x = element_text(size=10),
         legend.position="bottom",
         panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_line(color="grey95"),
         panel.grid.minor = element_blank(), 
         plot.caption = element_text(size=8)) 

#Plotting Price Against the room_type
ggplot(data=q0, aes(x=reorder(room_type, price, FUN=median), y=price)) +
  geom_point(fill="grey50")+
  geom_boxplot(fill="royalblue3", alpha=0.5)+
  theme_minimal() +
  theme( axis.title.x = element_text(size=10, face="bold"),
         axis.title.y = element_text(size=10, face="bold"),
         axis.text.x = element_text(size=10),
         legend.position="bottom",
         panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_line(color="grey95"),
         panel.grid.minor = element_blank(), 
         plot.caption = element_text(size=8)) 

#Plotting the distribution of price
ggplot(data = q0)+
  geom_bar(aes(price),fill= "#fd5c63",alpha=0.85)

ggplot(data = q0, aes(x=room_type, y=price, fill= "#fd5c63"))+
  geom_bar(stat = "identity")


