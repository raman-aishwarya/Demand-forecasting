#dependent variable=num_orders

Food_demand$id<-NULL
summary(Food_demand)

Food_demand$week<-NULL

quantile(Food_demand$emailer_for_promotion, probs = seq(0.90,1,0.01))
#values of 1 occur from 93% to 100% ; removing variable(difference in values 0 n 1 is less than 15%)

Food_demand$emailer_for_promotion<- NULL

quantile(Food_demand$homepage_featured, probs = seq(0.85,1,0.01))
#values of 1 occur from 90% to 100% ; removing variable

Food_demand$homepage_featured<-NULL

#check for outliers in dep var

str(Food_demand$num_orders)
boxplot(Food_demand$num_orders)

IQR(Food_demand$num_orders)
summary(Food_demand$num_orders)
UW<-324+1.5*269; UW

Food_demand1<-subset(Food_demand,Food_demand$num_orders<=727.5)

nrow(Food_demand)
nrow(Food_demand1)
#1999,1854 ; safe to remove outliers as data loss is just 7%

#cross validation

set.seed(100)
index<-sample(nrow(Food_demand1), 0.75*nrow(Food_demand1))
head(index)

train_food_demand<-Food_demand1[index,]
test_food_demand<-Food_demand1[-index,]

food_demand_model<-lm(num_orders~.,data=train_food_demand)
summary(food_demand_model)

library(car)
vif(food_demand_model)
#checkout_price and base_price are causing multicollinearity
#removing base_price from the model

food_demand_model_rev<-lm(num_orders~center_id + meal_id+checkout_price, train_food_demand)
vif(food_demand_model_rev)
#no multicollinearity

durbinWatsonTest(food_demand_model_rev)
#no autocorrelation

#predicting

train_food_demand$pred_order<-predict(food_demand_model_rev,train_food_demand)
head(train_food_demand)

library(caret)
RMSE(train_food_demand$pred_order,train_food_demand$num_orders)
#158.67

test_food_demand$pred_order<-predict(food_demand_model_rev,test_food_demand)
head(test_food_demand)
RMSE(test_food_demand$pred_order,test_food_demand$num_orders)
#165.47


#Random forest - Food Demand

library(randomForest)

set.seed(101)
food_demand_rf<-randomForest(num_orders~.,data=Food_demand1)
food_demand_rf

pred_food_demand<-predict(food_demand_rf,Food_demand1)

RMSE(pred_food_demand,Food_demand1$num_orders)
#74.09
