library(rpart)
library(randomForest)

df <- ISLR::College

#randomly split data 70/30
split <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))
df_train <- df[split,]
df_test <- df[!split,]


#decision tree

model <- rpart(Private ~ ., method="class", data = df_train)
summary(model)
rpart.plot::prp(model)

train_pred <- predict(model, df_train)
df_train$pred <- F
df_train$pred[train_pred[,2] >.50] <- T 

test_pred <- predict(model, df_test)
df_test$pred <- F
df_test$pred[test_pred[,2] >.50] <- T 

table(df_train$pred, df_train$Private) #acc = 95%
table(df_test$pred, df_test$Private) #acc = 90%



#random forest

model <- randomForest(Private ~ ., data = df_train, importance=T)
model$confusion # acc = 95%

df_test$pred <- predict(model, df_test)
table(df_test$pred, df_test$Private) #acc = 93%

#Test prediction accuracy improved slightly for a random forest
