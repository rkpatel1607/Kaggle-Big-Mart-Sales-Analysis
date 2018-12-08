#Stucture of the data
#After Univariate Analysis
str(df)

#Divide the data back to train and test
train <- as.data.frame(cbind(df[1:8523,],Item_Outlet_Sales))
test <- as.data.frame(df[8524:14204,])

#Modelling

#Random-Forest
library(randomForest)
model_RF <- randomForest(`Item_Outlet_Sales` ~ `Item_Weight`+`Item_Fat_Content`+`Item_Visibility`+`Item_Type`+`Item_MRP`+`Outlet_Establishment_Year`+`Outlet_Size`+`Outlet_Location_Type`+`Outlet_Type`,data = train)
summary(model_RF)

#Prediction
predict_RF <- predict(model_RF,test)

#Saving the submission
sample_submission <- as.data.frame(cbind(test$Item_Identifier,test$Outlet_Identifier,as.vector(predict_RF)))
names(sample_submission) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
write.csv(sample_submission,"sample_submission_RF.csv",row.names = FALSE)

#Results -1176 RMSE

#Linear- Regression
model_lm <- lm(`Item_Outlet_Sales` ~ `Item_Weight`+`Item_Fat_Content`+`Item_Visibility`+`Item_Type`+`Item_MRP`+`Outlet_Establishment_Year`+`Outlet_Size`+`Outlet_Location_Type`+`Outlet_Type`,data = train)
summary(model_lm)

#Prediction
predict_lm <- predict(model_lm,test)

#Saving the submission
sample_submission <- as.data.frame(cbind(test$Item_Identifier,test$Outlet_Identifier,as.vector(predict_lm)))
names(sample_submission) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
write.csv(sample_submission,"sample_submission_LM.csv",row.names = FALSE)

#Results - 1202 RMSE

#Decision Tree
library(rpart)
model_DT <- rpart(`Item_Outlet_Sales` ~ `Item_Weight`+`Item_Fat_Content`+`Item_Visibility`+`Item_Type`+`Item_MRP`+`Outlet_Establishment_Year`+`Outlet_Size`+`Outlet_Location_Type`+`Outlet_Type`,data = train)
print(model_DT)
summary(model_DT)

#Prediction of the test data
predictDT = predict(model_DT,test)

#Saving the submission
sample_submission <- as.data.frame(cbind(test$Item_Identifier,test$Outlet_Identifier,as.vector(predictDT)))
names(sample_submission) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
write.csv(sample_submission,"sample_submission_DT.csv",row.names = FALSE)

#Results - 1174 RMSE

#kNN Algorithm
install.packages('kknn')
library(kknn)

model_kknn <- kknn(`Item_Outlet_Sales` ~ `Item_Weight`+`Item_Fat_Content`+`Item_Visibility`+`Item_Type`+`Item_MRP`+`Outlet_Establishment_Year`+`Outlet_Size`+`Outlet_Location_Type`+`Outlet_Type`,train,test,k = 10,distance = 3)
summary(model_kknn)

#Prediction
predictKKNN <- as.vector(model_kknn$fitted.values)

#Saving the submission
sample_submission <- as.data.frame(cbind(test$Item_Identifier,test$Outlet_Identifier,predictKKNN))
names(sample_submission) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")
write.csv(sample_submission,"sample_submission_KKNN.csv",row.names = FALSE)

#Results - 1274 RMSE




