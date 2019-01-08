# Validation
library(doParallel)
cl <- makeCluster(detectCores()); registerDoParallel(cl)
ctrl = trainControl(method = "cv", number = 5)

## 
ptm1_start = proc.time()
lmfit = train(x = predictors, y = sales, method = "lm", trControl = ctrl)
ptm1_end = proc.time()

### Rigde regression
ridgeGrid = data.frame(.lambda = seq (0, 0.3, length = 10))

ptm2_start = proc.time()
ridgeModel = train(x = predictors, y = sales, method = "ridge",
                   tuneGrid = ridgeGrid, trControl = ctrl, 
                   preProcess = c("center", "scale"))
ptm2_end = proc.time()

stopCluster(cl); registerDoSEQ();

### elastic net regression 
enet_grid = expand.grid(.lambda = c(0.01, 0.1, 0.3), 
                        .fraction =seq(0.05, 1, length.out = 10))
set.seed(1)
enet_tuned_model = train(predictors, sales, method = "enet",
                         tuneGrid = enet_grid, trControl = ctrl, 
                         preProcess = c("center", "scale"))

## Non linear 
## single tree
library(rpart)
library(rpart.plot)
library(party)
library(partykit)

set.seed(10)

rpart_tune_model = train(predictors, sales, method = "rpart", 
                         tuneLength = 10, trControl = ctrl)



rpart_Model = rpart(Item_Outlet_Sales ~ ., data = new_train, 
                    control = rpart.control(cp = 0.001))
rpart_pred = predict(rpart_Model, newdata = new_test)
train_pred = predict(rpart_Model, newdata = new_train)



SampleSubmission <- read.csv("Documents/Big mart sales/SampleSubmission_TmnO39y.csv")

SampleSubmission$Item_Outlet_Sales = rpart_pred*new_test$Item_MRP
write.csv(SampleSubmission, "tree1.csv", row.names = FALSE, quote = FALSE)

