##### BIG Mart sales from Analytics Vidya Practice Problem 
lib_req <- c('data.table', 'testthat', 'gridExtra', 'ggplot2', 'caret', 'dplyr', 'tidyverse')
install_req_lib = lib_req[!lib_req %in% installed.packages()]
for(lib in install_req_lib) install.packages(lib, dependencies = TRUE)
sapply(lib_req, library, character = TRUE)


## one variable at a time
summary(Train$Item_Weight)
summary(Test$Item_Weight)

hist(Train$Item_Weight, breaks = seq(4, 22, by = 0.5), freq = TRUE)
hist(Test$Item_Weight)
boxplot(Train$Item_Weight, Test$Item_Weight, horizontal = TRUE, notch = TRUE, ylim = c(0, 22), las = 1, xlab = "weight" )
boxplot(Test$Item_Weight, horizontal = TRUE, notch = TRUE)

summary(Train$Item_Fat_Content)
summary(Test$Item_Fat_Content)
#Modifying item_fat_content variable 
train$Fat_content = ifelse(train$Item_Fat_Content == "reg" | train$Item_Fat_Content == "Regular", "R", "L")
train$Fat_content = as.factor(train$Fat_content)
train$Item_Fat_Content = train$Fat_content


test$Fat_content = ifelse(test$Item_Fat_Content == "reg" | test$Item_Fat_Content == "Regular", "R", "L")
test$Fat_content = as.factor(test$Fat_content)
test$Item_Fat_Content = test$Fat_content

## easy way is use revalue function in plyr


## Filling the missing values of Item_weight

for (i in 1:8523) { if (is.na(dataset$Item_Weight[i])) 
                       {for (j in 1:8523) { if (dataset$Item_Identifier[i] == dataset$Item_Identifier[j] & !is.na(dataset$Item_Weight[j]))
                                               {dataset$Item_Weight[i] = dataset$Item_Weight[j]}
                                            else 
                                                { next}
                                          }
                       }
                  else 
                  { next }
}            


df1$tof[is.na(df1$tof)] <- df2$atof[match(df1$country,df2$country)][which(is.na(df1$tof))]
df1

missing_data = train[,1:2]
missing_data$Item_Weight[is.na(missing_data$Item_Weight)] = missing_data$Item_Weight[match(missing_data$Item_Identifier, missing_data$Item_Identifier)][which(is.na(missing_data$Item_Weight))]






