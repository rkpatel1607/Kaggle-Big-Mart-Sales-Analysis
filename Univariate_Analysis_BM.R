#Load the data
#Train data
library(readr)
train <- read_csv("~/Downloads/Train_UWu5bXk.csv")

#Test data
test <- read_csv("~/Downloads/Test_u94Q5KV.csv")

#Combining test and train for univariate analysis
df <- rbind(train[,-12],test)

#Saving the target variable
target <- as.data.frame(train$Item_Outlet_Sales)
names(target) <- c("Item_Outlet_Sales")

#Structure of the data
str(df)

#Dimensions of the data
dim(df)

#Item-Weight
class(df$Item_Weight)

#Visualize -Histogram
library(ggplot2)
ggplot(df,aes_string(x = df$Item_Weight)) + geom_histogram(fill = "cornsilk",colour = "black") + geom_density() + theme_bw() +
  xlab("No of Items") + ylab("Weights") + ggtitle("Item_Weights") +
  theme(text = element_text(size = 20))

#Missing Value
sum(is.na(df$Item_Weight))

#Imputation of missing values
library(mice)
md.pattern(df)
imputed_Data <- mice(df, m=5, maxit = 50, method = 'pmm', seed = 500)
df <- complete(imputed_Data,2)

#Check if any missing value is left
apply(df,2,function(x)sum(is.na(x)))

#Item Fat Content
class(df$Item_Fat_Content)

unique(df$Item_Fat_Content)

#First Six values
head(df$Item_Fat_Content)

#Converting into factor variable
df$Item_Fat_Content = as.factor(df$Item_Fat_Content)

#Levelling
levels(df$Item_Fat_Content)[levels(df$Item_Fat_Content) == "Low Fat"] <- "0"
levels(df$Item_Fat_Content)[levels(df$Item_Fat_Content) == "low fat"] <- "0"
levels(df$Item_Fat_Content)[levels(df$Item_Fat_Content) == "LF"] <- "0"
levels(df$Item_Fat_Content)[levels(df$Item_Fat_Content) == "Regular"] <- "1"
levels(df$Item_Fat_Content)[levels(df$Item_Fat_Content) == "reg"] <- "1"

#Proportion of Fat Content
table(df$Item_Fat_Content)

#Item Visiblity
class(df$Item_Visibility)

#head
head(df$Item_Visibility)

#Visualize- Histogram
ggplot(df, aes_string(x = df$Item_Visibility)) + geom_density() + geom_histogram(fill = "cornsilk",colour = "black") + 
  theme_bw() + xlab("Items") + ylab("Visibility Level") + 
  ggtitle("Item Visibility") + theme(text = element_text(size = 20))


#Binning
bins <- as.data.frame(cut(df$Item_Visibility, 9, include.lowest = TRUE),labels = c("0","1","2","3","4","5","6","7","8","9"))
names(bins) <- c("Item_Visibility")
unique(bins$Item_Visibility)

#Using bins as item_visibility
df$Item_Visibility <- bins$Item_Visibility

install.packages('arules')
library(arules)
a <- as.data.frame(df$Item_Visibility)
a$`df$Item_Visibility` <- cut(a$`df$Item_Visibility`, breaks=c(0.02, 0.04, 0.06, 0.08, 0.1,0.125,0.150,0.175, 0.20,0.30,Inf), quantile=FALSE)

#Item_Type
class(df$Item_Type)

#Types of items
unique(df$Item_Type)

#Missing Values
sum(is.na(df$Item_Type))

#Count in each type of food
table(df$Item_Type)

#Percentage of each type of food
round(prop.table(table(df$Item_Type)),digits = 3)*100

#Converting into a Categorical Variable
df$Item_Type = as.factor(df$Item_Type)

#Visualize each type against fat content
ggplot(df, aes_string(x = df$Item_Type,y = df$Item_Fat_Content)) +
  geom_bar(stat = "identity",fill = "DarkSlateBlue") + theme_bw() + xlab("Type of Food") + ylab("Amount of Fatness") + ggtitle("Food type V/s Fat Content") +
  theme(text = element_text(size = 5))

#Item-MRP
class(df$Item_MRP)

#Missing Value
sum(is.na(df$Item_MRP))

#Visualize the MRP
ggplot(df,aes_string(df$Item_MRP)) + geom_density() +
  geom_histogram(fill = "DarkSlateBlue",colour = "Black") + theme_bw() + xlab("MRP (In Rs)") + 
  ggtitle("MRP of Food Items") + theme(text = element_text(size = 15))

#Standardization
p <- as.data.frame(df$Item_MRP)
p <- as.data.frame((p$`df$Item_MRP` - min(p$`df$Item_MRP`))/(max(p$`df$Item_MRP`)- p$`df$Item_MRP`))
names(p) = c("Items_MRP")
ggplot(p,aes_string(p$Items_MRP)) + geom_density() +
  geom_histogram(fill = "DarkSlateBlue",colour = "Black") + theme_bw() + xlab("MRP (In Rs)") + 
  ggtitle("MRP of Food Items") + theme(text = element_text(size = 15))

#Not used for data mining

#Outlet Identifier
class(df$Outlet_Identifier)

#Missing Values
sum(is.na(df$Outlet_Identifier))

#Outlet_Establishment_Year
class(df$Outlet_Establishment_Year)

#Unique Values
unique(df$Outlet_Establishment_Year)

#Converting into a factor variable
df$Outlet_Establishment_Year <- as.factor(df$Outlet_Establishment_Year)

#Table
table(df$Outlet_Establishment_Year)

#Outlet_Size
class(df$Outlet_Size)

#Unique Outlet Size
sum(is.na(df$Outlet_Size))

b <- as.data.frame(df$Outlet_Size[is.na(df$Outlet_Size)])
names(b) = c("Outlet_Size")

#Proportion of Each Outlet Size
table(df$Outlet_Size)

#Percentage of each outlet size
round(prop.table(table(df$Outlet_Size)),digits = 3)*100

b$Outlet_Size <- as.character(b$Outlet_Size)

b[1:786,1] <- 'Small'
b[787:2634,1] <- "Medium"
b[2635:4016,1] <- "High"

#Putting back the missing values 
df$Outlet_Size[is.na(df$Outlet_Size)] <- b$Outlet_Size

#Levelling
df$Outlet_Size = as.factor(df$Outlet_Size)
levels(df$Outlet_Size)[levels(df$Outlet_Size)== "Small"] = 0
levels(df$Outlet_Size)[levels(df$Outlet_Size)== "Medium"] = 1
levels(df$Outlet_Size)[levels(df$Outlet_Size)== "High"] = 2

#Outlet_Location_Site
class(df$Outlet_Location_Type)

#Location Type
unique(df$Outlet_Location_Type)

#Proportion of each category
table(df$Outlet_Location_Type)

#Percentage of each category
round(prop.table(table(df$Outlet_Location_Type)),digits = 3)*100

#Converting into factor variable
df$Outlet_Location_Type = as.factor(df$Outlet_Location_Type)

#Levelling
levels(df$Outlet_Location_Type)[levels(df$Outlet_Location_Type)== "Tier 1"] <- 0
levels(df$Outlet_Location_Type)[levels(df$Outlet_Location_Type) == "Tier 2"] <- 1
levels(df$Outlet_Location_Type)[levels(df$Outlet_Location_Type)== "Tier 3"] <- 2

#Outlet_Type
class(df$Outlet_Type)

unique(df$Outlet_Type)

#Converting into factor variable
df$Outlet_Type = as.factor(df$Outlet_Type)

#Levelling
levels(df$Outlet_Type)[levels(df$Outlet_Type)== "Supermarket Type1"] <- 0
levels(df$Outlet_Type)[levels(df$Outlet_Type)== "Supermarket Type2"] <- 1
levels(df$Outlet_Type)[levels(df$Outlet_Type)== "Supermarket Type3"] <- 2
levels(df$Outlet_Type)[levels(df$Outlet_Type)== "Grocery Store"] <- 3
