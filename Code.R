library(ggplot2)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(VIM)
library(doParallel)

## loading datasets 
Train <- read.csv("Documents/Big mart sales/Train_UWu5bXk.csv")
Test <- read.csv("Documents/Big mart sales/Test_u94Q5KV.csv")

## checking the dimensions of data set
dim(Train)
dim(Test)

# checking the variables and their types 
## Overall summery of each variable
## this gives you a brief over view of the data your are dealing with.
str(Train)
summary(Train)

## We will temporarily combine both test and train sets to do EDA
## EDA - exploratory Data Analysis
Test$Item_Outlet_Sales = 0 
# it can be set to any arbitary value / You can remove the respone variable from train set

fulldata = rbind(Train, Test)

## Now let's look at each variable one by one 
str(fulldata$Item_Identifier) 
summary(fulldata$Item_Identifier)
## this is not a unique id for each row. 
#But an unique id for each product. 1559 types of product.
#Same product can be found in different stores so Ids are repeated 
## creating new variables from ID variable first 2 letters and 3 letters

## Creating New vaiables
fulldata$Item_ID_Class = substring(fulldata$Item_Identifier, 1, 2)
fulldata$Item_ID_Class = as.factor(fulldata$Item_ID_Class)

# More information can be obtained from first three letters of ID.
fulldata$Item_ID_class_type = substring(fulldata$Item_Identifier, 1, 3)
fulldata$Item_ID_class_type = as.factor(fulldata$Item_ID_class_type)

# Interesting! So, OUT019 and OUT027 have not provided any weight data

# assuming that each item identifier actually identifies a unique item,
# hence a unique weight, let's create a dataframe containing the mean
# weights and standard deviations by item identifier
weightsByItem <- as.data.frame( ddply(na.omit(combi), 
                                      ~Item_Identifier, 
                                      summarise, 
                                      mean=mean(Item_Weight), 
                                      sd=sd(Item_Weight)))

# we can now use these values to fill in the missing weight values:
combi$Item_Weight <- ifelse(is.na(combi$Item_Weight), 
                            weightsByItem$mean[
                              match(combi$Item_Identifier, weightsByItem$Item_Identifier)], combi$Item_Weight)

#any values still missing?
table(is.na(combi))

# let's redo the plots we looked at earlier
# boxplot of weights vs Item type
ggplot(combi, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")

# boxplot of weights vs. Outlet Identifier
ggplot(combi, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")
# Interesting! So, OUT019 and OUT027 have not provided any weight data

ggplot(fulldata, aes(Item_Type, Item_Weight))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) +
  xlab("Item Type") + ylab("Item_Weight") + ggtitle("Weight Vs type")

ggplot(fulldata, aes(Outlet_Identifier, Item_Weight)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, color = "black")) + 
  labs(title = "Weight Vs outlet", x = "Outlet", y = "Weight")

# assuming that each item identifier actually identifies a unique item,
# hence a unique weight, let's create a dataframe containing the mean
# weights by item identifier
weights = as.data.frame(tapply(fulldata$Item_Weight[!is.na(fulldata$Item_Weight)], 
                               fulldata$Item_Identifier[!is.na(fulldata$Item_Weight)], 
                               mean))
weights$ID = row.names(weights)
weights$weight = weights$`tapply(fulldata$Item_Weight[!is.na(fulldata$Item_Weight)], fulldata$Item_Identifier[!is.na(fulldata$Item_Weight)], `

# we can now use these values to fill in the missing weight values:
fulldata$Item_Weight = ifelse(is.na(fulldata$Item_Weight), 
                           weights$weight[match(fulldata$Item_Identifier, weights$ID)], fulldata$Item_Weight)

## reploting Weight Vs Outlet_Identifier, Item_type
ggplot(fulldata, aes(Item_Type, Item_Weight))+
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) +
  xlab("Item Type") + ylab("Item_Weight") + ggtitle("Weight Vs type")

ggplot(fulldata, aes(Outlet_Identifier, Item_Weight)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, color = "black")) + 
  labs(title = "Weight Vs outlet", x = "Outlet", y = "Weight")

## Item_Fat_Content
#Modifying item_fat_content variable 
summary(fulldata$Item_Fat_Content)
#LF, low fat and Low Fat are represent same. Similarly reg and Regular are same. 
#So let's change that. we replace LF and low fat by 'L' and reg, Regular by 'R'.

fulldata$Fat_content = ifelse(fulldata$Item_Fat_Content == "reg" | fulldata$Item_Fat_Content == "Regular", "R", "L")
# change it to factor variable
fulldata$Fat_content = as.factor(fulldata$Fat_content)
fulldata$Item_Fat_Content = fulldata$Fat_content
#remove the variable
fulldata$Fat_content = NULL

#or this can simply be done by revalue function from plyr
fulldata$Item_Fat_Content <- revalue(fulldata$Item_Fat_Content,
                                  c("LF" = "L", "low fat" = "L","Low Fat" = "L", "reg" = "R", "Regular" = "R"))


#Further, there are different types of non-consumables products such as Health and Hygiene, 
#Household and Others are either Low Fat or Regular according to the data. 
#But Clearly, this makes no sense assing some amount of fat to utensils or household products. 
#Hence, we introduce an new fat level None for these levels.
levels(fulldata$Item_Fat_Content) = c(levels(fulldata$Fat_content), "N")
fulldata[which(fulldata$Item_Type == "Health and Hygiene"), ]$Item_Fat_Content = "N"
fulldata[which(fulldata$Item_Type == "Household"), ]$Item_Fat_Content = "N"
fulldata[which(fulldata$Item_Type == "Others"), ]$Item_Fat_Content = "N"


## Item_Visibility
summary(fulldata$Item_Visibility)

# From data technically, there are no missing values in the Item_Visibility category.
# However, many of the entries are zero. This looks like a mistake in the data entry.
# we will rplace this zero values with missing values and impute them.
# replace 0 by NA so that mice can work its magic
outletIdentifiers <- levels(fulldata$Outlet_Identifier)
itemTypes <- levels(fulldata$Item_Type)
for (outName in outletIdentifiers) {
  for (itemName in itemTypes) {
    fulldata[ which(fulldata$Outlet_Identifier == outName &
                      fulldata$Item_Type == itemName),]$Item_Visibility <-
      ifelse(
        fulldata[ which(fulldata$Outlet_Identifier == outName &
                          fulldata$Item_Type == itemName), ]$Item_Visibility == 0 ,
        NA ,
        fulldata[ which(fulldata$Outlet_Identifier == outName &
                          fulldata$Item_Type == itemName),]$Item_Visibility
      )
  }
}

# Item_Visibility, i.e. the percentage of display space in a store devoted to that particular item. 
#Looking at the average visibility of items in each shop,
# boxplot of Visibility vs Item type
ggplot(fulldata, aes(Item_Type, Item_Visibility, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Item Type")

# boxplot of Visibility vs. Outlet Identifier
ggplot(fulldata, aes(Outlet_Identifier, Item_Visibility, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")

# We can use the MICE package to impute those missing values 
# or we can do manually by mean matching using variable Outlet_type, Item_Identifier.
## This will save lot of time and memory. And also the accuracy is better then using mice.
# Here i giving code for both methods.

#########################################################################
#First using mice package.
library(mice)
# to compare the visibility distributions before
# and after imputing the missing values
# we create a copy of the non-vanishing entries
fulldata_NonZero_Vis <- subset(fulldata, Item_Visibility > 0)

#any missing values now?
table(is.na(fulldata))
colSums(is.na(fulldata))

#pattern of missing values
md.pattern(fulldata)

aggr_plot <- aggr(fulldata, col=c('lightgreen','blue'), 
                   numbers=TRUE, 
                   sortVars=TRUE, 
                   labels=names(fulldata), 
                   cex.axis=.7, 
                   gap=3, 
                   ylab=c("Histogram of missing data","Pattern"))
 
marginplot(fulldata)

# let mice impute the missing visibilities
new_fulldata <- mice(fulldata,m=1,maxit=1,meth='pmm',seed=0)
# summary of imputations
summary(new_fulldata)



# comparison of the distribution of existing
# and imputed visibilities
densityplot(new_fulldata)
stripplot(new_fulldata, pch = 16, cex = 1.2)

# let's replace the NA (formerly zero) values
# by the imputed ones
fulldata <- complete(new_fulldata,1)


#######################################################################
#### This is second way to fill the zero visibility vlaues
## This is faster and produces better results than mice.
## Filling missing values by mean matching manually
VisByItem1 <- as.data.frame( ddply(na.omit(fulldata),  
                                   ~Item_Identifier + Outlet_Type, 
                                   summarise, 
                                   mean=mean(Item_Visibility), 
                                   sd=sd(Item_Visibility)))

VisByItem1$sd = NULL
fulldata_NA = subset(fulldata, select = c("Item_Identifier", "Outlet_Type", "Item_Visibility"))
fulldata_NA$ID = 1:14204
fulldata_merged = merge(fulldata_NA, VisByItem1, all = TRUE)
fulldata_merged = fulldata_merged[order(fulldata_merged$ID), ]
fulldata_merged$Item_Visibility = ifelse(is.na(fulldata_merged$Item_Visibility), 
                                      fulldata_merged$mean, fulldata_merged$Item_Visibility)
fulldata$Item_Visibility = fulldata_merged$Item_Visibility
## Now filling the rest of missing values
VisByItem1 <- as.data.frame( ddply(na.omit(fulldata),  
                                   ~Item_Identifier,
                                   summarise, 
                                   mean=mean(Item_Visibility), 
                                   sd=sd(Item_Visibility)))
VisByItem1$sd = NULL
fulldata_NA = subset(fulldata, select = c("Item_Identifier", "Item_Visibility"))
fulldata_NA$ID = 1:14204
fulldata_merged = merge(fulldata_NA, VisByItem1, all = TRUE)
fulldata_merged = fulldata_merged[order(fulldata_merged$ID), ]
fulldata_merged$Item_Visibility = ifelse(is.na(fulldata_merged$Item_Visibility), 
                                      fulldata_merged$mean, fulldata_merged$Item_Visibility)
fulldata$Item_Visibility = fulldata_merged$Item_Visibility

## checking the pattern before and after filling the missing values.
fulldata_NonZero_Vis = subset(fulldata, fulldata$Item_Visibility > 0)
ggplot(fulldata_NonZero_Vis, aes(x = "Item_Visibility")) + geom_density()


# total visibility per shop can be shown in %ile.
# so that each store should be 100
shopSum <- as.data.frame(setNames(
  aggregate(fulldata$Item_Visibility, by=list(Category=fulldata$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

shopSum

# Now we have to normalize all visibilities such that
# the total per shop comes out to be 100%. 

for (outName in outletIdentifiers) {
  fulldata[ which(fulldata$Outlet_Identifier == outName),]$Item_Visibility <-
  fulldata[ which(fulldata$Outlet_Identifier == outName),]$Item_Visibility *
    100/shopSum[ which(shopSum$Outlet_Identifier == outName),]$TotVis
}



# let's see the plots  of visibilities before and after imputation
#beforeImputation
ggplot() + geom_density(aes(x=Item_Visibility), colour="green", data=fulldata_org)
         + ggtitle("Before Imputation")

#After Impution
ggplot()+geom_density(aes(x=Item_Visibility), colour="red", data=fulldata) 
+ ggtitle("After Imputing")

# Both in one graph
ggplot() + geom_density(aes(x=Item_Visibility), colour="green", data=fulldata_org)
geom_density(aes(x=Item_Visibility), colour="red", data=fulldata) 

# histograms of visibilities before and
# after imputation
ggplot(fulldata_NonZero_Vis[fulldata_NonZero_Vis$Outlet_Type %in% "Grocery Store", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) + 
  xlim(0.0,0.35) +
  xlab("Item visibility") + 
  ggtitle("Grocery Stores")

ggplot(fulldata[fulldata$Outlet_Type %in% "Grocery Store", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.35) +
  xlab("Item visibility") + 
  ggtitle("Grocery Stores")

ggplot(fulldata_NonZero_Vis[fulldata_NonZero_Vis$Outlet_Type %in% "Supermarket Type1", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.3) +
  xlab("Item visibility") + 
  ggtitle("Type 1")

ggplot(fulldata[fulldata$Outlet_Type %in% "Supermarket Type1", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.3) +
  xlab("Item visibility") + 
  ggtitle("Type 1")

ggplot(fulldata_NonZero_Vis[fulldata_NonZero_Vis$Outlet_Type %in% "Supermarket Type2", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 2")

ggplot(fulldata[fulldata$Outlet_Type %in% "Supermarket Type2", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 2")

ggplot(fulldata_NonZero_Vis[fulldata_NonZero_Vis$Outlet_Type %in% "Supermarket Type3", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 3")

ggplot(fulldata[fulldata$Outlet_Type %in% "Supermarket Type3", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 3")


# boxplot of Visibility vs. Outlet Identifier
ggplot(fulldata_NonZero_Vis, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier") 

ggplot(fulldata, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")

############################################################################################

#  Item_MRP

summary(fulldata$Item_MRP)

#distribution
ggplot(combi, aes(x=Item_MRP)) + geom_density(color = "blue", adjust=1/5)

#As we can see clearly there are different price level distributions. 
#So we can create a new factor variable which represents these four 
#levels to add more granurality to data.

fulldata$Item_MRP_Level = ifelse(combi$Item_MRP < 69, "Low",
                                 ifelse(combi$Item_MRP < 136, "Medium",
                                       ifelse(combi$Item_MRP < 203, "High", "V.High")))
fulldata$Item_MRP_Level = as.factor(fulldata$Item_MRP_Level)

## let's visually see those breaks in the plot.
ggplot(fulldata, aes(x=Item_MRP)) +
  geom_density(color = "blue", adjust=1/5) +
  geom_vline(xintercept = 69, color="red")+
  geom_vline(xintercept = 136, color="red")+
  geom_vline(xintercept = 203, color="red") + 
  ggtitle("Density of Item MRP")

####################
##Outlet Identifier 
summary(fulldata$Outlet_Identifier)

# so there 10 outlets we will later check these variable along with other store related variables.


## Outlet_Establishment year
summary(fulldata$Outlet_Establishment_Year)
# this is a numeric variable. Years from 1985 to 2009. 
## Given the data is collected in 2013. We can create a new variable
## related to number of years of establishment of store. 

fulldata$Years_of_estd <- as.factor(2013 - fulldata$Outlet_Establishment_Year)

#since we have created the new variable we can drop the old variable.
fulldata$Outlet_Establishment_Year = NULL


#####
##Outlet_Size
summary(fulldata$Outlet_Size)

#we can see 4016 outlet_sizes haven't mentioned. 
#we need to impute these values
#first we will look at different combination of stores.

table(fulldata$Outlet_Size, fulldata$Outlet_Type)
table(fulldata$Outlet_Size, fulldata$Outlet_Location_Type)
table(fulldata$Outlet_Size, fulldata$Outlet_Identifier)
table(fulldata$Outlet_Type, fulldata$Outlet_Location_Type)
table(fulldata$Outlet_Type, fulldata$Outlet_Identifier)
table(fulldata$Outlet_Identifier, fulldata$Outlet_Location_Type) 

#from the above tables we see that Outlet_type-Grocery_store 
#belongs to small size cateogoery. 

aggregate(fulldata$Outlet_Identifier, 
          by=list(Category=fulldata$Outlet_Identifier), FUN="length")

#clearly, the two outlets, OUT010 and OUT019 have reported far
# less data than the supermarkets.
# From the data and their description it's not really clear why.
# In the following I'll assume that it's just because they are
# much smaller and therefore have a smaller selection
# of goods to buy.
# As a check let's count the Item IDs:
aggregate(fulldata$Item_Identifier, 
          by=list(Category=fulldata$Outlet_Identifier), FUN="length")

# What else can we learn about the different types of shops?

# boxplot of  Sales vs. Outlet identifier
ggplot(fulldata[1:8523,], aes(Outlet_Identifier, Item_Outlet_Sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "green")) + 
  xlab("Outlet identifier") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet identifier")

# boxplot of  Sales vs. Outlet Type
ggplot(fulldata[1:8523,], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Years_of_estd)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "lightblue")) + 
  xlab("Outlet Type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Type")

# Sales in the one type 2 supermarket appear a bit low,
# as one would expect them to be higher than in
# the type 1 supermarkets.
# Maybe it's because it's still fairly new, having
# been founded 4 years ago.

# boxplot of  Sales vs. Outlet Type
ggplot(fulldata[1:8523,], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    fulldata$Outlet_Size, 
    by=list(Category=fulldata$Outlet_Identifier, 
            Category=fulldata$Outlet_Type,
            Category=fulldata$Outlet_Location_Type,
            Category=fulldata$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

##from above results we can conclude that a Grocery store 
##certainly falls in the size category Small
# Type 1 supermarkets are most often classified as Small,
# i.e. the mode is "Small"
# Hence we assign size small to all the missing entries in outlet size.

levels(fulldata$Outlet_Size)[1] = "Small"

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    fulldata$Outlet_Size, 
    by=list(Category=fulldata$Outlet_Identifier, 
            Category=fulldata$Outlet_Type,
            Category=fulldata$Outlet_Location_Type,
            Category=fulldata$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

# apply factor to Outlet_Size in order to drop the now
# unused level "Other"
fulldata$Outlet_Size <- factor(fulldata$Outlet_Size)


##Now let's check the rest of Store related variables.

#Outlet_Loction_Type


summary(fulldata$Outlet_Location_Type)

##Outlet stores are located in three different types of cities.
##Furthur we will see which tier is contributing more to sales.


ggplot(fulldata[1:8523,], aes(x = Outlet_Location_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet location") + 
ylab("Sales") + 
ggtitle("Sales vs Outlet location")



##Outlet_Type


summary(fulldata$Outlet_Type)


##Four different types of stores. Grocery and three different Supermarkets type stores.
##Next we will see which type of stores are contributing more to sales.


ggplot(fulldata[1:8523,], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet type") + 
ylab("Sales") + 
ggtitle("Sales vs Outlet type")



## Let's Check visually the Sales of different Items in different stores, cities, outlets 



ggplot(fulldata[1:8523,], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")


ggplot(fulldata[1:8523,], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")

ggplot(fulldata[1:8523,], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Location_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")


##let's find out the correlation among numerical variables 

# correlation between numerical variables
fulldata_Num_Corr <- cor(fulldata[1:8523,][sapply(fulldata[1:8523,], is.numeric)])
fulldata_Num_Corr

# a brief overview of the correlation matrix
library(corrplot) ## library required to plot correlations
corrplot::corrplot(fulldata_Num_Corr, method="number", type="upper")
corrplot::corrplot(fulldata_Num_Corr, method="number", type="upper", order="hclust")

#
# Item_Outlet_Sales has a strong positive correlation with Item_MRP
# and a somewhat weaker negative one with Item_Visibility


# Scatter plot of Item_Outlet_Sales vs Item_MRP
# coloured according to the Outlet type


ggplot(fulldata[1:8523,], aes(Item_MRP, Item_Outlet_Sales)) +
   geom_point(size = 2.5, aes(colour = factor(Outlet_Type))) +
   theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
   xlab("Item MRP") + 
   ylab("Item Outlet Sales") +
   ggtitle("Item Sales vs Item MRP in different Outlet Types")


# Scatter plot of Item_Outlet_Sales vs Item_MRP
# coloured according to the Outlet size


ggplot(fulldata[1:8523,], aes(Item_MRP, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Size))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item MRP") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item MRP in different Outlet sizes")



# Scatter plot of Item_Outlet_Sales vs Item_MRP
# coloured according to the Outlet Location type


ggplot(fulldata[1:8523,], aes(Item_MRP, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Location_Type))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item MRP") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item MRP in different types of cities")


# Scatter plot of Item_Outlet_Sales vs Item_MRP
# coloured according to the Outlet Identifier


ggplot(fulldata[1:8523,], aes(Item_MRP, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Identifier))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item MRP") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item MRP in different Outlets")


# Scatter plot of Item_Outlet_Sales vs Item_Visibility
# coloured according to the Outlet type
ggplot(fulldata[1:8523,], aes(Item_Visibility, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Type))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item Visibility in different Outlet types")


# Scatter plot of Item_Outlet_Sales vs Item_Visibility
# coloured according to the Outlet size
ggplot(fulldata[1:8523,], aes(Item_Visibility, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Size))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item Visibility in different Outlet Sizes")

# Scatter plot of Item_Outlet_Sales vs Item_Visibility
# coloured according to the Outlet identifier
ggplot(fulldata[1:8523,], aes(Item_Visibility, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Identifier))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item Visibility in different Outlets")


# Scatter plot of Item_MRP vs Item_Visibility
# coloured according to the Outlet Location type

ggplot(fulldata[1:8523,], aes(Item_Visibility, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Location_Type))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item MRP") +
  ggtitle("Item MRP vs Item Outlet Sales in different locations")


# Scatter plot of Item_MRP vs Item_Visibility
# coloured according to the Outlet type

ggplot(fulldata[1:8523,], aes(Item_Visibility, Item_MRP)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Type))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item MRP") +
  ggtitle("Item MRP vs Item MRP in different Outlet Type")



#As we have discussed above dividing sales by MRP does 
##reduce the number of outliers and also emphasizes the 
##differences between the different
##types of shop, so we'll just do that in the following

fulldata$Item_Outlet_Sales = fulldata$Item_Outlet_Sales/combi$Item_MRP

# let's resurrect the train and test data sets
new_train = fulldata[1:nrow(Train),]
new_test = fulldata[-(1:nrow(Train)),]

# and drop the faked Item_Outlet_Sales column in new_test
new_test$Item_Outlet_Sales = NULL

str(new_train)
str(new_test)

# First we need to save these files for futhur references and cleaning 

write.csv(new_train, file="new_train.csv", row.names=FALSE, quote = FALSE)
write.csv(new_test, file="new_test.csv", row.names=FALSE, quote = FALSE)



# scale Sales to be in interval [0,1]
maxSales = max(new_train$Item_Outlet_Sales)
new_train$Item_Outlet_Sales = new_train$Item_Outlet_Sales/maxSales

set.seed(0)

# one-hot encoding of the factor variables
# leave out the intercept column

new_train = as.data.frame(model.matrix( ~ . + 0, data = new_train))
new_test = as.data.frame(model.matrix( ~ . + 0, data = new_test))

str(new_train)

# define a vector of Item_Outlet_Sales
# and a dataframe of predictors
response_variable =  new_train$Item_Outlet_Sales
predictors_set = subset(new_train, select=-c(Item_Outlet_Sales))
