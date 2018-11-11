
#Importing dataset in R 
car_price <- read.csv("CarPrice_Assignment.csv")

#installing necessary packages
install.packages("MASS")
install.packages("car")

#Loading the necessary packages
library(tidyr)
library(MASS)
library(ggplot2)
library(car)

#####################################################################################################
#                              DATA UNDERSTANDING
#####################################################################################################

#checking the structure of car_price dataset.
str(car_price)

#View the dataset
View(car_price)

#Checking the total observation in car_price dataset
nrow(car_price)
#205


#####################################################################################################
#                              DATA CLEANING AND PREPARATION
#####################################################################################################

#Check for NA values
#----------------------------------------
sapply(car_price, function(x) sum(is.na(x)))
#There are no NA values in the dataset

# checking for blank "" values
#---------------------------------------------------
sapply(car_price, function(x) length(which(x == "")))
#There are no blank values in the dataset

# checking for duplicated values
#---------------------------------------------------
sum(duplicated(car_price))

#To check if there are any duplicate records we can use the below script and it will return TRUE if there are
#no duplicates.
length(unique(car_price$car_ID)) == nrow(car_price)
##There are no duplicate records in the dataset and car_ID is unique in the dataset

#The variable "CarName" is comprised of two parts - the first word is the name of 'car company' and 
#the second is the 'car model'. As per the requirement we need to consider only company name as the 
#independent variable for the model building. 

#Creating a derived metric "Car_Company" and "Car_Model"
car <- separate(car_price,CarName,into = c("Car_Company","Car_Model"),
                sep=" ",extra = "merge",fill = "right" )

#Removing the "Car_Model" column
car <- car[,-4]

#converting the Car_Company to lowercase and standardising the text in the column.
car$Car_Company <- tolower(car$Car_Company)
car$Car_Company <- gsub("vokswagen","volkswagen",car$Car_Company)
car$Car_Company <- gsub("vw","volkswagen",car$Car_Company)
car$Car_Company <- gsub("maxda","mazda",car$Car_Company)
car$Car_Company <- gsub("porcshce","porsche",car$Car_Company)
car$Car_Company <- gsub("toyouta","toyota",car$Car_Company)

car$Car_Company <- as.factor(car$Car_Company)
summary(car$Car_Company)

#Converting Symboling to factor since it is categorical as per Data Dictionary
car$symboling <- as.factor(car$symboling)

#Outlier treatment on wheelbase
#------------------------------------------------------------------------
quantile(car$wheelbase,seq(0,1,0.01))
boxplot(car$wheelbase)
#Outliers are present in wheelbase

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$wheelbase)

car$wheelbase[which(car$wheelbase > 110 )] <- 110

#Outlier treatment on carlength
#------------------------------------------------------------------------
quantile(car$carlength,seq(0,1,0.01))
boxplot(car$carlength)
#Outliers are present in carlength

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$carlength)

car$carlength[which(car$carlength < 155.900)] <- 155.900
car$carlength[which(car$carlength > 202.480)] <- 202.480

#Outlier treatment on carwidth
#------------------------------------------------------------------------
quantile(car$carwidth,seq(0,1,0.01))
boxplot(car$carwidth)
#Outliers can be ignored since its a small difference


#Outlier treatment on carheight
#------------------------------------------------------------------------
quantile(car$carheight,seq(0,1,0.01))
boxplot(car$carheight)
#Outliers are not present in carheight

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$carheight)

#Outlier treatment on curbweight
#------------------------------------------------------------------------
quantile(car$curbweight,seq(0,1,0.01))
boxplot(car$curbweight)
#Outliers are present in curbweight

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$curbweight)

car$curbweight[which(car$curbweight < 1819.72)] <- 1819.72

#Outlier treatment on enginesize
#------------------------------------------------------------------------
quantile(car$enginesize,seq(0,1,0.01))
boxplot(car$enginesize)
#Outliers are present in enginesize

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$enginesize)

car$enginesize[which(car$enginesize < 90)] <- 90
car$enginesize[which(car$enginesize > 209.00)] <- 209.00

#Outlier treatment on boreratio 
#------------------------------------------------------------------------
quantile(car$boreratio ,seq(0,1,0.01))
boxplot(car$boreratio )
#Outliers are not present in boreratio 

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$boreratio )

#Outlier treatment on stroke
#------------------------------------------------------------------------
quantile(car$stroke ,seq(0,1,0.01))
boxplot(car$stroke )
#Outliers are  present in stroke 

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$stroke )

car$stroke[which(car$stroke < 2.6400)] <- 2.6400


#Outlier treatment on compressionratio
#------------------------------------------------------------------------
quantile(car$compressionratio ,seq(0,1,0.01))
boxplot(car$compressionratio )
#Outliers are  present in stroke 

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$compressionratio )

car$compressionratio[which(car$compressionratio > 10.9400)] <- 10.9400

#Outlier treatment on horsepower
#------------------------------------------------------------------------
quantile(car$horsepower ,seq(0,1,0.01))
boxplot(car$horsepower)
#Outliers are  present in horsepower 

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$horsepower)

car$horsepower[which(car$horsepower > 207.00)] <- 207.00

#Outlier treatment on peakrpm
#------------------------------------------------------------------------
quantile(car$peakrpm ,seq(0,1,0.01))
boxplot(car$peakrpm)
#Outliers are  present in peakrpm 

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$peakrpm)

car$peakrpm[which(car$peakrpm > 6000)] <- 6000

#Outlier treatment on citympg
#------------------------------------------------------------------------
quantile(car$citympg ,seq(0,1,0.01))
boxplot(car$citympg)
#Outliers are  present in citympg

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$citympg)

car$citympg[which(car$citympg > 38.00)] <- 38.00

#Outlier treatment on highwaympg 
#------------------------------------------------------------------------
quantile(car$highwaympg  ,seq(0,1,0.01))
boxplot(car$highwaympg )
#Outliers are  present in highwaympg 

#Using boxplot.stat command to get the exact outlier values
boxplot.stats(car$highwaympg )

car$highwaympg [which(car$highwaympg  > 46.92)] <- 46.92


#Dummy Variable creation
#------------------------------------------------------------------------
#Symboling
#-----------------------------------
summary(car$symboling)
#Converting "Car_Company" into dummies . 
dummy <- data.frame(model.matrix(~symboling , data = car))
#Removing first column from the newly created dummy dataframe containing the dummy values for 
#the variable "Symboling".
dummy<-dummy[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "Symboling" 
#column
car <- cbind(car[,-2],dummy)

#Car_Company 
#------------------------------------
summary(car$Car_Company)
#Converting "Car_Company" into dummies . 
dummy_1 <- data.frame(model.matrix( ~Car_Company, data = car))
#check the dummy_1 data frame.
View(dummy_1)
#Removing first column from the newly created dummy_1 dataframe containing the dummy values for the
#variable "Car_Company". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "Car_Company"
#column
car <- cbind(car[,-2], dummy_1)

#fueltype 
#------------------------
summary(car$fueltype)
#To convert fueltype variable to numeric is to replace the levels- diesel with 1 and gas with 0.
levels(car$fueltype)<-c(1,0)
car$fueltype<- as.numeric(levels(car$fueltype))[car$fueltype]

#aspiration 
#-------------------------------
summary(car$aspiration)
#To convert aspiration variable to numeric is to replace the levels- std with 1 and turbo with 0.
levels(car$aspiration)<-c(1,0)
car$aspiration<- as.numeric(levels(car$aspiration))[car$aspiration]

#doornumber 
#---------------------------------
summary(car$doornumber)
#To convert doornumber variable to numeric is to replace the levels- four door with 4 and two door with 2.
levels(car$doornumber)<-c(4,2)
car$doornumber<- as.numeric(levels(car$doornumber))[car$doornumber]

#carbody 
#----------------------------------
summary(car$carbody)
#Converting "carbody" into dummies . 
dummy_2 <- data.frame(model.matrix( ~carbody, data = car))
#check the dummy_2 data frame.
View(dummy_2)
#Removing first column from the newly created dummy_2 dataframe containing the dummy values for the
#variable "carbody". 
dummy_2 <- dummy_2[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carbody"
#column
car <- cbind(car[,-5], dummy_2)

#drivewheel
#----------------------------------
summary(car$drivewheel)
#Converting "drivewheel" into dummies . 
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = car))
#Removing first column from the newly created dummy_3 dataframe containing the dummy values for the
#variable "drivewheel". 
dummy_3 <- dummy_3[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carbody"
#column
car <- cbind(car[,-5], dummy_3)

#enginelocation 
#----------------------------------
summary(car$enginelocation)
#To convert enginelocation variable to numeric is to replace the levels- front with 1 and rear with 0.
levels(car$enginelocation)<-c(1,0)
car$enginelocation<- as.numeric(levels(car$enginelocation))[car$enginelocation]

#enginetype 
#-------------------------------------
summary(car$enginetype)
dummy_4 <- data.frame(model.matrix( ~enginetype, data = car))
dummy_4 <- dummy_4[,-1]
car <- cbind(car[,-11], dummy_4)

#cylindernumber
#------------------------------------
summary(car$cylindernumber)
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = car))
dummy_5 <- dummy_5[,-1]
car <- cbind(car[,-11], dummy_5)

#fuelsystem 
#-------------------------------------
summary(car$fuelsystem)
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = car))
dummy_6 <- dummy_6[,-1]
car <- cbind(car[,-12], dummy_6)

#Deriving New variables
###############################################################
#Variables related to car specifications

car$carWLratio <- car$carwidth/car$carlength
car$carHWratio <- car$carheight/car$carwidth
car$carHLratio <- car$carheight/car$carlength

#Variables related to car performance 

car$avg_mpg <- (car$citympg + car$highwaympg)/2
car$powtoweightratio <- car$horsepower/car$curbweight

#####################################################################################################
#                              MODEL BUILDING STARTS HERE
#####################################################################################################

#set the seed to 100
set.seed(100)

#Save 70% of data set as the training data set and the remaining 30% as the testing data set

# sample function for getting the indices of 70% of observations. 
trainindices= sample(1:nrow(car), 0.7*nrow(car))

#create an object "train.car" and store the 70% of the data of car dataset by just passing the indices
#inside the dataset

train.car = car[trainindices,]

#Similarly store the rest of the observations into an object "test".
#Execute both train and test commands

test = car[-trainindices,]

#Deleting unnecessary columns
train.car <- train.car[,-1]

#Apply lm( ) 
model_1<-lm(price~.,data = train.car)

# The model is stored as an object in the variable 'model_1' 
# Now, we want to check the summary to analyse the results of the model using summary (model_1).

summary(model_1)
# R-squared value is 0.9816 and Adjusted R-squared is 0.967 ,but could observe only few variables are
#significant.

#To identify insignificant columns we can use Step AIC 
step <- stepAIC(model_1, direction="both")

#backward selection method used
#---------------------------------------------------------------------

model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + carwidth + 
                compressionratio + horsepower + Car_Companybmw + Car_Companybuick + 
                Car_Companychevrolet + Car_Companydodge + Car_Companyjaguar + 
                Car_Companymazda + Car_Companymitsubishi + Car_Companynissan + 
                Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                Car_Companyrenault + Car_Companysaab + Car_Companysubaru + 
                Car_Companytoyota + Car_Companyvolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                carWLratio + carHWratio + carHLratio + powtoweightratio,data = car)

 summary(model_2)  
 #Multiple R-squared:  0.9586,	Adjusted R-squared:  0.9488
 
#check for multicollinearity
sort(vif(model_2))
 
#fuelsystemmpfi can be removed since it is having very high vif value 7.2 and p value 0.12

model_3 <- lm(formula = price ~ aspiration + enginelocation + carlength + carwidth + 
                compressionratio + horsepower + Car_Companybmw + Car_Companybuick + 
                Car_Companychevrolet + Car_Companydodge + Car_Companyjaguar + 
                Car_Companymazda + Car_Companymitsubishi + Car_Companynissan + 
                Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                Car_Companyrenault + Car_Companysaab + Car_Companysubaru + 
                Car_Companytoyota + Car_Companyvolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl+ fuelsystemspdi + 
                carWLratio + carHWratio + carHLratio + powtoweightratio,data = car)

summary(model_3) 
#Multiple R-squared:  0.9579,	Adjusted R-squared:  0.9483
sort(vif(model_3))

#fuelsystem2bbl can be dropped since it is having vif = 2.97 and p value 0.421

model_4 <- lm(formula = price ~ aspiration + enginelocation + carlength + carwidth + 
                compressionratio + horsepower + Car_Companybmw + Car_Companybuick + 
                Car_Companychevrolet + Car_Companydodge + Car_Companyjaguar + 
                Car_Companymazda + Car_Companymitsubishi + Car_Companynissan + 
                Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                Car_Companyrenault + Car_Companysaab + Car_Companysubaru + 
                Car_Companytoyota + Car_Companyvolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemspdi + carWLratio + carHWratio + carHLratio + 
                powtoweightratio,data = car)

summary(model_4) 
#Multiple R-squared:  0.9578,	Adjusted R-squared:  0.9484
sort(vif(model_4))
#fuelsystemspdi can be dropped with high vif 2.5 and p value 0.643

model_5 <- lm(formula = price ~ aspiration + enginelocation + carlength + carwidth + 
                compressionratio + horsepower + Car_Companybmw + Car_Companybuick + 
                Car_Companychevrolet + Car_Companydodge + Car_Companyjaguar + 
                Car_Companymazda + Car_Companymitsubishi + Car_Companynissan + 
                Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                Car_Companyrenault + Car_Companysaab + Car_Companysubaru + 
                Car_Companytoyota + Car_Companyvolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + carWLratio + carHWratio + carHLratio + powtoweightratio,data = car)

summary(model_5) 
#Multiple R-squared:  0.9577,	Adjusted R-squared:  0.9487
sort(vif(model_5))
#carWL ratio can be dropped with high vif 1553 and p value 0.223


model_6 <- lm(formula = price ~ aspiration + enginelocation + carlength + carwidth + 
                compressionratio + horsepower + Car_Companybmw + Car_Companybuick + 
                Car_Companychevrolet + Car_Companydodge + Car_Companyjaguar + 
                Car_Companymazda + Car_Companymitsubishi + Car_Companynissan + 
                Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                Car_Companyrenault + Car_Companysaab + Car_Companysubaru + 
                Car_Companytoyota + Car_Companyvolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + carHWratio + carHLratio + powtoweightratio,data = car)
summary(model_6) 
#Multiple R-squared:  0.9574,	Adjusted R-squared:  0.9485
sort(vif(model_6))
# carwidth can be dropped with high vif 166 and p value 

model_7 <- lm(formula = price ~ aspiration + enginelocation + carlength  + compressionratio +
                horsepower + Car_Companybmw + Car_Companybuick + 
                Car_Companychevrolet + Car_Companydodge + Car_Companyjaguar + 
                Car_Companymazda + Car_Companymitsubishi + Car_Companynissan + 
                Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                Car_Companyrenault + Car_Companysaab + Car_Companysubaru + 
                Car_Companytoyota + Car_Companyvolkswagen + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + carHWratio + carHLratio + powtoweightratio,data = car)
summary(model_7) 
#Multiple R-squared:  0.9556,	Adjusted R-squared:  0.9467
sort(vif(model_7))
# Car_Companysubaru  can be dropped with high vif 4.17 and p value 0.06

model_8 <- lm(formula = price ~ aspiration + enginelocation + carlength  + compressionratio +
                horsepower + Car_Companybmw + Car_Companybuick +  Car_Companychevrolet + 
                Car_Companydodge + Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
                Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                Car_Companyrenault + Car_Companysaab  + Car_Companytoyota + Car_Companyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + carHWratio + carHLratio + powtoweightratio,data = car)
summary(model_8) 
#Multiple R-squared:  0.9547,	Adjusted R-squared:  0.9459
sort(vif(model_8))
# drivewheelrwd  can be dropped with high vif and p value 

model_9 <- lm(formula = price ~ aspiration + enginelocation + carlength  + compressionratio +
               horsepower + Car_Companybmw + Car_Companybuick +  Car_Companychevrolet + 
               Car_Companydodge + Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
               Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
               Car_Companyrenault + Car_Companysaab  + Car_Companytoyota + Car_Companyvolkswagen + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
               enginetypeohcv + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               carHWratio + carHLratio + powtoweightratio,data = car)
summary(model_9) 
#Multiple R-squared:  0.9545,	Adjusted R-squared:  0.9461
sort(vif(model_9))
# aspiration  can be dropped with high p value 

model_10 <- lm(formula = price ~ enginelocation + carlength  + compressionratio +
                 horsepower + Car_Companybmw + Car_Companybuick +  Car_Companychevrolet + 
                 Car_Companydodge + Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
                 Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                 Car_Companyrenault + Car_Companysaab  + Car_Companytoyota + Car_Companyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                 enginetypeohcv + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carHWratio + carHLratio + powtoweightratio,data = car)
summary(model_10) 
#Multiple R-squared:  0.9543,	Adjusted R-squared:  0.9461
sort(vif(model_10))
# enginetypeohcv   can be dropped with high p value 

model_11 <- lm(formula = price ~ enginelocation + carlength  + compressionratio +
                 horsepower + Car_Companybmw + Car_Companybuick +  Car_Companychevrolet + 
                 Car_Companydodge + Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
                 Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + 
                 Car_Companyrenault + Car_Companysaab  + Car_Companytoyota + Car_Companyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + carHWratio + carHLratio +
                 powtoweightratio,data = car)
summary(model_11) 
#Multiple R-squared:  0.954,	Adjusted R-squared:  0.9461
sort(vif(model_11))
# compressionratio   can be dropped with high vif and p value 

model_12 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companychevrolet +  Car_Companydodge + Car_Companyjaguar + Car_Companymazda + 
                 Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
                 Car_Companyporsche + Car_Companyrenault + Car_Companysaab  + Car_Companytoyota + 
                 Car_Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 carHWratio + carHLratio + powtoweightratio,data = car)
summary(model_12) 
#Multiple R-squared:  0.9539,	Adjusted R-squared:  0.9463
sort(vif(model_12))
# enginetypeohc   can be dropped with high vif and p value 

model_13 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companychevrolet +  Car_Companydodge + Car_Companyjaguar + Car_Companymazda + 
                 Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
                 Car_Companyporsche + Car_Companyrenault + Car_Companysaab  + Car_Companytoyota + 
                 Car_Companyvolkswagen + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + carHWratio + carHLratio +
                 powtoweightratio,data = car)
summary(model_13) 
#Multiple R-squared:  0.9522,	Adjusted R-squared:  0.9446
sort(vif(model_13))
# Car_Companysaab   can be dropped with high  p value 

model_14 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companychevrolet +  Car_Companydodge + Car_Companyjaguar + Car_Companymazda + 
                 Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
                 Car_Companyporsche + Car_Companyrenault + Car_Companytoyota + Car_Companyvolkswagen +
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + cylindernumberfive +
                 cylindernumberfour + cylindernumbersix + carHWratio + carHLratio +
                 powtoweightratio,data = car)
summary(model_14) 
#Multiple R-squared:  0.9522,	Adjusted R-squared:  0.9447
sort(vif(model_14))
# Car_Companyvolkswagen   can be dropped with high  p value 

model_15 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companychevrolet +  Car_Companydodge + Car_Companyjaguar + Car_Companymazda + 
                 Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
                 Car_Companyporsche + Car_Companyrenault + Car_Companytoyota + carbodyhardtop +
                 carbodyhatchback + carbodysedan + carbodywagon + cylindernumberfive +cylindernumberfour +
                 cylindernumbersix + carHWratio + carHLratio +powtoweightratio,data = car)

summary(model_15) 
#Multiple R-squared:  0.9513,	Adjusted R-squared:  0.9442
sort(vif(model_15))
# Car_Companyrenault can be dropped with high  p value 

model_16 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companychevrolet +  Car_Companydodge + Car_Companyjaguar + Car_Companymazda + 
                 Car_Companymitsubishi + Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
                 Car_Companyporsche + Car_Companytoyota + carbodyhardtop +carbodyhatchback + carbodysedan + 
                 carbodywagon + cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + 
                 carHLratio +powtoweightratio,data = car)

summary(model_16) 
#Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9439
sort(vif(model_16))
# Car_Companynissan can be dropped with high  p value 

model_17 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companychevrolet +  Car_Companydodge + Car_Companyjaguar + Car_Companymazda + 
                 Car_Companymitsubishi + Car_Companypeugeot + Car_Companyplymouth + 
                 Car_Companyporsche + Car_Companytoyota + carbodyhardtop +carbodyhatchback + carbodysedan + 
                 carbodywagon + cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + 
                 carHLratio +powtoweightratio,data = car)

summary(model_17) 
#Multiple R-squared:  0.9503,	Adjusted R-squared:  0.9437
sort(vif(model_17))
# Car_Companychevrolet can be dropped with p value 

model_18 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                  Car_Companydodge + Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
                 Car_Companypeugeot + Car_Companyplymouth + Car_Companyporsche + Car_Companytoyota + 
                 carbodyhardtop +carbodyhatchback + carbodysedan + carbodywagon + cylindernumberfive +
                 cylindernumberfour +cylindernumbersix + carHWratio + carHLratio +powtoweightratio,
                 data = car)

summary(model_18) 
#Multiple R-squared:  0.9503,	Adjusted R-squared:  0.944
sort(vif(model_18))
# Car_Companyplymouth can be dropped with high p value 

model_19 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companydodge + Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
                 Car_Companypeugeot + Car_Companyporsche + Car_Companytoyota + carbodyhardtop +
                 carbodyhatchback + carbodysedan + carbodywagon + cylindernumberfive +
                 cylindernumberfour +cylindernumbersix + carHWratio + carHLratio +powtoweightratio,
                 data = car)


summary(model_19) 
#Multiple R-squared:  0.9502,	Adjusted R-squared:  0.9442
sort(vif(model_19))
# Car_Companytoyota can be dropped with high p value

model_20 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companydodge + Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
                 Car_Companypeugeot + Car_Companyporsche + carbodyhardtop +carbodyhatchback + carbodysedan+
                 carbodywagon + cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + 
                 carHLratio +powtoweightratio, data = car)

summary(model_20) 
#Multiple R-squared:  0.9492,	Adjusted R-squared:  0.9434
sort(vif(model_20))
# Car_Companydodge can be dropped with high  p value

model_21 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw + Car_Companybuick + 
                 Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + Car_Companypeugeot + 
                 Car_Companyporsche + carbodyhardtop +carbodyhatchback + carbodysedan+carbodywagon + 
                 cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + carHLratio +
                 powtoweightratio, data = car)

summary(model_21) 
#Multiple R-squared:  0.9492,	Adjusted R-squared:  0.9437
sort(vif(model_21))
# Car_Companymazda can be dropped with high  p value

model_22 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar + Car_Companymitsubishi + Car_Companypeugeot + 
                 Car_Companyporsche + carbodyhardtop +carbodyhatchback + carbodysedan+carbodywagon + 
                 cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + carHLratio +
                 powtoweightratio, data = car)

summary(model_22) 
#Multiple R-squared:  0.9491,	Adjusted R-squared:  0.9439
sort(vif(model_22))
# Car_Companypeugeot can be dropped with high p value

model_23 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar + Car_Companymitsubishi+ 
                 Car_Companyporsche + carbodyhardtop +carbodyhatchback + carbodysedan+carbodywagon + 
                 cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + carHLratio +
                 powtoweightratio, data = car)
summary(model_23) 
#Multiple R-squared:  0.9481,	Adjusted R-squared:  0.943
sort(vif(model_23))
#  Car_Companyporsche can be dropped with high vif value and p value

model_24 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar + Car_Companymitsubishi+ carbodyhardtop +
                 carbodyhatchback + carbodysedan+carbodywagon + cylindernumberfive +cylindernumberfour +
                 cylindernumbersix + carHWratio + carHLratio +powtoweightratio, data = car)

summary(model_24) 
#Multiple R-squared:  0.946,	Adjusted R-squared:  0.9411
sort(vif(model_24))
# Car_Companymitsubishi  can be dropped with high  p value

model_25 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar + carbodyhardtop +carbodyhatchback + carbodysedan+
                 carbodywagon + cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + 
                 carHLratio +powtoweightratio, data = car)

summary(model_25) 
#Multiple R-squared:  0.9447,	Adjusted R-squared:  0.94
sort(vif(model_25))
# carbodyhardtop  can be dropped with high vif and p value

model_26 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar+carbodyhatchback + carbodysedan+ carbodywagon +
                 cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + 
                 carHLratio +powtoweightratio, data = car)

summary(model_26) 
#Multiple R-squared:  0.9424,	Adjusted R-squared:  0.9378
sort(vif(model_26))
# carbodysedan  can be dropped with high vif and p value

model_27 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar+carbodyhatchback + carbodywagon +
                 cylindernumberfive +cylindernumberfour +cylindernumbersix + carHWratio + 
                 carHLratio +powtoweightratio, data = car)

summary(model_27) 
#Multiple R-squared:  0.9414,	Adjusted R-squared:  0.937
sort(vif(model_27))
# carbodyhatchback    can be dropped with high  p value

model_28 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar + carbodywagon + cylindernumberfive +
                 cylindernumberfour +cylindernumbersix + carHWratio + carHLratio +powtoweightratio, 
                 data = car)

summary(model_28) 
#Multiple R-squared:  0.9385,	Adjusted R-squared:  0.9343
sort(vif(model_28))
# carbodywagon can be dropped with high  p value

model_29 <- lm(formula = price ~ enginelocation + carlength +horsepower + Car_Companybmw +
                 Car_Companybuick + Car_Companyjaguar  + cylindernumberfive +
                 cylindernumberfour +cylindernumbersix + carHWratio + carHLratio +powtoweightratio, 
                 data = car)

summary(model_29) 
#Multiple R-squared:  0.9366,	Adjusted R-squared:  0.9326
sort(vif(model_29))

# All variables are now highly significant, VIF values are also low and has p < 0.05.

# Now, execute this command
Predict_1 <- predict(model_29, test[-19])

##Next, append the predicted results with the test data set 
# to compare the actual prices with the predicted ones

test$test_price <- Predict_1


r <- cor(test$price,test$test_price)
rsquared <- r^2
rsquared

summary(model_29)

#r squared from test dataset is 0.894 and initial model/traning data is 0.9326. The difference is 
#approx 3%, which is reasonably an accurate model

#The final adjusted r square and the r square are similar.
#i.e.,R-squared:  0.9366,	Adjusted R-squared:  0.9326

#Below are the variables that are part of the final model
#-------------------------------------------------------------
#1.enginelocation
#2.carlength
#3.horsepower
#4.Car_Companybmw
#5.Car_Companybuick
#6.Car_Companyjaguar
#7.cylindernumberfive 
#8.cylindernumberfour
#9.cylindernumbersix
#10.carHWratio 
#11.carHLratio
#12.powtoweightratio

############################################## Conclusions ###########################################

# Model_29 predicts car price with sufficent accuracy, contains only highly significant variables
# with no multicollinearity
#Variables used for car price prediction:

# 1. Engine location - price varies with location of engine - rear/front
# 2. Luxury car brand - bmw, buick and jaguar have significantly higher prices than other cars
# 3. Number of cylinders, car featues like length,height and weight, and engine power are key engine
#    parameters controlling price

#Depending on these parameters the Geely Auto manufacturers can develop appropriate car models with
#reasonable pricing.

#*******************************************************************************************************