library(Amelia)
library(corrplot)
library(caret)
library(plotly)
library(dplyr)
library(ggplot2)
library(GGally)
library(purrr)
library(tidyr)
library(moments)
setwd("C:/Users/ASUS/Desktop/La_trobe/Assignment and tutorials/Predic_analy")
df <-  read.csv("C:/Users/ASUS/Desktop/La_trobe/Assignment and tutorials/Predic_analy/Assignment1/HousingValuationTest-V2.csv")
summary(df)

#Part B -1-b-i- #stats of numerical variables
summary(df$LotArea)
sd(df$LotArea)
skewness(df$LotArea)

summary(df$OverallQuality)
a<-df$OverallQuality
skewness(a)
sd(a)


summary(df$OverallCondition)
b<-df$OverallCondition
skewness(b)

summary(df$TotalBSF)
c<-df$TotalBSF
skewness(c)


summary(df$LowQualFinSF)
d<-df$LowQualFinSF
skewness(d)

summary(df$LivingArea)
e<-df$LivingArea
skewness(e)


summary(df$FullBath)

summary(df$HalfBath)

summary(df$BedroomAbvGr)


summary(df$KitchenAbvGr)
f<-df$KitchenAbvGr
skewness(f)

summary(df$TotalRmsAbvGrd)
g<- df$TotalRmsAbvGrd
skewness(g)


summary(df$Fireplaces)
h<-df$Fireplaces
skewness(h)

summary(df$GarageCars)


summary(df$PoolArea)
i<-df$PoolArea
skewness(i)

summary(df$OpenPorchSF)
j<-df$OpenPorchSF
skewness(j)

summary(df$MoSold)



summary(df$YrSold)


summary(df$SalePrice)
k<-df$SalePrice
skewness(k)

summary(df$YearBuilt)
l<-df$YearBuilt
skewness(l)
s_attr <- df$SalePrice
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="Histogram SalePrice")
boxplot(s_attr, main="Boxplot")

s_attr <- df$YrSold
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="Histogram YrSold")
boxplot(s_attr, main="Boxplot")

s_attr <- df$MoSold
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="Histogram MoSold")
boxplot(s_attr, main="Boxplot")

s_attr <- df$OpenPorchSF
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="Histogram OpenPorchSF")
boxplot(s_attr, main="Boxplot")


s_attr <- df$PoolArea
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="Histogram PoolArea")
boxplot(s_attr, main="Boxplot")

s_attr <- df$GarageCars
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="GarageCars")
boxplot(s_attr, main="Boxplot")


s_attr <- df$Fireplaces
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="FirePlaces")
boxplot(s_attr, main="Boxplot")


s_attr <- df$BedroomAbvGr
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="BedroomsAbvGr")
boxplot(s_attr, main="Boxplot")

s_attr <- df$HalfBath
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="HalfBath")
boxplot(s_attr, main="Boxplot")

s_attr <- df$HalfBath
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="HalfBath")
boxplot(s_attr, main="Boxplot")


s_attr <- df$FullBath
  par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="FullBath")
boxplot(s_attr, main="Boxplot")

s_attr <- df$LivingArea
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="LivingArea")
boxplot(s_attr, main="Boxplot")

s_attr <- df$TotalBSF
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="TotalBSF")
boxplot(s_attr, main="Boxplot")


s_attr <- df$OverallQuality
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="OverallQuality")
boxplot(s_attr, main="Boxplot")



s_attr <- df$OverallCondition
par(mfrow=c(1,2))
hist(s_attr, col="skyblue", main="OverallCondition")
boxplot(s_attr, main="Boxplot")

df %>% 
  keep(is.numeric) %>% #Selecting only numeric columns from a data frame
  gather() %>% # Gather takes multiple columns and collapses into key-value pairs
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + # each facet will use its own independent scale
  geom_histogram() # print histogram

colSums(is.na(df))
missmap(df,col=c('yellow','blue'),y.at=1,y.labels='',legend=TRUE)


# Removing missing values by replacing them with zero
df <- replace(df, is.na(df), 0)

colSums(is.na(df))
head(df)




#Removing missing values by deleting the rows.
#df1<-  read.csv("C:/Users/ASUS/Desktop/La_trobe/Assignment and tutorials/BUS5PB/Assignment1/HousingValuationTest-V2.csv")


colSums(is.na(df))
na.omit(df)


colSums(is.na(df))
head(df)

#Replacing missing values with mean

##df<-  read.csv("C:/Users/ASUS/Desktop/La_trobe/Assignment and tutorials/BUS5PB/Assignment1/HousingValuationTest-V2.csv")
df[] <- lapply(df, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))



#partB-3-e Transforming two right skewed variables
trans_sp <- log(df$SalePrice)
par(mfrow=c(1,2))
hist(df$SalePrice, col="green", main="Original")
hist(trans_sp, col="green", main="Transformed")

trans_tag <- log(df$ TotalRmsAbvGrd)
par(mfrow=c(1,2))
hist(df$TotalRmsAbvGrd, col="green", main="Original")
hist(trans_tag, col="green", main="Transformed")

#boxplots
par(mfrow=c(1,2))
boxplot(df$SalePrice, main="Boxplot")
boxplot(trans_sp, main="Boxplot")

par(mfrow=c(1,2))
boxplot(df$TotalRmsAbvGrd, main="Boxplot")
boxplot(trans_tag, main="Boxplot")
df
df1 <- df 




# Dealing with Nominal/Categorical variables through numbers
df1$CentralAir <- factor(df$CentralAir, levels=c("Y", "N"), labels=c(1,0))
df1$PavedDrive <- factor(df$PavedDrive, levels=c("Y", "N", "P"), labels=c(1,0,-1))
#transforming categorical(nominal) into numeric
#by one hot encoding

slope_Gtl<- as.numeric(df$Slope == "Gtl")
slope_Mod<- as.numeric(df$Slope == "Mod")
slope_Sev<- as.numeric(df$Slope == "Sev")

df1$slope_Gtl <- slope_Gtl
df1$slope_Mod <- slope_Mod
df1$slope_Sev <- slope_Sev





util_AllPub <- as.numeric(df$Utilities == "AllPub")
util_NoSewr <- as.numeric(df$Utilities == "NoSewr")
util_NoSewa <- as.numeric(df$Utilities == "NoSewa")
util_ELO <- as.numeric(df$Utilities == "ELO")

df1$util_AllPub <- util_AllPub
df1$util_NoSewr <- util_NoSewr
df1$util_NoSewa <- util_NoSewa
df1$util_ELO <- util_ELO




LotConfig_Inside <- as.numeric(df$LotConfig == "Inside")
LotConfig_Corner <- as.numeric(df$LotConfig == "Corner")
LotConfig_CulDSac <- as.numeric(df$LotConfig == "CulDSac")
LotConfig_FR2 <- as.numeric(df$LotConfig == "FR2")
LotConfig_FR3 <- as.numeric(df$LotConfig == "FR3")

df1$LotConfig_Inside <- LotConfig_Inside
df1$LotConfig_Corner <- LotConfig_Corner
df1$LotConfig_CulDSac <- LotConfig_CulDSac
df1$LotConfig_FR2 <- LotConfig_FR2
df1$LotConfig_FR3 <- LotConfig_FR3




DwellClass_1Fam <- as.numeric(df$DwellClass == "1Fam")
DwellClass_2FmCon <- as.numeric(df$DwellClass == "2FmCon")
DwellClass_Duplx <- as.numeric(df$DwellClass == "Duplx")
DwellClass_Twnhse <- as.numeric(df$DwellClass == "TwnhsE")
DwellClass_Twnhs1 <- as.numeric(df$DwellClass == "TwnhsI")

df1$DwellClass_1Fam <- DwellClass_1Fam
df1$DwellClass_2FmCon <- DwellClass_2FmCon
df1$DwellClass_Duplx <- DwellClass_2FmCon
df$DwellClass_Twnhse <- DwellClass_Twnhse
df$DwellClass_Twnhs1 <- DwellClass_Twnhs1






LandContour_Lvl <- as.numeric(df$LandContour == "Lvl")
LandContour_Low <- as.numeric(df$LandContour == "Low")
LandContour_Bnk <- as.numeric(df$LandContour == "Bnk")
LandContour_HLS <- as.numeric(df$LandContour == "HLS")

df1$LandContour_Lvl < LandContour_Lvl
df1$LandContour_Low <- LandContour_Low
df1$LandContour_Bnk <- LandContour_Bnk
df1$LandContour_HLS <- LandContour_HLS



GarageType_Attchd <- as.numeric(df$GarageType == "Attchd")
GarageType_Detchd <- as.numeric(df$GarageType == "Detchd")
GarageType_BuiltIn <- as.numeric(df$GarageType == "BuiltIn")
GarageType_0 <- as.numeric(df$GarageType == "0")
GarageType_Basmnt <- as.numeric(df$GarageType == "Basment")
GarageType_CarPort <- as.numeric(df$GarageType == "CarPort")
GarageType_2Types <- as.numeric(df$GarageType == "2Types")

df1$GarageType_Attchd < GarageType_Attchd
df1$GarageType_Detchd <- GarageType_Detchd
df1$GarageType_BuiltIn <- GarageType_BuiltIn
df1$GarageType_0 <- GarageType_0
df1$GarageType_Basmnt <- GarageType_Basmnt
df1$GarageType_CarPort <- GarageType_CarPort
df1$GarageType_2Types <- GarageType_2Types







View(df)

View(df1)




#Removing target variable sale price and seeing the correlation plot for the data
target_V <- df$SalePrice
#df <- subset(df, select = -c(SalePrice))


df %>% 
  keep(is.numeric) %>% #Selecting only numeric columns from a data frame
  gather() %>% # Gather takes multiple columns and collapses into key-value pairs
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + # each facet will use its own independent scale
  geom_histogram() # print histogram
ggcorr(df, label=TRUE)

##
##
##

#Removing variables with correlation ceofficeint greater than 0.5 and greater than -0.5

#df <- subset(df, select = -c( FullBath, BedroomAbvGr) )

#df <- subset(df, select = -c( TotalRmsAbvGrd) )

#df <- subset(df, select = -c(GarageCars, LowQualFinSF, KitchenAbvGr) )
#df <- subset(df, select = -c(HalfBath) )

#Plotting correlation graph after removing some variables with high coefficient of correlation

df %>% 
  keep(is.numeric) %>% #Selecting only numeric columns from a data frame
  gather() %>% # Gather takes multiple columns and collapses into key-value pairs
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + # each facet will use its own independent scale
  geom_histogram() # print histogram
ggcorr(df, label=TRUE)

# Selecting and converting removed variables into a df to find correlation with target variable SAlePRice
df_relation <- data.frame(df$SalePrice, df$TotalRmsAbvGrd,df$FullBath, df$HalfBath, df$BedroomAbvGr,
                          df$GarageCars, df$LowQualFinSF)
#FInding acorrelation for new df df_relation.

df_relation %>% 
  keep(is.numeric) %>% #Selecting only numeric columns from a data frame
  gather() %>% # Gather takes multiple columns and collapses into key-value pairs
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + # each facet will use its own independent scale
  geom_histogram() # print histogram
ggcorr(df_relation, label=TRUE)

M <- data.matrix(df_relation)
corrM <- cor(M)

#Adding target variable salePrice back in df
#df$SalePrice <- target_V
df_selected <- subset(df, select = c(SalePrice, LivingArea, OpenPorchSF, FullBath, PoolArea))
df_selected
target_V1 <- df_selected$SalePrice
##Creating separate df for models using selected variables
#df_model <-data.frame( df$LotShape, df$LivingArea)

#Making Linear model with tailored data set(df_model)
smpl_size <- floor(2/3 * nrow(df_selected))
set.seed(1)

df_selected <- df_selected[sample(nrow(df_selected)), ]  
SalePrice.train <- df_selected[1:smpl_size, ]
SalePrice.test <-  df_selected[(smpl_size+1):nrow(df_selected), ]

formula = SalePrice.train$SalePrice ~.
model <- lm(formula = formula, data = SalePrice.train[,c(2,3,4,5)])

SalePrice.train[,c(2,3,4,5)]
summary(model)$coefficients     


# Model equation 
as.formula(
  paste0("y ~ ", round(coefficients(model)[1],2), " + ", 
         paste(sprintf("%.2f * %s",coefficients(model)[-1], 
                       names(coefficients(model)[-1])), 
               collapse=" + ")
  )
)
   


SalePrice.train$predicted.target_V1 <- predict(model, SalePrice.train)
SalePrice.test$predicted.target_V1 <- predict(model, SalePrice.test)



#Predicted values using the test set

print("Actual Values")
head(SalePrice.test$mvalue[1:6])
print("Predicted Values")
head(SalePrice.test$predicted.target_V1[1:6])


#Printing R squared value to analyse model fit. 

r_squared <- summary(model)$r.squared
print(paste("R Squared: ", r_squared))
#R squared came out to be:  0.537342921291359

 #Printing RMSE
error <- (SalePrice.test$target_V1) - (SalePrice.test$predicted.target_V1) 
error
rmse <- sqrt(mean(error^2))
print(paste("Root Mean Square Error: ", rmse))





##DECISION TREES
##Part-c-2
##Making decision tree
##DT
##Three trees


library(rpart.plot)
library(GGally)
setwd("C:/Users/ASUS/Desktop/La_trobe/Assignment and tutorials/Predic_analy")
df <-  read.csv("C:/Users/ASUS/Desktop/La_trobe/Assignment and tutorials/Predic_analy/Assignment1/HousingValuationTest-V2.csv")
df <- replace(df, is.na(df), 0)


colSums(is.na(df))
df_D <- data.frame(df$SalePrice, df$LotArea, df$LotShape, df$Utilities, df$OverallQuality, df$LivingArea)
View(df_D)




smp_size <- floor(2/3 * nrow(df_D))
nrow(df_D)

set.seed(2)
df_D.selected <- df_D[sample(nrow(df_D)), ]
sample(5)

SalePrice.train <- df_D.selected[1:smp_size, ] 

SalePrice.test <- df_D.selected[(smp_size+1):nrow(df_D.selected), ] 



#Specifying target and input variables
formula = SalePrice.train$df.SalePrice ~.
SalePrice.train



dtree <- rpart(formula, data=SalePrice.train, method="anova")

#Checking the feature importance
dtree$variable.importance

#Visualizing the decision tree
rpart.plot(dtree, type = 4, fallen.leaves = FALSE)


#Printing the results of decision tree
print(dtree)

#Making Predictions and Assessment

#Predictions
predicted.SP1 <- predict(dtree, SalePrice.test)


#Model assessment
error <- SalePrice.test$df.SalePrice - predicted.SP1
rmse <- sqrt(mean(error^2))
print(paste("Root Mean Square Error: ", rmse))
printcp(dtree)


dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"]

pruned_dtree <- prune(dtree, cp = 0.04) 
#decision tree with cp value 0.04
rpart.plot(pruned_dtree, type = 4, fallen.leaves = FALSE)

predicted_pruned.df.SalePrice <- predict(pruned_dtree, SalePrice.test)
error_new <- SalePrice.test$df.SalePrice-predicted_pruned.df.SalePrice
rmse_new <- sqrt(mean(error_new^2))
print(paste("New Root Mean Square Error: ", rmse_new))

#decision tree with cp value 0.022
pruned_dtree <- prune(dtree, cp = 0.022) 
rpart.plot(pruned_dtree, type = 4, fallen.leaves = FALSE)

predicted_pruned.df.SalePrice <- predict(pruned_dtree, SalePrice.test)
error_new <- SalePrice.test$df.SalePrice-predicted_pruned.df.SalePrice
rmse_new <- sqrt(mean(error_new^2))
print(paste("New Root Mean Square Error: ", rmse_new))





