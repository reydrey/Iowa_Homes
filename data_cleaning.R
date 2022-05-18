set.seed(123)
train <- read.csv("data.csv")
# read a dataset and do summary()
summary(train)
# check dimension.Saleprice is dependent variable.
dim(train)
# check names of columns
names(train)
#null values
sum(is.na(train$Alley))/length(train$Alley)
sum(is.na(train$PoolQC))/length(train$PoolQC)
sum(is.na(train$Fence))/length(train$Fence)
sum(is.na(train$MiscFeature))/length(train$FiscFeature)
is.na(train$GarageYrBlt) <- 0
is.na(train$GarageArea) <- 0
is.na(train$TotalBsmtSF) <- 0
train$totalSF <- train$TotalBsmtSF + train$GarageArea + train$GrLivArea
# compute correlation
# extruct numeric independent variables
colLst <- c("BsmtUnfSF","GarageArea", 
            "BsmtFinSF1"
            ,"LotFrontage","LotArea"
            ,"MasVnrArea","BsmtFinSF1","TotalBsmtSF"
            ,"GrLivArea","BsmtFullBath","FullBath"
            ,"HalfBath","Fireplaces"
            ,"GarageCars","WoodDeckSF","OpenPorchSF"
            ,"X2ndFlrSF","X3SsnPorch", "SalePrice", "totalSF")
#stored in trainNumeric
trainNumeric <- train[,colLst]
#compute correlations
cor(trainNumeric)
# compute correlation with salePrice
cor(train$GrLivArea,train$SalePrice,use = 'complete.obs')
cor(train$GarageCars,train$SalePrice,use = 'complete.obs')
cor(train$GarageArea,train$SalePrice,use = 'complete.obs')
cor(train$TotalBsmtSF,train$SalePrice,use = 'complete.obs')
cor(train$totalSF,train$SalePrice,use = 'complete.obs')
# compute correlation with SalePrice
cor(trainNumeric,train$SalePrice)
# compute ratio of zero
sum(train$PoolArea == 0)/length(train$PoolArea)
sum(train$BsmtFinSF2 == 0)/length(train$BsmtFinSF2)
sum(train$X2ndFlrSF == 0)/length(train$X2ndFlrSF)
sum(train$LowQualFinSF == 0)/length(train$LowQualFinSF)
sum(train$BsmtFullBath == 0)/length(train$BsmtFullBath)
sum(train$BsmtHalfBath == 0)/length(train$BsmtHalfBath)
sum(train$HalfBath == 0)/length(train$HalfBath)
sum(train$WoodDeckSF == 0)/length(train$WoodDeckSF)
sum(train$EnclosedPorch == 0)/length(train$EnclosedPorch)
sum(train$X3SsnPorch == 0)/length(train$X3SsnPorch)
sum(train$ScreenPorch == 0)/length(train$ScreenPorch)
sum(train$MiscVal == 0)/length(train$MiscVal)
# change data type
train$OverallQual<-as.factor(train$OverallQual)
train$OverallCond<-as.factor(train$OverallCond)
train$YearBuilt<-as.factor(train$YearBuilt)
train$YearRemodAdd<-as.factor(train$YearRemodAdd)
train$GarageYrBlt<-as.factor(train$GarageYrBlt)
train$YrSold<-as.factor(train$YrSold)
# cleaned dataset
colLst2 <- c(
  "YearBuilt", "YearRemodAdd","GarageYrBlt"
  ,"MoSold","YrSold","OverallQual"
  ,"OverallCond","BsmtUnfSF","LotFrontage"
  ,"LotArea","MasVnrArea","BsmtFinSF1"
  ,"TotalBsmtSF","GrLivArea","BsmtFullBath"
  ,"FullBath","HalfBath","Fireplaces"
  ,"GarageCars","GarageArea","WoodDeckSF","OpenPorchSF"
  ,"X2ndFlrSF","X3SsnPorch","SaleCondition"
  ,"MSSubClass","LotShape","LotConfig"
  ,"Neighborhood","HouseStyle","Exterior1st"
  ,"Exterior2nd","MasVnrType","ExterQual"
  ,"BsmtQual","BsmtExposure","GarageFinish"
  ,"BsmtFinType1","Heating","HeatingQC"
  ,"CentralAir","KitchenQual","FireplaceQu"
  ,"GarageType","SalePrice","totalSF"
)
# cleaned dataset
dataCleaned <- train[,colLst2]
# standardalization
dataCleaned$BsmtUnfSF <- scale(dataCleaned$BsmtUnfSF)
dataCleaned$LotFrontage <- scale(dataCleaned$LotFrontage)
dataCleaned$LotArea <- scale(dataCleaned$LotArea)
dataCleaned$MasVnrArea <- scale(dataCleaned$MasVnrArea)
dataCleaned$BsmtFinSF1 <- scale(dataCleaned$BsmtFinSF1)
dataCleaned$TotalBsmtSF <- scale(dataCleaned$TotalBsmtSF)
dataCleaned$GrLivArea <- scale(dataCleaned$GrLivArea)
dataCleaned$FullBath <- scale(dataCleaned$FullBath)
dataCleaned$HalfBath <- scale(dataCleaned$HalfBath)
dataCleaned$Fireplaces <- scale(dataCleaned$Fireplaces)
dataCleaned$GarageCars <- scale(dataCleaned$GarageCars)
dataCleaned$WoodDeckSF <- scale(dataCleaned$WoodDeckSF)
dataCleaned$OpenPorchSF <- scale(dataCleaned$OpenPorchSF)
dataCleaned$X2ndFlrSF <- scale(dataCleaned$X2ndFlrSF)
dataCleaned$X3SsnPorch <- scale(dataCleaned$X3SsnPorch)
#dataCleaned$totalSF <- scale(dataCleaned$totalSF)
#regression model
model <- lm(SalePrice ~ ., data = dataCleaned)
summary(model)
is.na(dataCleaned$GarageYrBlt) <- 0
is.na(dataCleaned$GarageArea) <- 0
is.na(dataCleaned$TotalBsmtSF) <- 0
is.na(dataCleaned$LotFrontage) <- 0
dataD <- dataCleaned[(dataCleaned$SalePrice <= 139000),]
model1 <- lm(SalePrice ~ ., data=dataD)
summary(model1)
partition <- createDataPartition(y=dataD$SalePrice, p=0.80, list=FALSE)
trainingSet <- dataD[partition,]
testingSet <- dataD[-partition,]
preProcess <- preProcess(trainingSet, method=c("center", "scale"))
names(dataD)
model1 <- lm(SalePrice ~ ., data=dataD)
summary(model1)
model2 <- lm(SalePrice ~ MoSold + YrSold + OverallQual + OverallCond + 
               BsmtUnfSF + LotFrontage + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF +
               FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
               MSSubClass + LotShape + LotConfig + Neighborhood + HouseStyle + MasVnrArea + 
               BsmtExposure + GarageFinish + BsmtFinType1 + Heating + HeatingQC + 
               totalSF, data=dataD)

summary(model2)
model3 <- lm(SalePrice ~ MoSold + YrSold + OverallQual + OverallCond + 
                  BsmtUnfSF + LotFrontage + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
                + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
                  LotShape + LotConfig + Neighborhood + HouseStyle + MasVnrArea + 
                  BsmtExposure + GarageFinish + BsmtFinType1 + Heating + HeatingQC + 
                  totalSF, data=dataD)
summary(model3)
model4 <- lm(SalePrice ~ MoSold + YrSold + OverallQual + OverallCond + 
               BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
             + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
               LotShape + LotConfig + HouseStyle + MasVnrArea + BsmtExposure +
               GarageFinish + BsmtFinType1 + Heating + HeatingQC + 
               totalSF, data=dataD)
summary(model4)
model5 <- lm(SalePrice ~ MoSold + YrSold + OverallQual + OverallCond + 
               BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
             + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
               LotShape + LotConfig + HouseStyle + MasVnrArea + BsmtExposure +
               GarageFinish + Heating + HeatingQC + 
               totalSF, data=dataD)
summary(model5)
model6 <- lm(SalePrice ~ MoSold + YrSold + OverallQual + OverallCond + 
               BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
             + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
               LotShape + HouseStyle + MasVnrArea + BsmtExposure +
               GarageFinish + Heating + HeatingQC + 
               totalSF, data=dataD)
summary(model6)
model7 <- lm(SalePrice ~ YrSold + OverallQual + OverallCond + 
                BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
              + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
                LotShape + HouseStyle + MasVnrArea + BsmtExposure +
                GarageFinish + Heating + HeatingQC + 
                totalSF, data=dataD)
summary(model7)
model8 <- lm(SalePrice ~ YrSold + OverallQual + OverallCond + 
               BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
             + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
               LotShape + HouseStyle + MasVnrArea +
               GarageFinish + Heating + HeatingQC + 
               totalSF, data=dataD)
summary(model8)
model9 <- lm(SalePrice ~ YrSold + OverallQual + OverallCond + 
               BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
             + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
               LotShape + MasVnrArea +
               GarageFinish + Heating + HeatingQC + 
               totalSF, data=dataD)
summary(model9)
model10 <- lm(SalePrice ~ YrSold + OverallQual + OverallCond + 
                    BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
                  + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
                    LotShape + MasVnrArea +
                    GarageFinish + Heating + HeatingQC, data=dataD)
summary(model10)
model11 <- lm(SalePrice ~ YrSold + OverallQual + OverallCond + 
                BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
              + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
                LotShape + MasVnrArea +
                GarageFinish + HeatingQC, data=dataD)
summary(model11)
model12 <- lm(SalePrice ~ YrSold + OverallQual + OverallCond + 
                BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
              + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
                MasVnrArea +
                GarageFinish + HeatingQC, data=dataD)
summary(model12)
model13 <- lm(SalePrice ~ YrSold + OverallQual + OverallCond + 
                           BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
                         + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
                           MasVnrArea +
                           HeatingQC, data=dataD)
summary(model13)
model14 <- lm(SalePrice ~ YrSold + OverallQual +
                BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
              + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + OpenPorchSF + 
                MasVnrArea +
                HeatingQC, data=dataD)
summary(model14)
model15 <- lm(SalePrice ~ YrSold + OverallQual +
                BsmtUnfSF + LotArea + MasVnrArea + BsmtFinSF1 + TotalBsmtSF
              + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + 
                MasVnrArea +
                HeatingQC, data=dataD)
summary(model15)
model16 <- lm(SalePrice ~ OverallQual +
                BsmtUnfSF + LotArea + BsmtFinSF1 + TotalBsmtSF
              + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + 
                MasVnrArea +
                HeatingQC, data=dataD)
summary(model16)
# stepwise
m1 <- lm(SalePrice ~ totalSF, data=dataD)
summary(m1)
m2 <- lm(SalePrice ~ totalSF + GarageArea, data=dataD)
summary(m2)
m3 <- lm(SalePrice ~ totalSF + GarageArea + TotalBsmtSF, data=dataD)
summary(m3)
m4 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea, data=dataD)
summary(m4)
m5 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood, data=dataD)
summary(m5)
m6 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces, data=dataD)
summary(m6)
m7 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + TotalBsmtSF, data=dataD)
summary(m7)
m8 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + TotalBsmtSF + OverallQual, data=dataD)
summary(m8)
m9 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + TotalBsmtSF + OverallQual + WoodDeckSF, data=dataD)
summary(m9)
m10 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + TotalBsmtSF + OverallQual + WoodDeckSF + HeatingQC, data=dataD)
summary(m10)
m11 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + OverallQual + WoodDeckSF + HeatingQC, data=dataD)
summary(m11)
m12 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + OverallQual + WoodDeckSF + HeatingQC + BsmtFinSF1, data=dataD)
summary(m12)
m13 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + OverallQual + WoodDeckSF + HeatingQC + BsmtFinSF1 + MasVnrArea, data=dataD)
summary(m13)
m14 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + OverallQual + WoodDeckSF + HeatingQC + BsmtFinSF1 + MasVnrArea + BsmtUnfSF, data=dataD)
summary(m14)
m15 <- lm(SalePrice ~ totalSF + GarageArea + GrLivArea + Neighborhood + Fireplaces + OverallQual + WoodDeckSF + HeatingQC + BsmtFinSF1 + BsmtUnfSF, data=dataD)
summary(m15)
m16 <- lm(SalePrice ~ totalSF + GarageArea + Neighborhood + 
            OverallQual + WoodDeckSF + HeatingQC + BsmtUnfSF, data=dataD)
summary(m16)
m17 <- lm(SalePrice ~ totalSF + Neighborhood + OverallQual + WoodDeckSF + HeatingQC + BsmtUnfSF, data=dataD)
summary(m17)
model16 <- lm(SalePrice ~ OverallQual +
                BsmtUnfSF + LotArea + BsmtFinSF1 + TotalBsmtSF
              + FullBath + HalfBath + Fireplaces + GarageCars + WoodDeckSF + 
                MasVnrArea + Neighborhood +
                HeatingQC, data=dataD)

# create a variable to store only variables from the model
model_data <- select(dataD, SalePrice, totalSF, Neighborhood, OverallQual,
                     WoodDeckSF, HeatingQC, BsmtUnfSF)

# check for numerical variables
crPlots(m17)
# check for interaction and second order terms
base_model <- lm(SalePrice ~ .^2 + I(totalSF^2) + I(BsmtUnfSF^2), data=model_data) #second order + interaction terms
base2_model <- lm(SalePrice ~ .^2, data=model_data) #interaction terms (first order)
#interaction model
WoodDeckSF_TotalSF <- model_data$WoodDeckSF * model_data$totalSF
i_model <- lm(SalePrice ~ totalSF + Neighborhood + OverallQual + WoodDeckSF
              + HeatingQC + BsmtUnfSF + WoodDeckSF_TotalSF, data=model_data)
sf_term <- model_data$totalSF*model_data$BsmtUnfSF
i2_model <- lm(SalePrice ~ totalSF + Neighborhood + OverallQual + WoodDeckSF
               + HeatingQC + BsmtUnfSF + sf_term, data=model_data)
base3_model <- lm(SalePrice ~ . + I(totalSF^2) + I(BsmtUnfSF^2), data=model_data) # second order without interaction terms 
second.orderSF <- model_data$totalSF*model_data$totalSF
si_model <- lm(SalePrice ~ totalSF + OverallQual + WoodDeckSF
               + HeatingQC + BsmtUnfSF + 
                 sf_term + second.orderSF, data=model_data)

final_model <- i2_model
hist(final_model$residuals, breaks=100)
# mean of residuals should be close to 0
mean <- mean(final_model$residuals)
mean
sd <- sd(final_model$residuals)
residual_score <- (final_model$residuals - mean/sd)
residual_score
hist(residual_score, breaks=100)
set.seed(123)
library("car")
durbinWatsonTest(i2_model)
  
  
  
  
  
  
  