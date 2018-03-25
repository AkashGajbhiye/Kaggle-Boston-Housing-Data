#################################################
# HOUSEING PREDICTION MODEL USING RANDOM FOREST #                      
#################################################



# INITIALIZATION AND READ DATA
setwd("C:/Users/AKASH/Documents/Ds/housing")
train.house <- read.csv("train.csv")
test.house <- read.csv("test.csv")


# REQUIRED LIBRARIES
library(dplyr)
library(ggplot2)
library(class)
library(randomForest)

# DATA PREPARATION
train.house$IsTrain <- TRUE
test.house$IsTrain <- FALSE
test.house$SalePrice <- NA

# COMBINE DATA FOR CLEANING
houseData <- rbind(train.house,test.house)
summary(houseData)


# FETCH DETAILS OF MISSING DATA
for (i in 1:dim(houseData)[2]) {
  if (sum(is.na(houseData[,i])) > 0) {
    clsna <- as.character( names(houseData)[i]  )
    nas <- sum(is.na(houseData[,i]))
    tr <- paste(clsna , "    " , as.character(nas) )
    print(tr)
  }
  
}


# DATA CLEANING
## Exterior1st and  Exterior2nd
summary(houseData$Exterior1st)
summary(houseData$Exterior2nd)

houseData[is.na(houseData$Exterior1st),"Exterior1st"] <- 'VinylSd'       # Assumption 1
houseData[is.na(houseData$Exterior2nd), "Exterior2nd"] <- 'VinylSd'      # Assumption 2


## BsmtFinSF1 and other any missing basement details
summary(houseData$BsmtFinSF1)
houseData[is.na(houseData$BsmtFinSF1), "BsmtFinSF1" ] <- median(houseData$BsmtFinSF1, na.rm = TRUE)

summary(houseData$BsmtFinSF2)
houseData[is.na(houseData$BsmtFinSF2), "BsmtFinSF2" ] <- median(houseData$BsmtFinSF2, na.rm = TRUE)

summary(houseData$BsmtUnfSF)
houseData[is.na(houseData$BsmtUnfSF), "BsmtUnfSF" ] <- median(houseData$BsmtUnfSF, na.rm = TRUE)

summary(houseData$TotalBsmtSF)
houseData[is.na(houseData$TotalBsmtSF), "TotalBsmtSF" ] <- median(houseData$TotalBsmtSF, na.rm = TRUE)

## Electrical
summary(houseData$Electrical)
houseData[is.na(houseData$Electrical), "Electrical" ] <- 'SBrkr'

## GarageCars & GarageArea
summary(houseData$GarageCars)
houseData[is.na(houseData$GarageCars), "GarageCars" ] <- 2

summary(houseData$GarageArea)
houseData[is.na(houseData$GarageArea), "GarageArea" ] <- median(houseData$GarageArea, na.rm = TRUE)

## SaleType
summary(houseData$SaleType)
houseData[is.na(houseData$SaleType), "SaleType" ] <- 'WD'

## BsmtFullBath & BsmtHalfBath
summary(houseData$BsmtFullBath)
houseData[is.na(houseData$BsmtFullBath), "BsmtFullBath"]  <- median(houseData$BsmtFullBath, na.rm = TRUE)
summary(houseData$BsmtHalfBath)
houseData[is.na(houseData$BsmtHalfBath), "BsmtHalfBath"]  <- median(houseData$BsmtHalfBath, na.rm = TRUE)

## Utilities
summary(houseData$Utilities)
houseData[is.na(houseData$Utilities), "Utilities" ] <- 'AllPub'

## MSZoning
summary(houseData$MSZoning)
houseData[is.na(houseData$MSZoning), "MSZoning" ] <- 'RL'

## MasVnrType
summary(houseData$MasVnrType)
houseData[is.na(houseData$MasVnrType), "MasVnrType" ] <- 'None'

## MasVnrArea
summary(houseData$MasVnrArea)
houseData[is.na(houseData$MasVnrArea), "MasVnrArea" ] <- median(houseData$MasVnrArea, na.rm = TRUE)

## KitchenQual
summary(houseData$KitchenQual)
houseData[is.na(houseData$KitchenQual), "KitchenQual" ] <- 'Gd'

## Functional
summary(houseData$Functional)
houseData[is.na(houseData$Functional), "Functional" ] <- 'Typ'

## BsmtFinType1
summary(houseData$BsmtFinType1)
houseData[is.na(houseData$BsmtFinType1), "BsmtFinType1" ] <- 'Unf'

## BsmtExposure
summary(houseData$BsmtExposure)
houseData[is.na(houseData$BsmtExposure), "BsmtExposure" ] <- 'No'

## BsmtCond
summary(houseData$BsmtCond)
houseData[is.na(houseData$BsmtCond), "BsmtCond" ] <- 'TA'

## BsmtQual
summary(houseData$BsmtQual)
set.seed(123)
z <- sample(x = c('Ex', "Fa",  "Gd" , "TA"), 81, prob = c(0.09090909, 0.03100775, 0.4260042, 0.4520789) , replace = TRUE)
l <- 1
for (i in 1:dim(houseData)[1]) {
  if(is.na(houseData$BsmtQual[i])) {
    houseData$BsmtQual[i] <- z[l]
    l <- l + 1
    
  }
}

sum(is.na(houseData$BsmtQual))

## BsmtFinType2
summary(houseData$BsmtFinType2)
houseData[is.na(houseData$BsmtFinType2), "BsmtFinType2" ] <- 'Unf'

## GarageFinish
summary(houseData$GarageFinish)
Z <- sample(x = c('Fin', "RFn",  "Unf"), 159, prob = c(0.2605072, 0.2938406, 0.4456522) , replace = TRUE)
l <- 1
for (i in 1:dim(houseData)[1]) {
  if(is.na(houseData$GarageFinish[i])) {
    houseData$GarageFinish[i] <- z[l]
    l <- l + 1
    
  }
}


## GarageYrBlt
summary(houseData$GarageYrBlt)
for (i in 1:dim(houseData)[1]) {
  if(is.na(houseData$GarageYrBlt[i])) {
    houseData$GarageYrBlt[i] <- houseData$YearBuilt[i]
    }
}

## GarageType
summary(houseData$GarageType)
z <- sample(x = c('2Types', "Attchd",  "Basment", 'BuiltIn', 'CarPort', 'Detchd' ), 157, prob = c(0.008327299, 0.6238233, 0.01303403, 0.06734251, 0.005430847, 0.282042) , replace = TRUE)
l <- 1
for (i in 1:dim(houseData)[1]) {
  if(is.na(houseData$GarageType[i])) {
    houseData$GarageType[i] <- z[l]
    l <- l + 1
    
  }
}

## GarageCond
summary(houseData$GarageCond)
houseData[is.na(houseData$GarageCond), "GarageCond" ] <- 'TA'

## GarageQual
summary(houseData$GarageQual)
houseData[is.na(houseData$GarageQual), "GarageQual" ] <- 'TA'

## LotFrontage
summary(houseData$LotFrontage)
houseData[is.na(houseData$LotFrontage), "LotFrontage" ] <- mean(houseData$LotFrontage, na.rm = TRUE)

## "MiscFeature", "Fence", "PoolQC", "FireplaceQu", "Alley"

cols.dont.want <- c("MiscFeature", "Fence", "PoolQC", "FireplaceQu", "Alley") 
names(houseData)
houseData1 <- houseData[, ! names(houseData) %in% cols.dont.want, drop = F]

summary(houseData1)

# CHECK FOR CLEAN DATA
for (i in 1:dim(houseData1)[2]) {
  if (sum(is.na(houseData1[,i])) > 0) {
    clsna <- as.character( names(houseData1)[i]  )
    nas <- sum(is.na(houseData1[,i]))
    tr <- paste(clsna , "    " , as.character(nas) )
    print(tr)
  }
}


# SPLIT DATA FOR TESTING AND TRAINING
houseData_train <- filter(houseData1, IsTrain == TRUE)
houseData_test <-  filter(houseData1, IsTrain == FALSE)

splitter <- sample(1:1460, 1022, replace = FALSE)

houseData_train.train <- houseData_train[splitter,]
houseData_train.test <- houseData_train[-splitter,]

# RANDOM FOREST MODEL 
mdl <- randomForest(SalePrice~., data = houseData_train.train)
pred <- predict(mdl, newdata = houseData_train.test)
cor(houseData_train.test$SalePrice,pred)^2

tun <- tuneRF(houseData_train.train[,-76] , houseData_train.train[ , 76] , 
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 1000,
            trace = TRUE,
            improve = 0.05
)

mdl2 <- randomForest(SalePrice~., data = houseData_train.train, 
                      ntree = 1000,
                      mtry = 25,
                      importance = TRUE,
                      proximity = TRUE
)
pred <- predict(mdl2, newdata = houseData_train.test)
cor(houseData_train.test$SalePrice,pred)^2

# CREATE FINAL OUTPUT 
final <- NULL
final$Id <- houseData_test$Id
final$SalePrice <- predict(mdl2, newdata = houseData_test)
final <- data.frame(final)
write.csv(final, file = "Submission.csv", row.names = FALSE)

# Kaggle Score 0.15134