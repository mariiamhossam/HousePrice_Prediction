
library(dplyr)
library(DescTools)
library(glmnet)
library(corrplot)
library(gplots)
library(ggplot2)

#setwd("D:/4th year/2nd term/Distributed computing/house-prices-advanced-regression-techniques/House_Price_Project.R")
train_data <- read.csv("D:/College/8th Sem/Distributed Computing/Labs/Project_Data/train.csv")
test_data <- read.csv("D:/College/8th Sem/Distributed Computing/Labs/Project_Data/test.csv")

Id<- test_data$Id
null_counts <- sapply(train_data, function(col) sum(is.na(col) | col=="NaN" | col=="NA" | col==""))
# Print the results
#print(null_counts)

library(scales)
scaling <- function(column) {
  # (column - min(column, na.rm = TRUE)) / (max(column, na.rm = TRUE) - min(column, na.rm = TRUE))
  scaled <- rescale(column)
  return(scaled)
}

get_mode <- function(column)
{
  mode_value <- names(sort(table(column), decreasing = TRUE))[1]
  column <- ifelse(is.na(column), mode_value, column)
  return(column)
}

fill_with_zero <- function(column)
{
  column <- ifelse(is.na(column)|is.nan(column)| column == "NA"| column == "", 0, column)
  return(column)
}

preprocessing <- function(data) {
  
  #Dealing with Nulls
  col_median <- median(data[['LotFrontage']], na.rm = TRUE)
  data[['LotFrontage']][is.na(data[['LotFrontage']])] <- col_median
  col_median <- median(data[['GarageArea']], na.rm = TRUE)
  data[['GarageArea']][is.na(data[['GarageArea']])] <- col_median
  col_median <- median(data[['TotalBsmtSF']], na.rm = TRUE)
  data[['TotalBsmtSF']][is.na(data[['TotalBsmtSF']])] <- col_median
  col_median <- median(data[['BsmtUnfSF']], na.rm = TRUE)
  data[['BsmtUnfSF']][is.na(data[['BsmtUnfSF']])] <- col_median
  col_median <- median(data[['BsmtFinSF2']], na.rm = TRUE)
  data[['BsmtFinSF2']][is.na(data[['BsmtFinSF2']])] <- col_median
  col_median <- median(data[['BsmtFinSF1']], na.rm = TRUE)
  data[['BsmtFinSF1']][is.na(data[['BsmtFinSF1']])] <- col_median
  col_median <- median(data[['GarageYrBlt']], na.rm = TRUE)
  data[['GarageYrBlt']][is.na(data[['GarageYrBlt']])] <- col_median
  
  data[["BsmtFinType1"]] <- fill_with_zero(data[["BsmtFinType1"]])
  data[["BsmtFinType2"]] <- fill_with_zero(data[["BsmtFinType2"]])
  data[["GarageType"]] <- fill_with_zero(data[["GarageType"]])
  data[["PoolQC"]] <- fill_with_zero(data[["PoolQC"]])
  data[["BsmtCond"]] <- fill_with_zero(data[["BsmtCond"]])
  data[["BsmtQual"]] <- fill_with_zero(data[["BsmtQual"]])
  data[["BsmtExposure"]] <- fill_with_zero(data[["BsmtExposure"]])
  data[["Fence"]] <- fill_with_zero(data[["Fence"]])
  data[["MasVnrArea"]] <- fill_with_zero(data[["MasVnrArea"]])
  
  
  data[['Electrical']] <- get_mode(data[["Electrical"]])
  data[['FireplaceQu']] <- get_mode(data[["FireplaceQu"]])
  data[["GarageFinish"]] <- get_mode(data[["GarageFinish"]])
  data[['GarageQual']] <- get_mode(data[["GarageQual"]])
  data[['GarageCond']] <- get_mode(data[["GarageCond"]])
  data[['MasVnrType']] <- get_mode(data[["MasVnrType"]])
  data[['Functional']] <- get_mode(data[["Functional"]])
  data[['SaleType']] <- get_mode(data[["SaleType"]])
  data[['GarageCars']] <- get_mode(data[["GarageCars"]])
  data[['KitchenQual']] <- get_mode(data[["KitchenQual"]])
  data[['BsmtHalfBath']] <- get_mode(data[["BsmtHalfBath"]])
  data[['BsmtFullBath']] <- get_mode(data[["BsmtFullBath"]])
  data[['Exterior2nd']] <- get_mode(data[["Exterior2nd"]])
  data[['Exterior1st']] <- get_mode(data[["Exterior1st"]])
  data[['Utilities']] <- get_mode(data[["Utilities"]])
  data[['MSZoning']] <- get_mode(data[["MSZoning"]])
  
  # get the current year as a character string
  current_year <- format(Sys.Date(), "%Y")
  
  # convert the current year to an integer
  current_year <- as.integer(current_year)
  
  # subtract a year from each value in the "year" column
  data$YearBuilt <- current_year-data$YearBuilt
  data$GarageYrBlt <- current_year-data$GarageYrBlt 
  data$YrSold <- current_year-data$YrSold
  data$YearRemodAdd <- current_year-data$YearRemodAdd 
  data$YearRemodAdd<-ifelse(data$YearRemodAdd-data$YearBuilt == 0, 0, 1)

  #Get dummies for Categorical columns
  columns_to_transform <- c("MSZoning", "Street", "LotShape","LandContour","LotConfig","LandSlope",
                            "Neighborhood","BldgType","HouseStyle","RoofStyle","RoofMatl", "Electrical",
                            "Heating","GarageType","SaleType","SaleCondition", 
                            "Exterior1st","Exterior2nd","Condition1", "Condition2","Foundation","MasVnrType")
  
  # loop over the list of column names
  for (col in columns_to_transform) {
    # create the dummy variables for the current column
    df_with_dummies <- model.matrix(~ data[[col]] - 1, data)
    col_names <- paste0(col, "_", colnames(df_with_dummies))
    colnames(df_with_dummies) <- col_names
    # add the dummy variables to the original data frame
    data <- cbind(data, df_with_dummies)
  }
  
  
  # create a dictionary for ExterQual, ExterCond,BsmtQual,BsmtCond,HeatingQC,KitchenQual,
  #FireplaceQu,GarageQual,GarageCond,PoolQC
  dict <- c("Ex" = 5,
            "Gd" = 4,
            "TA" = 3,
            "Fa" = 2,
            "Po" = 1,
            "0"  = 0)
  # create a dictionary for BsmtExposure
  dict2 <- c("Gd" = 4,
             "Av" = 3,
             "Mn" = 2,
             "No" = 1,
             "0" = 0)
  
  
  # create a dictionary for BsmtFinType1 and BsmtFinType2
  dict3 <- c("GLQ" =6,
             "ALQ" = 5,
             "BLQ" = 4,
             "Rec" = 3,
             "LwQ" = 2,
             "Unf" = 1,
             "0"  = 0 )
  # create a dictionary for Functional
  dict4 <- c("Typ" = 7,
             "Min1"= 6,
             "Min2" =5,
             "Mod" = 4,
             "Maj1" =3,
             "Maj2"= 2,
             "Sev" = 1,
             "Sal" = 0)
  # create a dictionary for GarageFinish
  dict5 <- c("Fin" =3,
             "RFn"= 2,
             "Unf" =1,
             "0" = 0)
  # create a dictionary for PavedDrive
  dict6 <- c("Y" =3,
             "P"= 2,
             "N" =1)
  # create a dictionary for Fence
  dict7 <- c("GdPrv"=4,
             "MnPrv"=3,
             "GdWo" =2,
             "MnWw" =1,
             "0"=0)
  # create a dictionary for Utilities
  dict8 <- c("AllPub"=4,
             "NoSewr"=3,
             "NoSeWa" =2,
             "ELO" =1,
             "0"=0)
  
  # loop over the column and replace the values
  for (i in 1:nrow(data)) {
    data$ExterQual[i] <- dict[match(data$ExterQual[i], names(dict))]
    data$ExterCond[i] <- dict[match(data$ExterCond[i], names(dict))]
    data$BsmtQual[i] <- dict[match(data$BsmtQual[i], names(dict))]
    data$BsmtCond[i] <- dict[match(data$BsmtCond[i], names(dict))]
    data$BsmtExposure[i] <- dict2[match(data$BsmtExposure[i], names(dict2))]
    data$BsmtFinType1[i] <- dict3[match(data$BsmtFinType1[i], names(dict3))]
    data$BsmtFinType2[i] <- dict3[match(data$BsmtFinType2[i], names(dict3))]
    data$HeatingQC[i] <- dict[match(data$HeatingQC[i], names(dict))]
    data$KitchenQual[i] <- dict[match(data$KitchenQual[i], names(dict))]
    data$Functional[i] <- dict4[match(data$Functional[i], names(dict4))]
    data$FireplaceQu[i] <- dict[match(data$FireplaceQu[i], names(dict))]
    data$GarageFinish[i] <- dict5[match(data$GarageFinish[i], names(dict5))]
    data$GarageQual[i] <- dict[match(data$GarageQual[i], names(dict))]
    data$GarageCond[i] <- dict[match(data$GarageCond[i], names(dict))]
    data$PavedDrive[i] <- dict6[match(data$PavedDrive[i], names(dict6))]
    data$PoolQC[i] <- dict[match(data$PoolQC[i], names(dict))]
    data$Fence[i] <- dict7[match(data$Fence[i], names(dict7))]
    data$Utilities[i] <- dict8[match(data$Utilities[i], names(dict8))]
    
  }
  
  # create a list of column names to apply the function to
  columns_to_scale <- c("BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF","BsmtFinSF2","LotArea","TotalBsmtSF",
                        "LotFrontage","MSSubClass","X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","GarageArea",
                        "WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","MiscVal","MasVnrArea")
  
  # apply the function to each column in the list
  for(col in columns_to_scale)
  {
    data[[col]] <- scaling(data[[col]])
  }
  
  data$CentralAir<-ifelse(data$CentralAir == "Y", 1, 0)
  
  #Extra Feautures (A Plus)
  data$MiscFeature <- ifelse(is.na(data$MiscFeature), 0, 1)

  #dropping columns
  data <- select(data,-MSZoning,-Alley,-Street,-LotShape,-LandContour,-Utilities,-LotConfig,-LandSlope,-Neighborhood,-Condition1,
                 -Condition2,-BldgType,-HouseStyle,-RoofStyle,-Exterior1st,-RoofMatl,-Exterior2nd,-MasVnrType,
                 -Foundation,-Heating,-GarageType,-MoSold,-SaleType,-SaleCondition, -Electrical,-Id)
  # null_counts <- sapply(data, function(col) sum(is.na(col) | col=="NaN" | col=="NA" | col==""))
  # print(null_counts)
  
  # change the data type of all columns to numeric 
  data <- apply(data, 2, as.numeric)
  data <- as.data.frame(data)
  return(data)
}



train_feature_selection <- function(data,y){
  
  df <- cbind(data, SalePrice = y)
  correlations <- cor(df)  # calculate the corr of each column with SalePrice column
  
  col_gd <- colorRampPalette(c("#BB5577", "#FF9988", "#FFFFFF", "#99AACD", "#4422AA"))
  corrplot(correlations, method = "color", col = col_gd(20), type = "upper", tl.col = "black",tl.cex = 0.4)
  
  correlations <- cor(data,y)
  #print(correlations)
  
  
  data <- data[, correlations <= -0.12 | correlations >= 0.12]
  
  
  return(data)
}

y <- train_data$SalePrice
train_data <- select(train_data,-SalePrice)

# null_counts <- sapply(train_data, function(col) sum(is.na(col) | col=="NaN" | col=="NA" | col==""))
# print(null_counts)

# library(tidyverse)
# df_num <- train_data %>% select_if(is.numeric)
# par(mfrow = c(3, 3))
# # Create a histogram plot
# lapply(names(df_num), function(col) {
#   x <- df_num[[col]]
#   if (!all(is.na(x))) {
#     hist(x,col = "#B37FB3", main = col,xlab = "Values", ylab = "Frequency", breaks = 10)
#   }
# })

train_data <- preprocessing(train_data)
train_data<-train_feature_selection(train_data,y)
print(ncol(train_data))
print("#############################################################################################")

test_data <- preprocessing(test_data)
test_data$GarageCars <- as.integer(test_data$GarageCars)
test_data$BsmtHalfBath <- as.integer(test_data$BsmtHalfBath)
test_data$BsmtFullBath <- as.integer(test_data$BsmtFullBath)

# null_counts <- sapply(test_data, function(col) sum(is.na(col) | col=="NaN" | col=="NA" | col==""))
# print(null_counts)

common_cols <- intersect(names(train_data), names(test_data))
test_data <- select(test_data, all_of(common_cols))

print(ncol(train_data))
print(ncol(test_data))

write.csv(train_data, file = "train_data.csv", row.names = FALSE)
write.csv(test_data, file = "test_data.csv", row.names = FALSE)


# Decision Tree
# library(caret)
# library(rpart)
# DT_model <- rpart(y ~ ., train_data, method = "class")
# testPredictions <- predict(DT_model, test_data, type = "class")

# SVM model
# library(e1071)
# svm_model <- svm(y ~ ., data = train_data, kernel = "radial")
# testPredictions <- predict(svm_model, newdata = test_data)

# Gradient Boosting Regression 

# model <- gbm(y ~. ,data = train_data,distribution="gaussian")
# testPredictions <- predict(model, newdata = test_data)

# Linear Regression
model <- glm(y ~., family = gaussian, data = train_data)
testPredictions <- predict(model, newdata = test_data)

submission <- data.frame(SalePrice = testPredictions)


submission<-cbind(Id,submission)
write.csv(submission, "D:/College/8th Sem/Distributed Computing/Labs/Project_Data/submission.csv", row.names = FALSE)

