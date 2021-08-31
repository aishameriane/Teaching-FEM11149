##########################################################
# Sampling.R
#
# Description: This code treats the variables from the housing problem (Assignment 1)
# for the FEM11149 course and saves the files for test, train and prediction.
#
# Author: A. Schmidt
# Last update: 29/08/2021
##########################################################

#####################################
# LOADING PACKAGES
#####################################

if (!require("plyr")) install.packages("plyr") 
if (!require("kableExtra")) install.packages("kableExtra") 
if (!require("dplyr")) install.packages("dplyr") 

# I used this as reference: https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda/code

#####################################
# LOAD DATA
#####################################
# Data from here: https://www.kaggle.com/srikanthladda/house-price-prediction
url_file <- "https://raw.githubusercontent.com/aishameriane/Teaching-FEM11149/main/complete.csv" 
file <- read.csv(url_file)

# Finding the NA values
NAcol <- which(colSums(is.na(file)) > 0)
sort(colSums(sapply(file[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

#####################################
# TREATING VARIABLES
#####################################

## Create the generic scale for using in several variables
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

#########################
## Treating MiscFeature
#########################
table(file$MiscFeature)

file$MiscFeature[is.na(file$MiscFeature)] <- 'None'
file$MiscFeature                          <- as.factor(file$MiscFeature)

table(file$MiscFeature)

#########################
## Treating Alley
#########################

table(file$Alley)

file$Alley[is.na(file$Alley)] <- 'None'
file$Alley                    <- as.factor(file$Alley)

table(file$Alley)

#########################
## Treating Fence
#########################

table(file$Fence)

file$Fence[is.na(file$Fence)] <- 'None'
file$Fence                    <- as.factor(file$Fence)

table(file$Fence)

#########################
## Treating Fireplace
#########################
table(file$FireplaceQu)

file$FireplaceQu[is.na(file$FireplaceQu)] <- 'None'
file$FireplaceQu                          <-as.integer(revalue(file$FireplaceQu, Qualities))

table(file$FireplaceQu)

#########################
## Treating LotFrontage
#########################

summary(file$LotFrontage)

# Inputing the median

for (i in 1:nrow(file)){
  if(is.na(file$LotFrontage[i])){
    file$LotFrontage[i] <- as.integer(median(file$LotFrontage[file$Neighborhood==file$Neighborhood[i]], na.rm=TRUE)) 
  }
}

summary(file$LotFrontage)


#########################
## Treating LotShape
#########################

table(file$LotShape)

file$LotShape<-as.integer(revalue(file$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))

table(file$LotShape)

#########################
## Treating LotConfig
#########################

table(file$LotConfig)

file$LotConfig <- as.factor(file$LotConfig)

table(file$LotConfig)

#########################
## Treating the Garage variables
#########################

# When there is no info about the garage construction, just use the building year
file$GarageYrBlt[is.na(file$GarageYrBlt)] <- file$YearBuilt[is.na(file$GarageYrBlt)]

# Check if the NA in the qualitative variables are the same
length(which(is.na(file$GarageType)))
length(which(is.na(file$GarageFinish)))
length(which(is.na(file$GarageCond)))
length(which(is.na(file$GarageQual)))

# Looks the same
length(which(is.na(file$GarageType) & is.na(file$GarageFinish) & is.na(file$GarageCond) & is.na(file$GarageQual)))

# Garage type
table(file$GarageType)
file$GarageType[is.na(file$GarageType)] <- 'No Garage'
file$GarageType                         <- as.factor(file$GarageType)
table(file$GarageType)

# Interior finish
table(file$GarageFinish)
file$GarageFinish[is.na(file$GarageFinish)] <- 'No Garage'
file$GarageFinish                         <- as.factor(file$GarageFinish)
table(file$GarageFinish)

# Quality
table(file$GarageQual)
file$GarageQual[is.na(file$GarageQual)] <- 'None'
file$GarageQual                          <-as.integer(revalue(file$GarageQual, Qualities))

table(file$GarageQual)

# Condition

table(file$GarageCond)
file$GarageCond[is.na(file$GarageCond)] <- 'None'
file$GarageCond<-as.integer(revalue(file$GarageCond, Qualities))
table(file$GarageCond)

#########################
## Treating the Basement variables
#########################

#check if all NAs are in the same lines
length(which(is.na(file$BsmtQual)))
length(which(is.na(file$BsmtCond)))
length(which(is.na(file$BsmtExposure)))
length(which(is.na(file$BsmtFinType1)))
length(which(is.na(file$BsmtFinType2)))

length(which(is.na(file$BsmtQual) & is.na(file$BsmtCond) & is.na(file$BsmtExposure) & is.na(file$BsmtFinType1) & is.na(file$BsmtFinType2)))

# There are two extra missings in exposure and FinType2 than in the other 3 variables
# Check if it is the same house with the extra missing
length(which(is.na(file$BsmtExposure) & is.na(file$BsmtFinType2)))

# No -.-

# Find the two houses
file[!is.na(file$BsmtQual) & (is.na(file$BsmtCond) | is.na(file$BsmtExposure) | is.na(file$BsmtFinType1) | is.na(file$BsmtFinType2)), 
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

# Inputing missing
file$BsmtFinType2[333] <- names(sort(-table(file$BsmtFinType2)))[1]
file$BsmtExposure[949] <- names(sort(-table(file$BsmtExposure)))[1]

# Quality can be made ordinal
table(file$BsmtQual)
file$BsmtQual[is.na(file$BsmtQual)] <- 'None'
file$BsmtQual<-as.integer(revalue(file$BsmtQual, Qualities))
table(file$BsmtQual)

# Condition can be made ordinal

table(file$BsmtCond)
file$BsmtCond[is.na(file$BsmtCond)] <- 'None'
file$BsmtCond<-as.integer(revalue(file$BsmtCond, Qualities))
table(file$BsmtCond)

# BsmtExposure can be made ordinal

table(file$BsmtExposure)

file$BsmtExposure[is.na(file$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)

file$BsmtExposure<-as.integer(revalue(file$BsmtExposure, Exposure))
table(file$BsmtExposure)

# BsmtFinType1 can be made ordinal

table(file$BsmtFinType1)

file$BsmtFinType1[is.na(file$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

file$BsmtFinType1<-as.integer(revalue(file$BsmtFinType1, FinType))
table(file$BsmtFinType1)

# BsmtFinType2 can be made ordinal

table(file$BsmtFinType2)
file$BsmtFinType2[is.na(file$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

file$BsmtFinType2<-as.integer(revalue(file$BsmtFinType2, FinType))
table(file$BsmtFinType2)

# Remaining variables with NAs

file[(is.na(file$BsmtFullBath)|is.na(file$BsmtHalfBath)|is.na(file$BsmtFinSF1)|is.na(file$BsmtFinSF2)|is.na(file$BsmtUnfSF)|is.na(file$TotalBsmtSF)), 
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]

## Apparently, none

#########################
## Treating the Masonry  variables
#########################

length(which(is.na(file$MasVnrType)))
length(which(is.na(file$MasVnrArea)))

length(which(is.na(file$MasVnrType) & is.na(file$MasVnrArea)))

table(file$MasVnrType)
file$MasVnrType[is.na(file$MasVnrType)] <- 'None'
file[!is.na(file$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)

Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
file$MasVnrType <- as.integer(revalue(file$MasVnrType, Masonry))
table(file$MasVnrType)

file$MasVnrArea[is.na(file$MasVnrArea)] <-0

#########################
## Treating Kitchen  variables
########################

table(file$KitchenQual)
sum(is.na(file$KitchenQual)) # No missing values

file$KitchenQual<-as.integer(revalue(file$KitchenQual, Qualities))
table(file$KitchenQual)

#########################
## Treating utilities variables
########################

table(file$Utilities)

# No missing values
# There is only one house without water, so this variable will have very little prediction power

#########################
## Treating home functionality variables
########################

table(file$Functional)

#impute mode for the 1 NA
file$Functional[is.na(file$Functional)] <- names(sort(-table(file$Functional)))[1]

file$Functional <- as.integer(revalue(file$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(file$Functional)

#########################
## Treating exterior variables
########################

# Exterior1st

table(file$Exterior1st)
length(which(is.na(file$Exterior1st))) # No NAs

file$Exterior1st <- as.factor(file$Exterior1st)
table(file$Exterior1st)

# Exterior2nd

table(file$Exterior2nd)
length(which(is.na(file$Exterior2nd))) # No NAs

file$Exterior2nd <- as.factor(file$Exterior2nd)
table(file$Exterior2nd)

# ExterQual

table(file$ExterQual)
length(which(is.na(file$ExterQual))) # No NAs

file$ExterQual<-as.integer(revalue(file$ExterQual, Qualities))

table(file$ExterQual)

# ExterCond

table(file$ExterCond)
length(which(is.na(file$ExterCond))) # No NAs

file$ExterCond<-as.integer(revalue(file$ExterCond, Qualities))

table(file$ExterCond)

#########################
## Treating eletrical system variables
########################

table(file$Electrical)
length(which(is.na(file$Electrical))) # 1 NA

#imputing mode where there are NAs
file$Electrical[is.na(file$Electrical)] <- names(sort(-table(file$Electrical)))[1]

file$Electrical <- as.factor(file$Electrical)
table(file$Electrical)

#########################
## Treating sale and type condition variables
########################

# SaleType: Type of sale

table(file$SaleType)
length(which(is.na(file$SaleType))) # 0 NA

file$SaleType <- as.factor(file$SaleType)
table(file$SaleType)

# SaleCondition: Condition of sale

table(file$SaleCondition)
length(which(is.na(file$SaleCondition))) # 0 NA

file$SaleCondition <- as.factor(file$SaleCondition)
table(file$SaleCondition)

#########################
## Changing the city names
########################

table(file$Neighborhood)

file$Neighborhood <- revalue(file$Neighborhood, c(
  'Blmngtn'="Antwerp", 
  'Blueste'="Ghent", 
  'BrDale'="Charleroi", 
  'BrkSide'="Liège", 
  'ClearCr'="Brussels", 
  'CollgCr'="Schaerbeek", 
  'Crawfor'="Anderlecht", 
  'Edwards'="Bruges",
  'Gilbert'="Namur",
  'IDOTRR'="Leuven",
  'MeadowV'="SintJansMolenbeek",
  'Mitchel'="Mons",
  'NAmes'="Ixelles",
  'NoRidge'="Aalst",
  'NPkVill'="Mechelen",
  'NridgHt'="Uccle",
  'NWAmes'="LaLouviere",
  'OldTown'="Hasselt",
  'Sawyer'="SintNiklaas",
  'SawyerW'="Kortrijk",
  'Somerst'="Ostend",
  'StoneBr'="Tournai",
  'SWISU'="Genk",
  'Timber'="Seraing",
  'Veenker'="Roeselare"
  ))

file$Neighborhood <- as.factor(file$Neighborhood)

##########################
# DEALING WITH THE CATEGORIES
##########################

Charcol <- names(file[,sapply(file, is.character)])
Charcol
cat('There are', length(Charcol), 'remaining columns with character values')

#########################
## Foundation
########################

table(file$Foundation)
file$Foundation <- as.factor(file$Foundation)
table(file$Foundation)

#########################
## Heating
########################

table(file$Heating)
file$Heating <- as.factor(file$Heating)
table(file$Heating)

table(file$HeatingQC)
file$HeatingQC <- as.integer(revalue(file$HeatingQC, Qualities))
table(file$HeatingQC)

table(file$CentralAir)
file$CentralAir<-as.integer(revalue(file$CentralAir, c('N'=0, 'Y'=1)))
table(file$CentralAir)

#########################
## Dates
########################

str(file$YrSold)
str(file$MoSold)
file$MoSold <- as.factor(file$MoSold)

#########################
## Roof
########################

table(file$RoofStyle)
file$RoofStyle <- as.factor(file$RoofStyle)
table(file$RoofStyle)

table(file$RoofMatl)
file$RoofMatl <- as.factor(file$RoofMatl)
table(file$RoofMatl)

#########################
## Land 
########################

table(file$LandContour)
file$LandContour <- as.factor(file$LandContour)
table(file$LandContour)

table(file$LandSlope)
file$LandSlope<-as.integer(revalue(file$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(file$LandSlope)

#########################
## dwelling 
########################

table(file$BldgType)
file$BldgType <- as.factor(file$BldgType)
table(file$BldgType)

table(file$HouseStyle)
file$HouseStyle <- as.factor(file$HouseStyle)
table(file$HouseStyle)

#########################
## Neighborhood 
########################

table(file$Neighborhood)
file$Neighborhood <- as.factor(file$Neighborhood)
table(file$Neighborhood)

table(file$Condition1)
file$Condition1 <- as.factor(file$Condition1)
table(file$Condition1)

table(file$Condition2)
file$Condition2 <- as.factor(file$Condition2)
table(file$Condition2)

#########################
## Pavement 
########################

table(file$Street)
file$Street<-as.integer(revalue(file$Street, c('Grvl'=0, 'Pave'=1)))
table(file$Street)

table(file$PavedDrive)
file$PavedDrive<-as.integer(revalue(file$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(file$PavedDrive)

##########################
# Checking everything
##########################

apply(is.na(file), 2, which)

## Only Pool quality has missing, as I wanted it to be.

#####################################
# DELETING VARIABLES FOR ASSIGNMENT 1
#####################################
assignment_1 <- TRUE


if (assignment_1 == TRUE){
  variables_remove <- c("Utilities", "^Alley$",       "^LotFrontage$", "^LotShape$",    "^LandContour$", "^LandSlope$",    "^Condition1$",  "^Condition2$",
                        "^HouseStyle$", "^OverallCond$", "^YearRemodAdd$","^RoofStyle$",   "^RoofMatl$",     "^Exterior1st$", "^Exterior2nd$",
                        "^MasVnrType$", "^MasVnrArea$",  "^ExterQual$",   "^Foundation$",  "^BsmtQual$",     "^BsmtExposure$","^BsmtFinType1$",
                        "^BsmtFinSF1$", "^BsmtFinType2$","^BsmtFinSF2$",  "^BsmtUnfSF$",   "^TotalBsmtSF$",  "^Heating$",     "^HeatingQC$",
                        "^Electrical$", "^X1stFlrSF$",    "^X2ndFlrSF$",    "^LowQualFinSF$","^BsmtFullBath$","^BsmtHalfBath$","^HalfBath$",
                        "^Kitchen$",    "^FullBath$",    "^Bedroom$",   "^BedroomAbvGr$", "KitchenAbvGr",  "^FireplaceQu$", "^GarageType$",   "^GarageYrBlt$", "^GarageFinish$", 
                        "^GarageCars$", "^GarageArea$",  "^GarageCond$",  "^PavedDrive$",  "^WoodDeckSF$",   "^OpenPorchSF$", "^EnclosedPorch$",
                        "^X3SsnPorch$",  "^ScreenPorch$", "^Fence$",       "^MiscFeature$", "^MiscVal$",      "^MoSold$",     "^YrSold$",
                        "^SaleType$",   "^SaleCondition$")
  variable_list <- NA
  
  for (i in 1:length(variables_remove)){
    x <- grep(variables_remove[i], colnames(file))
    variable_list <- c(variable_list, x)
  }
  
  variable_list <- variable_list[-1]
  
  df_uniq <- unique(variable_list)
  length(df_uniq)
  length(variable_list)
  
  file <- file[, -variable_list]
  
  table(file$PoolQC)
  sum(is.na(file$PoolQC))
  file$PoolQC[is.na(file$PoolQC)] <- 'None'
  file$PoolQC<-as.integer(revalue(file$PoolQC, Qualities))
  table(file$PoolQC)
  
}

#########################
# SUBSETTING AND EXPORTING
#########################

if (assignment_1){
  set.seed(6969)
  path <- "C:\\Users\\aisha\\OneDrive\\Documentos\\PhD\\Teaching\\FEM11149 - Introduction to Data Science\\Assignments\\Assignment 1\\housing-price-prediction\\"
} else {
  set.seed(1234)
  path <- "C:\\Users\\aisha\\OneDrive\\Documentos\\PhD\\Teaching\\FEM11149 - Introduction to Data Science\\Datasets\\housing-price-prediction\\"
}

test_sample    <- sample(1:nrow(file), 200)
train_file     <- file[-test_sample,]

predict_sample <- test_sample[1:50]
test_sample    <- test_sample[51:200]

test_file    <- file[test_sample,]
predict_file <- file[predict_sample,]


write.csv(x = train_file, paste0(path, "housingprices_train_sample.csv"))
write.csv(x = test_file, paste0(path, "housingprices_test_sample.csv"))
write.csv(x = predict_file, paste0(path,"housingprices_predict_sample.csv"))
