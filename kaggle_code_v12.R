# Connecting to a MySQL on the server
library(sqldf)
library(RJDBC)
library(dplyr)
library(data.table)
library(mice)
library(caret)
library(leaps)
library(xgboost)
library(mgcv) # used to save models
library(ModelMetrics)
library(Metrics)
drv <- JDBC(driverClass="com.mysql.jdbc.Driver"
            ,classPath="C:/Users/Shubham Arora/Desktop/Kaggle/HousesPrices/mysql-connector-java-5.1.47.jar")
conn <- dbConnect(drv, url="jdbc:mysql://127.0.0.1:3306/khou", 
                  user="root", password="shubhamarora")


# run a SQL query and pull in data from the database
train <- dbGetQuery(conn, "select * from train")
scores <- dbGetQuery(conn, "select * from test")
scores$SalePrice <- 99999
submission <- dbGetQuery(conn, "select * from sample_submission")

train_test <- rbind(train, scores)
################################################################################
#EDA

# Checking for any duplicate records 
sqldf("select count(*), count(distinct Id) from train")

d1_train <- train_test
str(d1_train)
summary(d1_train)

#Based on the description of the categorical columns NA values means not present 
#Hence replacing NA values by 'Not Present' in those 
#categorical variables - Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1
#BsmtFinType2, FireplaceQu, GarageType, GarageFinish, GarageQual, GarageCond
#PoolQC,Fence, MiscFeature. The other NA values apart from them are missing values

d1_train$Alley[d1_train$Alley == 'NA'] <- 'NotPresent'
d1_train$BsmtQual[d1_train$BsmtQual == 'NA'] <- 'NotPresent'
d1_train$BsmtCond[d1_train$BsmtCond == 'NA'] <- 'NotPresent'
d1_train$BsmtExposure[d1_train$BsmtExposure == 'NA'] <- 'NotPresent'
d1_train$BsmtFinType1[d1_train$BsmtFinType1 == 'NA'] <- 'NotPresent'
d1_train$BsmtFinType2[d1_train$BsmtFinType2 == 'NA'] <- 'NotPresent'
d1_train$FireplaceQu[d1_train$FireplaceQu == 'NA'] <- 'NotPresent'
d1_train$GarageType[d1_train$GarageType == 'NA'] <- 'NotPresent'
d1_train$GarageFinish[d1_train$GarageFinish == 'NA'] <- 'NotPresent'
d1_train$GarageQual[d1_train$GarageQual == 'NA'] <- 'NotPresent'
d1_train$GarageCond[d1_train$GarageCond == 'NA'] <- 'NotPresent'
d1_train$PoolQC[d1_train$PoolQC == 'NA'] <- 'NotPresent'
d1_train$Fence[d1_train$Fence == 'NA'] <- 'NotPresent'
d1_train$MiscFeature[d1_train$MiscFeature == 'NA'] <- 'NotPresent'


# coerce feature types to their correct types for analysis
d1_train$MSSubClass <- as.factor(d1_train$MSSubClass)   
d1_train$MSZoning <- as.factor(d1_train$MSZoning)    
d1_train$LotFrontage <- as.numeric(d1_train$LotFrontage)   
d1_train$Street <- as.factor(d1_train$Street)       
d1_train$Alley <- as.factor(d1_train$Alley)        
d1_train$LotShape <- as.factor(d1_train$LotShape)
d1_train$LandContour <- as.factor(d1_train$LandContour)
d1_train$Utilities <- as.factor(d1_train$Utilities)
d1_train$LotConfig <- as.factor(d1_train$LotConfig)
d1_train$LandSlope <- as.factor(d1_train$LandSlope)
d1_train$Neighborhood <- as.factor(d1_train$Neighborhood)
d1_train$Condition1 <- as.factor(d1_train$Condition1)
d1_train$Condition2 <- as.factor(d1_train$Condition2)
d1_train$BldgType <- as.factor(d1_train$BldgType)
d1_train$HouseStyle <- as.factor(d1_train$HouseStyle) 
d1_train$RoofStyle <- as.factor(d1_train$RoofStyle)  
d1_train$RoofMatl <- as.factor(d1_train$RoofMatl)
d1_train$Exterior1st <- as.factor(d1_train$Exterior1st)
d1_train$Exterior2nd <- as.factor(d1_train$Exterior2nd)
d1_train$MasVnrType <- as.factor(d1_train$MasVnrType)
d1_train$MasVnrArea <- as.numeric(d1_train$MasVnrArea)
d1_train$ExterQual <- as.factor(d1_train$ExterQual)   
d1_train$ExterCond <- as.factor(d1_train$ExterCond)
d1_train$Foundation <- as.factor(d1_train$Foundation)
d1_train$BsmtQual <- as.factor(d1_train$BsmtQual)      
d1_train$BsmtCond <- as.factor(d1_train$BsmtCond)
d1_train$BsmtExposure <- as.factor(d1_train$BsmtExposure)
d1_train$BsmtFinType1 <- as.factor(d1_train$BsmtFinType1)
d1_train$BsmtFinSF1 <- as.numeric(d1_train$BsmtFinSF1)
d1_train$BsmtFinType2 <- as.factor(d1_train$BsmtFinType2)
d1_train$BsmtFinSF2 <- as.numeric(d1_train$BsmtFinSF2)
d1_train$BsmtUnfSF <- as.numeric(d1_train$BsmtUnfSF)
d1_train$TotalBsmtSF <- as.numeric(d1_train$TotalBsmtSF)
d1_train$Heating <- as.factor(d1_train$Heating)
d1_train$HeatingQC <- as.factor(d1_train$HeatingQC)
d1_train$CentralAir <- as.factor(d1_train$CentralAir)
d1_train$Electrical <- as.factor(d1_train$Electrical)
d1_train$BsmtFullBath <- as.numeric(d1_train$BsmtFullBath)
d1_train$BsmtHalfBath <- as.numeric(d1_train$BsmtHalfBath)
d1_train$KitchenQual <- as.factor(d1_train$KitchenQual)
d1_train$Functional <- as.factor(d1_train$Functional) 
d1_train$FireplaceQu <- as.factor(d1_train$FireplaceQu)   
d1_train$GarageType <- as.factor(d1_train$GarageType) 
d1_train$GarageYrBlt <- as.numeric(d1_train$GarageYrBlt)
d1_train$GarageFinish <- as.factor(d1_train$GarageFinish)
d1_train$GarageCars <- as.numeric(d1_train$GarageCars)
d1_train$GarageArea <- as.numeric(d1_train$GarageArea)
d1_train$GarageQual <- as.factor(d1_train$GarageQual)  
d1_train$GarageCond <- as.factor(d1_train$GarageCond)  
d1_train$PavedDrive <- as.factor(d1_train$PavedDrive)    
d1_train$PoolQC <- as.factor(d1_train$PoolQC)       
d1_train$Fence <- as.factor(d1_train$Fence)        
d1_train$MiscFeature <- as.factor(d1_train$MiscFeature) 
d1_train$SaleType <- as.factor(d1_train$SaleType)
d1_train$SaleCondition <- as.factor(d1_train$SaleCondition)


###################################################################
#Feature engineering
#Imputing the missing values of remaining column 

# %Missing values
Missing <- as.data.frame(sapply(d1_train,function(x) (sum(is.na(x))/length(x))*100))
Missing$names <- rownames(Missing)
colnames(Missing) <- c('MissingPercentage','Columns')

Missing_2 <- Missing[Missing$MissingPercentage>0,]

# There are 3 variables with missing values 
# Imputing missing values 

#write.csv(d1_train,"checkdata.csv")
#imputedValues <- mice(data=d1_train, m=3, method="cart", seed=2016)

d1_train.imp <- missForest(d1_train)
d1_train <- d1_train.imp$ximp


#write.csv(d1_train,"FinalComputedFile.csv")
#d1_train <- read.csv("FinalComputedFile.csv",)

# for(i in 1:ncol(d1_train)){
#   d1_train[is.na(d1_train[,i]), i] <- mean(d1_train[,i], na.rm = TRUE)
# }

###########################################################################
#Data Cleanup
d1_train$Id <- NULL

# the last column in our dataset is our target (SalesPrice). To more easily
# incorporate codes from other projects, labs, assignments, etc. that we have
# learned, we can change the name of that variable to just 'y'
names(d1_train)[80] <- "y"

# also lets make 'y' the first column in our dataset to be consistent in our 
# studies
d1_train <- d1_train[,c(80,1:79)]



################################################################################
## Creating Dummy Variables
################################################################################
# Here we want to take a 'factor variable' or 'categorical variable' and create
# columns for each factor level.

dummies <- dummyVars(y ~ ., data = d1_train)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = d1_train))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d1_train <- cbind(d1_train$y, ex)                              # combine your target variable with Xs
names(d1_train)[1] <- "y"                               # make target variable called 'y'
rm(dummies, ex)                                  # delete temporary things we no longer need



#######################################################################
#Correlationn

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d1_train[,2:ncol(d1_train)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # number of Xs having a corr > some value
summary(descrCor[upper.tri(descrCor)])  

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff?
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- d1_train[,2:ncol(d1_train)][,-highlyCorDescr] # remove those specific columns from your dataset
descrCor2 <- cor(filteredDescr) 

# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])


d1_train <- cbind(d1_train$y, filteredDescr)
names(d1_train)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up



# ################################################################################
# # Identifying linear dependencies and remove them
# ################################################################################
# # Find if any linear combinations exist and which column combos they are.
# # Below I add a vector of 1s at the beginning of the dataset. This helps ensure
# # the same features are identified and removed.
# 
# # first save response
# y <- d1_train$y
# 
# # create a column of 1s. This will help identify all the right linear combos
# d1_train <- cbind(rep(1, nrow(d1_train)), d1_train[2:ncol(d1_train)])
# names(d1_train)[1] <- "ones"
# 
# # identify the columns that are linear combos
# comboInfo <- findLinearCombos(d1_train)
# 
# 
# # remove columns identified that led to linear combos
# d1_train <- d1_train[, -comboInfo$remove]
# 
# # remove the "ones" column in the first column
# d1_train <- d1_train[, c(2:ncol(d1_train))]
# 
# # Add the target variable back to our data.frame
# d1_train <- cbind(y, d1_train)
# 
# rm(y, comboInfo)  # clean up


#######################################################################
#Plotting final dataset and removing outliers
ggplot(data=d1_train, aes(x=factor(OverallQual), y=y))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))


d1_train <- d1_train[-c(524,1299),]



################################################################################
# Standardize (and/ normalize) your input features.
################################################################################
# Here we standardize the input features (Xs) using the preProcess() function 
# by performing a typical Z-score standardization (method = c("center","scale")))
# This will make all the numeric features centered at 0 and have a standard
# deviation of 1. 
#
# As shown in previous class examples, we could have tried a min-max normalization
# ("range") and even tried to make the features more bell-shaped by doing a
# "YeoJohnson" transformation. If you wanted to do those things, you would just
# modify the method like so: method = c("range","YeoJohnson")

# To make sure I do not standardize the dummy variables I'll create a set that 
# contains the 0/1 variables (dCats) and the numeric features (dNums) 
numcols <- apply(X=d1_train, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(d1_train)
catcols <- apply(X=d1_train, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(d1_train)
d1_trainNums <- d1_train[,numcols]
d1_trainCats <- d1_train[,catcols]

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(d1_trainNums[,2:ncol(d1_trainNums)], method = c("center","scale"))

# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this, but it does!
d1_trainNums <- predict(preProcValues, d1_trainNums)

# combine the standardized numeric features with the dummy vars
d1_train <- cbind(d1_trainNums, d1_trainCats)

rm(preProcValues, numcols, catcols, d1_trainNums, d1_trainCats)  # clean up




################################################################################
# Data partitioning
################################################################################
set.seed(1234) # set a seed so you can replicate your results


d1_score <- d1_train[d1_train$y == 99999,]
d1_train <- d1_train[d1_train$y != 99999,]

d1_train$y <- log(d1_train$y)

#identify records that will be used in the training set. Here we are doing a
#85% train/ 15% test split. You might modify this.
inTrain <- createDataPartition(y = d1_train$y,   # outcome variable
                              p = .85,   # % of training data you want
                              list = F)

# create your partitions
d2_train <- d1_train[inTrain,]  # training data set
d2_validation <- d1_train[-inTrain,]  # test data set

#d1_score <- final_score



################################################################################
# Automatic feature selection using using forward and backward selection
################################################################################
# As discussed in class, you'll likely want to perform some feature selection 
# approaches to reduce the data dimensionality. There are a couple reasons for this.
# First, using alot of features will often increase training time, and for some
# models (e.g. neural nets) that can take a long time! Secondly, the more features
# you have, the more likley you will overfit to the train data, and thus have poor
# performance on your holdout/test set.
#
# Here I show you how to do forward and backard selection AUTOMATICALLY using the
# regsubsets() function from the leaps package. This is especially useful if you
# have many features like in this example.

# Here I fit a linear regression on all the features. Look how many are insignificant!!
# You'd be spending alot of time doing backward selection by hand.

#using random forest
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)

rf <- train(y ~ .,
            data = d2_train,
            method = "rf",
            importance=T,    # we add this in or it varImp cannot be computed
            trControl = ctrl,
            tuneLength = 10,
            metric = "RMSE"
)


#using gradient boost - not working
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

label_train <- d2_train$y
#d1_score$y <- NULL

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(d2_train[,-1]), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(d1_score[,-1]))


default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)


# xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, 
#                  showsd = T, stratified = T, print_every_n = 40, 
#                  early_stopping_rounds = 10, maximize = F)

xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 443)



#using lasso
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)

lassofit <- train(y ~ .,
                  data = d2_train,
                  method = "lars",
                  trControl = ctrl,
                  #preProcess=c("center","scale"), # not needed; already transformed
                  tuneLength = 15,                # this specifies various s values
                  metric = "RMSE")


#using svm 
svm_model<-svm(d2_train$y~., data=d2_train[,-1], cost = 1)

################################################################################
# Calcuate train and test performance for the models you generated 
################################################################################
# The postResample() function from caret can be used to estimate the root mean 
# squared error (RMSE), simple R2, and the mean absolute error (MAE) for numeric 
# outcomes.


#######Validation 

##Using Random Forest 
pred_val_rf <- predict(rf,d2_validation)
postResample(pred_val_rf,d2_validation$y)


##Using gradient boost 
pred_val_xgb <- predict(xgb_mod,as.matrix(d2_validation[,-1]))
postResample(pred_val_xgb,d2_validation$y)


##Using lasso
pred_val_lasso <- predict(lassofit,d2_validation)
postResample(pred_val_lasso,d2_validation$y)


##SVM
pred_val_svm <- predict(svm_model,d2_validation[,-1])
postResample(pred_val_svm,d2_validation$y)

#Weighted Average 
pred_val_weighted <- 0.3*pred_val_xgb + 0.1*pred_val_lasso + 0.6*pred_val_svm
postResample(pred_val_weighted,d2_validation$y)

##### Predicting on testing dataset

#d1_score <-final_score
d1_score$y <- NULL

#random forest
pred_test_rf <- exp(predict(rf, d1_score))-1
names(pred_test_rf) <- 'SalePrice'

#gradient boost
pred_test_xgb <- exp(predict(xgb_mod, dtest))-1

#lasso
pred_test_lasso <- exp(predict(lassofit, d1_score))-1

#SVM
pred_test_svm <- exp(predict(svm_model, d1_score))


final_pred <- (pred_test_xgb + pred_test_lasso + pred_test_svm)/3

write.csv(final_pred,"final.csv")



