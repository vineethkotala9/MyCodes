library(tidyverse)
library(ggplot2)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

##Loading the Data
#Load to df
historic_property <- read.csv('/Users/vineethkotala/Desktop/2nd Sem/Big Data/Problem sets/data (1)/historic_property_data.csv')
options(scipen=999)

#First 6 rows
head(historic_property, n = 6)
nrow(historic_property) #50000

#Column names and number
names(historic_property)
ncol(historic_property) #63

#Check for missing values
sum(!complete.cases(historic_property)) #49999
sum(is.na(historic_property))
###############################################################################
##Data Cleaning
#Drop variables
historic_property <- subset(historic_property, select = -c(meta_cdu, char_apts, char_tp_plan, char_tp_dsgn, char_attic_fnsh, char_renovation, char_porch))
ncol(historic_property) #56

#Check for missing Values
sum(!complete.cases(historic_property)) #8296

#Omit missing values
historic_property <- na.omit(historic_property)
nrow(historic_property) #41704

historic_property <- drop_na(historic_property)

#Check for missing values
sum(!complete.cases(historic_property)) #0

str(historic_property)
#Change to Factors or strings?


#########################Lasso Regression
#   
names(historic_property)
sum(is.na(historic_property))
set.seed(1)

train.index <- sample(c(1:dim(historic_property)[1]), 0.6*dim(historic_property)[1])
test.index <- (-train.index)
# 
# #Training Set
train.df <- historic_property[train.index, ]
# 
# #Testing Sete
test.df <- historic_property[-train.index, ]

## Converted Categorical variables to factors
historic_property$char_bsmt <- as.factor(historic_property$char_bsmt)
historic_property$char_air <- as.factor(historic_property$char_air)
historic_property$char_attic_type <- as.factor(historic_property$char_attic_type)
historic_property$char_bsmt_fin <- as.factor(historic_property$char_bsmt_fin)
historic_property$char_cnst_qlty <- as.factor(historic_property$char_cnst_qlty)
historic_property$char_ext_wall<- as.factor(historic_property$char_ext_wall)

historic_property$char_gar1_area <- as.factor(historic_property$char_gar1_area)
historic_property$char_gar1_att <- as.factor(historic_property$char_gar1_att)
historic_property$char_gar1_cnst <- as.factor(historic_property$char_gar1_cnst )
historic_property$char_use  <- as.factor(historic_property$char_use  )

#######################

str(historic_property)
str(historic_property[train.index,])
str(historic_property[test.index,])
historic_property <- subset(historic_property, select = -c(geo_property_zip ))
names(historic_property)


######################

y <- historic_property$sale_price
f <- formula(y ~ 0+.) # exclude intercept (`glmnet` will include one for us)
x <- model.matrix(f, data = historic_property)
cvfit <- cv.glmnet(x[train.index,], y[train.index])

############################
lambda.best <- cvfit$lambda.min
MSE <- min(cvfit$cvm)
cvfit$cvm[cvfit$lambda == lambda.best] #MSE
MSE/10e+6

#################################

historic_property$yhat_lasso <- predict(cvfit, newx=x[test.index], s = "lambda.min")
mean((y - historic_property$yhat_lasso)^2)
