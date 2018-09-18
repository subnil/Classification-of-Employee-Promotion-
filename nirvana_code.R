setwd("C:/Users/Subhranil/Desktop/code")

# loading libaries
library(caret)
library(dplyr)

# #Load raw data
wns_train = read.csv("C:/Users/Subhranil/Desktop/code/train.csv")
wns_test = read.csv("C:/Users/Subhranil/Desktop/code/test.csv")

# Convert Blank values to NAs
wns_train[wns_train ==""]= NA
wns_test[wns_test ==""]= NA

# Imputing previous year rating 4.5 for NA values for those employee who
# joined recently and don't have any previous years ratings
# The probablity of promotion of those who joined last year is 0.08 which lies
# between 4 and 5 ratting of promotions of other employee who joins atleast two
# years ago
wns_train$previous_year_rating = ifelse(is.na(wns_train$previous_year_rating),"4.5", wns_train$previous_year_rating)
wns_test$previous_year_rating = ifelse(is.na(wns_test$previous_year_rating),"4.5", wns_test$previous_year_rating)

# Check for NAs
colSums(is.na(wns_train))
colSums(is.na(wns_test))

# percentages of NAs
colSums(is.na(wns_train))/NROW(wns_train)
colSums(is.na(wns_test))/NROW(wns_test)

# structure
str(wns_train)

# checking the distribution of numeric variables
densityplot(wns_train$age)
densityplot(wns_test$age)

hist(wns_train$age)
hist(wns_test$age)

densityplot(wns_train$length_of_service)
densityplot(wns_test$length_of_service)

hist(wns_train$length_of_service)
hist(wns_test$length_of_service)

densityplot(wns_train$avg_training_score)
densityplot(wns_test$avg_training_score)

hist(wns_train$avg_training_score)
hist(wns_test$avg_training_score)

# Combining train and test for preprocess
wns_full = bind_rows(wns_train, wns_test)

# checking the structure of full data set
str(wns_full)

# Missing value imputation
library(missForest)
x4<-missForest(wns_full[-14])
wns_full_updated<-x4$ximp

# Add target variable again
wns_full_updated$is_promoted = wns_full$is_promoted

# Check for missing value after imputation
colSums(is.na(wns_full_updated))


# Feature Engineering

wns_full_updated$new_employee <- ifelse(wns_full_updated$length_of_service == 1, 1,0)

wns_full_updated$high_score <- ifelse(wns_full_updated$avg_training_score > 80, 1,0)

wns_full_updated$region_high <- ifelse(wns_full_updated$region %in% c('region_4',	'region_17',	'region_25',	'region_28',
                                                      'region_23',	'region_22',	'region_3',	'region_7',	'region_1'),1,0)

# saving in to different data frame
wns_full_final <-wns_full_updated

# Dividing into train and test
index <- 0:54808
train <- wns_full_final[index,]
test <- wns_full_final[-index,]

# Further diving train in to training and validation
set.seed(123)
index1= sample(54808,45000)
training= train[index1,]
validation= train[-index1,]

# Saving pre processed data set
write.csv(training, "training.csv",row.names = F)
write.csv(validation, "val.csv",row.names = F)
write.csv(train, "processed_train.csv", row.names = F)
write.csv(test,"processed_test.csv", row.names =F)

# Define F1 function 
f1 <- function(tp,fp,fn){
  
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  
  Fscore_rev <- (2 * precision * recall) / (precision + recall)
  print(return(Fscore_rev))
}

###############################################################################
###############################################################################
##############                 Model                        ##################
##############################################################################
##############################################################################
######################## H2o ############################
library(h2o)
## Create an H2O cloud 
h2o.init(nthreads=-1,            ## -1: use all available threads
         max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll()

# Loading data into h2o frame
train_h2o= h2o.importFile(path = normalizePath("C:/Users/Subhranil/Desktop/code/training.csv"))
validation_h2o= h2o.importFile(path = normalizePath("C:/Users/Subhranil/Desktop/code/val.csv"))
test_h2o= h2o.importFile(path = normalizePath("C:/Users/Subhranil/Desktop/code/processed_test.csv"))

# Diamension of the data sets
dim(train_h2o)
dim(validation_h2o)
dim(test_h2o)

# Structure of the data
str(train_h2o)
str(validation_h2o)
str(test_h2o)

# Dropping employee_id
train_h2o <- train_h2o[-c(1)] 
validation_h2o <- validation_h2o[-c(1)]
test_h2o <- test_h2o[-c(1)]

# Convert categorical variables to factor
# previous_year_rating
train_h2o$previous_year_rating = as.character(train_h2o$previous_year_rating)
validation_h2o$previous_year_rating = as.character(validation_h2o$previous_year_rating)
test_h2o$previous_year_rating = as.character(test_h2o$previous_year_rating)

train_h2o$previous_year_rating = as.factor(train_h2o$previous_year_rating)
validation_h2o$previous_year_rating = as.factor(validation_h2o$previous_year_rating)
test_h2o$previous_year_rating = as.factor(test_h2o$previous_year_rating)

# no_of_training
train_h2o$no_of_trainings  = as.character(train_h2o$no_of_trainings )
validation_h2o$no_of_trainings = as.character(validation_h2o$no_of_trainings)
test_h2o$no_of_trainings  = as.character(test_h2o$no_of_trainings)

train_h2o$no_of_trainings  = as.factor(train_h2o$no_of_trainings)
validation_h2o$no_of_trainings = as.factor(validation_h2o$no_of_trainings)
test_h2o$no_of_trainings  = as.factor(test_h2o$no_of_trainings)

# KPIs_met..80
train_h2o$KPIs_met..80. = as.character(train_h2o$KPIs_met..80.)
validation_h2o$KPIs_met..80. = as.character(validation_h2o$KPIs_met..80.)
test_h2o$KPIs_met..80. = as.character(test_h2o$KPIs_met..80.)

train_h2o$KPIs_met..80. = as.factor(train_h2o$KPIs_met..80.)
validation_h2o$KPIs_met..80. = as.factor(validation_h2o$KPIs_met..80.)
test_h2o$KPIs_met..80. = as.factor(test_h2o$KPIs_met..80.)

# awards_won
train_h2o$awards_won. = as.character(train_h2o$awards_won.)
validation_h2o$awards_won. = as.character(validation_h2o$awards_won.)
test_h2o$awards_won. = as.character(test_h2o$awards_won.)

train_h2o$awards_won. = as.factor(train_h2o$awards_won.)
validation_h2o$awards_won. = as.factor(validation_h2o$awards_won.)
test_h2o$awards_won. = as.factor(test_h2o$awards_won.)

# new_employee
train_h2o$new_employee   = as.character(train_h2o$new_employee)
validation_h2o$new_employee  = as.character(validation_h2o$new_employee)
test_h2o$new_employee    = as.character(test_h2o$new_employee)

train_h2o$new_employee  = as.factor(train_h2o$new_employee)
validation_h2o$new_employee  = as.factor(validation_h2o$new_employee)
test_h2o$new_employee  = as.factor(test_h2o$new_employee)

# is_promoted
train_h2o$is_promoted = as.character(train_h2o$is_promoted)
validation_h2o$is_promoted = as.character(validation_h2o$is_promoted)

train_h2o$is_promoted = as.factor(train_h2o$is_promoted)
validation_h2o$is_promoted = as.factor(validation_h2o$is_promoted)

# Response and predictord names
response = 'is_promoted'
predictors = setdiff(names(train_h2o), response)

# Modeling 
# Basic Model
m1= h2o.deeplearning(model_id = 'dl_model_first', training_frame = train_h2o,
                     validation_frame = validation_h2o, x= predictors, y= response, activation = 'Rectifier',
                     hidden = c(20,20,20), epochs = 1000, variable_importances = T)

# Save the model
h2o.saveModel(m1, "deeplearning")
# Check the modele summary
summary(m1)
# Train validation loss curve visualization
plot(m1)

# Confution matrics
h2o.confusionMatrix(m1, train_h2o)
h2o.confusionMatrix(m1,validation_h2o)

################################ Iteration 2  ################################
m2= h2o.deeplearning(model_id = 'dl_model_2', training_frame = train_h2o,
                     validation_frame = validation_h2o, x= predictors, y= response, activation = 'RectifierWithDropout',
                     hidden = c(80), epochs = 1000, variable_importances = T)
# Save model
h2o.saveModel(m2, "deeplearning1")
# Check model summary
summary(m2)
# Train validation loss curve
plot(m2)

# Check Feature importance
X<-as.data.frame(h2o.varimp(m2))

# Confution Matrix
h2o.confusionMatrix(m2, train_h2o)
h2o.confusionMatrix(m2,validation_h2o)

# F1 Score
t.cf<-h2o.confusionMatrix(m5, train_h2o)
val.cf<-h2o.confusionMatrix(m5,validation_h2o)
# F1 on training
f1(t.cf$`1`[2],t.cf$`1`[1], t.cf$`0`[2])
# F1 on validation
f1(val.cf$`1`[2],val.cf$`1`[1],val.cf$`0`[2])

# Prediction on test data
pred_test=h2o.predict(m2, newdata = test_h2o)

# Reload preprocessed test data
test <- read.csv("C:/Users/Subhranil/Desktop/code/test.csv")

# Saving prediction into a dataframe
x=as.data.frame(pred_test)

# Plot p0 probability
hist(x$p0)

# Extracting predicted class
results <-x$predict
results <- as.data.frame(results)
colnames(results) <- c('is_promoted')

# Putting Null inplace of NA 
test$is_promoted <- NULL

# Combining test and prediction
y <- cbind(test,results)

# Subset for final submission
y <- subset(y,select=c(employee_id,is_promoted))

# Write to csv
write.csv(y,'sub_dl_2.csv',row.names = FALSE)

################################ Iteration 3  ################################
m3= h2o.deeplearning(model_id = 'dl_model_3', training_frame = train_h2o,
                     validation_frame = validation_h2o, x= predictors, y= response, activation = 'RectifierWithDropout',
                     hidden = c(80), epochs = 1000, variable_importances = T)
# Save model
h2o.saveModel(m3, "deeplearning2")
# Check model summary
summary(m3)
# Train validation loss curve
plot(m3)

# Check Feature importance
X<-as.data.frame(h2o.varimp(m3))

# Confution Matrix
h2o.confusionMatrix(m3, train_h2o)
h2o.confusionMatrix(m3,validation_h2o)

# F1 Score
t.cf<-h2o.confusionMatrix(m3, train_h2o)
val.cf<-h2o.confusionMatrix(m3,validation_h2o)
# F1 on training
f1(t.cf$`1`[2],t.cf$`1`[1], t.cf$`0`[2])
# F1 on validation
f1(val.cf$`1`[2],val.cf$`1`[1],val.cf$`0`[2])

# Prediction on test data
pred_test=h2o.predict(m3, newdata = test_h2o)

# Reload preprocessed test data
test <- read.csv("C:/Users/Subhranil/Desktop/code/test.csv")

# Saving prediction into a dataframe
x=as.data.frame(pred_test)

# Plot p0 probability
hist(x$p0)

# Extracting predicted class
results <-x$predict
results <- as.data.frame(results)
colnames(results) <- c('is_promoted')

# Putting Null inplace of NA 
test$is_promoted <- NULL

# Combining test and prediction
y <- cbind(test,results)

# Subset for final submission
y <- subset(y,select=c(employee_id,is_promoted))

# Write to csv
write.csv(y,'sub_dl_4.csv',row.names = FALSE)

################################ Iteration 4  ################################
m4= h2o.deeplearning(model_id = 'dl_model_4', training_frame = train_h2o,
                     validation_frame = validation_h2o, x= predictors, y= response, activation = 'RectifierWithDropout',
                     hidden = c(80), epochs = 1000, variable_importances = T)
# Save model
h2o.saveModel(m4, "deeplearning7")
# Check model summary
summary(m4)
# Train validation loss curve
plot(m4)

# Check Feature importance
X<-as.data.frame(h2o.varimp(m4))

# Confution Matrix
h2o.confusionMatrix(m4, train_h2o)
h2o.confusionMatrix(m4,validation_h2o)

# F1 Score
t.cf<-h2o.confusionMatrix(m4, train_h2o)
val.cf<-h2o.confusionMatrix(m4,validation_h2o)
# F1 on training
f1(t.cf$`1`[2],t.cf$`1`[1], t.cf$`0`[2])
# F1 on validation
f1(val.cf$`1`[2],val.cf$`1`[1],val.cf$`0`[2])

# Prediction on test data
pred_test=h2o.predict(m4, newdata = test_h2o)

# Reload preprocessed test data
test <- read.csv("C:/Users/Subhranil/Desktop/code/test.csv")

# Saving prediction into a dataframe
x=as.data.frame(pred_test)

# Plot p0 probability
hist(x$p0)

# Extracting predicted class
results <-x$predict
results <- as.data.frame(results)
colnames(results) <- c('is_promoted')

# Putting Null inplace of NA 
test$is_promoted <- NULL

# Combining test and prediction
y <- cbind(test,results)

# Subset for final submission
y <- subset(y,select=c(employee_id,is_promoted))

# Write to csv
write.csv(y,'sub_dl_7.csv',row.names = FALSE)

################################ Iteration 5  ################################
m5= h2o.deeplearning(model_id = 'dl_model_4', training_frame = train_h2o,
                     validation_frame = validation_h2o, x= predictors, y= response, activation = 'RectifierWithDropout',
                     hidden = c(100), epochs = 1000, variable_importances = T)
# Save model
h2o.saveModel(m5, "deeplearning4")
# Check model summary
summary(m5)
# Train validation loss curve
plot(m5)

# Check Feature importance
X<-as.data.frame(h2o.varimp(m5))

# Confution Matrix
h2o.confusionMatrix(m5, train_h2o)
h2o.confusionMatrix(m5,validation_h2o)

# F1 Score
t.cf<-h2o.confusionMatrix(m5, train_h2o)
val.cf<-h2o.confusionMatrix(m5,validation_h2o)
# F1 on training
f1(t.cf$`1`[2],t.cf$`1`[1], t.cf$`0`[2])
# F1 on validation
f1(val.cf$`1`[2],val.cf$`1`[1],val.cf$`0`[2])

# Prediction on test data
pred_test=h2o.predict(m5, newdata = test_h2o)

# Reload preprocessed test data
test <- read.csv("C:/Users/Subhranil/Desktop/code/test.csv")

# Saving prediction into a dataframe
x=as.data.frame(pred_test)

# Plot p0 probability
hist(x$p0)

# Extracting predicted class
results <-x$predict
results <- as.data.frame(results)
colnames(results) <- c('is_promoted')

# Putting Null inplace of NA 
test$is_promoted <- NULL

# Combining test and prediction
y <- cbind(test,results)

# Subset for final submission
y <- subset(y,select=c(employee_id,is_promoted))

# Write to csv
write.csv(y,'sub_dl_8.csv',row.names = FALSE)


################################ Iteration 6 ################################
m6 = h2o.deeplearning(model_id = 'dl_model_5', training_frame = train_h2o,
                     validation_frame = validation_h2o, x= predictors, y= response, activation = 'RectifierWithDropout',
                     hidden = c(256), epochs = 1000, variable_importances = T)
# Save model
h2o.saveModel(m6, "DL_05")
# Check model summary
summary(m6)
# Train validation loss curve
plot(m5)

# Check Feature importance
X<-as.data.frame(h2o.varimp(m6))

# Confution Matrix
h2o.confusionMatrix(m6, train_h2o)
h2o.confusionMatrix(m6,validation_h2o)

# F1 Score
t.cf<-h2o.confusionMatrix(m6, train_h2o)
val.cf<-h2o.confusionMatrix(m6,validation_h2o)
# F1 on training
f1(t.cf$`1`[2],t.cf$`1`[1], t.cf$`0`[2])
# F1 on validation
f1(val.cf$`1`[2],val.cf$`1`[1],val.cf$`0`[2])

# Prediction on test data
pred_test=h2o.predict(m6, newdata = test_h2o)

# Reload preprocessed test data
test <- read.csv("C:/Users/Subhranil/Desktop/code/test.csv")

# Saving prediction into a dataframe
x=as.data.frame(pred_test)

# Plot p0 probability
hist(x$p0)

# Extracting predicted class
results <-x$predict
results <- as.data.frame(results)
colnames(results) <- c('is_promoted')

# Putting Null inplace of NA 
test$is_promoted <- NULL

# Combining test and prediction
y <- cbind(test,results)

# Subset for final submission
y <- subset(y,select=c(employee_id,is_promoted))

# Write to csv
write.csv(y,'409_25131_us_sub_dl_16thsept_05.csv',row.names = FALSE)

########################## ensemble ####################################
# Read prediction files
df1 <- read.csv("C:/Users/Subhranil/Desktop/code/sub_dl_7.csv")
df2 <- read.csv("C:/Users/Subhranil/Desktop/code/sub_dl_8.csv")

# Create new data frame with employee_id
df3 <- subset(df1, select = employee_id)

# Take vote
df3$is_promoted <- ifelse(df1$is_promoted == 1 | df2$is_promoted== 1, 1,0)

# Write to csv
write.csv(df3, "dl_en_3.csv",row.names = FALSE)

######################### ensemble with three model #####################
# Read prediction files
df4 <- read.csv("C:/Users/Subhranil/Desktop/code/dl_en_3.csv")
df5 <- read.csv("C:/Users/Subhranil/Desktop/code/409_25131_us_sub_dl_16thsept_05.csv")

# Create new data frame with employee_id 
df6 <- subset(df4, select = employee_id)

# Take vote
df6$is_promoted <- ifelse(df4$is_promoted == 1 | df5$is_promoted== 1, 1,0)

# Write to csv
write.csv(df6, "dl_en_4.csv",row.names = FALSE)
