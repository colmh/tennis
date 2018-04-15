# Jan 2018, BEtting CSV
# Predicting outsiders. Not much in this dataset though. Need to add in other varbiables?
# if i do and they are significant compared to the odds then i know where the bookies weakness is?

#read in csv
library(readr)
df<- read_csv("C:/Users/colm.sweetman.harte/Desktop/2017_betting_csv.csv")
# data fram
df <- data.frame(df)
# check if they outsider wins
df$outsider_win= ifelse(df$AvgW> df$AvgL,1,0)
# remove rows that don't have odds
df2 <- df[!is.na(df$outsider_win),]
#remove rows where there is no outsider.
df3<-df2[!(df2$AvgW == df2$AvgL),]
# outsider odds
df3$outsider_odds= ifelse(df3$AvgW > df3$AvgL,df3$AvgW,df3$AvgL)
df4 <- df3
df4$outsider_win = ifelse(df4$outsider_win ==1,'win','lose')
df4$outsider_win = as.factor(df4$outsider_win)
#iris %>% mutate_if(is.numeric,as.factor)

#****** Part 1 just using a Logistic Regression model.

library(dplyr)
model_dataset_v1 <- select(df3, outsider_win, outsider_odds)
model_dataset_v2 <- select(df4, outsider_win, outsider_odds)


# Simple
set.seed(88)
rows <- sample(nrow(model_dataset_v1))
# Randomly order data: Sonar
model_dataset_v1 <- model_dataset_v1[rows, ]
# Identify row to split on: split
split <- round(nrow(model_dataset_v1) * .80)
# Create train
train <- model_dataset_v1[1:split, ]
# Create test
test <- model_dataset_v1[(split + 1):nrow(model_dataset_v1), ]
#simple model
model <- glm(outsider_win~., family = 'binomial', train)
model_all <- glm(outsider_win~., family = 'binomial', model_dataset_v1)
# Predict on test: p
p <-predict(model, test, type = 'response')
#p <- as.data.frame(p)
p_class <- ifelse(p > 0.35, 1, 0)
#Confustion Maxrix
confusionMatrix(data = p_class,reference = test[["outsider_win"]], positive = "1")
#ROC, AUC?
library(caTools)
colAUC(p, test[["outsider_win"]], plotROC = TRUE)

library(ROCR) #merge data frames for this?
t1 <- cbind(as.numeric(levels(test[["outsider_win"]]))[test[["outsider_win"]]], p) # factors to numeric?
auc.tmp <- performance(t1,"auc") # Still won't work!!

library(pROC)
roc_obj <- roc(category, prediction)
roc_obj <- roc(test[["outsider_win"]], p)
auc(roc_obj)

# About .6665 AUC only... hmmmm??



#***** Part 2: Cross Validation using CARET
#change teh outcome to a factor


# Centre the variables?!
library(caret)
# Get constant Train control to compare models.
set.seed(21)
myFolds <- createFolds(model_dataset_v2$outsider_win , k = 5)
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

#Logistic Regression
model3<- train(
  outsider_win ~.,model_dataset_v2,
  metric = "ROC",
  method = "glm",
  family    = binomial,
  trControl = myControl
)

summary(model3)

getTrainPerf(model3)
model3$resample

first_holdout <- subset(model3$pred, Resample == "Fold01")
head(first_holdout)
# RF
# need mtry when more stats.
# need a algoithm to select variables? At the moment we only have 1!!
model4<- train(
  outsider_win ~.,model_dataset_v2,
  metric = "ROC",
  method = "rf",
  trControl = myControl
)
summary(model4)
model4$resample

# Not sure what i'm doing here!
classifierRandomForest = 
  train(case_success ~ ., 
        data = train_data, trControl = cvCtrl, method = "rf", metric="ROC", tuneGrid = newGrid)

