#############################################################################
# PROJECT:    Kaggle titanic intro competition
# DETAILS:  	k nearest neighbors cross-validation
# INPUT: 			train data
# OUTPUT:  		-
# R VERSION:	3.4.1
# PROGRAMMED:	3.1.2018 

#############################################################################


#############################################################################
# fit control
fitControl <- trainControl(method = "LGOCV")

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# data for training (including missing values)
data.train.strip <- data.frame(Sex=data.train$Sex, Pclass_factor=data.train$Pclass_factor,
                               Age=data.train$Age, SibSp.categ=data.train$SibSp.categ,
                               Parch.categ=data.train$Parch.categ, Fare=data.train$Fare,
                               Survived_factor=data.train$Survived_factor)

# Add Survived_factor to data.train.impu
data.train.impu$Survived_factor <- data.train$Survived_factor

# test data
data.test.strip <- data.frame(Sex=data.test$Sex, Pclass_factor=data.test$Pclass_factor,
                              Age=data.test$Age, SibSp.categ=data.test$SibSp.categ,
                              Parch.categ=data.test$Parch.categ, Fare=data.test$Fare)


#############################################################################
## cv with knn

# train data
TrainData <- data.train.impu[,-c(1,2,4,5.7)]
TrainClasses <- data.train.impu[,7]

knnfit <- train(Survived_factor ~ Age + Fare + Sex + Pclass_factor +
                  SibSp.categ + Parch.categ,
                method = "knn",
                trControl = fitControl,
                data = data.train.impu,
                tuneGrid = data.frame(.k = 1:10))

knnfit

# predict
test.pred <- predict(knnfit, data.test.impu)
test.pred <- ifelse(test.pred=="Yes", 1, 0)

# submittable data
data.submit <- data.frame(PassengerId=data.test[,1], Survived=test.pred)
# write file
setwd(der)
write.csv(data.submit, file="titanic_submit_knn.csv", row.names=FALSE, quote=FALSE)
