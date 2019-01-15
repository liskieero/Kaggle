#############################################################################
# PROJECT:    Kaggle titanic intro competition
# DETAILS:  	classification trees cross-validation
# INPUT: 			train data
# OUTPUT:  		-
# R VERSION:	3.4.1
# PROGRAMMED:	8.1.2018 

#############################################################################
library(rpart)

#############################################################################
# fit control
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

# imputed train data with Survived_factor added
data.train.impu$Survived_factor <- data.train.strip$Survived_factor



#############################################################################
## CART classification tree

# train
m1 <- rpart(Survived_factor ~ Sex + Pclass_factor + Age + 
              SibSp.categ + Parch.categ + Fare ,
              data = data.train.strip)

# predict
m1.test <- predict(m1, data.test, type="class")
m1.test <- ifelse(m1.test=="Yes", 1, 0)

# submittable data
data.submit <- data.frame(PassengerId=data.test[,1], Survived=m1.test)
# write file
setwd(der)
write.csv(data.submit, file="titanic_submit_CART.csv", row.names=FALSE, quote=FALSE)



#############################################################################
## CART classification tree with cross-validation

# train
ctFit <- train(Survived_factor ~ Sex + Pclass_factor + Age + 
                 SibSp.categ + Parch.categ + Fare ,
               data = data.train.impu,
               method="rpart",
               tuneLength=30,
               trControl=fitControl)

ctFit

# predict
ctFit.pred <- predict(ctFit, data.test, type="raw", na.action=na.pass)
ctFit.pred <- ifelse(ctFit.pred=="Yes", 1, 0)

# submittable data
data.submit <- data.frame(PassengerId=data.test[,1], Survived=ctFit.pred)
# write file
setwd(der)
write.csv(data.submit, file="titanic_submit_CART_cv.csv", row.names=FALSE, quote=FALSE)
