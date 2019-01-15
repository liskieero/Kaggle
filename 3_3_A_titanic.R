#############################################################################
# PROJECT:    Kaggle titanic intro competition
# DETAILS:  	neural networks cross-validation
# INPUT: 			train data
# OUTPUT:  		-
# R VERSION:	3.4.1
# PROGRAMMED:	28.12.2017 

#############################################################################
# fit control
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 10)

# data for imputation
data.cv <- data.train[,c(5,13,14,15,17,20)]

nnetGrid <- expand.grid(.size = 1:10, 
                        .decay = c(0, 0.1, 1, 2),
                        .bag=FALSE)

#############################################################################
# neural networks with model averaging and repeated cv
m1 <- train(Survived_factor ~ Pclass_factor + Sex + Age.scale + SibSp.categ +
              Parch.categ + Fare.scale,
            method="avNNet",
            linout=FALSE,
            maxit=2000,
            na.action=na.pass,
            preProcess="knnImpute",
            trace=FALSE,
            tuneGrid=nnetGrid,
            data=data.cv)
m1

###
# neural networks with  repeated cv
set.seed(1)
m2 <- train(Survived_factor ~
              Pclass_factor + Sex + Age.scale + 
              SibSp.categ + Fare.scale +
              I(Age.scale^2) + 
              SibSp.categ:Sex +
              Sex:Age.scale +
              Pclass_factor:Sex +
              I(Fare.scale^2),
            method="nnet",
            linout=FALSE,
            maxit=2000,
            na.action=na.pass,
            preProcess="knnImpute",
            trace=FALSE,
            data=data.cv)
m2





#######################################################################################
## predict

# predict
test.pred <- predict(m2, data.test, na.action=na.pass)
test.pred <- ifelse(test.pred=="Yes", 1, 0)

# submittable data
data.submit <- data.frame(PassengerId=data.test[,1], Survived=test.pred)
# write file
setwd(der)
write.csv(data.submit, file="titanic_submit_nn.csv", row.names=FALSE, quote=FALSE)
