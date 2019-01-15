#############################################################################
# PROJECT:    Kaggle titanic intro competition
# DETAILS:  	logistic regression cross-validation
# INPUT: 			train data
# OUTPUT:  		-
# R VERSION:	3.4.1
# PROGRAMMED:	20.12.2017

#############################################################################
# fit control
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 10,
  preProcOptions = list(k=5)
  )

# add Survived_factor to data.train.imputed and data.train.strip
data.train.impu$Survived_factor <- data.train$Survived_factor
data.train.strip$Survived_factor <- data.train$Survived_factor

#fitControl.roc <- trainControl(## 5-fold CV
#  method = "repeatedcv",
#  number = 5,
#  ## repeated ten times
#  repeats = 10,
#  summaryFunction = twoClassSummary,
#  classProbs = TRUE)

#####################################################################################
###
# test
set.seed(1)
m.lr.test <- train(Survived_factor ~
                     Pclass_factor + Sex + Age + 
                     SibSp.categ + Fare +
                     I(Age^2) + 
                     SibSp.categ:Sex +
                     Sex:Age +
                     Pclass_factor:Sex +
                     I(Fare^2),
                   method = "glm",
                   family = binomial(link="logit"),
                   trControl = fitControl,
                   data=data.train.impu)
m.lr.test

###
set.seed(1)
m.lr.test.2 <- train(Survived_factor ~
                     Pclass_factor + Sex + Age + 
                     SibSp.categ + Fare +
                     I(Age^2) + 
                     SibSp.categ:Sex +
                     Sex:Age +
                     Pclass_factor:Sex +
                     I(Fare^2),
                   method = "glm",
                   family = binomial(link="logit"),
                   trControl = fitControl,
                   data=data.train.strip,
                   na.action=na.pass,
                   preProcess="knnImpute")
m.lr.test.2

# predict
test.pred <- predict(m.lr.test, data.test.impu, type="raw")
test.pred.2 <- predict(m.lr.test.2, data.test.strip, na.action=na.pass, type="raw")
# unequal prediction classes
ind.uneq <- which(test.pred!=test.pred.2)
# probs predict
test.pred <- predict(m.lr.test, data.test.impu, type="prob")
test.pred.2 <- predict(m.lr.test.2, data.test.strip, na.action=na.pass, type="prob")
test.pred[ind.uneq,]
test.pred.2[ind.uneq,]

###
# cv logistic regression
m.lr <- train(data.train[,fullSet],
              y = data.train$Survived_factor,
              method = "glm",
              family = binomial(link="logit"),
              trControl = fitControl,
              na.action = na.omit)
m.lr

###
# logistic regression with roc output
m.lr.roc <- train(data.train[,fullSet],
              y = data.train$Survived_factor,
              method = "glm",
              family = binomial(link="logit"),
              trControl = fitControl.roc,
              na.action = na.omit)
m.lr.roc

###
# more complicated logistic regression
set.seed(1)
m.lr.full <- train(Survived_factor ~
                     Pclass_factor + Sex + Age.scale + 
                     SibSp.categ + Fare.scale +
                     I(Age.scale^2) + 
                     SibSp.categ:Sex +
                     Sex:Age.scale +
                     Pclass_factor:Sex +
                     I(Fare.scale^2),
              method = "glm",
              family = binomial(link="logit"),
              trControl = fitControl,
              na.action = na.omit,
              data=data.train)
m.lr.full

### 
# final model
set.seed(1)
m.lr.final <- train(Survived_factor ~
                    Pclass_factor + Sex + Age.scale + 
                    SibSp.categ + Fare.scale +
                    I(Age.scale^2) + 
                    SibSp.categ:Sex +
                    Sex:Age.scale +
                    Pclass_factor:Sex +
                    I(Fare.scale^2),
                  method = "glm",
                  family = binomial(link="logit"),
                  trControl = fitControl,
                  na.action=na.pass,
                  preProcess="medianImpute",
                  data=data.cv)
m.lr.final

### 
# final model #2
set.seed(1)
m.lr.final.2 <- train(Survived_factor ~
                      Pclass_factor + Sex + Age.scale + 
                      SibSp.categ +
                      Pclass_factor:Age.scale,
                    method = "glm",
                    family = binomial(link="logit"),
                    trControl = fitControl,
                    na.action=na.pass,
                    preProcess=c("medianImpute"),
                    data=data.cv)
m.lr.final.2

# predict
test.pred <- predict(m.lr.final, data.test, na.action=na.pass)
test.pred2 <- predict(m.lr.final.2, data.test, na.action=na.pass)
test.pred <- ifelse(test.pred=="Yes", 1, 0)
test.pred2 <- ifelse(test.pred=="Yes", 1, 0)

# submittable data
data.submit <- data.frame(PassengerId=data.test[,1], Survived=test.pred)
data.submit2 <- data.frame(PassengerId=data.test[,1], Survived=test.pred2)
# write file
setwd(der)
write.csv(data.submit, file="titanic_submit.csv", row.names=FALSE, quote=FALSE)
write.csv(data.submit2, file="titanic_submit2.csv", row.names=FALSE, quote=FALSE)

#####################################################################################
## regular logistic regression

# age predictor
m1 <- glm(Survived ~ Age + I(Age^2),
          family = binomial(link="logit"),
          data=data.train)

summary(m1)


m2 <- glm(Survived ~ Pclass_factor + Fare.scale + Pclass_factor:Fare.scale,
          family = binomial(link="logit"),
          data=data.train)

summary(m2)
Anova(m2, type="II")








###############################################################################
## graph

# predictor values
x <- seq(0,80,0.1)
# new data for predictions
newdat <- data.frame(Age=x)
# predictions, logit and original scale
newdat$pred <- predict(m1, newdata=newdat, type = "response")
# plot
plot(Survived~Age, data=data.train, xlab="Age", ylab="Survival probability",
     ylim=c(0,1))
#plot(newdat$Age, newdat$pred, type="n", ylim=c(0,1),
#     xlab="Age", ylab="Survival probability")
lines(pred ~ Age, newdat, col="red", lwd=2)





##############################################################################
fit = glm(vs ~ hp, data=mtcars, family=binomial)
summary(fit)
newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
plot(vs~hp, data=mtcars, col="red4")
lines(vs ~ hp, newdat, col="green4", lwd=2)
