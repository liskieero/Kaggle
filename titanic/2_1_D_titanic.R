#############################################################################
# PROJECT:    Kaggle titanic intro competition
# DETAILS:  	data manipulation
# INPUT: 			train data
# OUTPUT:  		-
# R VERSION:	3.4.1
# PROGRAMMED:	20.12.2017
#############################################################################

# survived as factor
data.train$Survived_factor <- factor(data.train$Survived, levels=c(0,1),
                                    labels=c("No", "Yes"))

# Pclass as factor
data.train$Pclass_factor <- factor(data.train$Pclass, levels=unique(data.train$Pclass),
                                   labels=make.names(unique(data.train$Pclass)))
data.test$Pclass_factor <- factor(data.test$Pclass, levels=unique(data.test$Pclass),
                                   labels=make.names(unique(data.test$Pclass)))

# SibSp as 4-level factor
data.train$SibSp.categ <- factor(ifelse(data.train$SibSp > 2, 3, data.train$SibSp))
data.test$SibSp.categ <- factor(ifelse(data.test$SibSp > 2, 3, data.test$SibSp))

# Parch as 3-level factor
data.train$Parch.categ <- factor(ifelse(data.train$Parch > 2, 3, data.train$Parch))
data.test$Parch.categ <- factor(ifelse(data.test$Parch > 2, 3, data.test$Parch))

# age categorical
age.categ <- cut(data.train$Age, breaks=seq(0,80,20))

# fare categorical
fare.categ <- factor(ifelse(data.train$Fare > 75, 1, 0))

# predictor character vector string
fullSet <- c("Pclass_factor","Sex","Age","SibSp","Parch","Fare")

# scale Age, SibSp, Parch and Fare
data.train$Age.scale <- scale(data.train$Age)
data.train$SibSp.scale <- scale(data.train$SibSp)
data.train$Parch.scale <- scale(data.train$Parch)
data.train$Fare.scale <- scale(data.train$Fare)
data.test$Age.scale <- scale(data.test$Age)
data.test$SibSp.scale <- scale(data.test$SibSp)
data.test$Parch.scale <- scale(data.test$Parch)
data.test$Fare.scale <- scale(data.test$Fare)



##################################################################################
## missing value imputation

## knn imputation
data.train.strip <- data.frame(Sex=data.train$Sex, Pclass_factor=data.train$Pclass_factor,
                                Age=data.train$Age, SibSp.categ=data.train$SibSp.categ,
                                Parch.categ=data.train$Parch.categ, Fare=data.train$Fare)
# preprocess (center, scale, knnImpute)
pp <- preProcess(data.train.strip, method="knnImpute")
# impute train
data.train.impu <- predict(pp, data.train.strip)

data.test.strip <- data.frame(Sex=data.test$Sex, Pclass_factor=data.test$Pclass_factor,
                                Age=data.test$Age, SibSp.categ=data.test$SibSp.categ,
                                Parch.categ=data.test$Parch.categ, Fare=data.test$Fare)
# impute test (using train)
data.test.impu <- predict(pp, data.test.strip)
