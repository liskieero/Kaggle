#############################################################################
# PROJECT:    Kaggle titanic intro competition
# DETAILS:  	descriptives
# INPUT: 			train data
# OUTPUT:  		-
# R VERSION:	3.4.1
# PROGRAMMED:	20.12.2017 

#############################################################################
## descriptives

# data dimensions
dim(data.train)

# survival
table(data.train$Survived)
prop.table(table(data.train$Survived))

# ticket class
table(data.train$Pclass)

# sex
table(data.train$Sex)
prop.table(table(data.train$Sex))

# age
summary(data.train$Age)
hist(data.train$Age)

# sibsp
table(data.train$SibSp)

# parch
table(data.train$Parch)

# fare
summary(data.train$Fare)
hist(data.train$Fare)


###############################################################################
## 2- and 3-way tables

# p_class and survived
table(data.train$Pclass_factor, data.train$Survived_factor)
prop.table(table(data.train$Pclass_factor, data.train$Survived_factor), 1)

# sex and survived
table(data.train$Sex, data.train$Survived_factor)
prop.table(table(data.train$Sex, data.train$Survived_factor), 1)

# sibsp and survived
table(data.train$SibSp, data.train$Survived_factor)
prop.table(table(data.train$SibSp, data.train$Survived_factor), 1)

# sibsp.cat and survived
table(data.train$SibSp.categ, data.train$Survived_factor)
prop.table(table(data.train$SibSp.categ, data.train$Survived_factor), 1)

# parch and survived
table(data.train$Parch, data.train$Survived_factor)
prop.table(table(data.train$Parch, data.train$Survived_factor), 1)

# parch and survived
table(data.train$Parch.categ, data.train$Survived_factor)
prop.table(table(data.train$Parch.categ, data.train$Survived_factor), 1)

# age.categ and survived
table(age.categ, data.train$Survived_factor)
prop.table(table(age.categ, data.train$Survived_factor), 1)

# age.categ and survived
table(age.categ, data.train$Survived_factor)
prop.table(table(age.categ, data.train$Survived_factor), 1)
prop.table(table(age.categ, data.train$Survived_factor, data.train$Sex), 1)

# fare.categ and survived
table(fare.categ, data.train$Survived_factor)
prop.table(table(fare.categ, data.train$Survived_factor), 1)


