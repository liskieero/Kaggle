#############################################################################
# PROJECT:    Kaggle titanic intro competition
# DETAILS:  	Read data
# INPUT: 			train data
# OUTPUT:  		-
# R VERSION:	3.4.1
# PROGRAMMED:	20.12.2017 

####################################################################
library(caret)
library(car)
library(nnet)
library(VIM)


####################################################################
main <- "C:/Projektit/Kaggle/competitions/titanic intro/"

doc <- paste(main, 'doc/', sep = '')
test  <- paste(main, 'test/', sep = '')
data  <- paste(test, 'data/', sep = '')
der   <- paste(data, 'der/', sep = '')
orig  <- paste(data, 'orig/', sep = '')
prg   <- paste(test, 'prg/', sep = '')

setwd(orig)


##################################################################################################
# read train
data.train <- read.csv("train.csv")
# read test
data.test <- read.csv("test.csv")
