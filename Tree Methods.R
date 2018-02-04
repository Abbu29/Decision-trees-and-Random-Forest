#Calling the built in data frame of ISLR
library(ISLR)
head(College)
#Exploring the data
library(ggplot2)
print(ggplot(College, aes(x= Room.Board, y= Grad.Rate))+ geom_point(aes(color = Private)))
print(ggplot(College, aes(F.Undergrad)) + geom_histogram(aes(fill= Private),color= 'black'))
print(ggplot(College, aes(Grad.Rate)) + geom_histogram(aes(fill= Private),color= 'black', bins = 30))
library(dplyr)
subset(College,Grad.Rate > 100)
College['Cazenovia College','Grad.Rate'] <- 100
#Spliting the data into training and testing sets
library(caTools)
sample <- sample.split(College$Private, SplitRatio = 0.7)
train <- subset(College, sample == T)
test <- subset(College, sample == F)
#Decision Tree
install.packages('rpart')
library(rpart)
tree <- rpart(College$Private ~ ., method='class', data= train)
printcp(tree)
tree.pred <- predict(tree,test)
head(tree.pred)
tree.preds <- as.data.frame(tree.pred)
joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}
tree.preds$Private <- sapply(tree.preds$Yes,joiner)
head(tree.preds)
table(tree.preds$Private,test$Private)
install.packages('rpart.plot')
library(rpart.plot)
prp(tree)
##########################
####Random Forest#########
##########################
install.packages("randomForest")
library(randomForest)
model <- randomForest(Private ~ ., data= train, importance=TRUE)
model$confusion
model$importance
forest.pred <- predict(model,test)
table(forest.pred, test$Private)








