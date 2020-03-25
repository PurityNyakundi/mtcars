mtcars
 library(dplyr)
library(ggplot2)
library(tidyverse)
library(ranger)

str(mtcars)


glimpse(mtcars)

mtcars$vs<-as.factor(mtcars$vs)
mtcars$carb<-as.factor(mtcars$carb)

str(mtcars)


mtcars$am<-as.factor(mtcars$am)
mtcars$gear<-as.factor(mtcars$gear)

str(mtcars)

mtcars$cyl<-as.factor(mtcars$cyl)
glimpse(mtcars)
str(mtcars)

#Time for EDA

ggplot(mtcars,mapping = aes(x = hp,y = disp,fill = vs))+
  geom_boxplot()+
  ggtitle("Displacement vs gross horsepower for the two different engines")+
  xlab("Gross horsepower")+
  ylab("Displacement (cu.in.)")+
  theme(panel.background = element_rect(fill = "white",colour = "blue"))
#To determine vs(type of engine 1 or 0 ) the displacement and gross horse power are important determiners

ggplot(mtcars,mapping = aes(x = hp,y = disp))+
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)
  ggtitle("Displacement vs gross horsepower for the two different engines")+
  xlab("Gross horsepower")+
  ylab("Displacement (cu.in.)")+
  theme(panel.background = element_rect(fill = "white",colour = "blue"))


ggplot(mtcars,mapping = aes(x = gear,fill = vs))+
  geom_histogram(stat = "count")+
  ggtitle("Check gear for the two engines")+
  xlab("gear")+
  ylab("Frequency")+
  theme(panel.background = element_rect(fill = "white"))

#as seen gear no. three and 5 has more suited for cars with 0 engine while 4 is good for engine 1
# gear determines the type of engine for a car


#check for the weight as a feature


ggplot(mtcars,mapping = aes(x = wt,y = qsec,color = vs))+
  geom_point()+
  facet_wrap(~am)+
  ggtitle("Check for different features")+
  xlab("Weighted")+
  ylab("miles")+
  theme(panel.background = element_rect(fill = "white"))

#lets check what dimensions we have for mtcar dataset
  dim(mtcars)
#not a lot of data 
  #check for cylinders
  ggplot(mtcars,mapping = aes(x = cyl,fill = vs))+
    geom_histogram(stat = "count",binwidth = 0.5)+
    facet_wrap(~am)+
  ggtitle("Number of cylinders where the two engines exist")+
    xlab("Numbers  of cylinders")+
    ylab("frequency")+
    theme(panel.background = element_rect(fill = "white"))
  
  ggplot(mtcars,mapping = aes(x = cyl,fill = vs))+
    geom_histogram(stat = "count",binwidth = 0.5)+
    ggtitle("Number of cylinders where the two engines exist")+
    xlab("Numbers  of cylinders")+
    ylab("frequency")+
    theme(panel.background = element_rect(fill = "white"))
  
#seems all features are important 
#train the data
  
#to reduce any overfitting start with random forest ranger package.
 test <- mtcars[17:32,]
 trainDataset<-mtcars[1:16,]
 view(test)
 view(trainDataset)
 set.seed(1234)
train<-ranger(vs~.,data = mtcars[1:16,],num.trees = 500,respect.unordered.factors = "order")
 train
#woow my out of bug error is only 6% meaning the accuracy 94%
 ft<-predict(train,data = test)
 table(test$vs,predictions(ft))
 
 set.seed(1234)
 train2<-ranger(mpg ~.,data = trainDataset,num.trees = 500,respect.unordered.factors = "order")
 train2
 ft2<-predict(train2,data = test)
 table(test$mpg,predictions(ft))
 
 str(mtcars)

