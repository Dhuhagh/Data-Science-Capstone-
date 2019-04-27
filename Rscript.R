## Rscript for Adult Census 

library(readr)
library(rpart)
library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)
library(knitr)
library(magrittr)
library(printr)
library(rpart.plot)
library(rattle)
library(randomForest)
##  loading the data 
adult <- read.csv("adult.csv")
# data url https://archive.ics.uci.edu/ml/datasets/Adult
## see structure of the data
str(adult)
# check for NA value 
adult %>% anyNA(adult)
adult <- na.omit(adult)
#simpilify the predicting by deleting these variables 

adult$fnlwgt <- NULL
adult$relationship <- NULL

#Expolotory analysis
# boxplot
p1 <- adult %>% ggplot(aes(income , age, fill= income)) + geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p2 <- adult %>% ggplot(aes(income , hours.per.week,fill= income)) + geom_boxplot(alpha=0.3)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p3 <- adult %>% ggplot(aes(income , education.num,fill= income)) + geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p4 <- adult %>% ggplot(aes(income , capital.gain,fill= income)) + geom_boxplot(alpha=0.3)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p5 <- adult %>% ggplot(aes(income , capital.loss,fill= income)) + geom_boxplot(alpha=0.3)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
gridExtra::grid.arrange(p1 , p2 , p3, p4 ,p5, ncol=2)

# plot for work class variable comparision 
adult %>% ggplot(aes(x = workclass ,fill= income)) + geom_bar() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

# plot for education variable comparision 
adult %>% ggplot(aes(education)) + geom_bar(aes(fill = income)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

# plot for occupations variable comparision 
adult %>% ggplot(aes(x= occupation)) + geom_bar(aes(fill = income ))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

#sex
adult %>% ggplot(aes(adult$sex)) + geom_bar(aes(fill = income ))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

# splitting the data

trainIndex <- createDataPartition(adult$income, times=1,p =0.8, list=FALSE)

train <- adult[trainIndex,]
test <-  adult[-trainIndex,]

#Model Fitting – Machine Learning Techniques #####

## GLM 
censusglm <- glm( income ~ . ,data = train  ,family = binomial )
summary.glm(censusglm,dispersion = NULL, correlation = FALSE,
            symbolic.cor = FALSE)

prob <- predict(censusglm, test , type = 'response')
pred <- rep('<=50K', length(prob))
pred[prob>=.5] <- '>50K'
# confusion matrix for GLM
tb <- table(pred, test$income)
tb
#accuracy GLM
glm_accuracy = sum(diag(tb))/sum(tb)

auc_results <- data_frame(Model = "Generalized Linear Model", Accuracy = glm_accuracy)


## Descision tree
censustree <- rpart( income ~ . , method="class", data = train  )
summary(censustree)

# tree plot 
rpart.plot(censustree)

#confusion matrix for CART and Accurracy 
t_prob <- predict(censustree , test , type = "class" )

tb2 <- confusionMatrix(t_prob,test$income)
tb2
tree_auc <- tb2$overall['Accuracy']
tree_auc  

# Model results 
auc_results <- rbind(auc_results, data.frame(Model = "Decision Tree Model", Accuracy = tree_auc))
auc_results
kable(auc_results,row.names = NA)
# Random forest model 

censusforest <- randomForest(income ~ . ,data = train,importance = TRUE)
censusforest

# fitted model and confusion matrix 
r_pred <- predict(censusforest , test )
tb3<- confusionMatrix(r_pred,test$income)
tb3
rf_auc <- tb3$overall['Accuracy']
#accuracy results 
auc_results <- rbind(auc_results, data.frame(Model = "Random Forest Model", Accuracy = rf_auc))
auc_results
