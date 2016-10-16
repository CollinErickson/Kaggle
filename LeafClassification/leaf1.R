library(magrittr)

# explore data
trainall <- read.csv('LeafClassification/train.csv')
trainall %>% head
trainall %>% names

trainall %>% .$species %>% table


#split to train and test

train_inds <- sample(1:nrow(trainall),size=.7*nrow(trainall),replace=F)
train <- trainall[train_inds,]
test <- trainall[-train_inds,]
train$id <- NULL
test$id <- NULL

klaR::NaiveBayes(species ~ ., data=train) # gives error

rf <- randomForest::randomForest(species ~ ., data=train)
rf_pred <- predict(rf, test)
(rf_pred == test$species) %>% table  
# gets 287 right, ten wrong