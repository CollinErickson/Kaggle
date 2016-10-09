library(magrittr)

# Load in data
trainALL <- read.csv("DigitRecognizer/train.csv")
testALL <- read.csv("DigitRecognizer/test.csv")
sample_submission <- read.csv("DigitRecognizer/sample_submission.csv")

# Split trainALL into training and testing
train_ind <- sample(1:nrow(trainALL), size = .7*nrow(trainALL), replace = F)
train <- trainALL[train_ind,]
test <- trainALL[-train_ind,]
rm(trainALL)

# Explore
class(train)
dim(train)
head(names(train))
tail(names(train))
table(train$label)
table(train$pixel500)


# Fit a linear model
fit <- lm(label ~ ., train)
pred <- round(predict(fit, test))
pred[pred < 0] <- 0
pred[pred > 9] <- 9
table(pred, test$label)
sum(pred == test$label) / length(pred) # 23.5% accurate
rm(fit)


library(nnet)
fitlog <- multinom(factor(label) ~ ., data=train, MaxNWts=1e6, maxit=200)
str(fitlog)
predlog <- predict(fitlog, testALL)

table(predlog, test$label)
sum(predlog == test$label) / length(predlog) 
# 57.3% accurate using only 1000 on training
# 86.7% accurate using all on training


outDF <- data.frame(ImageId=1:length(predlog), Label=predlog)
write.csv(x=outDF, file="DigitRecognizer/multinomSUBMISSION.csv", row.names = F)
head(read.csv("DigitRecognizer/multinomSUBMISSION.csv"))

fitnet <- nnet(factor(label) ~ ., data=train, MaxNWts=1e6, maxit=200,size=20)
str(fitnet)
prednet <- predict(fitnet, test) # probabilities
prednet2 <- apply(prednet, 1, which.max) - 1

table(prednet2, test$label)
sum(prednet2 == test$label) / length(prednet2) 
# 83.2% with maxit=200 and size=20



# SVM
#use_cols <- which(sapply(train, function(xx){!all(xx==xx[1])}))
# Only pick out the columns that aren't all zeros (had problem where only 1 was nonzero, so using 10%)
use_cols <- which(sapply(train, function(xx){sum(xx==xx[1])/length(xx)<.9})) #.1 too big, label might not get included
library(e1071)
svm.model <- svm(factor(label) ~ ., data=train[, use_cols]) # takes a couple minutes
svm.pred <- predict(svm.model, test)
svm.pred %>% str
table(svm.pred, test$label)
sum(svm.pred == test$label) / nrow(test)
# 94.6% very good
# 97.9% after fixing .1 to .9

#now run on all training data
rm(train);rm(test);rm(use_cols);rm(svm.model);rm(svm.pred) # to make sure I don't use these
use_cols <- which(sapply(trainALL, function(xx){sum(xx==xx[1])/length(xx)<.9}))
svm.model <- svm(factor(label) ~ ., data=trainALL[, use_cols])
svm.pred <- predict(svm.model, testALL)
svm.pred %>% str
table(svm.pred)#, testALL$label)
outDF <- data.frame(ImageId=1:length(svm.pred), Label=svm.pred)
write.csv(x=outDF, file="DigitRecognizer/svmSUBMISSION.csv", row.names = F)
head(read.csv("DigitRecognizer/svmSUBMISSION.csv"))
