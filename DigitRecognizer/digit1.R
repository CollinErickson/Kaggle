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
fitlog <- multinom(factor(label) ~ ., data=train, MaxNWts=1e6)
str(fitlog)
predlog <- predict(fitlog, test)

table(predlog, test$label)
sum(predlog == test$label) / length(predlog) 
# 57.3% accurate using only 1000 on training
# 86.7% accurate using all on training



fitnet <- nnet(factor(label) ~ ., data=train, MaxNWts=1e6, maxit=200,size=20)
str(fitlog)
predlog <- predict(fitlog, test)

table(predlog, test$label)
sum(predlog == test$label) / length(predlog) 