#read dataset
df <- read.csv("~/VoterPref (1).csv")
#factor data
df$PREFERENCE <- factor(df$PREFERENCE,levels=c("For","Against"))
df$GENDER <- factor(df$GENDER)
#partition data
set.seed(71923)
train <- sample(nrow(df), 0.7*nrow(df))
dftrain <- df[train,] 
dftest <- df[-train,]

#model
####

fit2 <- glm(PREFERENCE ~ AGE+INCOME+GENDER, data = dftrain, family = "binomial")
summary(fit2)

predicted.probability.train <- predict(fit2, type = "response") 
cutoff <- 0.5
Predictedtrain <- ifelse(predicted.probability.train > cutoff, "Against", "For")
Predictedtrain <- factor(Predictedtrain, levels=c("For","Against"))
Actualtrain <- dftrain$PREFERENCE
(confusiontrain <- table(Actualtrain, Predictedtrain))

#TEST DATA

ActualTest <- dftest$PREFERENCE
## The following command generates predictions as probabilities
cutoff <- 0.5
predicted.probability.test <- predict(fit2,newdata=dftest,type = "response") 

## Generate class predictions using cutoff value
PredictedTest <- ifelse(predicted.probability.test > cutoff, "Against", "For")

PredictedTest <- factor(PredictedTest, levels=c("For","Against"))
(confusion <- table(ActualTest, PredictedTest))

#ROC PLOT
## Using the pROC Library (install the pROC package if you don't already have it)
## 
library(pROC)
par(pty="s")
roc_rose <- plot(roc(Actualtrain, predicted.probability.train), print.auc = TRUE, col = "blue",legacy.axes=TRUE )
## Next, the additional argument "add = TRUE" adds the test ROC to the previous plot
roc_rose <- plot(roc(ActualTest, predicted.probability.test), print.auc = TRUE, 
                 col = "green", print.auc.y = .4, add = TRUE)
##



##
## Accuracy
##
(accuracyTest <- sum(ActualTest == PredictedTest)/nrow(dftest))
##
## Sensitivity
##
(sensitivityTest <- sum(PredictedTest == "Against" & ActualTest == "Against")/sum(ActualTest == "Against"))
##
## Specificity
##
(specificityTest <- sum(PredictedTest == "For" & ActualTest == "For")/sum(ActualTest == "For"))
##
# +---------------------------------------------------------------------------------------------------------+
cutoff <- seq(0, 1, length = nrow(dftrain))
acc <- numeric(nrow(dftrain))
sen <- numeric(nrow(dftrain))
spec <- numeric(nrow(dftrain))

## We'll collect it in a data frame.  (We could also just keep it in three vectors)
acc.table <- data.frame(Cutoff = cutoff, ACCURACY = acc,SENSITIVITY = sen, SPECIFICITY = spec)
##
for (i in 1:nrow(dftrain)) {
  Predicted <- ifelse(predicted.probability.train > cutoff[i], "Against","For")
  Predicted <- factor(Predicted,levels=c("For","Against"))
  # 
  confusion <- table(Actualtrain,Predicted)
  acc.table$ACCURACY[i] <- (confusion[1,1]+confusion[2,2])/sum(confusion)
  acc.table$SENSITIVITY[i] <- sum(predicted.probability.train > cutoff[i] & Actual == "Against")/sum(Actual == "Against")
  acc.table$SPECIFICITY[i] <- sum(predicted.probability.train <= cutoff[i] & Actual == "For")/sum(Actual == "For")
}

plot(ACCURACY ~ cutoff, data = acc.table, type = "o",xlab="Cutoff",ylab="Accuracy",col="blue",lty=2, ylim=c(0,1))
lines(SENSITIVITY~cutoff,data = acc.table, type="o",col="green",lty=2)
lines(SPECIFICITY~cutoff,data = acc.table, type="o",col="red",lty=2)
z <- which.max(acc.table$ACCURACY)
cutoff[z]
p<- acc.table$ACCURACY[z]
p
#accuracy of test at train cutoff
cutoff <- 0.4206009
predicted.probability.test <- predict(fit2,newdata=dftest,type = "response") 

## Generate class predictions using cutoff value
PredictedTest <- ifelse(predicted.probability > cutoff, "Against", "For")

PredictedTest <- factor(PredictedTest, levels=c("For","Against"))
(confusion <- table(ActualTest, PredictedTest))




##
## Accuracy
##
(accuracyTest <- sum(ActualTest == PredictedTest)/nrow(dftest))


#####
cutoff <- seq(0, 1, length = nrow(dftrain))
cost <- numeric(nrow(dftrain))

acc.table <- data.frame(Cutoff = cutoff, Cost = cost)

for (i in 1:nrow(dftrain)) {
  Predicted <- ifelse(predicted.probability.train > cutoff[i], "Against","For")
  Predicted <- factor(Predicted,levels=c("For","Against"))
  # 
  confusion <- table(Actualtrain,Predicted)
  acc.table$Cost[i] <- (confusion[1,2]*4+confusion[2,1]*1)
}

plot(Cost ~ cutoff, data = acc.table, type = "o",xlab="Cutoff",ylab="Cost",col="blue",lty=2, ylim=c(0,1000))

z <- which.min(acc.table$Cost)
cutoff[z]
p<- acc.table$Cost[z]
p


ActualTest <- dftest$PREFERENCE
## The following command generates predictions as probabilities
cutoff <- 0.8536466
predicted.probability.test <- predict(fit2,newdata=dftest,type = "response") 

## Generate class predictions using cutoff value
PredictedTest <- ifelse(predicted.probability.test > cutoff, "Against", "For")

PredictedTest <- factor(PredictedTest, levels=c("For","Against"))
(confusion <- table(ActualTest, PredictedTest))
x <- (confusion[1,2]*4+confusion[2,1]*1)






