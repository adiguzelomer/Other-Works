
library(data.table)

#Read The Data

house_train <- fread("D:/Users/26000215/Downloads/train (2).csv", sep = ",")

table(house_train$MiscFeature, useNA = "always")

house_train$MiscFeature[which(is.na(house_train$MiscFeature))] <- "S"

table(house_train$MiscFeature, useNA = "always")

train.data$Name <- as.character(train.data$Name)
table_words <- table(unlist(strsplit(train.data$Name, "\\s+")))
sort(table_words [grep('\\.', names(table_words))], decreasing = TRUE)

library(stringr)
tb <- cbind(train.data$Age, str_match(train.data$Name, "[a-zA-Z]+\\."))
table(tb[is.na(tb[,1]),2])

mean.mr <- mean(train.data$Age[grep(" Mr||.", train.data$Name) & 
                                 !is.na(train.data$Age)])
mean.mrs <- mean(train.data$Age[grep(" Mrs||.", train.data$Name) & 
                                  !is.na(train.data$Age)])
mean.dr <- mean(train.data$Age[grep(" Dr||.", train.data$Name) & 
                                 !is.na(train.data$Age)])
mean.miss <- mean(train.data$Age[grep(" Miss||.", train.data$Name) & 
                                   !is.na(train.data$Age)])
mean.master <- mean(train.data$Age[grep(" Master||.", train.data$Name) & 
                                     !is.na(train.data$Age)])

train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.mr
train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.mrs
train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.dr
train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.miss
train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] <- mean.master



head(Titanic)
View(Titanic)
library(dplyr)
glimpse(Titanic)

datam <- as.data.frame(Titanic)
counts <- table(datam$Survived, datam$Class)

barplot(table(datam$Class), main = "Dene")

barplot(counts, col = c("darkblue", "red"), legend = c("Perished", "Survived"),
        main = "Titanic Class Bar Plot")

counts <- table(datam$Survived, datam$Sex)
counts <- table(datam$Survived, datam$Class)

barplot(counts, col = c("darkblue", "red"), legend = c("Survived", "Dead"), main = "deneme")


hist(datam$Age[which(datam$Survived == "No")], main = "Dead Passenger Age Histogram",
     xlab = "Age", ylab = "Count", col = "blue", breaks = seq(0, 80, by = 2))
datam$Age <- as.numeric(datam$Age)

boxplot(datam$Age ~ datam$Class)

train.child <- datam$Survived[datam$Age < 13]
length(train.child[which(train.child == 1)]) / length(train.child)


mosaicplot(datam$Class ~ datam$Survived, color = TRUE)


#ROC

train.ctree.pred <- predict(train, ctree, testset)
train.ctree.prob <- 1 - unlist(treeresponse(train, ctree, testset), use.names = FALSE)[seq(1, nrow(testset)*2, 2)]

library(ROCR)

train.ctree.prob.rocr <- prediction(train.ctree.prob, testset$Survived)

train.ctree.perf <- performance(train.ctree.prob.rocr, "tpr", "fpr")
train.ctree.auc.perf <- performance(train.ctree.prob.rocr, measure = "auc", x.measure = "cutoff")

plot(train.ctree.perf, col = 2, colorize = T, main = paste("AUC:", train.ctree.auc.perf@y.values)
     
     mode <- function(x) {
       temp = table(x)
       names(temp)[temp == max(temp)]
     }
     
x = c(1,2,3,3,3,4,4,5,5,5,6)
     
mode(x)
 
boxplot(mtcars$mpg ~ factor(mtcars$gear), xlab = "gear", ylab = "mpg")
 
oneway.test(mtcars$mpg ~ factor(mtcars$gear))
     
mtcars.aov <- aov(mtcars$mpg ~ as.factor(mtcars$gear))
summary(mtcars.aov)
 
model.tables(mtcars.aov, "means")
     
mtcars_posthoc <- TukeyHSD(mtcars.aov)
mtcars_posthoc
     
plot(mtcars_posthoc)
     
     
par(mfrow = c(1,2))

boxplot(mtcars$mpg ~ mtcars$gear, subset = (mtcars$am == 0), xlab = "gear", ylab = "mpd", main = "automatic")
boxplot(mtcars$mpg ~ mtcars$gear, subset = (mtcars$am == 1), xlab = "gear", ylab = "mpd", main = "manual")


boxplot(mtcars$mpg ~ factor(mtcars$gear) * factor(mtcars$am), xlab = "gear * transmission", ylab = "mpg", 
       main = "Boxplot of mpg by gear * transmission")

interaction.plot(mtcars$gear, mtcars$am, mtcars$mpg, type = "b", col =c(1:3), leg.bty = "o", leg.bg = "beige", lwd = 2, pch = c(18,24,22),
                xlab = "Number of gears", ylab = "Mean miles per gallon", main = "interaction plot")

mpg_anova2 <- aov(mtcars$mpg ~ factor(mtcars$gear) * factor(mtcars$am))
summary(mpg_anova2)

TukeyHSD(mpg_anova2)

par(mfrow = c(1,2))
plot(TukeyHSD(mpg_anova2))

library(car)
data(Quartet)
plot(Quartet$x, Quartet$y2)

lmfit <- lm(Quartet$y2 ~ poly(Quartet$x, 2))
lines(sort(Quartet$x), lmfit$fit[order(Quartet$x)], col ="red")

plot(Quartet$x, Quartet$y2)
lmfit <- lm(Quartet$y2 ~ I(Quartet$x) + I(Quartet$x^2))


plot(Quartet$x, Quartet$y3)

library(MASS)
lmfit <- rlm(Quartet$y3 ~ Quartet$x)
abline(lmfit, col = "red")

data(SLID)
str(SLID)

par(mfrow = c(2,2))
plot(SLID$wages ~SLID$language)
plot(SLID$wages ~SLID$age)
plot(SLID$wages ~SLID$education)
plot(SLID$wages ~SLID$sex)

lmfit <- lm(wages ~ ., data = SLID)
summary(lmfit)

lmfit <- lm(wages ~ age + sex + education, data = SLID)
summary(lmfit)

par(mfrow = c(2,2))
plot(lmfit)

lmfit <- lm(log(wages) ~ age + sex + education, data = SLID)
plot(lmfit)

vif(lmfit)
sqrt(vif(lmfit)) > 2


install.packages("rms")
install.packages("lmtest")

library(lmtest)
#FOR HETEROSCEDASTICITY
bptest(lmfit)

library(rms)

olsfit <- ols(log(wages) ~ age + sex + education, data = SLID, x = TRUE, y = TRUE)
robcov(olsfit)


data("warpbreaks")
head(warpbreaks)

rs1 <- glm(breaks ~ tension, data = warpbreaks, family = "poisson")
summary(rs1)


install.packages("mgcv")
require(mgcv)

library(MASS)
attach(Boston)
str(Boston)


fit <- gam(dis ~ s(nox))

summary(fit)


plot(nox, dis)

x <- seq(0, 1, length = 500)
y <- predict(fit, data.frame(nox = x))

lines(x, y, col = "red", lwd = 2)

plot(fit)

fit2 <- gam(medv ~ crim + zn + crim:zn, data = Boston)
vis.gam(fit2)

gam.check(fit)


library(C50)
data(churn)
str(churnTrain)

churnTrain <- churnTrain[, !names(churnTrain) %in% c("state", "area_code", "account_length")]


set.seed(2)

ind <- sample(2, nrow(churnTrain), replace = TRUE, prob = c(0.7, 0.3))

trainset <- churnTrain[ind == 1,]
testset <- churnTrain[ind == 2,]

split.data <- function(data, p = 0.7, s = 666) 
{
  set.seed(s)
  index <- sample(1:dim(data)[1])
  train <- data[index[1:floor(dim(data)[1] * p)],]
  test <- data[index[((ceiling(dim(data)[1] * p)) + 1 ):dim(data)[1]],]
  return(list(train = train, test = test))
}


library(rpart)

churn.rp <- rpart(churn ~ ., data = trainset)
churn.rp


printcp(churn.rp)

plotcp(churn.rp)
summary(churn.rp)
plot(churn.rp, uniform = T, branch = 0.6, margin = 0.1)
text(churn.rp, all = T, use.n = T)

predictions <- predict(churn.rp, testset, type = "class")
table(testset$churn, predictions)

library(caret)
confusionMatrix(table(predictions, testset$churn))

min(churn.rp$cptable[, "xerror"])

which.min(churn.rp$cptable[, "xerror"])

churn.cp <- churn.rp$cptable[7, "CP"]
churn.cp


prune.tree <- prune(churn.rp, cp = churn.cp)

plot(prune.tree, uniform = T, margin = 0.1)
text(prune.tree, all = T, use.n = T)

predictions <- predict(prune.tree, testset, type = "class")
table(testset$churn, predictions)

confusionMatrix(table(predictions, testset$churn))

library(party)

ctree.model <- ctree(churn ~ ., data = trainset)
ctree.model

plot(ctree.model)

daycharge.model <- ctree(churn ~ total_day_charge, data = trainset)
plot(daycharge.model)

ctree.predict <- predict(ctree.model, testset)
table(ctree.predict, testset$churn)

confusionMatrix(table(ctree.predict, testset$churn))

tr <- treeresponse(ctree.model, newdata = testset[1:5,])
tr


library(class)

levels(trainset$international_plan) <- list("0" = "no", "1" = "yes")
levels(trainset$voice_mail_plan) <- list("0" = "no", "1" = "yes")
levels(testset$international_plan) <- list("0" = "no", "1" = "yes")
levels(testset$voice_mail_plan) <- list("0" = "no", "1" = "yes")


churn.knn <- knn(trainset[,!names(trainset) %in% c("churn")],
                 testset[, !names(testset) %in% c("churn")], 
                 trainset$churn, k = 3)
summary(churn.knn)

table(testset$churn, churn.knn)

confusionMatrix(table(testset$churn, churn.knn))


fit <- glm(churn ~ ., data = trainset, family = binomial)

summary(fit)

fit <- glm(churn ~ international_plan + voice_mail_plan + total_intl_calls + number_customer_service_calls,
           data = trainset, family = binomial)
summary(fit)

pred <- predict(fit, testset, type = "response")

Class <- pred > .5
summary(Class)

tb <- table(testset$churn, Class)
tb

churn.mod <- ifelse(testset$churn == "yes", 1, 0)
pred_class <- churn.mod
pred_class[pred <= .5] <- 1 - pred_class[pred <= .5]
ctb <- table(churn.mod, pred_class)
ctb

confusionMatrix(ctb)

library(e1071)

classifier <- naiveBayes(trainset[, !names(trainset) %in% c("churn")], trainset$churn)
classifier


bayes.table <- table(predict(classifier, testset[, !names(testset) %in% c("churn")]),
                     testset$churn)
bayes.table

confusionMatrix(bayes.table)


library(e1071)

model <- svm(churn ~ ., data = trainset, kernel = "radial", cost = 1, gamma = 1/ncol(trainset))

summary(model)

install.packages("klaR")
library(klaR)

model.light <- svmlight(churn~., data = trainset, kernel = "radial", cost = 1, gamma = 1/ncol(trainset))

summary(model.light)

data("iris")

iris.subset <- subset(iris, select = c("Sepal.Length", "Sepal.Width", "Species"), 
                      Species %in% c("setosa", "virginica"))

plot(x = iris.subset$Sepal.Length, y = iris.subset$Sepal.Width, col = iris.subset$Species,
     pch = 19)

svm.model <- svm(Species ~ ., data = iris.subset, kernel = "linear", cost =1, scale = FALSE)

points(iris.subset[svm.model$index, c(1,2)], col = "blue", cex = 2)

w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a = -b/w[1,2], b = -w[1,1] / w[1,2], col = "red", lty = 5)

plot(x = iris.subset$Sepal.Length, y = iris.subset$Sepal.Width, col = iris.subset$Species,
     pch = 19)

svm.model <- svm(Species ~ ., data = iris.subset, type = "C-classification", 
                 kernel = "linear", cost = 1000, scale = FALSE)

points(iris.subset[svm.model$index, c(1,2)], col = "blue", cex = 2)
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a = -b/w[1,2], b = -w[1,1] / w[1,2], col = "red", lty = 5)


data("iris")

model.iris <- svm(Species ~ ., iris)

plot(model.iris, iris, Petal.Width ~ Petal.Length, 
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

plot(model, trainset, total_day_minutes ~ total_intl_charge)


svm.pred <- predict(model, testset[, !names(testset) %in% c("churn")])

svm.table <- table(svm.pred, testset$churn)
svm.table

classAgreement(svm.table)

library(caret)
confusionMatrix(svm.table)

library(car)
data("Quartet")

model.regression <- svm(Quartet$y1 ~ Quartet$x, type = "eps-regression")
predict.y <- predict(model.regression, Quartet$x)
predict.y

plot(Quartet$x, Quartet$y1, pch = 19)
points(Quartet$x, predict.y, pch = 15, col = "red")


tuned <- tune.svm(churn~., data = trainset, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)

model.tuned <- svm(churn~., data = trainset, gamma = tuned$best.parameters$gamma,
                   cost = tuned$best.parameters$cost)
summary(model.tuned)

svm.tuned.pred <- predict(model.tuned, testset[, !names(testset) %in% c("churn")])

svm.tuned.table <- table(svm.tuned.pred, testset$churn)
svm.tuned.table
confusionMatrix(svm.tuned.table)



#NEURAL NETWORK

data("iris")
ind <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))

trainset <- iris[ind == 1,]
testset <- iris[ind == 2,]

library(neuralnet)
trainset$setosa <- trainset$Species == "setosa"
trainset$virginica <- trainset$Species == "virginica"
trainset$versicolor <- trainset$Species == "versicolor"

network <- neuralnet(versicolor + virginica + setosa ~ Sepal.Length + Sepal.Width + 
                       Petal.Length + Petal.Width, trainset, hidden = 3)
network

network$result.matrix

head(network$generalized.weights[[1]])
plot(network)

par(mfrow = c(2,2))
gwplot(network, selected.covariate = "Petal.Width")
gwplot(network, selected.covariate = "Sepal.Width")
gwplot(network, selected.covariate = "Petal.Length")
gwplot(network, selected.covariate = "Sepal.Length")


net.predict <- compute(network, testset[-5])$net.result
net.prediction <- c("versicolor", "virginica", "setosa")[apply(net.predict, 1, which.max)]

predict.table <- table(testset$Species, net.prediction)
predict.table

classAgreement(predict.table)

confusionMatrix(predict.table)

compute(network, testset[-5])

library(nnet)

data(iris)

set.seed(2)

ind <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
trainset <- iris[ind == 1,]
testset <- iris[ind == 2,]

iris.nn <- nnet(Species~., data = trainset, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
summary(iris.nn)

iris.predict <- predict(iris.nn, testset, type = "class")
nn.table <- table(testset$Species, iris.predict)
confusionMatrix(nn.table)
head(predict(iris.nn, testset))





library(e1071)

ind <- cut(1:nrow(churnTrain), breaks = 10, labels = F)

accuracies <- c()

for (i in 1:10)
{
  fit <- svm(churn~., churnTrain[ind != i, ])
  predictions <- predict(fit, churnTrain[ind == i, !names(churnTrain) %in% c("churn")])
  correct_count <- sum(predictions == churnTrain[ind == i, c("churn")])
  accuracies <- append(correct_count / nrow(churnTrain[ind == i,]), accuracies)
}

accuracies

mean(accuracies)
tuned <- tune.svm(churn~., data = trainset, gamma = 10^-2, cost = 10^2, 
                  tunecontrol = tune.control(cross = 10))
summary(tuned)
tuned$performances

svmfit <- tuned$best.model
table(trainset[, c("churn")], predict(svmfit))


control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model <- train(churn~., data = trainset, method = "rpart", preProcess = "scale", 
               trControl = control)
model


importance <- varImp(model, scale = F)
importance

plot(importance)

library(rpart)
model.rp <- rpart(churn~., data = trainset)
model.rp$variable.importance

library(rminer)
model <- fit(churn~., trainset, model = "svm")

VariableImportance <- Importance(model, trainset, method = "sensv")

L <- list(runs = 1, sen = t(VariableImportance$imp), 
          sresponses = VariableImportance$sresponses)
mgraph(L, graph = "IMP", leg = names(trainset), col = "gray", Grid = 10)


new_train <- trainset[, !names(churnTrain) %in% c("churn", "international_plan",
                                                  "voice_mail_plan", "state", "area_code")]

cor_mat <- cor(new_train)

highlyCorrelated <- findCorrelation(cor_mat, cutoff = 0.75)

names(new_train)[highlyCorrelated]

library(e1071)
data(churn)
intl_plan <- model.matrix(~trainset.international_plan -1,
                          data = data.frame(trainset$international_plan))

colnames(intl_plan) <- c("trainset.international_planno" = "intl_no",
                         "trainset.international_planyes" = "intl_yes")

voice_plan <- model.matrix(~trainset.voice_mail_plan -1, 
                           data = data.frame(trainset$voice_mail_plan))
colnames(voice_plan) <- c("trainset.voice_mail_planno" = "voice_no",
                         "trainset.voice_mail_planyes" = "voice_yes")

trainset$international_plan <- NULL
trainset$voice_mail_plan <- NULL
trainset <- cbind(intl_plan, voice_plan, trainset)



intl_plan <- model.matrix(~testset.international_plan -1,
                          data = data.frame(testset$international_plan))

colnames(intl_plan) <- c("testset.international_planno" = "intl_no",
                         "testset.international_planyes" = "intl_yes")

voice_plan <- model.matrix(~testset.voice_mail_plan -1, 
                           data = data.frame(testset$voice_mail_plan))
colnames(voice_plan) <- c("testset.voice_mail_planno" = "voice_no",
                          "testset.voice_mail_planyes" = "voice_yes")

testset$international_plan <- NULL
testset$voice_mail_plan <- NULL
testset <- cbind(intl_plan, voice_plan, testset)

library(caret)
ldaControl <- rfeControl(functions = ldaFuncs, method = "cv")

ldaProfile <- rfe(trainset[, !names(trainset) %in% c("churn")],
                  trainset[, c("churn")], sizes = c(1:18), rfeControl = ldaControl)
ldaProfile

plot(ldaProfile, type = c("o", "g"))

ldaProfile$optVariables

ldaProfile$fit

postResample(predict(ldaProfile, testset[, !names(testset) %in% c("churn")]), 
             testset[, c("churn")])



library(car)
data("Quartet")

plot(Quartet$x, Quartet$y3)
lmfit <- lm(Quartet$y3 ~ Quartet$x)
abline(lmfit, col = "red")

predicted <- predict(lmfit, newdata = Quartet[c("x")])

actual <- Quartet$y3
rmse <- (mean((predicted - actual)^2)) ^ 0.5
rmse

mu <- mean(actual)
rse <- mean((predicted - actual)^2) / mean((mu-actual)^2)
rse

rsquare <- 1 - rse
rsquare

tune(lm, y3 ~ x, data = Quartet)


svm.model <- train(churn ~., data = trainset, method = "svmRadial")

svm.pred <- predict(svm.model, testset[, !names(testset) %in% c("churn")])

table(svm.pred, testset[, c("churn")])

confusionMatrix(svm.pred, testset[, c("churn")])


library(ROCR)

svmfit <- svm(churn ~., data = trainset, prob = TRUE)

pred <- predict(svmfit, testset[, !names(testset) %in% c("churn")], 
                probability = TRUE)

pred.prob <- attr(pred,  "probabilities")
pred.to.roc <- pred.prob[,2]

pred.rocr <- prediction(pred.to.roc, testset$churn)

perf.rocr <- performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr <- performance(pred.rocr, "tpr", "fpr")

plot(perf.tpr.rocr, colorize  = T, main = paste("AUC:", (perf.rocr@y.values)))


library("pROC")

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = T,
                        summaryFunction = twoClassSummary)

glm.model <- train(churn ~., data = trainset, method = "glm", metric = "ROC",
                   trControl = control)

svm.model <- train(churn ~., data = trainset, method = "svmRadial", metric = "ROC",
                   trControl = control)

rpart.model <- train(churn ~., data = trainset, method = "rpart", metric = "ROC",
                     trControl = control)

glm.probs <- predict(glm.model, testset[, !names(testset) %in% c("churn")], type = "prob")
svm.probs <- predict(svm.model, testset[, !names(testset) %in% c("churn")], type = "prob")
rpart.probs <- predict(svm.model, testset[, !names(testset) %in% c("churn")], type = "prob")


glm.ROC <- roc(response = testset[, c("churn")], predictor = glm.probs$yes,
               levels = levels(testset[, c("churn")]))
plot(glm.ROC, type = "S", col = "red")

svm.ROC <- roc(response = testset[, c("churn")], predictor = svm.probs$yes,
               levels = levels(testset[, c("churn")]))
plot(svm.ROC, type = "S", add = TRUE, col = "green")

rpart.ROC <- roc(response = testset[, c("churn")], predictor = rpart.probs$yes,
               levels = levels(testset[, c("churn")]))
plot(rpart.ROC, type = "S", add = T, col = "blue")




cv.values <- resamples(list(glm = glm.model, svm = svm.model, rpart = rpart.model))

summary(cv.values)

dotplot(cv.values, metric = "ROC")
bwplot(cv.values, layout = c(3,1))
