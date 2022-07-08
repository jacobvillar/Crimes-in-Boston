library(readxl)
library(caret)
library(party)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(ipred)
library(adabag)

data = read_excel("task3_final_crime.xlsx")

set.seed(1779)
sample = sample(c(TRUE, FALSE), nrow(data), replace = T, prob = c(0.7,0.3))
train = data[sample, ]
test = data[!sample, ]
nrow(train) + nrow(test) == nrow(data)
train$season = as.factor(train$season)
train$timeday = as.factor(train$timeday)
train$dayweek = as.factor(train$dayweek)
train$offense_type = as.factor(train$offense_type)
test$season = as.factor(test$season)
test$timeday = as.factor(test$timeday)
test$dayweek = as.factor(test$dayweek)
test$offense_type = as.factor(test$offense_type)

#write_xlsx(train, "task3_train.xlsx")
#write_xlsx(test, "task3_test.xlsx")
data_train = train
data_test = test

train_x = subset(train, select = c(season, timeday, dayweek))
train_y = train$offense_type
nb_4 = train(train_x,
             train_y,
             'nb',
             trControl = trainControl(method = 'cv', number = 10))
nb_4$finalModel
prob_nb = predict(nb_4, test, type = "prob")
pred_nb = ifelse(prob_nb[,2] > 0.5, "1", "0")
nbcm = caret::confusionMatrix(as.factor(pred_nb), as.factor(test$offense_type))

log = glm(offense_type ~ season + timeday + dayweek,
            data = train,
            family = "binomial")
summary(log)
prob_log = predict(log, test, type = "response")
pred_log = ifelse(prob_log > 0.5, "1", "0")
confusionMatrix(as.factor(pred_log), as.factor(test$offense_type))

png(file = "tree.png")
tree = ctree(offense_type ~ season + timeday + dayweek,
             data = train)
tree
treeresponse(tree, newdata = test[1:5,])
plot(tree)
dev.off()
traverse <- function(node) {
  if (node$terminal) {
    return(node$prediction[2])
  }
  return(c(node$prediction[2],
           traverse(node$left), traverse(node$right)))
}
traverse(tree@tree)[c(3,5,6,8,10,12,13)]
prob_tree = predict(tree, test, type = "response")
confusionMatrix(as.factor(prob_tree), as.factor(test$offense_type))


# fit = rpart(offense_type ~ dayweek + season + timeday,
#             data = train,
#             method = 'class', minsplit = 2, minbucket = 1)
# rpart.plot(fit, extra = 106)
# fancyRpartPlot(fit)

cmd = function(cm){
  TP = cm$table[1,1]
  TN = cm$table[2,2]
  FP = cm$table[1,2]
  FN = cm$table[2,1]
  n = TP + TN + FP + FN
  p = TP / (TP + FP)
  r = TP / (TP + FN)
  fm = (2 * p * r) / (p + r)
  cat("TP = ", TP, "\n")
  cat("TN = ", TN, "\n")
  cat("FP = ", FP, "\n")
  cat("FN = ", FN, "\n")
  cat("accuracy = ", (TP + TN) / n, "\n")
  cat("error = ", (FP + FN) / n, "\n")
  cat("sensitivity = ", TP / (TP + FN), "\n")
  cat("specificity = ", TN / (TN + FP), "\n")
  cat("precision = ", p, "\n")
  cat("recall = ", r, "\n")
  cat("fm = ", fm, "\n")
}

pred_nb = ifelse(prob_nb[,2] > 0.52, "1", "0")
nbcm = caret::confusionMatrix(as.factor(pred_nb), as.factor(test$offense_type))
cmd(nbcm)

prob_tree = predict(tree, test, type = "prob")
preds = c()
for(i in 1:length(prob_tree)){
  preds = c(preds, prob_tree[[i]][2])
}
pred_tree = ifelse(preds > 0.52, "1", "0")
dtcm = confusionMatrix(as.factor(pred_tree), as.factor(test$offense_type))
cmd(dtcm)

bag = bagging(
  formula = offense_type ~ season + timeday + dayweek,
  data = train,
  nbagg = 1000,
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)
prob_bag = as.data.frame(predict(bag, test, type = "prob"))
preds_bag = c()
for(i in 1:nrow(prob_bag)){
  preds_bag = c(preds_bag, prob_bag[i,2])
}
pred_bag = ifelse(preds_bag > 0.50, "1", "0")
dtcm_bag = confusionMatrix(as.factor(pred_bag), as.factor(test$offense_type))
cmd(dtcm_bag)

boost = boosting(
  formula = offense_type ~ season + timeday + dayweek,
  data = train,
  boos = TRUE,
  mfinal = 1
)
