library(rpart)
library(mlbench)

#For Classification Problem
data("Ionosphere")
#summary(Ionosphere)

#to ensure reproducible results we need to set seed. No. can be anything
set.seed(912)

#split the data into training and test sets
Ionosphere[,"train"] <- ifelse(runif(nrow(Ionosphere))<.8,1,0)

#Create training and test sets
train_set<-Ionosphere[Ionosphere$train == 1,]
test_set <-Ionosphere[Ionosphere$train == 0,]

#get column index of train column
getColNum <- grep("train",names(train_set))

#remove train column from train and test sets
train_set <- train_set[,-getColNum]
test_set <- test_set[,-getColNum]

#get column index of predicted variable in dataset
typeColNum <- grep("Class", names(Ionosphere))

#build model (Create a Decision Tree)
rpart_model <- rpart(Class ~ ., data = train_set, method = "class")

#plot tree
plot(rpart_model)
text(rpart_model)

#predict outcome of Class variable in Test set which we removed
rpart_predict <- predict(rpart_model, test_set[,-typeColNum], type ="class")
mean(rpart_predict == test_set$Class)


#Confusion Matrix
table(pred = rpart_predict, actual = test_set$Class)
prop.table(table(pred = rpart_predict, actual = test_set$Class))

#print cost-compexity parameter
printcp(rpart_model)

# get index of CP with lowest xerror to prepare for Prunning Tree
opt <- which.min(rpart_model$cptable[,"xerror"])

#get its value
cp <- rpart_model$cptable[opt,"CP"]

#prune tree
pruned_model <- prune(rpart_model,cp)
#plot tree
plot(pruned_model);text(pruned_model)

#find proportion of correct predictions using test set by using Prunned Tree
rpart_pruned_predict <- predict(pruned_model,test_set[,-typeColNum],type="class")
mean(rpart_pruned_predict==test_set$Class)

#We need to verify above result by doing a multiple run so below is function to do multiple runs
multiple_runs_classification <- function(train_fraction,n,dataset,prune_tree=FALSE){
  fraction_correct <- rep(NA,n)
  set.seed(912)
  for (i in 1:n){
    dataset[,"train"] <- ifelse(runif(nrow(dataset))<train_fraction,1,0)
    trainColNum <- grep("train",names(dataset))
    typeColNum <- grep("Class",names(dataset))
    trainset <- dataset[dataset$train==1,-trainColNum]
    testset <- dataset[dataset$train==0,-trainColNum]
    rpart_model <- rpart(Class~.,data = train_set, method="class")
    if(prune_tree==FALSE) {
      rpart_test_predict <- predict(rpart_model,test_set[,-typeColNum],type="class")
      fraction_correct[i] <- mean(rpart_test_predict==test_set$Class)
    }else{
      opt <- which.min(rpart_model$cptable[,"xerror"])
      cp <- rpart_model$cptable[opt, "CP"]
      pruned_model <- prune(rpart_model,cp)
      rpart_pruned_predict <- predict(pruned_model,test_set[,-typeColNum],type="class")
      fraction_correct[i] <- mean(rpart_pruned_predict==test_set$Class)
    }
  }
  return(fraction_correct)
}

#For 50 runs, no pruning
unpruned_set <- multiple_runs_classification(0.8,50,Ionosphere)
mean(unpruned_set)

#For 50 runs, with pruning
pruned_set <- multiple_runs_classification(0.8,50,Ionosphere,prune_tree=TRUE)
mean(pruned_set)


#Rather then running Decision Trees multiple times, we can simply try Random Forest

library(randomForest)

#Build Random Forest Model
model.rf <- randomForest(Class ~., data = train_set, importance = TRUE, xtest=test_set[,-typeColNum], ntree=500)

#Fetch Summary
model.rf

#check accuracy for Test Set
mean(model.rf$test$predicted==test_set$Class)

#Confusion Matrix
table(model.rf$test$predicted,test_set$Class)

#Plot
varImpPlot(model.rf)
