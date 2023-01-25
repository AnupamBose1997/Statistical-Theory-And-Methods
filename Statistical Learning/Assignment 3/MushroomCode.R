library(mlbench)
library(sf)
library(tmap)
library(caret)
library(gbm)
library(tidyverse)
library(GGally)
library(rgl)
library(reshape2)
library(vegan)
library(dummies)
library(rpart)
library(rpart.plot)

#install.packages('rpart.plot')

df <- read_csv("mushrooms.csv", col_names = TRUE) 
glimpse(df)
str(df)
##Making each variable as a factor
df <-df %>%
  map_df(function(.x) as.factor(.x))


number_class <- function(x){
  x <- length(levels(x))
}

#levels of each column
x <- df %>% map_dbl(function(.x) number_class(.x)) %>% as_tibble() %>% 
  rownames_to_column() %>% arrange(desc(value))
colnames(x) <- c("Variable name", "Number of levels")

x <-x %>%
  mutate(Variable_Name=colnames(df[as.numeric(x$`Variable name`)])) %>%
  select(Variable_Name,`Number of levels`)
x
#checking missing data  
map_dbl(df, function(.x) {sum(is.na(.x))})

#visualisations
ggplot(df, aes(x = CapShape, y = CapSurface, col = Edible)) + 
  geom_jitter(alpha = 0.8) + 
  scale_color_manual(breaks = c("Edible", "Poisonous"), 
                     values = c("#2eed95", "steelblue"))

ggplot(df, aes(x = CapColor, y = CapSurface, col = Edible)) + 
  geom_jitter(alpha = 0.8) + 
  scale_color_manual(breaks = c("Edible", "Poisonous"), 
                     values = c("#2eed95", "steelblue"))

ggplot(df, aes(x = Edible, y = Odor, col = Edible)) + 
  geom_jitter(alpha = 0.8) + 
  scale_color_manual(breaks = c("Edible", "Poisonous"), 
                     values = c("#2eed95", "steelblue"))



#cross-validation
set.seed(1800) #For reproducibility
#creating partition for training data
df_sample = createDataPartition(df$Edible, p=0.7, list= F)
df_train <- df[df_sample, ]
df_test <- df[-df_sample, ]

TreeModel1 <- rpart(Edible ~ ., data = df_train, method = "class",cp = 0.002)
TreeModel2 <- rpart(Edible ~ ., data = df_train, method = "class",cp = 0.003)
TreeModel3 <- rpart(Edible ~ ., data = df_train, method = "class",cp = 0.004)
TreeModel4 <- rpart(Edible ~ ., data = df_train, method = "class",cp = 0.00001)

cfmat1=confusionMatrix(data=predict(TreeModel1, type = "class"), 
                       reference = df_train$Edible, 
                       positive="Edible")
cfmat2=confusionMatrix(data=predict(TreeModel2, type = "class"), 
                       reference = df_train$Edible, 
                       positive="Edible")
cfmat3=confusionMatrix(data=predict(TreeModel3, type = "class"), 
                       reference = df_train$Edible, 
                       positive="Edible")
cfmat4=confusionMatrix(data=predict(TreeModel4, type = "class"), 
                       reference = df_train$Edible, 
                       positive="Edible")

cfmat1$overall[1]
cfmat2$overall[1]
cfmat3$overall[1]
cfmat4$overall[1]

plotcp(TreeModel4)
printcp(TreeModel4)
rpart.plot(TreeModel4)

TreeModel5 <- rpart(Edible ~ ., data = df_train, method = "class",cp = 0.00054705)
cfmat5=confusionMatrix(data=predict(TreeModel5, type = "class"), 
                       reference = df_train$Edible, 
                       positive="Edible")
cfmat5$overall[1]

rpart.plot(TreeModel5)
printcp(TreeModel5)





#checking quality of splits
round(prop.table(table(df$Edible)), 2)
round(prop.table(table(df_train$Edible)), 2)
round(prop.table(table(df_test$Edible)), 2)

#Decision Tree
set.seed(1810)
TreeModel <- rpart(Edible ~ ., data = df_train, method = "class")
summary(TreeModel)
printcp(TreeModel)

TreeModel$cptable[which.min(TreeModel$cptable[, "xerror"]), "CP"]
#confusion Matrix
confusionMatrix(data=predict(TreeModel, type = "class"), 
                reference = df_train$Edible, 
                positive="Edible")
#increasing accuracy using cp parameter
TreeModelCP_pruned <- rpart(Edible ~ ., data = df_train, 
                            method = "class", cp = 0.00001)

printcp(TreeModelCP_pruned)
plotcp(TreeModelCP_pruned)

TreeModelCP_pruned$cptable[which.min(TreeModelCP_pruned$cptable[, "xerror"]), "CP"]

#choosing best complexity parameter
bestcp <- round(TreeModelCP_pruned$cptable[which.min(TreeModelCP_pruned$cptable[, "xerror"]), "CP"],4)
model_tree_pruned <- prune(TreeModelCP_pruned, cp = bestcp)

rpart.plot(model_tree_pruned, extra = 104, box.palette = "GnBu", 
           branch.lty = 3, shadow.col = "gray", nn = TRUE)

#confusion matrix
confusionMatrix(data=predict(model_tree_pruned, type = "class"), 
                reference = df_train$Edible, 
                positive="Edible")

test_tree <- predict(model_tree_pruned, newdata = df_test)
confusionMatrix(data = predict(model_tree_pruned, newdata = df_test, type = "class"), 
                reference = df_test$Edible, 
                positive = "Edible")


start.time <- Sys.time()
#Best model selection code
winner = rep(NA, 50)
for (iteration in 1:50){
  #Make a new random training data - test data split
  idx=createDataPartition(df$Edible, p=0.7, list= F)
  #idx = sample(1:8124, 5687)
  train_data = df[idx, ]
  test_data = df[-idx, ]
  cpval = rep(NA, 50)
  BestAcc = rep(NA, 50)
  
  for (i in 1:50){
    #Fit model with the training data
    TreeModelCV <- rpart(Edible ~ ., data = train_data, 
                                method = "class", cp = 0.00054705)
    
    cpval[i]=TreeModelCV$cptable[which.min(TreeModelCV$cptable[, "xerror"]), "CP"]
    TreeModelCVPruned=prune(TreeModelCV, cp = cpval[i])
    cfmat=confusionMatrix(data=predict(TreeModelCVPruned, type = "class"), 
                    reference = train_data$Edible, 
                    positive="Edible")
    
    BestAcc[i]=cfmat$overall[1]
  }
  #Winning model for this iteration is stored
  winner[iteration] = max(BestAcc)
}


MostAccModel=which.max(winner)
bestcpval=cpval[MostAccModel]

MostAccModel=rpart(Edible ~ ., data = train_data, 
      method = "class", cp = bestcpval)
confusionMatrix(data=predict(MostAccModel, type = "class"), 
                      reference = train_data$Edible, 
                      positive="Edible")

#testing model on testing data

#C.P = 0.00054705
confusionMatrix(data = predict(TreeModel5, newdata = df, type = "class"), 
                reference = df$Edible, 
                positive = "Edible")

summary(TreeModel5)
rpart.plot(TreeModel5)
#C.P = 0.00001
confusionMatrix(data = predict(TreeModel4, newdata = df, type = "class"), 
                reference = df$Edible, 
                positive = "Edible")



#TASK 2 (RANDOM FOREST)
library(randomForest)

df1 <- read_csv("mushrooms.csv", col_names = TRUE)
#df_sample = createDataPartition(df1$Edible, p=0.7, list= F)

#df_train_rf <- df1[df_sample, ]
#df_test_rf <- df1[-df_sample, ]

#cross-validation
set.seed(3) #For reproducibility
#creating partition for training data
df_sample = createDataPartition(df1$Edible, p=0.7, list= F)
df_train_rf <- df1[df_sample, ]
df_test_rf <- df1[-df_sample, ]

formulas = c("Edible ~ CapShape", "Edible ~ CapSurface", "Edible ~ CapColor","Edible ~ Odor","Edible ~ Height",
             "Edible ~ CapShape + CapSurface", "Edible ~ CapShape + CapColor", "Edible ~ CapShape + Odor","Edible ~ CapShape + Height",
             "Edible ~ CapSurface + CapColor","Edible ~ CapSurface + Odor","Edible ~ CapSurface + Height","Edible ~ CapColor + Odor",
             "Edible ~ CapColor + Height","Edible ~ Odor + Height",
             "Edible ~ CapShape + CapSurface + CapColor","Edible ~ CapSurface + CapColor + Odor","Edible ~ CapColor + Odor + Height",
             "Edible ~ CapShape + CapColor + Odor","Edible ~ CapShape + Odor + Height","Edible ~ CapShape + CapSurface + Odor",
             "Edible ~ CapShape + CapSurface + Height","Edible ~ CapSurface + CapColor + Height","Edible ~ CapSurface + Odor + Height",
             "Edible ~ CapShape + CapColor + Height",
             "Edible ~ CapShape + CapSurface + CapColor + Odor","Edible ~ CapSurface + CapColor + Odor + Height",
             "Edible ~ CapShape + CapColor + Odor + Height","Edible ~ CapShape + CapSurface + Odor + Height",
             "Edible ~ CapShape + CapSurface + CapColor + Height",
             "Edible ~ CapShape + CapSurface + CapColor + Odor + Height")
length(formulas)



df_train_rf$Edible = as.factor(df_train_rf$Edible)
accuracy = rep(NA, length(formulas))
for(i in 1:length(formulas)){
  myForest = randomForest(formula=as.formula(formulas[i]), 
                          data=df_train_rf)
  prediction = predict(myForest, df_test_rf)
  prediction_df = data.frame(df_test_rf$Edible, prediction)
  
  cnf_mat = table(prediction_df$df_test_rf.Edible, prediction_df$prediction)
  
  accuracy[i] = (cnf_mat[1,1] + cnf_mat[2,2])/dim(df_test_rf)[1]
}



which.max(accuracy)

formulas[which.max(accuracy)]


plot(1:length(formulas), accuracy, xlab="Model Number", ylab="Accuracy")



set.seed(3)
rf_winner = rep(NA, 5)
for(iter in 1:5){
  accuracy = rep(NA, length(formulas))
  idx = createDataPartition(df1$Edible, p=0.7, list = F)
  df_train_rf = df1[idx, ]
  df_test_rf = df1[-idx, ]
  df_train_rf$Edible = as.factor(df_train_rf$Edible)
  for(i in 1:length(formulas)){
    myForest = randomForest(formula=as.formula(formulas[i]), 
                            data=df_train_rf)
    prediction = predict(myForest, df_test_rf)
    prediction_df = data.frame(df_test_rf$Edible, prediction)
    
    cnf_mat = table(prediction_df$df_test_rf.Edible, prediction_df$prediction)
    
    accuracy[i] = (cnf_mat[1,1] + cnf_mat[2,2])/dim(df_test_rf)[1]
    
  }
  rf_winner[iter] = which.max(accuracy)
}
which.max(accuracy)

formulas[26]

plot(1:length(formulas), accuracy1, xlab="Model Number", ylab="Accuracy")

myForest1 = randomForest(formula=as.formula(formulas[26]), 
                         data=df_train_rf)
prediction1 = predict(myForest1, df1)
prediction_df1 = data.frame(df1$Edible, prediction1)

cnf_mat1 = table(prediction_df1$df1.Edible, prediction_df1$prediction1)
cnf_mat1
accuracy1 = (cnf_mat1[1,1] + cnf_mat1[2,2])/dim(df1)[1]
accuracy1

formulas[13]
formulas[17]

hist(rf_winner, breaks = seq(0.5, 31.5, 1), xlab="Model", ylab="Frequency", main="")

varImpPlot(myForest,
           sort = T,
           main="Fig 11: Feature importance by random forest model 27")
