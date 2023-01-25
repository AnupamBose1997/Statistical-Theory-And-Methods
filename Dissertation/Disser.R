#libraries
library(mlbench)
library(sf)
library(tmap)
library(caret)
library(gbm)
library(tidyverse)
library(GGally)
library(rgl)
library(reshape2)
library(Metrics)
library(caret)
library(cowplot)
library(ggpubr)


df = read_csv("desdata.csv", col_names = TRUE)
df = read_csv("MNDO_full_set_logS.csv", col_names = TRUE)

glimpse(df)
summary(df$LogS)
boxplot(df$LogS)
hist(df$LogS)

data_cor <-  cor(df[ , colnames(df) != "LogS" & colnames(df) !="Compound"],  # Calculate correlations
      df$LogS)

as.data.frame(data_cor)%>%
  rownames_to_column("Features") %>%
  mutate(Corr=V1) %>%
  select(Features,Corr) -> data_cor

data_cor %>%
  mutate(Corr=round(Corr,digits=2))%>%
  ggplot(aes(x=Features,y=Corr,label=Corr)) +
  geom_point()+
  geom_text(size=4,vjust=-0.5) 




df <- df[,-c(1)]

set.seed(1234) #For reproducibility
#creating partition for training data
train.index = createDataPartition(df$LogS, p=0.7, list= F)

#dataframe of test and train data
data.train = df[train.index,]
data.test = df[-train.index,]


#Summary of the target variable
summary(data.train$LogS)
hist(data.train$LogS)
summary(data.test$LogS)
hist(data.test$LogS)


#rescaling predictor variables
data.train.z =
  data.train %>% select(-LogS) %>%
  mutate_if(is_logical,as.character) %>%
  mutate_if(is_double,scale) %>% data.frame()

data.test.z =
  data.test %>% select(-LogS) %>%
  mutate_if(is_logical,as.character) %>%
  mutate_if(is_double,scale) %>% data.frame()

#add unscaled Y variable back
data.train.z$LogS = data.train$LogS
data.test.z$LogS = data.test$LogS

## Set up tuning grid
caretGrid <- expand.grid(interaction.depth=c(1, 3, 5,7,9,11,13), n.trees = (0:50)*100,
                         shrinkage=c(0.01,0.02, 0.03),
                         n.minobsinnode=c(1,2,3))

metric <- "RMSE"

trainControl <- trainControl(method="cv", number=10)


## run the model over the grid
set.seed(1234)
gbm.caret <- train(LogS ~ ., data = data.train.z, distribution="gaussian", method="gbm",
                   trControl=trainControl, verbose=FALSE,
                   tuneGrid=caretGrid, bag.fraction=0.75)

## Examine the results
print(gbm.caret)
ggplot(gbm.caret)




##Correlation Plot #install.packages("corrplot")
library(corrplot)

corrplot(cor(df),        # Correlation matrix
         method = "shade", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = TRUE,      # If TRUE (default), adds the diagonal
         tl.col = "black", # Labels color
         bg = "white",     # Background color
         title = "",       # Main title
         col = NULL)       # Color palette


corrplot(cor(df),
         method = "circle",       
         order = "hclust",         # Ordering method of the matrix
         hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
         addrect = 2,              # If order = "hclust", number of cluster rectangles
         rect.col = 3,             # Color of the rectangles
         rect.lwd = 3,
         tl.col = '#01665e')             # Line width of the rectangles

hist(df$LogS)
?corrplot()
# explore the results
names(gbm.caret)

# see best tune
gbm.caret[6]

# see grid results
head(data.frame(gbm.caret[4]))

# check
dim(caretGrid)
dim(data.frame(gbm.caret[4]))

## Find the best parameter combination
# put into a data.frame
grid_df = data.frame(gbm.caret[4])
head(grid_df)

# confirm best model and assign to params object
grid_df[which.min(grid_df$results.RMSE), ]

# assign to params and inspect
params = grid_df[which.min(grid_df$results.RMSE), 1:4 ]
params

## Create final model
# because parameters are known, model can be fit without parameter tuning
fitControl <- trainControl(method = "none", classProbs = FALSE)

set.seed(1234)
# extract the values from params
gbmFit <- train(LogS ~ ., data=data.train.z, distribution="gaussian", method = "gbm",
                trControl = fitControl,
                verbose = FALSE,
                ## only a single model is passed to the
                tuneGrid = data.frame(interaction.depth = 11,
                                      n.trees = 1050,
                                      shrinkage = 0.02,
                                      n.minobsinnode = 1))



## Prediction and Model evaluation
# generate predictions
pred = predict(gbmFit, newdata = data.test.z)
# plot these against observed
data.frame(Predicted = gbm_prediction, Observed = data.test.z$LogS) %>%
  ggplot(aes(x = Observed, y = Predicted))+ geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = "loess", col = "red")+
  geom_smooth(method = "lm")

gbm_prediction = predict(gbmFit, newdata = data.test.z)

gbm_Logrel= sum((gbm_prediction-data.test.z$LogS)>-0.7 & (gbm_prediction-data.test.z$LogS)<0.7)/268


# generate some prediction accuracy measures
postResample(pred = pred, obs = data.test.z$LogS)


# examine variable importance
ggplot(varImp(gbmFit, scale = FALSE),colour='blue')

varImp(gbmFit, scale = FALSE)

print.varImp.13 <- function(x = vimp, top = 13) {
  printObj <- data.frame(as.matrix(sortImp(x, top)))
  printObj$name = rownames(printObj)
  printObj
}

dfm = data.frame(print.varImp.13(varImp(gbmFit)), method = "GBM")

dfm %>%
  ggplot(aes(reorder(name, Overall), Overall,fill=name)) +
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col() +
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() + xlab("") + ylab("Variable Importance") +
  theme(axis.text.y = element_text(size = 7)) +
  geom_text(aes(y=round(dfm$Overall,2), label=round(dfm$Overall,2)),size = 3.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  theme_minimal()




#################################################################
#####################################################################
####################################################
###################################

library('rminer')
#SVR

#install.packages('e1071') -- for svm 
library('e1071')
set.seed(1234)
regressor = svm(formula = LogS ~ .,
                data = data.train.z,
                type = 'eps-regression',
                kernel = 'radial',
                cost = 4,
                gamma = 0.0769,
                epsilon = 0.164
                )

summary(regressor)

rmse <- sqrt(mean((data.test.z$LogS - predict(regressor,data.test.z))^2))
rmse

svr_predictions<-predict(regressor,data.test.z)


svr_Logrel= sum((svr_predictions-data.test.z$LogS)>-0.7 & (svr_predictions-data.test.z$LogS)<0.7)/268


##Tuning
set.seed(1234)
# perform a grid search
tuneResult_svr <- tune(svm, LogS ~ .,  data = data.train.z,
                      ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tuneResult_svr)
plot(tuneResult_svr)

tuneResult_svr$best.model

rmse <- sqrt(mean((data.test.z$LogS - predict(tuneResult_svr,data.test.z))^2))
rmse
set.seed(1234)
tuneResult_svr1 <- tune(svm, LogS ~ .,  data = data.train.z,
                       ranges = list(epsilon = seq(0.1,0.3,0.01), cost = c(1,2,3,4,5,6))
)

print(tuneResult_svr1)
plot(tuneResult_svr1)

tuneResult_svr1$best.model

set.seed(1234)
tuneResult_svr2 <- tune(svm, LogS ~ .,  data = data.train.z,
                        ranges = list(epsilon = seq(0.16,0.17,0.001), cost = seq(4,6,0.1))
)

plot(tuneResult_svr2)

tuneResult_svr2$best.model

################################################################
################################################################
###########   KNN    ####################


set.seed(1234)

tuneGrid <- expand.grid(
  k = seq(1,50,by = 1)
)



ctrl <- trainControl(method="cv", number=10)
set.seed(1234)
knn_model <- train(
  LogS ~ .,
  data = data.train.z,
  method = 'knn',
  trControl = ctrl,
  tuneGrid = data.frame(k = seq(1,50,by = 1))
)

knn_model$bestTune
plot(knn_model)



## Create final model
# because parameters are known, model can be fit without parameter tuning
fitControl <- trainControl(method = "none", classProbs = FALSE)


set.seed(1234)
final_knn_model <- train(
  LogS ~ .,
  data = data.train.z,
  method = 'knn',
  trControl = fitControl,
  tuneGrid = data.frame(k = 3))



knn_predictions = predict(final_knn_model, newdata = data.test.z)

# RMSE
sqrt(mean((data.test.z$LogS - knn_predictions)^2))



knn_Logrel= sum((knn_predictions-data.test.z$LogS)>-0.7 & (knn_predictions-data.test.z$LogS)<0.7)/268

###############################################################################
######################################################################
##DECISION TREE############


library(rpart)
library(rpart.plot)

set.seed(1234)
dtree <- rpart(LogS ~ ., data=data.train.z, control=rpart.control(cp=0),method='anova')

par(mfrow=c(1,1))
rsq.rpart(dtree)
printcp(dtree)
plotcp(dtree)

min(dtree$cptable[,c(4)])


#identify best cp value to use
best_dtree <- dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"]

set.seed(1234)
#produce a pruned tree based on the best cp value
pruned_dtree <- prune(dtree, cp=best_dtree)

summary(pruned_dtree)
rpart.plot(pruned_dtree)
pruned_dtree

plot(pruned_dtree$variable.importance)

data.frame(imp = pruned_dtree$variable.importance) %>%
  rownames_to_column() %>%
  rename("features" = rowname) %>%
  ggplot()+
  geom_col(aes(x = imp, y = reorder(features,imp)),col = "white", show.legend = F) +
  #coord_flip() +
  scale_fill_grey() +
  theme_bw()+
  xlab("Importance")+
  ylab("Features")
  
  


#plot the pruned tree
prp(pruned_dtree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

plot(dtree$cptable[,c(2)],dtree$cptable[,c(4)])




#use pruned tree to predict
dtree_predictions<-predict(pruned_dtree, newdata=data.test.z)

#rmse
sqrt(mean((data.test.z$LogS - predict(pruned_dtree,data.test.z ))^2))


dtree_Logrel= sum((dtree_predictions-data.test.z$LogS)>-0.7 & (dtree_predictions-data.test.z$LogS)<0.7)/268



#########################################################################################################################
########################################################
### Linear Regression ############
set.seed(1234)
linearReg <- lm(LogS ~ ., data=data.train.z)  # build linear regression model on full data
print(linearReg)
summary(linearReg)

data.frame(varImp(linearReg, scale = FALSE)) %>%
  rownames_to_column() %>%
  rename("features" = rowname) %>%
  ggplot()+
  geom_col(aes(x = Overall, y = reorder(features,Overall)),col = "white", show.legend = F,fill = c("#86B1CD","#CEB28B","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F")) +
  #coord_flip() +
  scale_fill_grey() +
  theme_bw()+
  xlab("Importance")+
  ylab("")+
  ggtitle('Linear Regression Model')

LinearReg_prediction <- predict(linearReg, data.test.z)

summary(data.train.z$LogS - linearReg$fitted.values)

data.frame(Residual=data.train.z$LogS - linearReg$fitted.values)%>%
  ggplot() +
  geom_histogram(aes(x = Residual, y=..density..),
                 bins = 30, fill="indianred", col="white")+
  geom_density(aes(x = Residual), alpha=.5, fill="#FF6666")+
  xlab("Histogram of Residual")
  

#rmse
sqrt(mean((data.test.z$LogS - LinearReg_prediction)^2))


lr_Logrel= sum((LinearReg_prediction-data.test.z$LogS)>-0.7 & (LinearReg_prediction-data.test.z$LogS)<0.7)/268


#############################################################################
#############################################################################
#############################################################################
#######################   RANDOM FOREST    ##################################



set.seed(1234)


ranger_grid = expand.grid(mtry = 1:13, splitrule = c("extratrees","variance") , min.node.size = 1:15 )
trainControl <- trainControl(method="cv", number=10)
ranger_fit <- train(LogS ~ ., data= data.train.z, method= 'ranger', importance = 'impurity', type = 'regression',
                                trcontorl = trainControl,
                                tuneGrid=ranger_grid)

ggplot(ranger_fit)
plot(ranger_fit)
summary(ranger_fit)
names(ranger_fit)

ranger_fit[6]

head(data.frame(ranger_fit[4]))

grid_df = data.frame(ranger_fit[4])
head(grid_df)

grid_df[which.min(grid_df$results.RMSE), ]

ranger_fit$bestTune



set.seed(1234)
fitControl <- trainControl(method = "none", classProbs = FALSE)

ranger_fit_rf <- train(LogS ~ ., data= data.train.z, method= 'ranger', importance = 'impurity', type = 'regression',
                                 trcontorl = fitControl,
                                 tuneGrid = expand.grid(mtry = 13, splitrule = 'extratrees',
                                                        min.node.size = 3))


Rf_prediction <- predict(ranger_fit_rf, data.test.z)

#rmse
sqrt(mean((data.test.z$LogS - Rf_prediction)^2))


rf_Logrel= sum((Rf_prediction-data.test.z$LogS)>-0.7 & (Rf_prediction-data.test.z$LogS)<0.7)/268

#variable importance:
# examine variable importance
ggplot(varImp(ranger_fit_rf, scale = FALSE),colour='blue')

varImp(ranger_fit_rf, scale = FALSE)

print.varImp.13 <- function(x = vimp, top = 13) {
  printObj <- data.frame(as.matrix(sortImp(x, top)))
  printObj$name = rownames(printObj)
  printObj
}


df_rf = data.frame(print.varImp.13(varImp(ranger_fit_rf)), method = "Random Forest")


df_rf %>%
  ggplot(aes(reorder(name, Overall), Overall)) +
  geom_col(fill = c("#86B1CD","#CEB28B","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F")) +
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() + xlab("") + ylab("Variable Importance") +
  theme(axis.text.y = element_text(size = 7)) + geom_text(aes(y=round(df_rf$Overall,2), label=round(df_rf$Overall,2)),size = 3.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  theme_minimal()




################################################################################
########################################################################################
#########     ANN    ####################
#require(nnet)
#library(e1071)

ann_prediction = read_csv("ann_prediction.csv", col_names = TRUE)


#ann_prediction
sqrt(mean((data.test.z$LogS - ann_prediction$AnnPred)^2))

ann_logrel=sum((ann_prediction-data.test.z$LogS)>-0.7 & (ann_prediction-data.test.z$LogS)<0.7)/268


#####################################################################################
#####################################################################################
#####################################################################################
################      VARIABLE IMPORTANCE OF MODELS   ####################################

#gbm
print.varImp.13 <- function(x = vimp, top = 13) {
  printObj <- data.frame(as.matrix(sortImp(x, top)))
  printObj$name = rownames(printObj)
  printObj
}

dfm = data.frame(print.varImp.13(varImp(gbmFit)), method = "GBM")

gbmplot=dfm %>%
  ggplot(aes(reorder(name, Overall), Overall,fill=name)) +
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col() +
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() + xlab("") + ylab("Variable Importance") +
  theme(axis.text.y = element_text(size = 7)) +
  geom_text(aes(y=round(dfm$Overall,2), label=round(dfm$Overall,2)),size = 2.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  theme_minimal()



#knn
varImp(final_knn_model,scale=FALSE)

dfknn = data.frame(print.varImp.13(varImp(final_knn_model)), method = "KNN")

knnplot=dfknn %>%
  ggplot(aes(reorder(name, Overall), Overall,fill=name)) +
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col() +
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() + xlab("") + ylab("Variable Importance") +
  theme(axis.text.y = element_text(size = 7)) +
  geom_text(aes(y=round(dfknn$Overall,2), label=round(dfknn$Overall,2)),size = 2.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  theme_minimal()


#Linear Regression

dflr = data.frame(print.varImp.13(varImp(linearReg,scale=TRUE)), method = "Linear Regression")

lrplot=data.frame(varImp(linearReg, scale = TRUE)) %>%
  rownames_to_column() %>%
  rename("features" = rowname) %>%
  mutate(Overall = (100*Overall)/13.1768871)%>%
  mutate(method='Linear Regression')%>%
  ggplot(aes(reorder(features, Overall), Overall,fill=features))+
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col()+
  coord_flip() +
  geom_text(aes(y=round(Overall,2), label=round(Overall,2)),size = 2.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  xlab("") + ylab("Variable Importance")+theme_minimal()+
  facet_wrap( ~ method, ncol = 3, scales = "fixed") 



#RandomForest
df_rf = data.frame(print.varImp.13(varImp(ranger_fit_rf)), method = "Random Forest")


rfplot=df_rf %>%
  ggplot(aes(reorder(name, Overall), Overall,fill=name)) +
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col() +
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() + xlab("") + ylab("Variable Importance") +
  theme(axis.text.y = element_text(size = 7)) + geom_text(aes(y=round(df_rf$Overall,2), label=round(df_rf$Overall,2)),size = 2.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  theme_minimal()


#Decision Tree
dtreeplot=data.frame(imp = pruned_dtree$variable.importance) %>%
  rownames_to_column() %>%
  rename("features" = rowname) %>%
  mutate(imp = (100*imp)/max(imp))%>%
  mutate(method='Decision Tree')%>%
  ggplot(aes(reorder(features, imp), imp,fill=features))+
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col()+
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() +
  geom_text(aes(y=round(imp,2), label=round(imp,2)),size = 2.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  xlab("") + ylab("Variable Importance")+theme_minimal()


#Lasso
lassoplot=data.frame(varImp(lasso_model,scale = FALSE, lambda=lambda_best)) %>%
  rownames_to_column() %>%
  rename("features" = rowname) %>%
  mutate(Overall = (100*Overall)/max(Overall))%>%
  mutate(method='Lasso Regression')%>%
  ggplot(aes(reorder(features, Overall), Overall,fill=features))+
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col()+
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() +
  geom_text(aes(y=round(Overall,2), label=round(Overall,2)),size = 2.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  xlab("") + ylab("Variable Importance")+theme_minimal()

#Ridge
ridgeplot=data.frame(varImp(ridge_reg, scale = FALSE, lambda=optimal_lambda)) %>%
  rownames_to_column() %>%
  rename("features" = rowname) %>%
  mutate(Overall = (100*Overall)/max(Overall))%>%
  mutate(method='Ridge Regression')%>%
  ggplot(aes(reorder(features, Overall), Overall,fill=features))+
  scale_fill_manual(breaks = c("Most_neg","volume","MW","SASA","G_sol","Het_charges","Most_pos","O_charges","sol_dip","Lsolu_Hsolv","C_charges","Lsolv_Hsolu","DeltaG_sol"),
                    values = c("#CEB28B","#86B1CD","#EDBC63","#C2D567","#CDD796","#F8CDDE","#E9D3DE","#D5CFD6","#C59CC5","#C09CBF","#C9DAC3","#E1EBA0","#FFED6F"))+
  geom_col()+
  facet_wrap( ~ method, ncol = 3, scales = "fixed") +
  coord_flip() +
  geom_text(aes(y=round(Overall,2), label=round(Overall,2)),size = 2.3,hjust=-0.3, fontface = "bold", family = "Fira Sans"
  )+
  xlab("") + ylab("Variable Importance")+theme_minimal()



group1 <- plot_grid(
  gbmplot + theme(legend.position="none"),
  rfplot + theme(legend.position="none"),
  nrow = 1
)

group2 <- plot_grid(
  dtreeplot + theme(legend.position="none"),
  knnplot + theme(legend.position="none"),
  nrow = 1
)

group3 <- plot_grid(
  lassoplot + theme(legend.position="none"),
  ridgeplot + theme(legend.position="none"),
  nrow = 1
)

#as_ggplot(get_legend(lrplot))

plot1<-plot_grid(plot_grid(group1,group2,group3,ncol = 1),as_ggplot(get_legend(lrplot)),nrow=1,rel_widths = c(0.8,0.128))

plot2<-lrplot+ theme(legend.position="none")



#ggsave('var_imp_plot.jpeg',width=10,height=8,units=c("cm"),dpi=1000)

#plot_grid(lrplot,plot_grid(group1,group2,group3,ncol = 1),ncol = 1)	
gbmplot
knnplot
rfplot
lrplot
ridgeplot
dtreeplot
lassoplot


