plot(gbm.caret$results$n.trees,gbm.caret$results$RMSE)

RMSE_VALUE<-c(gbm.caret$results$RMSE)

NUM_OF_TREES <- c(gbm.caret$results$n.trees)

#number of trees
ntreeplot<-data.frame(NUM_OF_TREES,RMSE_VALUE) %>%
  ggplot(aes(x=NUM_OF_TREES,y=RMSE_VALUE,fill=NUM_OF_TREES)) +
  geom_smooth(method = "loess", col = "878787")+
  theme_void()+
  theme_light()+
  xlab('Number of Trees')+
  ylab('RMSE')

shrinkage <- c(gbm.caret$results$shrinkage)
shrinkage_plot<-data.frame(shrinkage,RMSE_VALUE) %>%
  ggplot(aes(x=shrinkage,y=RMSE_VALUE)) +
  geom_smooth(method = "loess", col = "878787")+
  theme_void()+
  theme_light()+
  xlab('Shrinkage')+
  ylab('RMSE')

plot_grid(ntreeplot,shrinkage_plot,nrow = 1)


interaction_depth <-c(gbm.caret$results$interaction.depth)
data.frame(interaction_depth,RMSE_VALUE) %>%
  ggplot(aes(x=interaction_depth,y=RMSE_VALUE)) +
  geom_smooth(method = "loess", col = "878787")+
  geom_point(colour='#35978f')
theme_void()+
  theme_light()+
  xlab('Shrinkage')+
  ylab('RMSE')



minobsinnode<-c(gbm.caret$results$n.minobsinnode)
data.frame(minobsinnode,RMSE_VALUE) %>%
  ggplot(aes(x=minobsinnode,y=RMSE_VALUE)) +
  geom_smooth(method = "loess", col = "878787")+
  geom_point(colour='#35978f')
theme_void()+
  theme_light()+
  xlab('Shrinkage')+
  ylab('RMSE')

glimpse(df)
  
  
  #Showing non-linearity
  logS<-c(df$LogS)
most_neg <- c(df$Most_neg)

data.frame(df$volume,df$LogS) %>%
  ggplot(aes(x=df.volume,y=df.LogS)) +
  geom_point(colour='#35978f')+
  geom_smooth(method = "loess", col = "#543005")+
  theme_void()+
  theme_light()+
  xlab('volume: Molar Volume')+
  ylab('LogS: Solubility')

data.frame(df$Most_neg,df$LogS) %>%
  ggplot(aes(x=df.Most_neg,y=df.LogS)) +
  geom_point(colour='#35978f')+
  geom_smooth(method = "loess", col = "#543005")+
  theme_void()+
  theme_light()+
  xlab('Most_neg: Charge on most negative atom of solution structure')+
  ylab('LogS: Solubility')
#plot(df$LogS ,df$LogS)

#nonlinearity: #deltaG_sol #O_charges
#linear: G_sol,volume, MW, SASA

ggplot() +
  geom_histogram(data = df, aes(x = LogS, y=..density..),
                 bins = 30, fill="indianred", col="white")+
  geom_density(data = df, aes(x = LogS), alpha=.5, fill="#FF6666")+
  xlab("Histogram of LogS")
quantile(df$LogS)




######################################################
######################################################
######################################################
######################################################
######################################################
######################################################

          #########ENSEMBLE#######

SvrPred<-as.numeric(svr_predictions)
LassoPred<-as.numeric(Lasso_prediction)
RidgePred<-as.numeric(predictions_test_ridge)
DtreePred<-as.numeric(dtree_predictions)
KnnPred<-as.numeric(knn_predictions)
GbmPred<-as.numeric(gbm_prediction)
LrPred<-as.numeric(LinearReg_prediction)
rfPred<-as.numeric(Rf_prediction)
annPred <- as.numeric(ann_prediction$AnnPred)

PredAVG<-data.frame(SvrPred,LassoPred,RidgePred,DtreePred,KnnPred,GbmPred,LrPred,rfPred,annPred)

PredAVG<-PredAVG %>%
  mutate(avg = (SvrPred+LassoPred+RidgePred+DtreePred+KnnPred+LrPred)/6)
  
sqrt(mean((data.test.z$LogS - PredAVG$avg)^2))


#svr - 0.97
#Lasso - 1.143
#ridge - 1.142
#LR - 1.144
#knn - 1.03
#dtree - 1.19
#rf - 0.887
#ann - 0.8824
#gbm - 0.8397

library(xlsx)
ModelRMSE=read.xlsx2("MODEL_RMSE.xlsx",sheetIndex = 1)
ModelRMSE<-ModelRMSE %>%
  mutate(RMSE = as.numeric(RMSE))%>%
  mutate(PercentageLogS =as.numeric(ModelRMSE$X.LogS.0.7))

library(RColorBrewer)
#version 3 - choosing colors
allcols= 20
getPalette = colorRampPalette(brewer.pal(12, "Set3"))
fill=getPalette(allcols)
allcols<-fill

################################################################
#rmse of all models
rmseplot<-ModelRMSE %>%
  ggplot(aes(x=ModelRMSE$RMSE,y=reorder(ModelRMSE$Model,ModelRMSE$RMSE)))+
  geom_col(aes(fill = Model))+
  scale_fill_manual(breaks = c("GBM","ANN","Random Forest","Ensembled","SVR","KNN","Ridge Regression","Lasso Regression" ,"Linear Regression","Decision Tree"),
                    values = c("#8DD3C7","#CFECBB","#F4F4B9","#CFCCCF","#D1A7B9","#F4867C","#C0979F","#86B1CD","#CEB28B","#EDBC63"))+
#  coord_flip()+
  geom_text(label=ModelRMSE$RMSE,
            hjust=0) +
  xlab('Models')+
  ylab('RMSE') +
  ggtitle('Individual Model vs Ensembled Model Performance - RMSE')+
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  theme(legend.text = element_text(face = "bold"))

################################################################
#%logS+-0.7 of all models
logsplot<-ModelRMSE %>%
  ggplot(aes(x=ModelRMSE$PercentageLogS,y=reorder(ModelRMSE$Model,ModelRMSE$PercentageLogS)))+
  geom_col(aes(fill = Model))+
  scale_fill_manual(breaks = c("GBM","ANN","Random Forest","Ensembled","SVR","KNN","Ridge Regression","Lasso Regression" ,"Linear Regression","Decision Tree"),
                    values = c("#8DD3C7","#CFECBB","#F4F4B9","#CFCCCF","#D1A7B9","#F4867C","#C0979F","#86B1CD","#CEB28B","#EDBC63"))+
#  coord_flip()+
  geom_text(label=ModelRMSE$PercentageLogS,
            hjust=0) +
  xlab('Models')+
  ylab('Percentage LogS+_0.7') +
  ggtitle('Individual Model vs Ensembled Model Performance - %LogS')+
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  theme(legend.text = element_text(face = "bold"))
####################################################################
###################################################################33
#plot combine
plot_grid(plot_grid(rmseplot +theme(legend.position="none"),
          logsplot+theme(legend.position="none"),
          ncol=1),as_ggplot(get_legend(rmseplot)),nrow=1,rel_widths = c(0.8,0.128))


####################################################################
#rmse of simple models
ModelRMSE2=read.xlsx2("MODEL_RMSE.xlsx",sheetIndex = 2)
ModelRMSE2<-ModelRMSE2 %>%
  mutate(RMSE = as.numeric(RMSE))

ModelRMSE2 %>%
  ggplot(aes(x=ModelRMSE2$RMSE,y=reorder(ModelRMSE2$Model,ModelRMSE2$RMSE)))+
  geom_col(aes(fill = Model))+
  scale_fill_manual(breaks = c("GBM","ANN","Random Forest","Ensembled","SVR","KNN","Ridge Regression","Lasso Regression" ,"Linear Regression","Decision Tree"),
                    values = c("#8DD3C7","#CFECBB","#F4F4B9","#CFCCCF","#D1A7B9","#F4867C","#C0979F","#86B1CD","#CEB28B","#EDBC63"))+
#  coord_flip()+
  geom_text(label=ModelRMSE2$RMSE,
            hjust=0) +
  xlab('Models')+
  ylab('RMSE') +
  ggtitle('Individual Models vs Ensembled Model Performance - RMSE')+
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  theme(legend.text = element_text(face = "bold"))


#############################################################################
####################################################################
#%LogS of simple models
ModelRMSE2=read.xlsx2("MODEL_RMSE.xlsx",sheetIndex = 2)
ModelRMSE2<-ModelRMSE2 %>%
  mutate(RMSE = as.numeric(RMSE))%>%
  mutate(X.LogS.0.7 = as.numeric(X.LogS.0.7))

ModelRMSE2 %>%
  ggplot(aes(x=ModelRMSE2$X.LogS.0.7,y=reorder(ModelRMSE2$Model,ModelRMSE2$X.LogS.0.7)))+
  geom_col(aes(fill = Model))+
  scale_fill_manual(breaks = c("GBM","ANN","Random Forest","Ensembled","SVR","KNN","Ridge Regression","Lasso Regression" ,"Linear Regression","Decision Tree"),
                    values = c("#8DD3C7","#CFECBB","#F4F4B9","#CFCCCF","#D1A7B9","#F4867C","#C0979F","#86B1CD","#CEB28B","#EDBC63"))+
  #  coord_flip()+
  geom_text(label=ModelRMSE2$X.LogS.0.7,
            hjust=0) +
  xlab('Models')+
  ylab('Percentage LogS+_0.7') +
  ggtitle('Individual Models vs Ensembled Model Performance - %LogS')+
  theme_bw()+
  theme(axis.text = element_text(face="bold"))+
  theme(legend.text = element_text(face = "bold"))


#############################################################################
#Metrics
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

#ensembled
rmse(data.test.z$LogS,PredAVG$avg)
mae(data.test.z$LogS,PredAVG$avg)
RSQUARE(data.test.z$LogS,PredAVG$avg)
ensembled_Logrel= sum((PredAVG$avg-data.test.z$LogS)>-0.7 & (PredAVG$avg-data.test.z$LogS)<0.7)/268
ensembled_Logrel

#sum((PredAVG$avg-data.test.z$LogS)>-0.7 & (PredAVG$avg-data.test.z$LogS)<0.7)/268

################################
#gbm
rmse(data.test.z$LogS,GbmPred)
mae(data.test.z$LogS,GbmPred)
RSQUARE(data.test.z$LogS,GbmPred)
gbm_Logrel

#svr
rmse(data.test.z$LogS,SvrPred)
mae(data.test.z$LogS,SvrPred)
RSQUARE(data.test.z$LogS,SvrPred)
svr_Logrel

#Lasso
rmse(data.test.z$LogS,LassoPred)
mae(data.test.z$LogS,LassoPred)
RSQUARE(data.test.z$LogS,LassoPred)
lasso_Logrel

#ridge
rmse(data.test.z$LogS,RidgePred)
mae(data.test.z$LogS,RidgePred)
RSQUARE(data.test.z$LogS,RidgePred)
ridge_Logrel

#decisionTree
rmse(data.test.z$LogS,DtreePred)
mae(data.test.z$LogS,DtreePred)
RSQUARE(data.test.z$LogS,DtreePred)
dtree_Logrel

#KNN
rmse(data.test.z$LogS,KnnPred)
mae(data.test.z$LogS,KnnPred)
RSQUARE(data.test.z$LogS,KnnPred)
knn_Logrel

#LR
rmse(data.test.z$LogS,LrPred)
mae(data.test.z$LogS,LrPred)
RSQUARE(data.test.z$LogS,LrPred)
lr_Logrel

#rf
rmse(data.test.z$LogS,rfPred)
mae(data.test.z$LogS,rfPred)
RSQUARE(data.test.z$LogS,rfPred)
rf_Logrel

#ANN
rmse(data.test.z$LogS,annPred)
mae(data.test.z$LogS,annPred)
RSQUARE(data.test.z$LogS,annPred)
ann_logrel







