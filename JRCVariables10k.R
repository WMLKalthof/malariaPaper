#loop with models
#install.packages("gbm")
library(gbm)
library(dismo)
library(dplyr)
#install.packages("matrixStats")
library(pdp)

setwd("C:/thesis/treeModel")

#these are 90% of the PfPR point observations with related variables  
data <-  read.csv("statsCombinedDocuments/allVariablesCombinedJRC.csv")

# remote variables that have only NA 
data <- Filter(function(x)!all(is.na(x)), data)

# creating empty dataframes to put the model performance statistics in, nrow is for how many different types of models are in this run 

modelNrs <- 2

df_deviance <- data.frame(matrix(ncol=0,nrow=modelNrs))

RMSE_combined = data.frame(matrix(ncol=0,nrow=modelNrs))
MAE_combined = data.frame(matrix(ncol=0,nrow=modelNrs))
IQR_combined = data.frame(matrix(ncol=0,nrow=modelNrs))
adj.R2_combined = data.frame(matrix(ncol=0,nrow=modelNrs))

## Create data frames for the average PDP plots, 51 is the standard amount of steps of the x values in the plots it creates 
Var1Y <- data.frame(matrix(ncol=0,nrow=51))
Var2Y <- data.frame(matrix(ncol=0,nrow=51))

## create data frames for the RC variables, variables here need to match the variables in the data set. RC of each run will be added to this dataframe, after which the mean is calculated
RCModel1 = data.frame(matrix(ncol=0,nrow=30))
Rx1 <- c("NDVI_b50", "NDVI_b10000", "precMax_b50", "precMax_b10000", "precMin_b50", "precMin_b10000", "precMaxAmplitude_b50", "precMaxAmplitude_b10000", "tempAv_b1000", "tempAv_b10000", 
         "elevation_b10000", "elevation_b50", "distance30_b10000p70", "distance1000_b10000p70", "distance5000_b10000p70", "travelTime_b10000", "travelTime_b1000", "precAv_b10000", 
         "precAv_b50", "distance30_b10000p10", "distance30_b50p50", "distance30_b1000p70", "distance30_b10000p0", "distance500_b50p10", "distance1000_b10000p10", "distance5000_b10000p90", 
         "waterArea_b10000p90", "waterNDWI_b10000p50", "distance30_b1000p0", "distance30_b1000p10")
RCModel1 <- RCModel1 %>% cbind(RCModel1, Rx1) %>% rename("var" = "Rx1")

RCModel2 = data.frame(matrix(ncol=0,nrow=22))
Rx2 <- c("NDVI_b50", "NDVI_b10000", "tempAv_b1000", "tempAv_b10000", 
         "elevation_b10000", "elevation_b50", "distance30_b10000p70", "distance1000_b10000p70", "distance5000_b10000p70", "travelTime_b10000", "travelTime_b1000", 
         "distance30_b10000p10", "distance30_b50p50", "distance30_b1000p70", "distance30_b10000p0", "distance500_b50p10", "distance1000_b10000p10", "distance5000_b10000p90", 
         "waterArea_b10000p90", "waterNDWI_b10000p50", "distance30_b1000p0", "distance30_b1000p10")
RCModel2 <- RCModel2 %>% cbind(RCModel2, Rx2) %>% rename("var" = "Rx2")

# RCModel3 = data.frame(matrix(ncol=0,nrow=16))
# Rx3 <- c("NDVI_b50", "NDVI_b10000", "precMax_b50", "precMax_b10000", "precMin_b50", "precMin_b10000", "precMaxAmplitude_b50", "precMaxAmplitude_b10000","meanTemp_b1000", "meanTemp_b10000", 
#          "elevation_b10000", "elevation_b50", "travelTime_b10000", "travelTime_b1000", "total_precipitation_b10000", 
#          "total_precipitation_b50", "waterArea_b10000p0", "distance30_b10000p0", "waterArea_b10000p10", "distance30_b10000p10", "distance500_b10000p10", "distance1000_b10000p10", "distance5000_b10000p10", "waterArea_b10000p20", "distance30_b10000p20", "distance500_b10000p20", "distance1000_b10000p20", "distance5000_b10000p20", "waterArea_b10000p50", "distance30_b10000p50", "distance500_b10000p50", "distance1000_b10000p50", "distance5000_b10000p50", "waterArea_b10000p70", "distance30_b10000p70", "distance500_b10000p70", "distance1000_b10000p70", "distance5000_b10000p70", "waterArea_b10000p90", "distance30_b10000p90", "distance500_b10000p90", "distance1000_b10000p90", "distance5000_b10000p90")
# RCModel3 <- RCModel3 %>% cbind(RCModel3, Rx3) %>% rename("var" = "Rx3")
# 
# RCModel4 = data.frame(matrix(ncol=0,nrow=55))
# Rx4 <- c("NDVI_b50", "NDVI_b10000", "precMax_b50", "precMax_b10000", "precMin_b50", "precMin_b10000", "precMaxAmplitude_b50", "precMaxAmplitude_b10000","meanTemp_b1000", "meanTemp_b10000", 
#          "elevation_b10000", "elevation_b50", "travelTime_b10000", "travelTime_b1000", "total_precipitation_b10000", 
#          "total_precipitation_b50")
# RCModel4 <- RCModel4 %>% cbind(RCModel4, Rx4) %>% rename("var" = "Rx4")


for ( i in 1:50){
  set.seed(i)
  
  # splitting the dataset in 90% training and 10% predicting/testing
  training <- sample(1:nrow(data),6212)
  testing <- setdiff(rownames(data),training)
  training <- data[training,]
  testing <- data[testing,]
  
  # training the model with different combinations of variables using 90% of the data
  #model 1: Confounders + all variables (4)
  data1 <- select(training, c(pf_pr, NDVI_b50, NDVI_b10000, precMax_b50, precMax_b10000, precMin_b50, precMin_b10000, precMaxAmplitude_b50, precMaxAmplitude_b10000,
                              tempAv_b1000, tempAv_b10000, elevation_b10000, elevation_b50, distance30_b10000p70, distance1000_b10000p70, distance5000_b10000p70,
                              travelTime_b10000, travelTime_b1000, precAv_b10000, precAv_b50, waterNDWI_b10000p50, distance30_b10000p10,
                              distance30_b50p50, distance30_b1000p70, distance30_b10000p0, distance500_b50p10, distance1000_b10000p10, 
                              distance5000_b10000p90, waterArea_b10000p90, distance30_b1000p0, distance30_b1000p10))
  BRT_malaria1 <- gbm.fixed(data=data1, gbm.x=2:31 ,gbm.y=1, family="gaussian", tree.complexity = 10, learning.rate = 0.005, n.trees = 5000,  bag.fraction = 0.6)
  
  
  #model 2: Confounders + surface water without precipitation 
  data2 <- select(training, c(pf_pr, NDVI_b50, NDVI_b10000, 
                              tempAv_b1000, tempAv_b10000, elevation_b10000, elevation_b50, distance30_b10000p70, distance1000_b10000p70, distance5000_b10000p70,
                              travelTime_b10000, travelTime_b1000,  waterNDWI_b10000p50,
                              distance30_b10000p10, distance30_b50p50, distance30_b1000p70, distance500_b50p10, distance1000_b10000p10, 
                              distance5000_b10000p90, waterArea_b10000p90, distance30_b10000p0, distance30_b1000p0, distance30_b1000p10))
  BRT_malaria2 <- gbm.fixed(data=data2, gbm.x=2:23 ,gbm.y=1, family="gaussian", tree.complexity = 10, learning.rate = 0.005, n.trees = 5000,  bag.fraction = 0.6)
  
  # #model 3: Confounders + 100 k buffers
  # data3 <- select(training, c(pf_pr, NDVI_b50, NDVI_b10000, precMax_b50, precMax_b10000, precMin_b50, precMin_b10000, precMaxAmplitude_b50, precMaxAmplitude_b10000,
  #                             meanTemp_b1000, meanTemp_b10000, elevation_b10000, elevation_b50, travelTime_b10000, travelTime_b1000, total_precipitation_b10000, 
  #                             total_precipitation_b50, contains("_b10000p")))
  # BRT_malaria3 <- gbm.fixed(data=data3, gbm.x=2:44 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # #model 4: Confounders + 10 best performing 
  # data4 <- select(training, c(pf_pr, NDVI_b50, NDVI_b10000, precMax_b50, precMax_b10000, precMin_b50, precMin_b10000, precMaxAmplitude_b50, precMaxAmplitude_b10000,
  #                             meanTemp_b1000, meanTemp_b10000, elevation_b10000, elevation_b50, travelTime_b10000, travelTime_b1000, total_precipitation_b10000, 
  #                             total_precipitation_b50))
  # BRT_malaria4 <- gbm.fixed(data=data4, gbm.x=2:17 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # #model 5: Only precipitation 
  # data5 <- select(training, c(pf_pr, starts_with("total_precipitation")))
  # BRT_malaria5 <- gbm.fixed(data=data5, gbm.x=2:6 ,gbm.y=1, family="gaussian", tree.complexity = 4, learning.rate = 0.004, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # #model 6: Only best performing water statistics 
  # data6 <- select(training, c(pf_pr, distance5000_b10000p50, 
  #                             distance20_b10000p10, distance5000_b10000p90, waterArea5000_b10000p90, waterArea5000_b10000p10, distance5000_b10000p0, distance1000_b10000p50, waterBodies_b10000p10, waterBodies_b10000p0, distance1000_b100p20, 
  #                             distance20_b10000p20, distance1000_b10000p20, distance500_b10000p90, distance500_b10000p0, distance1000_b10000p50, distance5000_b10000p10, distance20_b10000p90, distance500_b10000p50, waterArea20_b10000p90, 
  #                             distance1000_b10000p90, distance5000_b1000p50 , distance20_b10000p10, distance1000_b1000p20, distance5000_b10000p50, distance20_b10000p0, distance5000_b10000p90, distance1000_b10000p10, distance1000_b10000p0, 
  #                             waterArea5000_b10000p50, distance500_b10000p10, distance20_b1000p90, waterBodies_b10000p10, distance20_b10000p50, distance20_b10000p20, distance5000_b10000p20, waterArea5000_b10000p20, waterBodies_b10000p20, 
  #                             distance5000_b10000p20, distance20_b10000p50, distance500_b10000p90, waterBodies_b10000p50, waterArea_b10000p90, distance20_b10000p90, waterArea1000_b10000p10, waterBodies_b10000p0, waterArea1000_b10000p90, 
  #                             distance500_b10000p0, waterArea20_b10000p10, distance20_b50p50, distance500_b100p50, distance5000_b1000p90, distance500_b10000p20, distance1000_b50p50, distance20_b10000p0, waterArea500_b10000p90, 
  #                             waterArea5000_b10000p90, distance500_b50p10, distance1000_b10000p20, distance5000_b1000p20, distance1000_b10000p10, distance5000_b100p20, distance5000_b10000p10, distance5000_b1000p10, distance1000_b100p10))
  # BRT_malaria6 <- gbm.fixed(data=data6, gbm.x=2:65 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # # #model 7: All confounders plus precipitation + 5000m resolution of water statistics
  # data7 <- select(training, c(pf_pr, starts_with("housingMean"), starts_with("travelTime"), starts_with("popDensity"), starts_with("meanITN"), starts_with("meanTemp"), starts_with("elevation"), starts_with("total_precipitation"), contains("waterArea5000_b"), contains("distance5000_b")))
  # BRT_malaria7 <- gbm.fixed(data=data7, gbm.x=2:70 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  # #model 8: model 5 + urban accessibility
  # data8 <- select(training, c(pf_pr, starts_with("tem"), starts_with("pre"), occurrence, seasonality, access))
  # BRT_malaria8 <- gbm.fixed(data=data8, gbm.x=2:28 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # #model 9: model 2 + PCR-GLOBWB 2 + remote sensing + urban accessibility
  # data9 <- select(training, c(pf_pr, starts_with("tem"), starts_with("waTem"), starts_with("pre"), starts_with("chanSto"), starts_with("dis"), starts_with("dynFra"), starts_with("run"), starts_with("satLow"), starts_with("satUp"), starts_with("groWa"), starts_with("groFos"), starts_with("actThi"), starts_with("eva"), starts_with("totRun"), starts_with("totThi"), starts_with("netPre"), access, occurrence, seasonality))
  # BRT_malaria9 <- gbm.fixed(data=data9, gbm.x=2:196 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  # 
  # #model 10: PCR-GLOBWB 2
  # data10 <- select(training, c(pf_pr, starts_with("chanSto"), starts_with("waTem"), starts_with("dis"), starts_with("dynFra"), starts_with("run"), starts_with("satLow"), starts_with("satUp"), starts_with("groWa"), starts_with("groFos"), starts_with("actThi"), starts_with("eva"), starts_with("totRun"), starts_with("totThi"), starts_with("netPre")))
  # BRT_malaria10 <- gbm.fixed(data=data10, gbm.x=2:169 ,gbm.y=1, family="gaussian", tree.complexity = 5, learning.rate = 0.005, n.trees = 2000,  bag.fraction = 0.6)
  
  # Using the remaining 10% of the data to predict/test PfPr values 
  preds1 <- predict.gbm(BRT_malaria1, testing,
                        n.trees=BRT_malaria1$gbm.call$n.trees, type="response")
  preds2 <- predict.gbm(BRT_malaria2, testing,
                        n.trees=BRT_malaria2$gbm.call$n.trees, type="response")
  # preds3 <- predict.gbm(BRT_malaria3, testing,
  #                       n.trees=BRT_malaria3$gbm.call$n.trees, type="response")
  # preds4 <- predict.gbm(BRT_malaria4, testing,
  #                       n.trees=BRT_malaria4$gbm.call$n.trees, type="response")
  # preds5 <- predict.gbm(BRT_malaria5, testing,
  #                       n.trees=BRT_malaria5$gbm.call$n.trees, type="response")
  # preds6 <- predict.gbm(BRT_malaria6, testing,
  #                       n.trees=BRT_malaria6$gbm.call$n.trees, type="response")
  # preds7 <- predict.gbm(BRT_malaria7, testing,
  #                       n.trees=BRT_malaria7$gbm.call$n.trees, type="response")
  # preds8 <- predict.gbm(BRT_malaria8, testing,
  #                       n.trees=BRT_malaria8$gbm.call$n.trees, type="response")
  # preds9 <- predict.gbm(BRT_malaria9, testing,
  #                       n.trees=BRT_malaria9$gbm.call$n.trees, type="response")
  # preds10 <- predict.gbm(BRT_malaria10, testing,
  #                        n.trees=BRT_malaria10$gbm.call$n.trees, type="response")
  
  predictions <- list(preds1,preds2)
  # preds3, preds4)
  
  # creating the prediction maps --> not needed for me 
  # prmap1 <- predict.gbm(BRT_malaria1, df_surwa_map,
  #                       n.trees=BRT_malaria1$gbm.call$n.trees, type="response") 
  # df_prmap1 <- cbind(df_prmap1, prmap1)
  # 
  # prmap2 <- predict.gbm(BRT_malaria2, df_surwa_map,
  #                       n.trees=BRT_malaria2$gbm.call$n.trees, type="response") 
  # df_prmap2 <- cbind(df_prmap2, prmap2)
  # 
  # prmap3 <- predict.gbm(BRT_malaria3, df_surwa_map,
  #                       n.trees=BRT_malaria3$gbm.call$n.trees, type="response") 
  # df_prmap3 <- cbind(df_prmap3, prmap3)
  # 
  # prmap4 <- predict.gbm(BRT_malaria4, df_surwa_map,
  #                       n.trees=BRT_malaria4$gbm.call$n.trees, type="response") 
  # df_prmap4 <- cbind(df_prmap4, prmap4)
  # 
  # prmap5 <- predict.gbm(BRT_malaria5, df_surwa_map,
  #                       n.trees=BRT_malaria5$gbm.call$n.trees, type="response") 
  # df_prmap5 <- cbind(df_prmap5, prmap5)
  # 
  # prmap6 <- predict.gbm(BRT_malaria6, df_surwa_map,
  #                       n.trees=BRT_malaria6$gbm.call$n.trees, type="response")
  # df_prmap6 <- cbind(df_prmap6, prmap6)
  # 
  # prmap7 <- predict.gbm(BRT_malaria7, df_surwa_map,
  #                       n.trees=BRT_malaria7$gbm.call$n.trees, type="response") 
  # df_prmap7 <- cbind(df_prmap7, prmap7)
  # 
  # prmap8 <- predict.gbm(BRT_malaria8, df_surwa_map,
  #                       n.trees=BRT_malaria8$gbm.call$n.trees, type="response") 
  # df_prmap8 <- cbind(df_prmap8, prmap8)
  # 
  # prmap9 <- predict.gbm(BRT_malaria9, df_surwa_map,
  #                       n.trees=BRT_malaria9$gbm.call$n.trees, type="response") 
  # df_prmap9 <- cbind(df_prmap9, prmap9)
  # 
  # prmap10 <- predict.gbm(BRT_malaria10, df_surwa_map,
  #                        n.trees=BRT_malaria10$gbm.call$n.trees, type="response") 
  # df_prmap10 <- cbind(df_prmap10, prmap10)
  
  deviance <- rbind(BRT_malaria1$self.statistics$resid.deviance, BRT_malaria2$self.statistics$resid.deviance)
  #BRT_malaria2$self.statistics$resid.deviance, BRT_malaria3$self.statistics$resid.deviance, BRT_malaria4$self.statistics$resid.deviance)
  
  df_deviance <- cbind(df_deviance, deviance)
  
  f_RMSE <- function(x,y=testing$pf_pr) {sqrt(mean((x-y)^2))}
  RMSE <- sapply(predictions, f_RMSE)
  RMSE_combined <- cbind(RMSE_combined, RMSE)
  
  f_MAE <- function(x,y=testing$pf_pr){mean(abs(x-y))}
  MAE <- sapply(predictions, f_MAE)
  MAE_combined <- cbind(MAE_combined, MAE)
  
  f_IQR <- function(x,y=testing$pf_pr){IQR(abs(x-y))} 
  IQR <- sapply(predictions, f_IQR)
  IQR_combined <- cbind(IQR_combined, IQR)
  
  R1 <- summary(lm(preds1~testing$pf_pr))$adj.r.squared
  R2 <- summary(lm(preds2~testing$pf_pr))$adj.r.squared
  # R3 <- summary(lm(preds3~testing$pf_pr))$adj.r.squared
  # R4 <- summary(lm(preds4~testing$pf_pr))$adj.r.squared
  # R5 <- summary(lm(preds5~testing$pf_pr))$adj.r.squared
  # R6 <- summary(lm(preds6~testing$pf_pr))$adj.r.squared
  # R7 <- summary(lm(preds7~testing$pf_pr))$adj.r.squared
  # R8 <- summary(lm(preds8~testing$pf_pr))$adj.r.squared
  # R9 <- summary(lm(preds9~testing$pf_pr))$adj.r.squared
  # R10 <- summary(lm(preds10~testing$pf_pr))$adj.r.squared
  adj.R2 <- c(R1, R2)
  #, R2, R3, R4)
  adj.R2_combined <- cbind(adj.R2_combined, adj.R2)
  
  # ## add the RC of each run of each variable to the empty dataframes 
  RC1 <- BRT_malaria1$contributions
  RCModel1 <- merge(x = RCModel1 , y = RC1 , by = "var", all.x = TRUE)
  
  RC2 <- BRT_malaria2$contributions
  RCModel2 <- merge(x = RCModel2 , y = RC2 , by = "var", all.x = TRUE)
  
  # RC3 <- BRT_malaria3$contributions
  # RCModel3 <- merge(x = RCModel3 , y = RC3 , by = "var", all.x = TRUE)
  # 
  # RC4 <- BRT_malaria4$contributions
  # RCModel4 <- merge(x = RCModel4 , y = RC4 , by = "var", all.x = TRUE)
  
  # # ## Create PDP data plots to empty dataframes, only the y variables change between runs. Has to be done per specific variable of a single run  
  # Var1 <- partial(BRT_malaria1, pred.var = c("distance30_b10000p0"), train = data1, n.trees = 2000)
  # Var1Y <- cbind(Var1Y, Var1$yhat)
  # 
  # Var2 <- partial(BRT_malaria2, pred.var = c("distance30_b10000p0"), train = data2, n.trees = 2000)
  # Var2Y <- cbind(Var2Y, Var2$yhat)
  
  
  
}


library(matrixStats)

runName <- '10kJRC2'

# Average model performances
df_deviance$mean  <- rowMeans(df_deviance)
RMSE_mean <- rowMeans(RMSE_combined)
MAE_mean <- rowMeans(MAE_combined)
IQR_mean <- rowMeans(IQR_combined)
R2_mean <- rowMeans(adj.R2_combined)
performance <- cbind(RMSE_mean, MAE_mean, IQR_mean, R2_mean, deviance)
colnames(performance)<-c("RMSE", "MAE", "IQR", "adj.R2", "Deviance")
performance

#save the mean performance statistics and export it 
write.csv(performance, file=sprintf("Results/performanceStats/tableSummary%s.csv", runName), row.names = FALSE)

# Save the raw data of the performance statistics so it can be made into graphs in a different document 

write.csv(RMSE_combined, file=sprintf("Results/performanceStats/RMSE_combined%s.csv", runName), row.names = FALSE)

write.csv(MAE_combined, file=sprintf("Results/performanceStats/MAE_combined%s.csv", runName), row.names = FALSE)

write.csv(IQR_combined, file=sprintf("Results/performanceStats/IQR_combined%s.csv", runName), row.names = FALSE)

write.csv(adj.R2_combined, file=sprintf("Results/performanceStats/adj.R2_combined%s.csv", runName), row.names = FALSE)

write.csv(df_deviance, file=sprintf("Results/performanceStats/df_deviance%s.csv", runName), row.names = FALSE)



# Average RC of models, calculates the mean of each row except the first column 
RCModel1$mean <- rowMeans(RCModel1[,- 1 ])
RCModel1$min <- rowMins(as.matrix(RCModel1[,- 1 ]))
RCModel1$max <- rowMaxs(as.matrix(RCModel1[,- 1 ]))

RC1Mean <- RCModel1[,c("var", "mean", "min", "max")]
write.csv(RC1Mean, file=sprintf("Results/relativeContribution/RC1Mean%s.csv", runName), row.names = FALSE)

RCModel2$mean <- rowMeans(RCModel2[,- 1 ])
RCModel2$min <- rowMins(as.matrix(RCModel2[,- 1 ]))
RCModel2$max <- rowMaxs(as.matrix(RCModel2[,- 1 ]))
RC2Mean <- RCModel2[,c("var", "mean", "min", "max")]
write.csv(RC2Mean, file=sprintf("Results/relativeContribution/RC2Mean%s.csv", runName), row.names = FALSE)

# RCModel3$mean <- rowMeans(RCModel3[,- 1 ])
# RCModel3$min <- rowMins(as.matrix(RCModel3[,- 1 ]))
# RCModel3$max <- rowMaxs(as.matrix(RCModel3[,- 1 ]))
# RC3Mean <- RCModel3[,c("var", "mean", "min", "max")]
# write.csv(RC3Mean, file=sprintf("Results/relativeContribution/RC3Mean%s.csv", runName), row.names = FALSE)
# 
# RCModel4$mean <- rowMeans(RCModel4[,- 1 ])
# RCModel4$min <- rowMins(as.matrix(RCModel4[,- 1 ]))
# RCModel4$max <- rowMaxs(as.matrix(RCModel4[,- 1 ]))
# RC4Mean <- RCModel4[,c("var", "mean", "min", "max")]
# write.csv(RC4Mean, file=sprintf("Results/relativeContribution/RC4Mean%s.csv", runName), row.names = FALSE)


# Average PDP Plot of multiple runs, varX is the variable name of the variable of interest 
Var1Y$mean <- rowMeans(Var1Y)
Var1Y$x <- Var1$distance30_b10000p0
write.csv(Var1Y, file=sprintf("Results/pdpPlots/var1%s.csv", runName), row.names = FALSE)


Var2Y$mean <- rowMeans(Var2Y)
Var2Y$x <- Var2$distance30_b10000p0
write.csv(Var2Y, file=sprintf("Results/pdpPlots/var2%s.csv", runName), row.names = FALSE)


#PDP plots
gbm.plot(BRT_malaria1, n.plots =24, common.scale= TRUE,  write.title = F, x.label = "water temperature in January (K)")

# simple summary

summary(BRT_malaria1, cBars = 20, method = relative.influence, las = 2)
summary(BRT_malaria2, cBars = 20, method = relative.influence, las = 2)
summary(BRT_malaria3, cBars = 20, method = relative.influence, las = 2)
summary(BRT_malaria4, cBars = 20, method = relative.influence, las = 2)