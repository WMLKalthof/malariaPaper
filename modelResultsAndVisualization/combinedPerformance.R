## plot all values 
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(gridExtra)

setwd("C:/thesis/treeModel")

runName <- 'finalNovel'

################################################################################################ Performance statistics ################################################################################################

# Save the raw data of the performance statistics so it can be made into graphs in a different document 

performance <- read.csv(sprintf("Results/performanceStats/tableSummary%s.csv", runName))

RMSE_combined <- read.csv(sprintf("Results/performanceStats/RMSE_combined%s.csv", runName))

MAE_combined <- read.csv(sprintf("Results/performanceStats/MAE_combined%s.csv", runName))

IQR_combined <- read.csv(sprintf("Results/performanceStats/IQR_combined%s.csv", runName))

adj.R2_combined <- read.csv(sprintf("Results/performanceStats/adj.R2_combined%s.csv", runName))

df_deviance <- read.csv(sprintf("Results/performanceStats/df_deviance%s.csv", runName))

runName <- 'finalClassic'

performance2 <- read.csv(sprintf("Results/performanceStats/tableSummary%s.csv", runName))

RMSE_combined2 <- read.csv(sprintf("Results/performanceStats/RMSE_combined%s.csv", runName))

MAE_combined2 <- read.csv(sprintf("Results/performanceStats/MAE_combined%s.csv", runName))

IQR_combined2 <- read.csv(sprintf("Results/performanceStats/IQR_combined%s.csv", runName))

adj.R2_combined2 <- read.csv(sprintf("Results/performanceStats/adj.R2_combined%s.csv", runName))

df_deviance2 <- read.csv(sprintf("Results/performanceStats/df_deviance%s.csv", runName))



performance <- rbind(performance, performance2)

RMSE_combined <- rbind(RMSE_combined, RMSE_combined2)

MAE_combined <- rbind(MAE_combined, MAE_combined2)

IQR_combined <- rbind(IQR_combined, IQR_combined2)

adj.R2_combined <- rbind(adj.R2_combined, adj.R2_combined2)

df_deviance <- rbind(df_deviance, df_deviance2)

## create test datasets
RMSE_combinedTest <- RMSE_combined

MAE_combinedTest <- MAE_combined

IQR_combinedTest <- IQR_combined

adj.R2_combinedTest <- adj.R2_combined

df_devianceTest <- df_deviance


modelRun <- 'final'

namesCol <- c("1", "2","3", "4", "5", "6", "7")

RMSE_combinedTest <- t(RMSE_combinedTest)
colnames(RMSE_combinedTest) <- namesCol
RMSE_combined_stacked <- stack(as.data.frame(RMSE_combinedTest))
write.csv(RMSE_combined_stacked, file=sprintf("Results/finalResults/RMSE_combined_stacked%s.csv", modelRun), row.names = FALSE)

MAE_combinedTest <- t(MAE_combinedTest)
colnames(MAE_combinedTest) <- namesCol
MAE_combined_stacked <- stack(as.data.frame(MAE_combinedTest))
write.csv(MAE_combined_stacked, file=sprintf("Results/finalResults/MAE_combined_stacked%s.csv", modelRun), row.names = FALSE)

IQR_combinedTest <- t(IQR_combinedTest)
colnames(IQR_combinedTest) <- namesCol
IQR_combined_stacked <- stack(as.data.frame(IQR_combinedTest))
write.csv(IQR_combined_stacked, file=sprintf("Results/finalResults/IQR_combined_stacked%s.csv", modelRun), row.names = FALSE)

adj.R2_combinedTest <- t(adj.R2_combinedTest)
colnames(adj.R2_combinedTest) <- namesCol
R2_combined_stacked <- stack(as.data.frame(adj.R2_combinedTest))
write.csv(R2_combined_stacked, file=sprintf("Results/finalResults/R2_combined_stacked%s.csv", modelRun), row.names = FALSE)

df_devianceTest <- t(df_devianceTest)
colnames(df_devianceTest) <- namesCol
Deviance_combined_stacked <- stack(as.data.frame(df_devianceTest))
write.csv(Deviance_combined_stacked, file=sprintf("Results/finalResults/Deviance_combined_stacked%s.csv", modelRun), row.names = FALSE)

########################### Load files ###########################
RMSE_combined_stacked <- read.csv("Results/finalResults/RMSE_combined_stackedfinal.csv")
RMSE_combined_stacked <- RMSE_combined_stacked %>% 
  filter(!if_any(everything(), ~ . == 4))

Deviance_combined_stacked <- read.csv("Results/finalResults/Deviance_combined_stackedfinal.csv")
Deviance_combined_stacked <- Deviance_combined_stacked %>% 
  filter(!if_any(everything(), ~ . == 4))

IQR_combined_stacked <- read.csv("Results/finalResults/IQR_combined_stackedfinal.csv")
IQR_combined_stacked <- IQR_combined_stacked %>% 
  filter(!if_any(everything(), ~ . == 4))

R2_combined_stacked <- read.csv("Results/finalResults/R2_combined_stackedfinal.csv")
R2_combined_stacked <- R2_combined_stacked %>% 
  filter(!if_any(everything(), ~ . == 4))


# plot
plotRMSE <- ggplot(RMSE_combined_stacked, 
                      aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                          y=values)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(limits = c(0.16,0.22)) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("RMSE") + xlab("Model type") +
  labs(title = "RMSE | 100 km radius") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size=14), 
        legend.position="none")



##plot with ggplot 
plotIQR <- ggplot(IQR_combined_stacked, 
                     aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                         y=values)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(limits = c(0.12,0.18)) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("IQR") + xlab("Model type") +
  labs(title = "IQR of 50 runs") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size=14, angle = 65, hjust=1.05, vjust=1), 
        axis.title.y = element_text(size=14))


plotR2 <- ggplot(R2_combined_stacked, 
                    aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                        y=values)) +
  scale_y_continuous(limits = c(0.375,0.625)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("R2") + xlab("Model type") +
  labs(title = "R2 of 50 runs | 100 km radius") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        axis.title.x = element_text(size=14), 
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size=14), 
        legend.position="none")


plotDeviance <- ggplot(Deviance_combined_stacked, 
                          aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                              y=values)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(limits = c(110,170)) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("Residual deviance") + xlab("Model type") +
  labs(title = "Res. deviance | 100 km radius") +
  scale_x_discrete(labels = c("Confounders", "Precipitation", "High res | Novel", "Low res | Novel","Classic","Optimal")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size=14, angle = 65, hjust=1.05, vjust=1), 
        axis.title.y = element_text(size=14))


grid.arrange(plotR2,plotDeviance, plotRMSE, plotIQR, ncol = 2, nrow =2)

#################################################################################################################################################### 10k ##########################################################################
RMSE_combined_stacked10k <- read.csv("Results/finalResults/RMSE_combined_stacked10k2.csv")
RMSE_combined_stacked10k <- RMSE_combined_stacked10k %>% 
  filter(!if_any(everything(), ~ . == 4))

Deviance_combined_stacked10k <- read.csv("Results/finalResults/Deviance_combined_stacked10k2.csv")
Deviance_combined_stacked10k <- Deviance_combined_stacked10k %>% 
  filter(!if_any(everything(), ~ . == 4))

IQR_combined_stacked10k <- read.csv("Results/finalResults/IQR_combined_stacked10k2.csv")
IQR_combined_stacked10k <- IQR_combined_stacked10k %>% 
  filter(!if_any(everything(), ~ . == 4))

R2_combined_stacked10k <- read.csv("Results/finalResults/R2_combined_stacked10k2.csv")
R2_combined_stacked10k <- R2_combined_stacked10k %>% 
  filter(!if_any(everything(), ~ . == 4))

# plot
plotRMSE10k <- ggplot(RMSE_combined_stacked10k, 
                   aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                       y=values)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(limits = c(0.16,0.22)) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("RMSE") + xlab("Model type") +
  labs(title = "RMSE | 10 km radius") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size=14), 
        legend.position="none")



##plot with ggplot 
plotIQR10k <- ggplot(IQR_combined_stacked10k, 
                  aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                      y=values)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(limits = c(0.12,0.18)) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("IQR") + xlab("Model type") +
  labs(title = "IQR of 50 runs") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size=14, angle = 65, hjust=1.05, vjust=1), 
        axis.title.y = element_text(size=14))


plotR210k <- ggplot(R2_combined_stacked10k, 
                 aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                     y=values)) +
  scale_y_continuous(limits = c(0.375,0.625)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("R2") + xlab("Model type") +
  labs(title = "R2 of 50 runs | 10 km radius") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        axis.title.x = element_text(size=14), 
        axis.text.x = element_blank(), 
        axis.title.y = element_text(size=14), 
        legend.position="none")


plotDeviance10k <- ggplot(Deviance_combined_stacked10k, 
                       aes(x=factor(ind, levels=c("6","3", "2","5", "7","1")), 
                           y=values)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(limits = c(110,170)) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  ylab("Residual deviance") + xlab("Model type") +
  scale_x_discrete(labels = c("Confounders", "Precipitation", "High res | Novel", "Low res | Novel","Classic","Optimal")) +
  labs(title = "Res. deviance | 10 km radius") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size=14, angle = 65, hjust=1.05, vjust=1), 
        axis.title.y = element_text(size=14))


grid.arrange(plotR210k, plotDeviance10k, plotRMSE10k,plotIQR10k,ncol = 2, nrow =2)

grid.arrange(plotR210k, plotR2, plotRMSE10k, plotRMSE, plotDeviance10k,plotDeviance,ncol = 2, nrow =3)
