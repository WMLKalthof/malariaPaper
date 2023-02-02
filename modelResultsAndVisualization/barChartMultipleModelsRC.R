## plot all values 
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(forcats)
setwd("C:/thesis/treeModel")

runName <- 'finalClassic'

## First part is to take the raw results of the different scripts and scenarios and put them together
#################################################################################### Average per variable type ###############################
## Model 1
RC1import <- read.csv(sprintf("Results/relativeContribution/RC1Mean%s.csv", runName))
RC1avVar <- RC1import %>% 
  separate(var, c("var", "b"), sep = "[_A-Z0-9]", extra = "merge") %>% 
  group_by(var = ifelse(b %in% var, b, var)) %>% 
  summarize(mean = sum(mean), .groups = "drop")

RC1avVar$model <- 6



RC2import <- read.csv(sprintf("Results/relativeContribution/RC2Mean%s.csv", runName))
RC2avVar <- RC2import %>% 
  separate(var, c("var", "b"), sep = "[_A-Z0-9]", extra = "merge") %>% 
  group_by(var = ifelse(b %in% var, b, var)) %>% 
  summarize(mean = sum(mean), .groups = "drop")

RC2avVar$model <- 7


RC3import <- read.csv(sprintf("Results/relativeContribution/RC3Mean%s.csv", runName))
RC3avVar <- RC3import %>% 
  separate(var, c("var", "b"), sep = "[_A-Z0-9]", extra = "merge") %>% 
  group_by(var = ifelse(b %in% var, b, var)) %>% 
  summarize(mean = sum(mean), .groups = "drop")

RC3avVar$model <- 3


combinedRCClassic <- rbind(RC1avVar, RC2avVar)

combinedRCJRC <- rbind(RC1avVar, RC2avVar)

combinedRCNovel <- rbind(RC1avVar, RC2avVar, RC3avVar)


modelRun <- 'final2'
totalRC <- rbind(combinedRCNovel, combinedRCJRC, combinedRCClassic)
write.csv(totalRC, file=sprintf("Results/finalResults/totalRC%s.csv", modelRun), row.names = FALSE)


## Here you can load in the finalized results for the visualization 

data100k <- read.csv("Results/finalResults/totalRCfinal2.csv")
data10k <- read.csv("Results/finalResults/totalRC10k2.csv")

## remove the 4th scenario as we are not using that anymore 
data100k <- data100k %>% 
  filter(!if_any(everything(), ~ . == 4))
data10k <- data10k %>% 
  filter(!if_any(everything(), ~ . == 4))


plot100k <- ggplot(data100k, 
       aes(x= factor(model, levels=c( "6","3", "2","5", "7","1")), y=mean, fill= factor(var, levels=c( "distance", "water", "prec")))) + 
  geom_col(position = "stack") +
  ylab("Relative contribution (%)") + xlab("Model type") +
  labs(title = "Relative contribution | max 100 km buffer radius") +
  scale_fill_manual(values=c("dodgerblue","dodgerblue4", "cyan", "#grey"), name = "Predictor type", labels = c("Distance to water","NDWI v PCTWA v NRWABO", "Precipitation", "Confounders")) +
  scale_x_discrete(labels = c("Confounders", "Precipitation", "High res | Novel", "Low res | Novel","Classic","Optimal")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title.x = element_text(size=14), 
        axis.text.x = element_text(size=14, angle = 65, hjust=1.05, vjust=1), 
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))

plot10k <- ggplot(data10k, 
       aes(x= factor(model, levels=c("6","3" ,"2" , "5", "7","1")), y=mean, fill= factor(var, levels=c( "distance", "water", "prec")))) + 
  geom_col(position = "stack") +
  ylab("Relative contribution (%)") + xlab("Model type") +
  labs(title = "Relative contribution | max 10 km buffer radius") +
  scale_fill_manual(values=c("dodgerblue","dodgerblue4", "cyan", "#grey"), name = "Predictor type", labels = c("Distance to water","NDWI v PCTWA v NRWABO", "Precipitation", "Confounders")) +
  scale_x_discrete(labels = c("Confounders", "Precipitation", "High res | Novel", "Low res | Novel","Classic","Optimal")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title.x = element_text(size=14), 
        axis.text.x = element_text(size=14, angle = 65, hjust=1.05, vjust=1), 
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
        #legend.position="none")  


grid.arrange(plot10k,plot100k,  ncol = 2, nrow =1)
