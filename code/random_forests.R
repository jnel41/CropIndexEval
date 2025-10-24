# -----------------------------------------------------------------
#
#   Random Forests
#   For Jess Stephenson | Oct 24, 2025
#   Rebekah Scott | Statistical Consulting Manager
#   Department of Statistics, Iowa State University 
#
# -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(caret) # lets you do a bunch of ml models 
library(randomForest)

# read in data ----

ia_CPI <- readr::read_csv("data/raw/ia_cpi.csv") |> 
  mutate(NCCPIx100 = NCCPIall * 100,
         cpi_diff = abs(OCProdIdx - NCCPIx100),
         state = "IA")

# data structure ----

# look at cols 
colnames(ia_CPI)
# [1] "mukey"          "gSSURGOversion" "MUsymbol"       "WTDepAprJun"   
# [5] "FloodFreq"      "PondFreq"       "DrainCls"       "DrainClsWet"   
# [9] "HydroGrp"       "Hywidric"         "OCProdIdx"      "OCprodIdxSrc"  
# [13] "NCCPIall"       "RootZnDepth"    "RootZnAWS"      "Droughty"      
# [17] "PotWetandSoil"  "NCCPIx100"      "cpi_diff"       "state"  
summary(ia_CPI)

length(unique(ia_CPI$mukey)) # 9930 unique ids, skip 
unique(ia_CPI$gSSURGOversion) # same data for all, skip 
length(unique(ia_CPI$MUsymbol)) # 2382, some repetition 
unique(ia_CPI$WTDepAprJun) # numeric
unique(ia_CPI$FloodFreq) # "None" "Rare" "Frequent" "Occasional" "NULL"  
summary(ia_CPI$PondFreq) # numeric? 
unique(ia_CPI$DrainCls) # 7 values, not sure what to do 
# [1] "Excessively drained"          "Well drained"                
# [3] "Very poorly drained"          "Moderately well drained"     
# [5] "Somewhat excessively drained" "Poorly drained"              
# [7] "Somewhat poorly drained"      "NULL"      
unique(ia_CPI$DrainClsWet) # same as last
unique(ia_CPI$HydroGrp) # 3 cats, "A"  "C"  "B"  "B/D"  "D"  "C/D"  "A/D"  "NULL"
summary(ia_CPI$Hydric) # numeric? 
summary(ia_CPI$OCProdIdx) # CSR2??
unique(ia_CPI$OCprodIdxSrc) # one value, ignore 
summary(ia_CPI$NCCPIall) # response on 0-1 scale 
unique(ia_CPI$RootZnDepth) # numeric 
unique(ia_CPI$RootZnAWS) # numeric 
unique(ia_CPI$Droughty) # numeric 
unique(ia_CPI$PotWetandSoil) # numeric 
unique(ia_CPI$NCCPIx100) # NCCPI 
unique(ia_CPI$cpi_diff) # CSR2 - NCCPI 
unique(ia_CPI$state) # one value, ignore for now 

# encode data for machine learning ----

# questions: 
# what to do with null values? I filtered them out 
length(unique(ia_CPI$MUsymbol)) # what does this mean?  
# not sure what to do with DrainClsWet, DrainCls

ia_CPI_ml <- ia_CPI |> 
  mutate(
    # convert numeric values, create NAs
    WTDepAprJun = as.numeric(WTDepAprJun), 
    PondFreq = as.numeric(PondFreq),
    Hydric = as.numeric(Hydric),
    RootZnDepth = as.numeric(RootZnDepth), 
    RootZnAWS = as.numeric(RootZnAWS),
    Droughty = as.numeric(Droughty),
    PotWetandSoil = as.numeric(PotWetandSoil),
    
    # FloodFreq on a Likert  scale 
    FloodFreq = case_match(FloodFreq, 
                           "None" ~ 0, "Rare" ~ 1, "Occasional" ~ 2,
                           "Frequent" ~ 3, "Null" ~ NA), 
    
    # DrainCls split to WellDrain, PoorDrain, ExcessDrain
    WellDrain = case_match(DrainCls, 
                              "Moderately well drained" ~ 1, 
                              "Well drained" ~ 2, 
                             .default = 0),
    PoorDrain = case_match(DrainCls, 
                                "Somewhat poorly drained" ~ 1, 
                                "Poorly drained" ~ 2, 
                                "Very poorly drained" ~ 3, 
                               .default = 0), 
    ExcessDrain = case_match(DrainCls, 
                                "Somewhat excessively drained" ~ 1, 
                                "Excessively drained" ~ 2,  
                               .default = 0),
    
    # HydroGrp into four binary variables for ABCD
    HydroGrpA = ifelse(HydroGrp %in% c("A", "A/D"), 1, 0), 
    HydroGrpB = ifelse(HydroGrp %in% c("B", "B/D"), 1, 0), 
    HydroGrpC = ifelse(HydroGrp %in% c("C", "C/D"), 1, 0), 
    HydroGrpD = ifelse(HydroGrp %in% c("D", "A/D", "B/D", "C/D"), 1, 0), 
    CSR2Bigger = factor(ifelse(OCProdIdx - NCCPIx100 >= 0, 1, 0))
    ) |> select(OCProdIdx, CSR2Bigger, NCCPIx100,
                WTDepAprJun, PondFreq, Hydric, RootZnDepth, RootZnAWS, 
                Droughty, PotWetandSoil, FloodFreq, 
                WellDrain, PoorDrain, ExcessDrain, 
                HydroGrpA, HydroGrpB, HydroGrpC, HydroGrpD)

rownames(ia_CPI_ml) <- NULL

ia_CPI_ml # 9930
na.omit(ia_CPI_ml) # 3903 complete cases 

save(file = "data/ia_CPI.Rdata", ia_CPI_ml) # save data frame as .Rdata

# random forest: regression ----

load(file ="data/ia_CPI.Rdata") # load cleaned data 

ia_CPI_ml2 <- na.omit(ia_CPI_ml)
rownames(ia_CPI_ml2) <- NULL

# regression with NCCPI + others predicting CSR2
set.seed(3377)

# first, tune with cross validation 
RFOOBF_R <- train(y = ia_CPI_ml2$OCProdIdx, x = ia_CPI_ml2[,3:18], method="rf",
              tuneGrid = data.frame(.mtry = 1:15), importance = TRUE,
              trControl = trainControl(method = "oob"), ntree = 500, 
              nodesize = 5)

RFOOBF_R # mtry = 7

RFOOBF_R_plot <- ggplot(RFOOBF_R) +
  labs(title = "Regression RF Tuning by OOB Error", y = "OOB RMSE", 
       subtitle = "Tuning paramaters: mtry = 7, ntree = 500, nodesize = 5") + 
  theme_bw() +  theme(plot.title = element_text(hjust = 0.5), 
                      plot.subtitle = element_text(hjust = 0.5))

RFOOBF_R_plot
ggsave("figures/RF_OOB_regression.pdf", 
       RFOOBF_R_plot, width = 7, height = 5, dpi = 300)

# var importance 
varImp(RFOOBF_R)

# could use train / test set 
# RF_test_pred <- predict(RFOOBF, test_set)
# RF_test_rmse <- RMSE(RF_test_pred, test_set$y)

# random forest: classification (over / under) ----

set.seed(3377)
RFOOBF_C <- train(y = ia_CPI_ml2$CSR2Bigger, x = ia_CPI_ml2[,4:18], method="rf",
                  tuneGrid = data.frame(.mtry = 1:15), importance = TRUE,
                  trControl = trainControl(method = "oob"), ntree = 500, 
                  nodesize = 5)

RFOOBF_C # mtry = 10

RFOOBF_C_plot <- ggplot(RFOOBF_C) +
  labs(title = "Classification RF Tuning by OOB Error", y= "OOB Accurary", 
       subtitle = "Tuning paramaters: mtry = 10, ntree = 500, nodesize = 5") + 
  theme_bw() +  theme(plot.title = element_text(hjust = 0.5), 
                      plot.subtitle = element_text(hjust = 0.5))

RFOOBF_C_plot
ggsave("figures/RF_OOB_classification.pdf", 
       RFOOBF_C_plot, width = 7, height = 5, dpi = 300)

# var importance 
varImp(RFOOBF_C)

# plot a tree ----
# I'm not 100% sure if I trust this code 
# from # https://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree

# run the forest 
set.seed(3377)
rf_class <- randomForest(as.factor(CSR2Bigger) ~ ., data = ia_CPI_ml2[,c(2, 4:18)], 
                         mtry = 10, ntree = 500, nodesize = 5)

# devtools::install_github("munoztd0/reprtree")
library(reprtree)
pdf("figures/classification_tree.pdf", height = 10, width = 30)
plot.getTree(rf_class, k = 5)
getTree(rf_class, k = 1, labelVar = TRUE)
dev.off()

ReprTree(rf_class)

# other options 

# https://stats.stackexchange.com/questions/21152/obtaining-knowledge-from-a-random-forest