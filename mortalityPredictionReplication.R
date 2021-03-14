rm(list=ls())
set.seed(987654321)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(ggalluvial)
library(xgboost)
library(rBayesianOptimization)
library(Amelia)
library(patchwork)
library(SHAPforxgboost)
library(tidyquant)
library(tidyverse)
library(caret)
library(PRROC)
library(xgboostExplainer)
library(viridis)
library(openxlsx)

#install.packages("lightgbm")

# xgboostExplainer was installed using:
# install.packages("remotes")
# remotes::install_github("davidADSP/xgboostExplainer")

protocol_fill_color = "grey25"

theme_bluewhite <- function (base_size = 11, base_family = "serif") {
  theme_bw() %+replace% 
    theme(
      text = element_text(family = "serif"),
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "grey97"),
      panel.border = element_rect(color = "darkred", fill = NA, size = 1), ##05014a
      axis.line = element_line(color = "grey97"),
      axis.ticks = element_line(color = "grey25"),
      axis.title = element_text(size = 10),
      axis.text = element_text(color = "grey25", size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 15, hjust = 0.5),
      strip.background = element_rect(fill = '#05014a'),
      strip.text = element_text(size = 10, colour = 'white'), # changes the facet wrap text size and color
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

patientFeatures <- openxlsx::read.xlsx("C:/Users/Matt/Desktop/PhD Thesis/Chapter_3_a_1_SISubmission/data/covid19PatientData.xlsx", detectDates = TRUE)

patientFeaturesData <- patientFeatures %>% 
  rename(Survival_death = `Survival/death`) %>% 
  mutate(
    Gender = as.numeric(as.factor(Gender)) - 1,
    Admission.to.ICU = as.numeric(as.factor(Admission.to.ICU)) - 1,
    Survival_death = as.numeric(as.factor(Survival_death)) - 1,
    Date.of.admission = as.Date(Date.of.admission),
    Date.of.discharge = as.Date(Date.of.discharge),
    Date.blood.analysis = as.Date(Date.blood.analysis),
    daysInHospital = Date.of.discharge - Date.of.admission,
    daysInHospital = as.numeric(daysInHospital)
    ) %>% 
  select(-c(Date.of.presentation.emergency.room, Date.of.admission, Date.of.discharge, Date.of.death, Days.from.admission.to.death, Date.blood.analysis))

  
#######################################################################################################
#######################################################################################################
#######################################################################################################

library(coronavirus)
data("coronavirus")
data <- coronavirus

rect_dat = data.frame(
  xmin = as.Date(c(
    "2020-03-18")),   # first date of admission
  xmax = as.Date(c(
    "2020-06-03"))    # last day of discharge
) 

data %>% 
  filter(country == "Netherlands") %>%
  filter(province != "Aruba" & province != "Bonaire, Sint Eustatius and Saba" & province != "Curacao" & province != "Sint Maarten") %>% 
  filter(type == "confirmed") %>% 
  ggplot(aes(x = date, y = cases)) +
  geom_line(color = "darkblue") +
  geom_rect(data = rect_dat,
            aes(ymin = -Inf, ymax = Inf, xmin = xmin, xmax = xmax),
            inherit.aes = 0, fill = "darkred", alpha = 0.1) +
  ggtitle("Confirmed cases: Netherlands") +
  labs(x = "2020", y = "# Cases") +
  theme_bluewhite() +
  theme(
    axis.text.x = element_text(size = 12.5),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 12.5),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 20)
  )

#ggsave("C:/Users/Matt/Desktop/PhD Thesis/Chapter_3_a_1_SISubmission/paper/figures/dailyCOVID19Cases.pdf")

#######################################################################################################
#######################################################################################################
#######################################################################################################


summaryStatsPatientFeatures <- patientFeaturesData %>% 
  group_by(Survival_death) %>% 
  summarise(
    MeanAge = mean(Age),
    SDAge = sd(Age),
    MeanDaysinHospital = mean(daysInHospital, na.rm = TRUE),
    SDDaysinHospital = sd(daysInHospital, na.rm = TRUE),
    
    MeanLymphocyteCount = mean(Lymphocytes, na.rm = TRUE),
    SDLymphocyteCount = sd(Lymphocytes, na.rm = TRUE),
    MeanLeukocytes = mean(Leukocytes, na.rm = TRUE),
    SDLeukocytes = sd(Leukocytes, na.rm = TRUE),
    MeanLD = mean(LD, na.rm = TRUE),
    SDLD = sd(LD, na.rm = TRUE),
    MeanCRP = mean(CRP, na.rm = TRUE),
    SDCRP = sd(CRP, na.rm = TRUE),

    Females = sum(as.numeric(as.character(Gender))),
    Males = n() - Females,
    Females = round(Females / n() * 100, 0),
    Males = round(Males / n() * 100, 0)
  ) %>% 
  pivot_longer(cols = MeanAge:Males) %>% 
  pivot_wider(names_from = Survival_death, values_from = c(value))

knitr::kable(
  summaryStatsPatientFeatures,
  col.names = c('', 'Survived', 'Perished'),
  row.names = NA,
  caption = "Summary Statistics Patient Characteristics",
  #digits = 1,
  align = "lcc",
  booktabs = TRUE,
  #table.envir = "table",
  linesep = "",
  escape = FALSE,
  format = 'latex',
  label = "patientCharacteristics",
  digits = 2
  #position = "!b"
)

summaryStatsPatientFeatures %>%
  mutate(
    name = case_when(
      name == "MeanAge" ~ "Mean",
      name == "SDAge" ~ "SD",
      name == "MeanDaysinHospital" ~ "Mean",
      name == "SDDaysinHospital" ~ "SD",
      
      name == "MeanLymphocyteCount" ~ "Mean",
      name == "SDLymphocyteCount" ~ "SD",
      name == "MeanLeukocytes" ~ "Mean",
      name == "SDLeukocytes" ~ "SD",
      name == "MeanLD" ~ "Mean",
      name == "SDLD" ~ "SD",
      name == "MeanCRP" ~ "Mean",
      name == "SDCRP" ~ "SD",
      # name == "MeanEutrophilsCount" ~ "Mean",
      # name == "SDEutrophilsCount" ~ "SD",
      # name == "MeanEosinophils" ~ "Mean",
      # name == "SDEosinophils" ~ "SD",
      name == "Females" ~ "Females",
      name == "Males" ~ "Males"
    )
  ) %>%  
  kable("latex", booktabs = T, digits = 2) %>%
  kable_styling(position = "center", font_size = 7, latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  row_spec(0, angle = 0) %>%
  add_header_above(c(" " = 1, "Survived" = 1, "Perished" = 1)) %>%
  pack_rows("Age", 1, 2, latex_gap_space = "2em") %>%
  pack_rows("Days in Hospital", 3, 4, latex_gap_space = "2em") %>%
  pack_rows("Lymphocyte", 5, 6, latex_gap_space = "2em") %>%
  pack_rows("Leukocytes", 7, 8, latex_gap_space = "2em") %>%
  pack_rows("LD", 9, 10, latex_gap_space = "2em") %>%
  pack_rows("CRP", 11, 12, latex_gap_space = "2em") %>%
  # pack_rows("Eutrophils Count", 15, 16, latex_gap_space = "2em") %>%
  # pack_rows("Eosinophils", 13, 14, latex_gap_space = "2em") %>% 
  pack_rows("Gender (Percent)", 13, 14, latex_gap_space = "2em")

#write.csv(summaryStatsPatientFeatures, "/root/capsule/results/summaryStatsPatientFeatures.csv", row.names = FALSE)


#######################################################################################################
#######################################################################################################
#######################################################################################################

smp_size <- floor(0.75 * nrow(patientFeaturesData))
train_ind <- sample(seq_len(nrow(patientFeaturesData)), size = smp_size)

train <- patientFeaturesData[train_ind, ]
test <- patientFeaturesData[-train_ind, ]


X_train <- train %>% 
  select(-c(Survival_death)) %>% 
  as.matrix()

Y_train <- train %>% 
  select(c(Survival_death)) %>% 
  as.matrix()

X_test <- test %>% 
  select(-c(Survival_death)) %>% 
  as.matrix()

Y_test <- test %>% 
  select(c(Survival_death)) %>% 
  as.matrix()


MultiMLDataSet <- bind_cols(data.frame(Y_train), data.frame(X_train))

#######################################################################################################
#######################################################################################################
#######################################################################################################

######################### ######################### 
######################### ######################### 
######################### Naive Bayes #########################
#library(e1071)   # NOTE: do not put the package at the top - it conflicts with showwaterfall2

Naive_Bayes_Model <- e1071::naiveBayes(as.factor(Survival_death) ~ ., data = MultiMLDataSet)
Naive_Bayes_Model

NB_Predictions = predict(Naive_Bayes_Model, X_test, type = "class")


######################### ######################### 
######################### ######################### 
######################### Logistic Model #########################

Logistic_Model <- glm(Survival_death ~ ., data = na.omit(MultiMLDataSet), family = "binomial")

Logistic_Predictions = predict(Logistic_Model, data.frame(X_test), type = "response")

Logistic_Predictions = ifelse(Logistic_Predictions > 0.5, 1, 0)

######################### ######################### 
######################### ######################### 
######################### Random Forest #########################
#library(randomForest)  # NOTE: do not put the package at the top - it conflicts with showwaterfall2

Random_Forest_Model <- randomForest::randomForest(factor(Survival_death) ~., data = MultiMLDataSet, na.action = na.omit, ntree = 50, importance = TRUE)

RF_Predictions <- predict(Random_Forest_Model, X_test)

######################### ######################### 
######################### ######################### 
######################### adaBoost ######################### 
library(JOUSBoost)
adaBoost_Y_train <- ifelse(Y_train == 0, -1, 1) # adaBoost expects a 0 and 1 prediction
adaBoost_Model <- adaboost(X_train, adaBoost_Y_train, tree_depth = 8, n_rounds = 100, verbose = FALSE, control = NULL)
adaBoost_Predictions <- predict(adaBoost_Model, X_test)
adaBoost_Predictions <- ifelse(adaBoost_Predictions == -1, 0, 1) # convert back to 0 and 1 predictions

######################### ######################### 
######################### ######################### 
######################### Classification Tree ######################### 

library(rpart)

classTree <- rpart(factor(Survival_death) ~ ., data = MultiMLDataSet, method = "class")
print(classTree)
plotcp(classTree)
summary(classTree)

classTreePredictions <- predict(classTree, data.frame(X_test), type = "class")

######################### ######################### 
######################### ######################### 
######################### XGBoost #########################


dtrain <- xgb.DMatrix(data = X_train, label = Y_train)

cv_folds <- KFold(Y_train, nfolds = 5, stratified = TRUE, seed = 0)

# xgb_cv_bayes <- function(eta, max.depth, min_child_weight, subsample) {
#   cv <- xgb.cv(
#     params = list(
#       booster = "gbtree",
#       eta = eta,
#       max_depth = max.depth,
#       min_child_weight = min_child_weight,
#       subsample = subsample,
#       colsample_bytree = 0.6,
#       lambda = 1,
#       alpha = 0,
#       objective = "binary:logistic",
#       eval_metric = "auc"
#       ),
#     data = dtrain,
#     nround = 100,
#     folds = cv_folds,
#     prediction = TRUE,
#     showsd = TRUE,
#     early.stop.round = 5,
#     maximize = TRUE,
#     verbose = 0
#     )
#   list(
#     Score = cv$evaluation_log[, max(test_auc_mean)], Pred = cv$pred
#     )
# }
# 
# OPT_Res <- BayesianOptimization(
#   xgb_cv_bayes,
#   bounds = list(
#     eta = c(0.01L, 0.05L, 0.1L, 0.3L),
#     max.depth = c(6L, 8L, 12L),
#     min_child_weight = c(1L, 10L),
#     subsample = c(0.5, 0.8, 1)
#     ),
#   init_grid_dt = NULL,
#   init_points = 10,
#   n_iter = 50,
#   acq = "ucb",
#   kappa = 2.576,
#   eps = 0.0,
#   verbose = TRUE
#   )
# 
# params <- list(
#   "eta" = unname(OPT_Res$Best_Par["eta"]),
#   "max_depth" = unname(OPT_Res$Best_Par["max.depth"]),
#   "colsample_bytree" = 1,
#   "min_child_weight" = unname(OPT_Res$Best_Par["min_child_weight"]),
#   "subsample"= unname(OPT_Res$Best_Par["subsample"]),
#   "objective"="binary:logistic",
#   "gamma" = 1,
#   "lambda" = 1,
#   "alpha" = 0,
#   "max_delta_step" = 0,
#   "colsample_bylevel" = 1,
#   "eval_metric"= "auc",
#   "set.seed" = 176
#   )

params <- list(
  "eta" = 0.1,
  "max_depth" = 5,
  "colsample_bytree" = 1,
  "min_child_weight" = 1,
  "subsample"= 0.73,
  "objective"="binary:logistic",
  "gamma" = 1,
  "lambda" = 1,
  "alpha" = 0,
  "max_delta_step" = 0,
  "colsample_bylevel" = 1,
  "eval_metric"= "auc",
  "set.seed" = 176
)

watchlist <- list("train" = dtrain)
nround = 10   
xgb.model <- xgb.train(params, dtrain, nround, watchlist)

dtest <- xgb.DMatrix(data = X_test)
predictions <- predict(object = xgb.model, newdata = dtest, type = 'prob')
XGBPredictions <- ifelse(predictions > 0.5, 1, 0)

results <- cbind(test, predictions) %>% 
  mutate(
    pred_status = case_when(
      predictions > 0.50 ~ 1,
      predictions <= 0.50 ~ 0
    ),
    correct = case_when(
      Survival_death == pred_status ~ "Correct",
      TRUE ~ "Incorrect"
    ),
    outcome_text = case_when(
      Survival_death == 1 ~ "Perished",
      Survival_death == 0 ~ "Survived"
    )
  ) %>% 
  mutate_at(
    vars(Survival_death, pred_status, correct, outcome_text), funs(factor)
  ) %>% 
  add_count(Survival_death, pred_status)


######################### ######################### 
######################### ######################### 
######################### Light GBM ######################### 

# NOTE: Not included - RStudio crashes when run.

###################### Compare ML models #############################
library(caret)
con_NB <- confusionMatrix(NB_Predictions, factor(Y_test))
con_Logistic <- confusionMatrix(factor(Logistic_Predictions), factor(Y_test))
con_RF <- confusionMatrix(RF_Predictions, factor(Y_test))
con_adaBoost <- confusionMatrix(factor(adaBoost_Predictions), factor(Y_test))
con_classTree <- confusionMatrix(classTreePredictions, factor(Y_test))     
#con_LGBM <- confusionMatrix(factor(lightGBM_Predictions), factor(Y_test))
con_XGB <- confusionMatrix(factor(XGBPredictions), factor(Y_test))

#######################################################################################################
#######################################################################################################
#######################################################################################################

################## Performance metrics: #####################

############## Naive Bayes Calculations #####################
MCC_NB <- list(
  TP <- con_NB$table[[1]],
  FP <- con_NB$table[[2]],
  FN <- con_NB$table[[3]],
  TN <- con_NB$table[[4]]
) %>%  # # MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))
  pmap_dbl(., ~ ((..1 * ..4) - (..2 * ..3))/sqrt((..1 + ..2) * (..1 + ..3) * (..4 + ..2) * (..4 + ..3)))

NB_pred_outcome <- cbind(as.numeric(NB_Predictions), as.numeric(Y_test)) %>% 
  data.frame() %>% 
  setNames(c("predictions", "outcome"))

NB_fg <- NB_pred_outcome %>% 
  filter(outcome == 1) %>%
  pull(predictions)

NB_bg <- NB_pred_outcome %>% 
  filter(outcome == 0) %>%
  pull(predictions)

NB_pr <- PRROC::pr.curve(
  scores.class0 = NB_fg, scores.class1 = NB_bg, curve = TRUE)
############## END Naive Bayes Calculations ###################

############## Logistic Calculations #####################
MCC_Logistic <- list(
  TP <- con_Logistic$table[[1]],
  FP <- con_Logistic$table[[2]],
  FN <- con_Logistic$table[[3]],
  TN <- con_Logistic$table[[4]]
) %>%  # # MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))
  pmap_dbl(., ~ ((..1 * ..4) - (..2 * ..3))/sqrt((..1 + ..2) * (..1 + ..3) * (..4 + ..2) * (..4 + ..3)))

Logistic_pred_outcome <- cbind(as.numeric(Logistic_Predictions), as.numeric(Y_test)) %>% 
  data.frame() %>% 
  setNames(c("predictions", "outcome"))

Logistic_fg <- Logistic_pred_outcome %>% 
  filter(outcome == 1) %>%
  drop_na() %>% 
  pull(predictions)

Logistic_bg <- Logistic_pred_outcome %>% 
  filter(outcome == 0) %>%
  drop_na() %>% 
  pull(predictions)

Logistic_pr <- PRROC::pr.curve(
  scores.class0 = Logistic_fg, scores.class1 = Logistic_bg, curve = TRUE)
############## END Logistic Calculations ###################

############## Random Forest Calculations #####################
MCC_RF <- list(
  TP <- con_RF$table[[1]],
  FP <- con_RF$table[[2]],
  FN <- con_RF$table[[3]],
  TN <- con_RF$table[[4]]
) %>%  # # MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))
  pmap_dbl(., ~ ((..1 * ..4) - (..2 * ..3))/sqrt((..1 + ..2) * (..1 + ..3) * (..4 + ..2) * (..4 + ..3)))

RF_pred_outcome <- cbind(as.numeric(RF_Predictions), as.numeric(Y_test)) %>% 
  data.frame() %>% 
  setNames(c("predictions", "outcome"))

RF_fg <- RF_pred_outcome %>% 
  filter(outcome == 1) %>%
  drop_na() %>% 
  pull(predictions)

RF_bg <- RF_pred_outcome %>% 
  filter(outcome == 0) %>%
  drop_na() %>% 
  pull(predictions)

RF_pr <- PRROC::pr.curve(
  scores.class0 = RF_fg, scores.class1 = RF_bg, curve = TRUE)
############## END Random Forest Calculations ###################

############## adaBoost Calculations #####################
MCC_adaBoost <- list(
  TP <- con_adaBoost$table[[1]],
  FP <- con_adaBoost$table[[2]],
  FN <- con_adaBoost$table[[3]],
  TN <- con_adaBoost$table[[4]]
) %>%  # # MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))
  pmap_dbl(., ~ ((..1 * ..4) - (..2 * ..3))/sqrt((..1 + ..2) * (..1 + ..3) * (..4 + ..2) * (..4 + ..3)))

adaBoost_pred_outcome <- cbind(as.numeric(adaBoost_Predictions), as.numeric(Y_test)) %>% 
  data.frame() %>% 
  setNames(c("predictions", "outcome"))

adaBoost_fg <- adaBoost_pred_outcome %>% 
  filter(outcome == 1) %>%
  pull(predictions)

adaBoost_bg <- adaBoost_pred_outcome %>% 
  filter(outcome == 0) %>%
  pull(predictions)

adaBoost_pr <- PRROC::pr.curve(
  scores.class0 = adaBoost_fg, scores.class1 = adaBoost_bg, curve = TRUE)
############## END adaBoost Calculations ###################

############## Class Tree Calculations #####################
MCC_classTree <- list(
  TP <- con_classTree$table[[1]],
  FP <- con_classTree$table[[2]],
  FN <- con_classTree$table[[3]],
  TN <- con_classTree$table[[4]]
) %>%  # # MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))
  pmap_dbl(., ~ ((..1 * ..4) - (..2 * ..3))/sqrt((..1 + ..2) * (..1 + ..3) * (..4 + ..2) * (..4 + ..3)))

classTree_pred_outcome <- cbind(as.numeric(classTreePredictions), as.numeric(Y_test)) %>% 
  data.frame() %>% 
  setNames(c("predictions", "outcome"))

classTree_fg <- classTree_pred_outcome %>% 
  filter(outcome == 1) %>%
  pull(predictions)

classTree_bg <- classTree_pred_outcome %>% 
  filter(outcome == 0) %>%
  pull(predictions)

classTree_pr <- PRROC::pr.curve(
  scores.class0 = classTree_fg, scores.class1 = classTree_bg, curve = TRUE)
############## END Classification Tree Calculations ###################

############## Light GBM Calculations #####################
# MCC_LGBM <- list(
#   TP <- con_LGBM$table[[1]],
#   FP <- con_LGBM$table[[2]],
#   FN <- con_LGBM$table[[3]],
#   TN <- con_LGBM$table[[4]]
# ) %>%  # # MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))
#   pmap_dbl(., ~ ((..1 * ..4) - (..2 * ..3))/sqrt((..1 + ..2) * (..1 + ..3) * (..4 + ..2) * (..4 + ..3)))
# 
# LGBM_pred_outcome <- cbind(as.numeric(lightGBM_Predictions), as.numeric(Y_test)) %>% 
#   data.frame() %>% 
#   setNames(c("predictions", "outcome"))
# 
# LGBM_fg <- LGBM_pred_outcome %>% 
#   filter(outcome == 1) %>%
#   pull(predictions)
# 
# LGBM_bg <- LGBM_pred_outcome %>% 
#   filter(outcome == 0) %>%
#   pull(predictions)
# 
# LGBM_pr <- PRROC::pr.curve(
#   scores.class0 = LGBM_fg, scores.class1 = LGBM_bg, curve = TRUE)
############## END Light GBM Calculations ###################

#######################################################################################################
#######################################################################################################
#######################################################################################################

############## XGBoost Calculations #####################
MCC_XGB <- list(
  TP <- con_XGB$table[[1]],
  FP <- con_XGB$table[[2]],
  FN <- con_XGB$table[[3]],
  TN <- con_XGB$table[[4]]
) %>%  # # MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))
  pmap_dbl(., ~ ((..1 * ..4) - (..2 * ..3))/sqrt((..1 + ..2) * (..1 + ..3) * (..4 + ..2) * (..4 + ..3)))

XGB_pred_outcome <- cbind(as.numeric(XGBPredictions), as.numeric(Y_test)) %>% 
  data.frame() %>% 
  setNames(c("predictions", "outcome"))

XGB_fg <- XGB_pred_outcome %>% 
  filter(outcome == 1) %>%
  pull(predictions)

XGB_bg <- XGB_pred_outcome %>% 
  filter(outcome == 0) %>%
  pull(predictions)

XGB_pr <- PRROC::pr.curve(
  scores.class0 = XGB_fg, scores.class1 = XGB_bg, curve = TRUE)
############## END XGBoost Calculations ###################


#######################################################################################################
#######################################################################################################
#######################################################################################################

performanceTable <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1", "MCC", "AUC", "AUPRC", "TP", "FP", "FN", "TN"),
  `Naive Bayes` = c(
    con_NB$overall["Accuracy"],
    con_NB$byClass["Sensitivity"],
    con_NB$byClass["Specificity"],
    con_NB$byClass["Precision"],
    con_NB$byClass["F1"],
    MCC_NB,
    pROC::auc(as.numeric(Y_test), as.numeric(NB_Predictions)),
    NB_pr$auc.integral,
    con_NB$table[[1]],
    con_NB$table[[2]],
    con_NB$table[[3]],
    con_NB$table[[4]]
  ),
  `Logistic Regression` = c(
    con_Logistic$overall["Accuracy"],
    con_Logistic$byClass["Sensitivity"],
    con_Logistic$byClass["Specificity"],
    con_Logistic$byClass["Precision"],
    con_Logistic$byClass["F1"],
    MCC_Logistic,
    pROC::auc(as.numeric(Y_test), as.numeric(Logistic_Predictions)),
    Logistic_pr$auc.integral,
    con_Logistic$table[[1]],
    con_Logistic$table[[2]],
    con_Logistic$table[[3]],
    con_Logistic$table[[4]]
  ),
  `Random Forest` = c(
    con_RF$overall["Accuracy"],
    con_RF$byClass["Sensitivity"],
    con_RF$byClass["Specificity"],
    con_RF$byClass["Precision"],
    con_RF$byClass["F1"],
    MCC_RF,
    pROC::auc(as.numeric(Y_test), as.numeric(RF_Predictions)),
    RF_pr$auc.integral,
    con_RF$table[[1]],
    con_RF$table[[2]],
    con_RF$table[[3]],
    con_RF$table[[4]]
  ),
  `adaBoost` = c(
    con_adaBoost$overall["Accuracy"],
    con_adaBoost$byClass["Sensitivity"],
    con_adaBoost$byClass["Specificity"],
    con_adaBoost$byClass["Precision"],
    con_adaBoost$byClass["F1"],
    MCC_adaBoost,
    pROC::auc(as.numeric(Y_test), as.numeric(adaBoost_Predictions)),
    adaBoost_pr$auc.integral,
    con_adaBoost$table[[1]],
    con_adaBoost$table[[2]],
    con_adaBoost$table[[3]],
    con_adaBoost$table[[4]]
  ),
  `Classification Tree` = c(
    con_classTree$overall["Accuracy"],
    con_classTree$byClass["Sensitivity"],
    con_classTree$byClass["Specificity"],
    con_classTree$byClass["Precision"],
    con_classTree$byClass["F1"],
    MCC_classTree,
    pROC::auc(as.numeric(Y_test), as.numeric(classTreePredictions)),
    classTree_pr$auc.integral,
    con_classTree$table[[1]],
    con_classTree$table[[2]],
    con_classTree$table[[3]],
    con_classTree$table[[4]]
  ),
  # `Light GBM` = c(
  #   con_LGBM$overall["Accuracy"],
  #   con_LGBM$byClass["Sensitivity"],
  #   con_LGBM$byClass["Specificity"],
  #   con_LGBM$byClass["Precision"],
  #   con_LGBM$byClass["F1"],
  #   MCC_LGBM,
  #   pROC::auc(as.numeric(Y_test), as.numeric(lightGBM_Predictions)),
  #   LGBM_pr$auc.integral,
  #   con_LGBM$table[[1]],
  #   con_LGBM$table[[2]],
  #   con_LGBM$table[[3]],
  #   con_LGBM$table[[4]]
  # ),
  `XGBoost` = c(
    con_XGB$overall["Accuracy"],
    con_XGB$byClass["Sensitivity"],
    con_XGB$byClass["Specificity"],
    con_XGB$byClass["Precision"],
    con_XGB$byClass["F1"],
    MCC_XGB,
    pROC::auc(as.numeric(Y_test), as.numeric(XGBPredictions)),
    XGB_pr$auc.integral,
    con_XGB$table[[1]],
    con_XGB$table[[2]],
    con_XGB$table[[3]],
    con_XGB$table[[4]]
  )
)

#write.csv(performanceTable, "/root/capsule/results/performanceTable.csv", row.names = FALSE)

#######################################################################################################
#######################################################################################################
#######################################################################################################

performanceTable %>% 
  kable("latex", booktabs = T, digits = 2) %>% 
  kable_styling(position = "center", font_size = 7, latex_options = c("striped", "hold_position")) %>%
  row_spec(0, angle = 0) %>% 
  add_footnote(c("Note: The Logistic Regression and Random Forest model removes missing values from its final results and cannot be adequately \n compared with the other results. ", "MCC: Matthew's Correlation Coefficient \n AUC: Area Under the Curve \n AUPRC: Area Under the Precision Recall Curve \n TP: True Positive | FP: False Positive | FN: False Negative | TN: True Negative"), notation = "symbol")

#######################################################################################################
#######################################################################################################
#######################################################################################################

#######################################################################################################
#######################################################################################################
#######################################################################################################

####################### XGBoost Importance ############################
#xgb.imp <- xgb.importance(model = xgb.model)
#xgb.plot.importance(xgb.imp, top_n = 10)

xgbimp <- xgb.importance(colnames(X_train), model = xgb.model, data = X_train, label = Y_train)
xgbimp <- xgbimp %>%
  mutate(rank = dense_rank(desc(Gain)))

ggplot(data=xgbimp[which(xgbimp$rank <= 20),], aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat="identity", color = "black", fill = "darkblue") + 
  coord_flip() +
  labs(title = "XGBoost Feature Importance (Most Important)", x = "Features", y = "Information Gain") +
  theme_bluewhite() +
  theme(
    #axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 17.5),
    plot.title = element_text(hjust = 0.5, size = 22.5)
  ) 

#ggsave("C:/Users/Matt/Desktop/PhD Thesis/Chapter_3_a_1_SISubmission/paper/figures/xgbImportancePlots.pdf")


#######################################################################################################
#######################################################################################################
#######################################################################################################

########################## SHAP Plots ################################

shapLong <- shap.prep(xgb_model = xgb.model, X_train = X_train)

shapLong %>%
  ggplot() +
  coord_flip() +
  ggforce::geom_sina(aes(
    x = variable,
    y = value,
    color = stdfvalue),
    method = "counts", maxwidth = 1, size = 1, alpha = 0.8) +
  scale_color_gradient(
    low = "#FDE725FF",
    high = "darkblue",
    breaks = c(0, 1),
    labels = c("     Low", "     High"),
    guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
  geom_hline(yintercept = 0) +
  theme_bluewhite() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 17.5),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 17.5),
    plot.title = element_text(hjust = 0.5, size = 22.5)
  ) +
  labs(x = "", y = "Shapley Value (impact on model output)", color = "Variable Value") +
  ggtitle("SHapley Additive exPlanations")

#ggsave("C:/Users/Matt/Desktop/PhD Thesis/Chapter_3_a_1_SISubmission/paper/figures/shapleyValuesVariables.pdf")

#######################################################################################################
#######################################################################################################
#######################################################################################################

set.seed(671234)

shapValues <- shap.values(xgb_model = xgb.model, X_train = X_train)
shapForceData <- shap.prep.stack.data(shap_contrib = shapValues$shap_score, top_n = 4, n_groups = 1)

rawFeatureValues <- data.frame(X_train) %>%
  select(Age, daysInHospital, Admission.to.ICU, LD) %>% 
  setNames(c("rfv_Age", "rfv_daysInHospital", "rfv_Admission.to.ICU", "rfv_LD"))


shapForceDataWithRawFeatureValues <- shapForceData %>%
  select(Age, daysInHospital, Admission.to.ICU, LD, group, ID) %>% 
  bind_cols(rawFeatureValues) %>% 
  bind_cols(data.frame(Y_train) %>% 
              setNames(c("Outcome")))


# find observation from Shap Force plot

findObservationsForCaseStudies <- shapForceData %>% 
  bind_cols(data.frame(Y_train)) %>% 
  arrange(Survival_death) %>% 
  tibble() %>% 
  mutate(id_rank = row_number()) %>% 
  pivot_longer(cols = c(Age:rest_variables)) %>% 
  filter(!name == "BIAS" & !name == "pred_mod") %>% 
  add_count(id_rank, wt = value, name = "sum_value") %>% 
  arrange(Survival_death, sum_value)

# ShapCaseStudyIDs <- findObservationsForCaseStudies %>% 
#   group_by(Survival_death) %>% 
#   filter(row_number()==1 | row_number()==n()) %>%  # we want the first and last observation id for each outcome
#   pull(ID) # gives us four results in total

# for not and simplicity - take a random sample of 2 observations from each outcome
# ShapCaseStudyIDs <- findObservationsForCaseStudies %>%
#   group_by(Survival_death) %>%
#   sample_n(2) %>%
#   pull(ID)

# I also select just some interesting case studies
ShapCaseStudyIDs = c(35, 187, 49, 67)


shapForceDataWithRawFeatureValuesToPlot <- shapForceDataWithRawFeatureValues %>% 
  filter(ID %in% ShapCaseStudyIDs) %>% 
  pivot_longer(cols = -c(group, ID, Outcome)) %>% 
  tidyr::extract(col = name, into = c("type","variable"),
                 regex = "(rfv)?_?(.*)") %>% 
  mutate(type = ifelse(nchar(type) == 0, "value", type)) %>% 
  #mutate(type = replace_na(type, "value")) %>% 
  pivot_wider(id_cols = c(variable, Outcome, ID, group), values_from = value, names_from = type)


shapForceDataWithRawFeatureValuesToPlot %>% 
  ggplot(aes(x = as.factor(1), y = value, fill = variable)) +
  facet_grid(~ID) +
  geom_bar(stat = "identity", width = 0.70) + 
  geom_text(aes(label = round(value, 2), x = 0.525), position = position_stack(vjust = 0.5), color = "black") + 
  geom_text(aes(label = rfv, x = 1.475), position = position_stack(vjust = 0.5), color = "black") + 
  geom_text(aes(label = variable, x = 1), position = position_stack(vjust = 0.5), color = "white") + 
  labs(x = "") +
  scale_fill_viridis(option = "cividis", discrete = TRUE) +
  #scale_fill_brewer(palette = "Paired") +
  geom_hline(yintercept = 0) +
  geom_rect(data = subset(shapForceDataWithRawFeatureValuesToPlot, Outcome == 1), 
            aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), 
            fill="red", alpha = 0.05) +
  geom_rect(data = subset(shapForceDataWithRawFeatureValuesToPlot, Outcome == 0), 
            aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), 
            fill="blue", alpha = 0.05) +
  xlab("Patient") +
  ylab("Shapley Value") +
  ggtitle("Shapley value patient case study") +
  theme_bluewhite() +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title.x = element_text(size = 17.5),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 17.5),
    plot.title = element_text(hjust = 0.5, size = 22.5)
  )

#ggsave("C:/Users/Matt/Desktop/PhD Thesis/Chapter_3_a_1_SISubmission/paper/figures/shapCaseStudiesPlot.pdf")


################################################################################################

shapLong %>% 
  filter(variable == "daysInHospital" | variable == "Age") %>% 
  group_by(variable) %>% 
  mutate(
    rowID = row_number(),
  ) %>% 
  select(-stdfvalue, -mean_value) %>% 
  pivot_wider(names_from = variable, values_from = c(value, rfvalue)) %>% 
  bind_cols(as.numeric(Y_train)) %>% 
  rename(Outcome = `...7`) %>% 
  #setNames(c("ID", "rowID", "value_Age", "value_daysInHospital", "rfvalue_Age", "rfvalue_daysInHospital", "Outcome"))
  ggplot(aes(x = rfvalue_daysInHospital, y = value_daysInHospital,
             #fill = rfvalue_X...lymphocyte, 
             color = factor(Outcome)
             #alpha = rfvalue_X...lymphocyte, 
             #size = rfvalue_X...lymphocyte
  )) +
  geom_point(size = 2) +
  scale_color_viridis(option = "D", direction = -1, discrete = TRUE) +
  #scale_fill_viridis(option = "inferno")
  #scale_size(range = c(5, 0.1), guide = 'none') +
  guides(
    colour = guide_legend(
      title = "Outcome",
      override.aes = list(size = 5, shape = 19))) +
  labs(x = "Days in hospital", y = "Shapley Value (Days in Hospital)") +
  ggtitle("SHapley Additive exPlanations on variable interactions") +
  theme_bluewhite() +
  theme(legend.position = "right") +
  geom_vline(xintercept = 10.5) +
  theme(
    axis.title.x = element_text(size = 17.5),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 17.5),
    plot.title = element_text(hjust = 0.5, size = 22.5),
    legend.title = element_text(size=17.5),
    legend.text = element_text(size = 15)
  )


#ggsave("C:/Users/Matt/Desktop/PhD Thesis/Chapter_3_a_1_SISubmission/paper/figures/shapleyInteractionPlots.pdf")


#######################################################################################################
#######################################################################################################
#######################################################################################################

###########################################
library(DALEX)
library(ceterisParibus)

explainMultiMLDataSetNAOmit <- na.omit(MultiMLDataSet)
explainMultiMLDataSet <- MultiMLDataSet

#########################################
rf_explain <- explain(Random_Forest_Model, 
                      data = explainMultiMLDataSetNAOmit[, 2:ncol(explainMultiMLDataSetNAOmit)], y = explainMultiMLDataSetNAOmit$outcome)

# randomWhatIfExplainer <- explainMultiMLDataSetNAOmit %>% 
#   sample_n(1) %>% 
#   rownames_to_column("randomID") %>% 
#   pull(randomID)
randomWhatIfExplainer <- 147
new_obs <- explainMultiMLDataSetNAOmit[rownames(explainMultiMLDataSetNAOmit) == randomWhatIfExplainer, ]

wi_rf_model <- what_if(rf_explain, observation = new_obs,
                       selected_variables = c("Age", "daysInHospital", "LD", "Admission.to.ICU"))

#######################################################################################################
#######################################################################################################
#######################################################################################################

plot(wi_rf_model, split = "variables", color = "variables", quantiles = FALSE) +
  theme_bluewhite() +
  xlab("Variable value") +
  ylab("Prediction probability") +
  labs(title = "Ceteris paribus / What-if analysis",
       caption = paste("Patient with the following characteristics: \n", "Outcome: ", new_obs[, "Survival_death"], " | ",
                       "Age: ", new_obs[, "Age"], " | ",
                       "Days in hospital: ", new_obs[, "daysInHospital"], " | ",
                       "Addmission to ICU: ", new_obs[, "Admission.to.ICU"], " | ",
                       "LD: ", new_obs[, "LD"])) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title.x = element_text(size = 17.5),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 17.5),
    plot.title = element_text(hjust = 0.5, size = 22.5)
  )


ggsave("C:/Users/Matt/Desktop/PhD Thesis/Chapter_3_a_1_SISubmission/paper/figures/whatIfAnalysisPlot.pdf")

#######################################################################################################
#######################################################################################################
#######################################################################################################
