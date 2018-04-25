# 3 Selecting Best Variables

# 3.3.1 Going deeper into variable ranking
library(caret)
library(funModeling)
library(dplyr)

# Excluding all NA rows from the data, in this case,
# NAs are not the main issue to solve, so we'll skip the 6 cases
# which have NA (or missing values).
heart_disease=na.omit(heart_disease)

# Setting a 4-fold cross-validation
fitControl = trainControl(method = "cv",
                          number = 4,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

# Creating the random forest model, finding the best tuning parameter set
set.seed(999)
fit_rf = train(x=select(heart_disease, -has_heart_disease, -heart_disease_severity),
               y = heart_disease$has_heart_disease,
               method = "rf",
               trControl = fitControl,
               verbose = FALSE,
               metric = "ROC")

# Creating the gradient boosting machine model, finding the best tuning parameter set
fit_gbm = train(x=select(heart_disease, -has_heart_disease, -heart_disease_severity),
                y = heart_disease$has_heart_disease,
                method = "gbm",
                trControl = fitControl,
                verbose = FALSE,
                metric = "ROC")

# Here we manipulate to show a nice the table described before
var_imp_rf=data.frame(varImp(fit_rf, scale=T)["importance"]) %>%
  dplyr::mutate(variable=rownames(.)) %>% dplyr::rename(importance_rf=Overall) %>%
  dplyr::arrange(-importance_rf) %>%
  dplyr::mutate(rank_rf=seq(1:nrow(.)))

var_imp_gbm=as.data.frame(varImp(fit_gbm, scale=T)["importance"])  %>%
  dplyr::mutate(variable=rownames(.)) %>% dplyr::rename(importance_gbm=Overall) %>%
  dplyr::arrange(-importance_gbm) %>%
  dplyr::mutate(rank_gbm=seq(1:nrow(.)))

final_res=merge(var_imp_rf, var_imp_gbm, by="variable")

final_res$rank_diff=final_res$rank_rf-final_res$rank_gbm

# Printing the results!
final_res

# 3.7.1 Example in R: Variables working in groups
# setting cross-validation 4-fold
fitControl =
  trainControl(method = "cv",
               number = 4,
               classProbs = TRUE,
               summaryFunction = twoClassSummary
  )

create_model<-function(input_variables)
{
  # create gradient boosting machine model
  # based on input variables
  fit_model = train(x=select(heart_disease,
                             one_of(input_variables)
  ),
  y = heart_disease$has_heart_disease,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE,
  metric = "ROC")
  # returning the ROC as the performance metric
  max_roc_value=max(fit_model$results$ROC)
  return(max_roc_value)
}
roc_1=create_model("max_heart_rate")
roc_2=create_model("chest_pain")
roc_3=create_model(c("max_heart_rate", "chest_pain"))
avg_improvement=round(100*(((roc_3-roc_1)/roc_1)+
                             ((roc_3-roc_2)/roc_2))/2,
                      2)
avg_improvement_text=sprintf("Average improvement: %s%%",
                             avg_improvement)

results =
  sprintf("ROC model based on 'max_heart_rate': %s.;
          based on 'chest_pain': %s; and based on both: %s",
          round(roc_1,2),
          round(roc_2,2),
          round(roc_3, 2)
  )
# printing the results!
cat(c(results, avg_improvement_text), sep="\n\n")

# 3.7.4 Rank best features using information theory
variable_importance =
  var_rank_info(heart_disease, "has_heart_disease")

# Printing results
variable_importance
# Plotting
ggplot(variable_importance,
       aes(x = reorder(var, gr),
           y = gr, fill = var)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("Variable Importance
       (based on Information Gain)"
  ) +
  guides(fill = FALSE)
