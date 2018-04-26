# 2.2 High Cardinality Variable in Descriptive Stats
# Loading funModeling >=1.6 which contains functions to deal with this.
library(funModeling)
library(dplyr)
library(caret)

# 2.2.2 High Cardinality in Descriptive Statistics
# plotting first 10 rows
head(data_country, 10)
# exploring data, displaying only first 10 rows
head(freq(data_country, "country"), 10)
# exploring data
freq(data_country, "has_flu")

# 'freq' function, from 'funModeling' package, retrieves the cumulative_percentage
# that will help to do the cut.
country_freq=freq(data_country, 'country', plot = F)

# Since 'country_freq' is an ordered table by frequency, let's inspect the
# first 10 rows with the most share.
country_freq[1:10,]

data_country$country_2=ifelse(data_country$country %in% country_freq[1:10,'country'], data_country$country, 'other')
freq(data_country, 'country_2')

# 2.3 High Cardinality Variable in Predictive Modeling
# plotting first 10 rows
head(data_country, 10)
# exploring data, displaying only first 10 rows
head(freq(data_country, "country"), 10)
# exploring data
freq(data_country, "has_flu")

# 2.3.3 The case
# `categ_analysis` is available in "funModeling" >= v1.6, please install it
# before using it.
country_profiling=categ_analysis(data=data_country,
                                 input="country",
                                 target = "has_flu")
# Printing first 15 rows (countries) out of 70.
head(country_profiling, 15)

# 2.3.4 Analysis for Predictive Modeling
# What are the countries that maximize the likelihood of finding people with flu?
# Ordering country_profiling by mean_target and then take the first 6 countries
arrange(country_profiling, -mean_target) %>%  head(.)

# 2.3.4.1 Case 1: Reducing by re-categorizing less representative values
# rename the countries that have less than 1% of presence in data to others
countries_high_rep=filter(country_profiling, perc_rows>0.01) %>% .$country
# If not in countries_high_rep then assign `other` category
data_country$country_new=ifelse(data_country$country %in% countries_high_rep,
                                data_country$country,
                                "other")
# Checking again the likelihood:
country_profiling_new=categ_analysis(data=data_country, input="country_new", target = "has_flu")
country_profiling_new
# Watch out about applying this technique blindly. Sometimes in a highly
# unbalanced target prediction -e.g. anomaly detection- the abnormal behavior
# is present in less than 1% of cases.

# 2.3.4.2 Case 2: Reducing by automatic grouping
# Reducing the cardinality
country_groups=auto_grouping(data = data_country,
                             input = "country",
                             target="has_flu", # Can only be Categorical
                             n_groups=9,
                             seed = 999)
country_groups$df_equivalence
country_groups$recateg_results

# First we need to add the new category column to the original dataset.
data_country_2=data_country %>%
  inner_join(country_groups$df_equivalence,
             by="country")
# group_4, group_5 and group_5 will be low_likelihood
# group_7 and group_3 will be the low_target_share
data_country_2$country_rec=
  ifelse(data_country_2$country_rec %in%
           c("group_4", "group_5", "group_9"),
         "low_likelihood",
         data_country_2$country_rec
  )

data_country_2$country_rec=
  ifelse(data_country_2$country_rec %in%
           c("group_7", "group_3"),
         "low_target_share",
         data_country_2$country_rec
  )
# Checking the final grouping (country_rec variable)
categ_analysis(data=data_country_2, input="country_rec", target = "has_flu")


# 2.3.6 Do predictive models handle high cardinality? Part 1
# Building the first model, without reducing cardinality.
fitControl <- trainControl(method = "cv",
                           number = 4,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
fit_gbm_1 <- train(has_flu ~ country,
                   data = data_country_2,
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE,
                   metric = "ROC")
# Getting best ROC value
roc=round(max(fit_gbm_1$results$ROC),2)
roc
# Building the second model, based on the country_rec variable
fit_gbm_2 <- train(has_flu ~ country_rec,
                   data = data_country_2,
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE,
                   metric = "ROC")

# Getting new best ROC value
new_roc=round(max(fit_gbm_2$results$ROC),2)
new_roc
