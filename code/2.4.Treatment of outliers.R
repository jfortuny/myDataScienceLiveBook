# 2.4 Treatment of outliers
library(funModeling)
library(dplyr)
library(caret)

# Creating a sample dataset
set.seed(31415)
df_1=data.frame(var=round(10000*rbeta(1000,0.15,2.5)))

# Plotting
ggplot(df_1, aes(var, fill=var)) + geom_histogram(bins=20) +
  theme_light()
# Where to set the thresholds of extreme?
# Calculating the percentiles for the top 3% and top 1%
percentile_var=quantile(df_1$var, c(0.98, 0.99, 0.999), na.rm = T)
df_p=data.frame(value=percentile_var,
                percentile=c("a_98th", "b_99th", "c_99.9th"))
# Plotting the same distribution plus the percentiles
# Plotting the same distribution plus the percentiles
ggplot(df_1, aes(var)) + geom_histogram(bins = 20) +
  geom_vline(data = df_p, aes(xintercept = value,
                              colour = percentile), show.legend = TRUE,
             linetype = "dashed") + theme_light()

# 2.4.6 Step 1: How to detect outliers
# 2.4.6.0.1 Bottom and top values method
quantile(heart_disease$age, probs = c(0.01, 0.99),
         na.rm = T)
# All values for those aged less than 35 or more than 71 years will
# be considered outliers.

# 2.4.6.0.2 Tukey's method
# The bottom threshold is: Q1 − 3*IQR. All below are considered as outliers.
# The top threshold is: Q1 + 3*IQR. All above are considered as outliers.
tukey_outlier(heart_disease$age)
# all below nine and all above 100 will be considered as outliers.

# 2.4.6.0.3 Hampel's method
# The bottom threshold is: median_value − 3*mad_value.
# 3 can be changed via parameter k_mad_value; larger values expand the boundaries.
# The top threshold is: median_value + 3*mad_value.
hampel_outlier(heart_disease$age)
# All below 29.31 and all above 82.68 will be considered as outliers.

# 2.4.7 Step 2: What to do with the outliers?
