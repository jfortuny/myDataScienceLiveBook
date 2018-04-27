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
# 2.4.7.1 Scenario 1: Prepare outliers for data profiling
head(df_1)
profiling_num(df_1$var)
# 2.4.7.1.1 Using prep_outliers for data profiling
# Using Tukey’s method
df_1$var_tukey = prep_outliers(df_1$var,
                               type = "set_na", method = "tukey")
# before
df_status(df_1$var, print_results = F) %>%
  select(variable, q_na, p_na)
df_status(df_1$var_tukey, print_results = F) %>%
  select(variable, q_na, p_na)
profiling_num(df_1, print_results = F) %>%
  select(variable, mean, std_dev, variation_coef,
         kurtosis, range_98)
# Hampel’s method
df_1$var_hampel = prep_outliers(df_1$var,
                                type = "set_na", method = "hampel")
df_status(df_1, print_results = F) %>% select(variable,
                                              q_na, p_na)
# Bottom and top X% method
df_1$var_top2 = prep_outliers(df_1$var, type = "set_na",
                              method = "bottom_top", top_percent = 0.02)
df_status(df_1, print_results = F) %>% select(variable,
                                              q_na, p_na)
prof_num = profiling_num(df_1, print_results = F) %>%
  select(variable, mean, std_dev, variation_coef,
         kurtosis, range_98)
prof_num
head(df_1)
# Plotting
df_1_m = reshape2::melt(df_1)
head(df_1_m)
plotar(df_1_m, target = "variable", input = "value",
       plot_type = "boxplot")

# 2.4.7.3 Imputing outliers for predictive modeling
# Creating data frame with outliers
# deactivating scientific notation
options(scipen = 999)
# setting the seed to have a reproducible example
set.seed(10)
# creating the variables
df_2 = data.frame(var1 = rchisq(1000, df = 1),
                  var2 = rnorm(1000))
# forcing outliers
df_2 = rbind(df_2, 135, rep(400, 30), 245, 300, 303,
             200)