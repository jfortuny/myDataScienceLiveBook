# 1.2 Correlation and Relationship

# Loading needed libraries
library(funModeling) # contains heart_disease data
library(minerva) # contains MIC statistic
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra) # allow us to plot two plots in a row
options(scipen=999) # disable scientific notation

# 1.2.2 Linear correlation
correlation_table(data = heart_disease, target = "has_heart_disease")
# R statistic is highly influenced by outliers and non-linear relationships.

# 1.2.2.1 Correlation on Anscombe’s Quartet
anscombe_data <- read.delim(file="https://goo.gl/mVLz5L", header = T)
# calculating the correlation (R squared, or R2) for
#every pair, every value is the same: 0.86.
cor_1 <- cor(anscombe_data$x1, anscombe_data$y1)
cor_2 <- cor(anscombe_data$x2, anscombe_data$y2)
cor_3 <- cor(anscombe_data$x3, anscombe_data$y3)
cor_4 <- cor(anscombe_data$x4, anscombe_data$y4)
# Correlation is the same; let's plot the variables with a new function
# defining the function
plot_anscombe <- function(x, y, value, type)
{
  # 'anscombe_data' is a global variable, this is
  # a bad programming practice ;)
  p=ggplot(anscombe_data, aes_string(x,y))  +
    geom_smooth(method='lm', fill=NA) +
    geom_point(aes(colour=factor(1),
                   fill = factor(1)),
               shape=21, size = 2
    ) +
    ylim(2, 13) +
    xlim(4, 19) +
    theme_minimal() +
    theme(legend.position="none") +
    annotate("text",
             x = 12,
             y =4.5,
             label =
               sprintf("%s: %s",
                       type,
                       round(value,2)
               )
    )

  return(p)
}

# plotting in a 2x2 grid
grid.arrange(plot_anscombe("x1", "y1", cor_1, "R2"),
             plot_anscombe("x2", "y2", cor_2, "R2"),
             plot_anscombe("x3", "y3", cor_3, "R2"),
             plot_anscombe("x4", "y4", cor_4, "R2"),
             ncol=2,
             nrow=2)

# 1.2.3 Correlation based on Information Theory
# 1.2.3.1 An example in R: A perfect relationship
x<-seq(0, 20, length.out=500)
df_exp<-data.frame(x=x, y=dexp(x, rate=0.65))
ggplot(df_exp, aes(x=x, y=y)) + geom_line(color='steelblue') + theme_minimal()

# position [1,2] contains the correlation of both variables,
# excluding the correlation measure of each variable against itself.
# Calculating linear correlation
res_cor_R2<-cor(df_exp)[1,2]^2
sprintf("R2: %s", round(res_cor_R2,2))
# now computing the MIC metric
res_mine<-mine(df_exp)
sprintf("MIC: %s", res_mine$MIC[1,2])
# MIC value goes from 0 to 1. Being 0 implies no correlation and 1 highest correlation.

# 1.2.4 Adding noise
df_exp$y_noise_1<-jitter(df_exp$y, factor = 1000, amount = NULL)
ggplot(df_exp, aes(x=x, y=y_noise_1)) +
  geom_line(color='steelblue') + theme_minimal()
# calculating R squared
res_R2<-cor(df_exp)^2
res_R2
# Calculating mine
res_mine_2<-mine(df_exp)
# Printing MIC
res_mine_2$MIC

# 1.2.5 Measuring non-linearity (MIC-R2)
# MIC r2: non-linearity metric
round(res_mine_2$MICR2, 3)
# calculating MIC r2 manually
round(res_mine_2$MIC-res_R2, 3)

# creating data example
df_example<-data.frame(x=df_exp$x,
                       y_exp=df_exp$y,
                       y_linear=3*df_exp$x+2)
# getting mine metrics
res_mine_3<-mine(df_example)
# generating labels to print the results
results_linear <-
  sprintf("MIC: %s \n MIC-R2 (non-linearity): %s",
          res_mine_3$MIC[1,3],
          round(res_mine_3$MICR2[1,3],2)
  )
results_exp <-
  sprintf("MIC: %s \n MIC-R2 (non-linearity): %s",
          res_mine_3$MIC[1,2],
          round(res_mine_3$MICR2[1,2],4)
  )
# Plotting results
# Creating plot exponential variable
p_exp<-ggplot(df_example, aes(x=x, y=y_exp)) +
  geom_line(color='steelblue') +
  annotate("text", x = 11, y =0.4, label = results_exp) +
  theme_minimal()
# Creating plot linear variable
p_linear<-ggplot(df_example, aes(x=x, y=y_linear)) +
  geom_line(color='steelblue') +
  annotate("text", x = 8, y = 55,
           label = results_linear) +
  theme_minimal()
grid.arrange(p_exp,p_linear,ncol=2)

# 1.2.6 Measuring information on Anscombe Quartet
# calculating the MIC for every pair
mic_1<-mine(anscombe_data$x1, anscombe_data$y1, alpha=0.8)$MIC
mic_2<-mine(anscombe_data$x2, anscombe_data$y2, alpha=0.8)$MIC
mic_3<-mine(anscombe_data$x3, anscombe_data$y3, alpha=0.8)$MIC
mic_4<-mine(anscombe_data$x4, anscombe_data$y4, alpha=0.8)$MIC
# plotting MIC in a 2x2 grid
grid.arrange(plot_anscombe("x1", "y1", mic_1, "MIC"),
             plot_anscombe("x2", "y2", mic_2,"MIC"),
             plot_anscombe("x3", "y3", mic_3,"MIC"),
             plot_anscombe("x4", "y4", mic_4,"MIC"),
             ncol=2,
             nrow=2)
# Calculating the MIC for every pair, note the "MIC-R2" object has the hyphen when the input are two vectors, unlike when it takes a data frame which is "MICR2".
mic_r2_1<-mine(anscombe_data$x1, anscombe_data$y1, alpha = 0.8)$`MIC-R2`
mic_r2_2<-mine(anscombe_data$x2, anscombe_data$y2, alpha = 0.8)$`MIC-R2`
mic_r2_3<-mine(anscombe_data$x3, anscombe_data$y3, alpha = 0.8)$`MIC-R2`
mic_r2_4<-mine(anscombe_data$x4, anscombe_data$y4, alpha = 0.8)$`MIC-R2`
# Ordering according mic_r2
df_mic_r2<-data.frame(pair=c(1,2,3,4),
                      mic_r2=c(mic_r2_1,mic_r2_2,mic_r2_3,mic_r2_4)) %>% arrange(-mic_r2)
df_mic_r2

# 1.2.7 Measuring non-monotonicity: MAS measure
# creating sample data (simulating time series)
time_x<-sort(runif(n=1000, min=0, max=1))
y_1<-4*(time_x-0.5)^2
y_2<-4*(time_x-0.5)^3
# Calculating MAS for both series
mas_y1=round(mine(time_x,y_1)$MAS,2)
mas_y2=mine(time_x,y_2)$MAS
# Putting all together
df_mono=data.frame(time_x=time_x, y_1=y_1, y_2=y_2)
# Plotting
label_p_y_1 =
  sprintf("MAS=%s (goes down \n and up => not-monotonic)",
          mas_y1)
p_y_1=ggplot(df_mono, aes(x=time_x, y=y_1)) +
  geom_line(color='steelblue') +
  theme_minimal()  +
  annotate("text", x = 0.45, y =0.75,
           label = label_p_y_1)
label_p_y_2=
  sprintf("MAS=%s (goes up => monotonic)", mas_y2)
p_y_2=ggplot(df_mono, aes(x=time_x, y=y_2)) +
  geom_line(color='steelblue') +
  theme_minimal() +
  annotate("text", x = 0.43, y =0.35,
           label = label_p_y_2)
grid.arrange(p_y_1,p_y_2,ncol=2)

# 1.2.7.1 A more real example: Time Series
# reading data
df_time_series =
  read.delim(file="https://goo.gl/QDUjfd")
# converting to long format so they can be plotted
df_time_series_long=melt(df_time_series, id="time")
# Plotting
plot_time_series =
  ggplot(data=df_time_series_long,
         aes(x=time, y=value, colour=variable)) +
  geom_line() +
  theme_minimal() +
  scale_color_brewer(palette="Set2")
plot_time_series
# Calculating and printing MAS values for time series data
mine_ts=mine(df_time_series)
mine_ts$MAS

# 1.2.9 Correlation on categorical variables
library(caret)

# selecting just a few variables
heart_disease_2 =
  select(heart_disease, max_heart_rate, oldpeak,
         thal, chest_pain,exer_angina, has_heart_disease)

# this conversion from categorical to a numeric is merely
# to have a cleaner plot
heart_disease_2$has_heart_disease=
  ifelse(heart_disease_2$has_heart_disease=="yes", 1, 0)

# it converts all categorical variables (factor and
# character for R) into numerical variables.
# skipping the original so the data is ready to use
dmy = dummyVars(" ~ .", data = heart_disease_2)

heart_disease_3 =
  data.frame(predict(dmy, newdata = heart_disease_2))
# Important: If you recieve this message
# `Error: Missing values present in input variable 'x'.
# Consider using use = 'pairwise.complete.obs'.`
# is because data has missing values.
# Please don't omit NA without an impact analysis first,
# in this case it is not important.
heart_disease_4=na.omit(heart_disease_3)

# compute the mic!
mine_res_hd=mine(heart_disease_4)
mine_res_hd$MIC[1:5,1:5]
# library wto plot that matrix
library(corrplot)
# to use the color pallete brewer.pal
library(RColorBrewer)

# hack to visualize the maximum value of the
# scale excluding the diagonal (variable against itself)
diag(mine_res_hd$MIC)=0

# Correlation plot with circles.
corrplot(mine_res_hd$MIC,
         method="circle",
         col=brewer.pal(n=10, name="PuOr"),
         # only display upper diagonal
         type="lower",
         #label color, size and rotation
         tl.col="red",
         tl.cex = 0.9,
         tl.srt=90,
         # dont print diagonal (var against itself)
         diag=FALSE,
         # accept a any matrix, mic in this case
         #(not a correlation element)
         is.corr = F

)
# Correlation plot with color and correlation MIC
corrplot(mine_res_hd$MIC,
         method="color",
         type="lower",
         number.cex=0.7,
         # Add coefficient of correlation
         addCoef.col = "black",
         tl.col="red",
         tl.srt=90,
         tl.cex = 0.9,
         diag=FALSE,
         is.corr = F
)
cross_plot(heart_disease,
           input = "chest_pain",
           target = "has_heart_disease",
           plot_type = "percentual")

# 1.2.10 Correlation analysis based on information theory
# Getting the index of the variable to
# predict: has_heart_disease
target="has_heart_disease"
index_target=grep(target, colnames(heart_disease_4))
# master takes the index column number to calculate all
# the correlations
mic_predictive=mine(heart_disease_4,
                    master = index_target)$MIC
# creating the data frame containing the results,
# ordering descently by its correlation and excluding
# the correlation of target vs itself
df_predictive =
  data.frame(variable=rownames(mic_predictive),
             mic=mic_predictive[,1],
             stringsAsFactors = F) %>%
  arrange(-mic) %>%
  filter(variable!=target)
# creating a colorful plot showing importance variable
# based on MIC measure
ggplot(df_predictive,
       aes(x=reorder(variable, mic),y=mic, fill=variable)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("Variable Importance (based on MIC)") +
  guides(fill=FALSE)
