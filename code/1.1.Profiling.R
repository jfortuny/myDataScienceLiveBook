# 1.1 Profiling
library(funModeling)
library(dplyr)
data("heart_disease")
str(heart_disease)

# 1.1.1.1 Checking missing values, zeros, data type, and unique values
df_status(heart_disease)
# 1.1.1.3 Filtering unwanted cases
my_data_status<-df_status(heart_disease, print_results = F)
# Removing variables with a high number of zeros
vars_to_remove<-filter(my_data_status, p_zeros > 60)  %>% .$variable
vars_to_remove
# Keeping all columns except the ones present in 'vars_to_remove' vector
heart_disease_2<-select(heart_disease, -one_of(vars_to_remove))

# 1.1.2 Profiling categorical variables
freq(data=heart_disease, input = c('thal','chest_pain'))
# All categorical variables
freq(data=heart_disease)

# 1.1.2.1 Introducing the describe function
# Just keeping two variables to use in this example
heart_disease_3<-select(heart_disease, thal, chest_pain)
# Profiling the data!
describe(heart_disease_3)

# 1.1.3 Profiling numerical variables
# Loading data from the book repository without altering the format
data_world<-read.csv(file = "https://goo.gl/2TrDgN", header = T, stringsAsFactors = F, na.strings = "..")
freq(data=data_world)
# Excluding missing values in Series.Code. The data downloaded from the web page contains four lines
# with "free-text" at the bottom of the file.
data_world<-filter(data_world, Series.Code!="")

# The magical function that keeps the newest values for each metric. If you're not familiar with R, then skip it.
max_ix<-function(d)
{
  ix=which(!is.na(d))
  res=ifelse(length(ix)==0, NA, d[max(ix)])
  return(res)
}

data_world$newest_value<-apply(data_world[,5:ncol(data_world)], 1, FUN=max_ix)
head(data_world, 5)
names(data_world)
names<-unique(select(data_world, c("ï..Series.Name", "Series.Code")))
head(names,5)
colnames(names) <- c("Series.Name", "Series.Code")
# Rename a few Series.Code values for readability
df_conv_world<-data.frame(
  new_name=c("urban_poverty_headcount",
             "rural_poverty_headcount",
             "gini_index",
             "pop_living_slums",
             "poverty_headcount_1.9"),
  Series.Code=c("SI.POV.URHC",
                "SI.POV.RUHC",
                "SI.POV.GINI",
                "EN.POP.SLUM.UR.ZS",
                "SI.POV.DDAY"),
  stringsAsFactors = F)
# adding the new indicator value
data_world_2 <- left_join(data_world,
                         df_conv_world,
                         by="Series.Code",
                         all.x=T)
data_world_2 <- mutate(data_world_2,
                       Series.Code_2 = ifelse(!is.na(new_name),
                                              as.character(data_world_2$new_name),
                                              data_world_2$Series.Code)
                       )
# Convert from long to wide format based on the new column Series.Code_2 (one column for each value)
data_world_wide <- reshape2::dcast(data_world_2, Country.Name  ~ Series.Code_2, value.var = "newest_value")
head(data_world_wide)

# 1.1.3.3 Part 2: Doing the numerical profiling in R
vars_to_profile <- c("gini_index", "poverty_headcount_1.9")
data_subset <- select(data_world_wide, one_of(vars_to_profile))
describe(data_subset)
# Full numerical profiling in one function automatically excludes non-numerical variables
profiling_num(data_world_wide)

# 1.1.3.3.2 Profiling numerical variables by plotting
plot_num(data_world_wide)


