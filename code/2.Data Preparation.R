# 2 Data Preparation
# This book suggests using binary variables as numeric when 0 is FALSE and 1 is TRUE.

# 2.1.4 Converting categorical variables into numerical
library(caret) # contains dummyVars function
library(dplyr) # data munging library
library(funModeling) # df_status function

# Checking categorical variables
status=df_status(heart_disease, print_results = F)
filter(status,  type %in% c("factor", "character")) %>% select(variable)

# It converts all categorical variables (factor and character) into numerical variables
# It skips the original variable, so no need to remove it after the conversion, the data is ready to use.
dmy = dummyVars(" ~ .", data = heart_disease)
heart_disease_2 = data.frame(predict(dmy, newdata = heart_disease))
# Checking the new numerical data set:
colnames(heart_disease_2)
df_status(heart_disease_2)

# 2.1.5.3 Be aware when converting categorical into numerical variables
# creating data -toy- sample
df_pc=data.frame(visits=c(10, 59, 27, 33),
                 postal_code=c("AA1", "BA5", "CG3", "HJ1"),
                 transformation_1=c(1,2,3,4),
                 transformation_2=c(1, 4, 2, 3 ))

# printing table
knitr::kable(df_pc)
# plotting
library(gridExtra)

# transformation 1
plot_1=ggplot(df_pc, aes(x=transformation_1, y=visits, label=postal_code)) +
  geom_point(aes(color=postal_code), size=4) +
  geom_smooth(method=loess, group=1, se=FALSE, color="lightblue", linetype="dashed") +
  theme_minimal()  + theme(legend.position="none") +
  geom_label(aes(fill = factor(postal_code)), colour = "white", fontface = "bold")


# transformation 2
plot_2=ggplot(df_pc, aes(x=transformation_2, y=visits, label=postal_code)) +
  geom_point(aes(color=postal_code), size=4) +
  geom_smooth(method=lm, group=1, se=FALSE, color="lightblue", linetype="dashed") +
  theme_minimal()  +
  theme(legend.position="none") +
  geom_label(aes(fill = factor(postal_code)), colour = "white", fontface = "bold")

# arranging plots side-by-side
grid.arrange(plot_1, plot_2, ncol=2)

# 2.1.6 Discretizing numerical variables
data_stunting=read.csv(file = "https://goo.gl/hFEUfN",
                       header = T,
                       stringsAsFactors = F)

# renaming the metric
data_stunting=
  dplyr::rename(
    data_stunting,
    share_stunted_child="WHO.â....Share.of.stunted.children.under.5."
  )

# doing the grouping mentioned before
d_stunt_grp = group_by(data_stunting, Entity) %>%
  filter(Year == max(Year)) %>%
  dplyr::summarise(share_stunted_child=
                     max(share_stunted_child)
  )

# 2.1.6.2 Equal range
# Creating equal range variable, add `dig.lab=9`
# parameter to deactivate scientific notation as with
# the `cut` function.
d_stunt_grp$share_stunted_child_eq_range=
  cut_interval(d_stunt_grp$share_stunted_child, n = 4)
# The ‘describe’ function from Hmiscpackage is
# extremely useful to profile data
describe(d_stunt_grp$share_stunted_child_eq_range)
# Plotting the variable
p2=ggplot(d_stunt_grp,
          aes(share_stunted_child_eq_range)
) +
  geom_bar(fill="#009E73") +
  theme_bw()
p2

# 2.1.6.3 Equal frequency
d_stunt_grp$stunt_child_ef=
  equal_freq(var = d_stunt_grp$share_stunted_child,
             n_bins = 4
  )

# profiling variable
describe(d_stunt_grp$stunt_child_ef)
p3=ggplot(d_stunt_grp, aes(stunt_child_ef)) +
  geom_bar(fill="#CC79A7") + theme_bw()
p3

# 2.1.6.4 Custom bins
# parameter dig.lab "disable" scientific notation
d_stunt_grp$share_stunted_child_custom=
  cut(d_stunt_grp$share_stunted_child,
      breaks = c(0, 2, 9.4, 29, 100)
  )

describe(d_stunt_grp$share_stunted_child_custom)
p4=ggplot(d_stunt_grp, aes(share_stunted_child_custom)) +
  geom_bar(fill="#0072B2") +
  theme_bw()
p4

# 2.1.8 Automatic data frame discretization
df_status(heart_disease, print_results = F) %>%
  select(variable, type, unique, q_na) %>%
  arrange(type)

# creating a copy to keep original data clean
heart_disease_2=heart_disease

# Introducing some missing values in the first 30 rows of the oldpeak variable
heart_disease_2$oldpeak[1:30]=NA

# Step 1) Getting the bin thresholds for each input variable:
d_bins=discretize_get_bins(data=heart_disease_2,
                           input=c("max_heart_rate", "oldpeak"),
                           n_bins=5)
# Checking `d_bins` object:
d_bins

# Step 2) Applying the thresholds for each variable:
# Now it can be applied on the same data frame or in
# a new one (for example, in a predictive model that
# changes data over time)
heart_disease_discretized =
  discretize_df(data=heart_disease_2,
                data_bins=d_bins,
                stringsAsFactors=T)
freq(heart_disease_discretized %>%
       select(max_heart_rate,oldpeak),
     plot = F)
p5=ggplot(heart_disease_discretized,
          aes(max_heart_rate)) +
  geom_bar(fill="#0072B2") +
  theme_bw() +
  theme(axis.text.x =
          element_text(angle = 45, vjust = 1, hjust=1)
  )

p6=ggplot(heart_disease_discretized,
          aes(oldpeak)) +
  geom_bar(fill="#CC79A7") +
  theme_bw() +
  theme(axis.text.x =
          element_text(angle = 45, vjust = 1, hjust=1)
  )

gridExtra::grid.arrange(p5, p6, ncol=2)
