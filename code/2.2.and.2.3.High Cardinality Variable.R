# 2.2 High Cardinality Variable in Descriptive Stats

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
