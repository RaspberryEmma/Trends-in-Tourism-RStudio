# Math6147 - Data Analytics
# R Coursework - R Script
# 15/10/21
# Student ID: 2940 4045

# Read Me -----
# 
# Welcome to my R file!
# This code will only require the tidyverse library in order to run, there are no other external dependencies.
# To find the data, please edit the "file_name" variable only
# 
# End of Read Me

# Setup -----

library(tidyverse)

rm(list = ls()) # reset environment
file_name = ".../R_CW_2021.csv"
travel_data = read.csv( file_name ) # load data
print(head(travel_data))

# suppresses warnings incurred from the 3 box-plots produced under question 1 when restricting axis
# not strictly necessary, can be removed without affecting functionality
options(warn = -1)


# Question 1 - How characteristics of travel vary between countries -----


# comparing visits between countries -----
a <- ggplot(data = travel_data, mapping = aes(x = country, y = visits)) +
    ggtitle("Comparing various EU Countries by Number of Visits") +
    labs(x = "Country", y = "Number of Visits") +
    coord_flip()
a + geom_col()
a + geom_boxplot() + ylim(0, 10000) # adjust axis for better fit


# comparing nights between countries -----
b <- ggplot(data = travel_data, mapping = aes(x = country, y = nights)) +
    ggtitle("Comparing Countries by Number of Nights Spent While On Visit") +
    labs(x = "Country", y = "Nights Spent") +
    coord_flip()
b + geom_col()
b + geom_boxplot() + ylim(0, 100000) # adjust axis for better fit


# comparing spend between countries -----
c <- ggplot(data = travel_data, mapping = aes(x = country, y = spend)) +
    ggtitle("Comparing Countries by Total Expenditure Made Abroad") +
    labs(x = "Country", y = "Money Spent (£)") +
    coord_flip()
c + geom_col()
c + geom_boxplot() + ylim(0, 10000000) # adjust axis for better fit


# comparing sex between countries -----
travel_data %>%
 mutate(sex = as.factor(sex)) %>%
 ggplot(mapping = aes(x = country, fill = sex)) +
 geom_bar(position = "dodge") +
 ggtitle("Comparing Countries by Sex of Visitors") +
 labs(x = "Country", y = "Frequency in Dataset") +
 coord_flip()


# comparing age between countries -----
travel_data %>%
 mutate(age = as.factor(age)) %>%
 ggplot(mapping = aes(x = country, fill = age)) +
 geom_bar(position = "dodge") +
 ggtitle("Comparing Countries by Age of Visitors") +
 labs(x = "Country", y = "Frequency in Dataset") +
 coord_flip()


# comparing mode between countries -----
travel_data %>%
 mutate(age = as.factor(mode)) %>%
 ggplot(mapping = aes(x = country, fill = mode)) +
 geom_bar(position = "dodge") +
 ggtitle("Comparing Countries by Mode of Transport") +
 labs(x = "Country", y = "Frequency in Dataset") +
 coord_flip()


# Question 2 - Potential relationships between visits, spends and nights -----

# plots comparing each of our three variables in turn -----

v_n <- ggplot(data = travel_data, mapping = aes(x = visits, y = nights)) +
       ggtitle("Comparing Visits and Nights") +
       labs(x = "Visits", y = "Nights")

v_s <- ggplot(data = travel_data, mapping = aes(x = visits, y = spend)) +
       ggtitle("Comparing Visits and Total Spend") +
       labs(x = "Visits", y = "Spend")

n_s <- ggplot(data = travel_data, mapping = aes(x = nights, y = spend)) +
       ggtitle("Comparing Nights and Total Spend") +
       labs(x = "Nights", y = "Spend")

v_n + geom_point(mapping = aes(colour = spend))  + labs(colour = "Total Money Spent (£)")
v_s + geom_point(mapping = aes(colour = nights)) + labs(colour = "Number of Nights Spent")
n_s + geom_point(mapping = aes(colour = visits)) + labs(colour = "Number of Visits made")

# correlations between our three variables -----

v_n_corr <- cor.test(travel_data$visits, travel_data$nights, method = "pearson")
print(v_n_corr)

v_s_corr <- cor.test(travel_data$visits, travel_data$spend, method = "pearson")
print(v_s_corr)

n_s_corr <- cor.test(travel_data$nights, travel_data$spend, method = "pearson")
print(n_s_corr)


# Question 3 - Effect of age and sex on visits, spends and nights -----

q3_data <- travel_data

# effect of age and sex on visits -----
q3_data %>%
  mutate(sex = as.factor(sex)) %>%
  ggplot(mapping = aes(x = age, y = visits, fill = sex)) +
  geom_col(position = "dodge") +
  ggtitle("Effect of Age and Sex on Visits") +
  labs(x = "Age", y = "Visits")


# effect of age and sex on spend -----
q3_data %>%
  mutate(sex = as.factor(sex)) %>%
  ggplot(mapping = aes(x = age, y = spend, fill = sex)) +
  geom_col(position = "dodge") +
  ggtitle("Effect of Age and Sex on Spends") +
  labs(x = "Age", y = "Spend")


# effect of age and sex on nights -----
q3_data %>%
  mutate(sex = as.factor(sex)) %>%
  ggplot(mapping = aes(x = age, y = nights, fill = sex)) +
  geom_col(position = "dodge") +
  ggtitle("Effect of Age and Sex on Nights") +
  labs(x = "Age", y = "Nights")


# Question 4 - Comparing France and Spain -----

# preparing the data -----
france_data = travel_data[ which(travel_data$country == "France"), ]
spain_data  = travel_data[ which(travel_data$country == "Spain"), ]

comparison_data <- france_data %>%
  mutate(Type = 'France') %>%
  bind_rows(spain_data %>%
  mutate(Type = 'Spain'))

# comparing age of visitors -----
comparison_data %>%
  mutate(age = as.factor(age)) %>%
  ggplot(mapping = aes(x = Type, fill = age)) +
  geom_bar(position = "dodge") +
  ggtitle("France and Spain - Age of Visitors") +
  labs(x = "Age", y = "Frequency in Dataset")

# comparing sex of visitors -----
comparison_data %>%
  mutate(sex = as.factor(sex)) %>%
  ggplot(mapping = aes(x = Type, fill = sex)) +
  geom_bar(position = "dodge") +
  ggtitle("France and Spain - Sex of Visitors") +
  labs(x = "Country", y = "Frequency in Dataset")

# comparing duration of visits -----
comparison_data %>%
  mutate(duration = as.factor(duration)) %>%
  ggplot(mapping = aes(x = Type, fill = duration)) +
  geom_bar(position = "dodge") +
  ggtitle("France and Spain - Duration of Visits") +
  labs(x = "Duration", y = "Frequency in Dataset")

# comparing number of visits made -----
ggplot(comparison_data ,aes(visits, color = Type)) +
  geom_freqpoly() +
  ggtitle("France and Spain - Visits Made") +
  labs(x = "Visits", y = "Frequency")

# comparing number of nights spent -----
ggplot(comparison_data ,aes(nights, color = Type)) +
  geom_freqpoly() +
  ggtitle("France and Spain - Nights Spent") +
  labs(x = "Nights Spent", y = "Frequency")

# comparing amount of money spent -----
ggplot(comparison_data ,aes(spend, color = Type)) +
  geom_freqpoly() +
  ggtitle("France and Spain - Money Spent") +
  labs(x = "Money Spent", y = "Frequency")

# comparing mode of transport -----
comparison_data %>%
  mutate(mode = as.factor(mode)) %>%
  ggplot(mapping = aes(x = Type, fill = mode)) +
  geom_bar(position = "dodge") +
  ggtitle("France and Spain - Mode of Transport") +
  labs(x = "Mode", y = "Frequency in Dataset")

