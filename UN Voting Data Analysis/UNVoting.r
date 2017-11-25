##########################################################
##Description:
#You'll explore the historical voting of the United Nations General Assembly, including analyzing differences in voting between countries, across time, and among international issues. 
#In the process we'll gain more practice with the dplyr and ggplot2 packages, learn about the broom package for tidying model output, and experience the kind of start-to-finish exploratory analysis common in data science.
#Steps involved while analyzing the data sets are as follows:
#Data cleaning and summarizing with dplyr
#Data visualization with ggplot2
#Tidy modeling with broom
#Joining and tidying
##########################################################
##DataSets used are as follows:
#votes.rds <- Number of votes per country per year.
#Description.rds <- Topics raised in the session and countries vote for the topics.
#Country Code <- Translation of country code to country name.
##########################################################
##Packages used are as follows:
#dplyr <- Pipe operator, filter, mutate, select operation on data set.
#ggplot2 <- Data Visualization using line chart, scatter plot, abline for regession models.
#broom <- Tidying linear regression models.
#tidyr <- nest and unnest a data frame.
#purrr <- map function to apply formula to each element in a data set.
###########################################################
library(dplyr)
# Load the ggplot2 package
library(ggplot2)
# Load the broom package
library(broom)
# Load the tidyr package to use nest9) and unnest(), gather()
library(tidyr)
#Load the purrr package to use map() - applying a formula to each item in a list.
library(purrr)

#Reading the Votes file and joining it with COW Country Code to extract corresponding Country names.
votes <- readRDS("votes.rds")
votes$year = votes$session + 1945
countrycode <- read.csv("COW country codes.csv")
votes_processed <- inner_join(votes, countrycode, by = c("ccode" = "CCode"))
colnames(votes_processed)[colnames(votes_processed) == 'StateNme'] <- 'Country'

#summarize total number of votes by year
votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

#summarize total number of votes by country
# Summarize by country: by_country
by_country <- votes_processed %>%
  group_by(Country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))
by_country

# Now that we've summarized the dataset by country, we can start examining it and answering interesting questions.
# For example, one might be especially interested in the countries that voted "yes" least often, or the ones that voted "yes" most often.

# Sort in ascending order of percent_yes
by_country %>%
  arrange(percent_yes)

# Now sort in descending order
by_country %>%
  arrange(desc(percent_yes))

# Filtering summarized output
# We noticed that the country that voted least frequently, Zanzibar, had only 2 votes in the entire dataset. We certainly can't make any substantial conclusions based on that data!
# Typically in a progressive analysis, when we find that a few of our observations have very little data while others have plenty, we will set some threshold to filter them out.
# Filter out countries with fewer than 100 votes
by_country %>%
  arrange(percent_yes) %>%
  filter(total > 100)
#Result:
# Country total percent_yes
# <fctr> <int>       <dbl>
#   1        United States of America  2568   0.2694704
# 2                           Palau   369   0.3387534
# 3                          Israel  2380   0.3407563
# 4         German Federal Republic  1075   0.3972093
# 5                  United Kingdom  2558   0.4167318
# 6                          France 80864   0.4265928
# 7  Federated States of Micronesia   724   0.4419890
# 8                Marshall Islands   757   0.4914135
# 9                         Belgium 82176   0.4922118
# 10                         Canada  2576   0.5081522
# # ... with 187 more rows

#Plotting a line over time
#We will now use the ggplot2 package to turn your results into a visualization of the percentage of "yes" votes over time.
# Define by_year
by_year <- votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Create line plot
ggplot(by_year, aes(year, percent_yes)) +
  geom_line() + 
  ggtitle("percentage of yes votes over years \n Line Chart")

# Creation of Scatter Plot with addition of smooth curve
ggplot(by_year, aes(year, percent_yes)) +
  geom_point() +
  geom_smooth() + 
  ggtitle("percentage of yes votes over years \n Scatter Plot")

#Summarizing by year and country
#We are more interested in trends of voting within specific countries than in the overall trend. 
#So instead of summarizing just by year, we will summarize by both year and country, constructing a dataset that shows what fraction of the time each country votes "yes" in each year.
by_year_country <- votes_processed %>%
  group_by(Country, year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# year     Country total percent_yes
# <dbl>      <fctr> <int>       <dbl>
#   1   1947 Afghanistan    34   0.3823529
# 2   1947   Argentina    38   0.5789474
# 3   1947   Australia    38   0.5526316
# 4   1947     Belarus    38   0.5000000
# 5   1947     Belgium  1216   0.6052632
# 6   1947     Bolivia    37   0.5945946
# 7   1947      Brazil    38   0.6578947
# 8   1947      Canada    38   0.6052632
# 9   1947       Chile    38   0.6578947
# 10  1947    Colombia    35   0.5428571
# # ... with 4,734 more rows

#Plotting just the UK over time
#Now that we have the percentage of time that each country voted "yes" within each year, we can plot the trend for a particular country. 
#In this case, we'll look at the trend for just the United Kingdom.

# Create a filtered version: UK_by_year
UK_by_year <- by_year_country %>%
  filter(Country == "United Kingdom")

# Line plot of percent_yes over time for UK only
ggplot(UK_by_year, aes(year, percent_yes)) +
  geom_line() +
  ggtitle("Percentage yes of UK over time \n Line Chart")

#Plotting multiple countries
#Plotting just one country at a time is interesting, but we really want to compare trends between countries. For example, suppose we want to compare voting trends for the United States, the UK, France, and India.
#We'll have to filter to include all four of these countries and use another aesthetic (not just x- and y-axes) to distinguish the countries on the resulting visualization. 
#Instead, we'll use the color aesthetic to represent different countries.
# Vector of four countries to examine
countries <- c("United States", "United Kingdom",
               "France", "India")

# Filter by_year_country: filtered_4_countries
filtered_4_countries <- by_year_country %>% filter(Country %in% countries)

# Line plot of % yes in four countries
ggplot(filtered_4_countries, aes(x = year, y = percent_yes, color =Country)) +
  geom_line()

#Faceting by country
#Now we'll take a look at six countries. 
#Previously we used color to represent distinct countries, this gets a little too crowded with six.

# Vector of six countries to examine
countries <- c("China", "United Kingdom",
               "France", "Japan", "Brazil", "India")

# Filtered by_year_country: filtered_6_countries
filtered_6_countries <- by_year_country %>%
  filter(Country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries, aes(year, percent_yes)) +
  geom_line() +
  ggtitle("Percentage of yes over time \n Faceted by specific countires") +
  facet_wrap(~ Country, scales = "free_y")

#Linear regression on the United States
#A linear regression is a model that lets us examine how one variable changes with respect to another by fitting a best fit line. It is done with the lm()function in R.
#Here, we'll fit a linear regression to just the percentage of "yes" votes from the United States.

# Percentage of yes votes from the US by year: US_by_year
UK_by_year <- by_year_country %>%
  filter(Country == "United Kingdom")

# Print the US_by_year data
UKby_year

# Perform a linear regression of percent_yes by year: US_fit
UK_fit <- lm(percent_yes ~ year, data = UK_by_year)

# Perform summary() on the US_fit object
summary(UK_fit)

#Tidying models with broom
#Now we will use the tidy()function in the broom package to turn that model into a tidy data frame
# Call the tidy() function on the UK_fit object
tidy(UK_fit)
# term     estimate    std.error statistic    p.value
# 1 (Intercept) -3.266547873 1.9577739504 -1.668501 0.10497360
# 2        year  0.001869434 0.0009887262  1.890750 0.06774177

#Combining models for multiple countries
# One important advantage of changing models to tidied data frames is that they can be combined.
# We had fit a linear model to the percentage of "yes" votes for each year in the United Kingdom. Now you'll fit the same model for India and combine the results from both countries.

# Fit model for the India
IN_by_year <- by_year_country %>%
  filter(Country == "India")
IN_fit <- lm(percent_yes ~ year, IN_by_year)

# Create US_tidied and UK_tidied
UK_tidied <- tidy(UK_fit)
IN_tidied <- tidy(IN_fit)

# Combine the two tidied models
bind_rows(UK_tidied, IN_tidied)
# term     estimate    std.error statistic     p.value
# 1 (Intercept) -3.266547873 1.9577739504 -1.668501 0.104973604
# 2        year  0.001869434 0.0009887262  1.890750 0.067741769
# 3 (Intercept) -5.274456178 1.9721493051 -2.674471 0.011690752
# 4        year  0.003059099 0.0009959861  3.071427 0.004324983

#Nesting a Data Frame
#Right now, the by_year_country data frame has one row per country-vote pair. So that we can model each country individually, we're going to "nest" all columns besides country, which will result in a data frame with one row per country. 
#The data for each individual country will then be stored in a list column called data.



by_year_country <- by_year_country %>% select(2:4)
# Nest all columns besides country
nested <- nest(by_year_country, - Country)

# nested
# colnames(nested)[colnames(nested) == 'data'] <- 'data1'

#We can use nested$data to access this list column and double brackets to access a particular element. 
#For example, nested$data[[1]] would give you the data frame with Afghanistan's voting history (the percent_yes per year), since Afghanistan is the first row of the table.
# For example nested data of Brazil can be retried using following code:
nested$data[[7]] 

#Unnesting
#The opposite of the nest() operation is the unnest() operation. 
#This takes each of the data frames in the list column and brings those rows back to the main data frame.
# 
# nested %>%
#   unnest(data1)

#Performing linear regression on each nested dataset
#Now that you've divided the data for each country into a separate dataset in the data column, you need to fit a linear model to each of these datasets.
#The map() function from purrr works by applying a formula to each item in a list, where . represents the individual item.
# This means that to fit a model to each dataset, you can do:
#   map(data, ~ lm(percent_yes ~ year, data = .))
# where . represents each individual item from the data column in by_year_country. 

# Perform a linear regression on each item in the data column
by_year_country %>%
  nest(-Country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy))

#Output:
# A tibble: 200 � 4
# Country              data    model               tidied
# <fctr>            <list>   <list>               <list>
#   1        Afghanistan <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   2            Albania <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   3            Algeria <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   4            Andorra <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   5             Angola <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   6  Antigua & Barbuda <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   7          Argentina <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   8            Armenia <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   9          Australia <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   10           Austria <tibble [34 � 3]> <S3: lm> <data.frame [2 � 5]>
#   # ... with 190 more rows 

#Now that we have a tidied version of each model stored in the tidied column. We would want to combine all of those into a large data frame, similar to how you combined the UKand IN tidied models earlier. 
#Recall that the unnest()function from tidyr achieves this.

# Add one more step that unnests the tidied column
country_coefficients <- by_year_country %>%
  nest(-Country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Print the resulting country_coefficients variable
country_coefficients  

# > country_coefficients
# # A tibble: 400 � 6
# Country            term      estimate   std.error statistic      p.value
# <fctr>          <chr>         <dbl>       <dbl>     <dbl>        <dbl>
# 1  Afghanistan (Intercept)  -6.086817653 2.542917862 -2.393635 2.271690e-02
# 2  Afghanistan        year   0.003458754 0.001284239  2.693233 1.116950e-02
# 3      Albania (Intercept) -12.650311249 3.350250153 -3.775930 6.543711e-04
# 4      Albania        year   0.006603772 0.001691962  3.903025 4.595611e-04
# 5      Algeria (Intercept) -29.965445162 4.068425314 -7.365367 2.237526e-08
# 6      Algeria        year   0.015473937 0.002054659  7.531145 1.413751e-08
# 7      Andorra (Intercept) -23.043446352 3.002579013 -7.674551 9.527418e-09
# 8      Andorra        year   0.011734405 0.001516380  7.738435 7.997516e-09
# 9       Angola (Intercept) -31.119806208 4.027720266 -7.726407 8.265183e-09
# 10      Angola        year   0.015923866 0.002034102  7.828449 6.254552e-09
# # ... with 390 more rows


#We currently have both the intercept and slope terms for each by-country model. 
#We're probably more interested in how each is changing over time, so we want to focus on the slope terms.
# Filter for only the slope terms
country_coefficients %>% filter(term == "year")	

# Filtering for significant countries
# p.adjust(p.value) on a vector of p-values returns a set that we can trust.
# Here we'll add two steps to process the slope_terms dataset: use a mutate to create the new, adjusted p-value column, and filter to filter for those below a .05 threshold.

# Filter by adjusted p-values
filtered_countries <- country_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05)

# Sort for the countries increasing most quickly
filtered_countries %>% arrange(desc(estimate))

# Sort for the countries decreasing most quickly
filtered_countries %>% arrange(estimate)

# We have created the votes_processed dataset, containing information about each country's votes. You'll now combine that with the new descriptions dataset, which includes topic information about each country, so that you can analyze votes within particular topics.
# To do this, you'll make use of the inner_join() function from dplyr.

descriptions <- readRDS("descriptions (1).rds")
# Join them together based on the "rcid" and "session" columns
votes_joined <- votes_processed %>% inner_join(descriptions, by = c("rcid", "session"))

# # Filtering the joined dataset
# There are six columns in the descriptions dataset (and therefore in the new joined dataset) that describe the topic of a resolution:
# 1.	me: Palestinian conflict
# 2.	nu: Nuclear weapons and nuclear material
# 3.	di: Arms control and disarmament
# 4.	hr: Human rights
# 5.	co: Colonialism
# 6.	ec: Economic development
# Each contains a 1 if the resolution is related to this topic and a 0 otherwise.

# Filter for votes related to colonialism
votes_joined %>% filter(co == 1)

#Visualizing colonialism votes
UK_co_by_year <- votes_joined %>%
  filter(Country == "United Kingdom", co == 1) %>%
  group_by(year) %>%
  summarize(percent_yes = mean(vote == 1))

# Graph the % of "yes" votes over time
ggplot(UK_co_by_year, aes(year, percent_yes)) +
  geom_line()

# Using gather to tidy a dataset
# In order to represent the joined vote-topic data in a tidy form so we can analyze and graph by topic, we need to transform the data so that each row has one combination of country-vote-topic. 
# This will change the data from having six columns (me, nu, di, hr, co, ec) to having two columns (topic and has_topic).

# Perform gather and filter
votes_gathered <- votes_joined %>% gather(topic, has_topic, me:ec) %>% filter(has_topic == 1)

# Recoding the topics
# There's one more step of data cleaning to make this more interpretable. Right now, topics are represented by two-letter codes:
# 1.	me: Palestinian conflict
# 2.	nu: Nuclear weapons and nuclear material
# 3.	di: Arms control and disarmament
# 4.	hr: Human rights
# 5.	co: Colonialism
# 6.	ec: Economic development
# So that we can interpret the data more easily, recode the data to replace these codes with their full name.
#

# Replace the two-letter codes in topic: votes_tidied
votes_tidied <-  votes_gathered%>%
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons and nuclear material",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))

# Summarize by country, year, and topic
#Now that you have topic as an additional variable, you can summarize the votes for each combination of country, year, and topic 
#e.g. for the United States in 2013 on the topic of nuclear weapons.

# Summarize the percentage "yes" per country-year-topic
by_country_year_topic <- votes_tidied %>%
  group_by(Country, year, topic) %>%
  summarize(total = n(), percent_yes = mean(vote == 1)) %>%
  ungroup()

by_country_year_topic
# > by_country_year_topic
# # A tibble: 37,335 � 5
# Country  year                                topic total percent_yes
# <fctr> <dbl>                                <chr> <int>       <dbl>
# 1  Afghanistan  1947                          Colonialism     9  0.44444444
# 2  Afghanistan  1947                 Economic development     1  0.00000000
# 3  Afghanistan  1947                         Human rights     4  0.00000000
# 4  Afghanistan  1947                 Palestinian conflict     6  0.00000000
# 5  Afghanistan  1949         Arms control and disarmament     4  0.00000000
# 6  Afghanistan  1949                          Colonialism    22  0.86363636
# 7  Afghanistan  1949                 Economic development    11  0.09090909
# 8  Afghanistan  1949                         Human rights     3  0.00000000
# 9  Afghanistan  1949 Nuclear weapons and nuclear material     3  0.00000000
# 10 Afghanistan  1949                 Palestinian conflict    11  0.81818182
# # ... with 37,325 more rows

#Visualizing trends in topics for one country
#You can now visualize the trends in percentage "yes" over time for all six topics side-by-side. 
#Here, you'll visualize them just for United Kingdom.

# Filter by_country_year_topic for just the US
UK_by_country_year_topic <- by_country_year_topic %>% filter(Country == "United Kingdom")

# Plot % yes over time for the US, faceting by topic
UK_by_country_year_topic %>% 
  ggplot(aes(x= year, y = percent_yes)) +
  geom_line() +
  facet_wrap(~topic)


#Linear models for each combination of country and topic
# Fit model on the by_country_year_topic dataset
country_topic_coefficients <-  by_country_year_topic %>%
  nest(-Country, -topic) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Print country_topic_coefficients
country_topic_coefficients
# > country_topic_coefficients
# # A tibble: 2,400 � 7
# Country                        topic        term      estimate   std.error  statistic
# <fctr>                          <chr>       <chr>         <dbl>       <dbl>      <dbl>
# 1  Afghanistan                  Colonialism (Intercept)  -2.781507722 4.207317587 -0.6611119
# 2  Afghanistan                  Colonialism        year   0.001826514 0.002124804  0.8596157
# 3  Afghanistan         Economic development (Intercept) -10.965430258 4.448884554 -2.4647595
# 4  Afghanistan         Economic development        year   0.005935676 0.002245315  2.6435829
# 5  Afghanistan                 Human rights (Intercept)  -3.171885627 4.752900638 -0.6673579
# 6  Afghanistan                 Human rights        year   0.001985230 0.002400337  0.8270629
# 7  Afghanistan         Palestinian conflict (Intercept)  -7.552260810 4.787293580 -1.5775637
# 8  Afghanistan         Palestinian conflict        year   0.004220446 0.002413577  1.7486272
# 9  Afghanistan Arms control and disarmament (Intercept) -10.355495881 5.314424909 -1.9485638
# 10 Afghanistan Arms control and disarmament        year   0.005625035 0.002678805  2.0998295
# # ... with 2,390 more rows, and 1 more variables: p.value <dbl>

#Filter only for Slope terms with significant P value
# Create country_topic_filtered
country_topic_filtered <- country_topic_coefficients %>% filter(term == "year") %>% 
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05)

#Visualization of the models:
#We found that over its history, Vanuatu (an island nation in the Pacific Ocean) sharply changed its pattern of voting on the topic of Palestinian conflict.
#Let's examine this country's voting patterns more closely. Recall that the by_country_year_topic dataset contained one row for each combination of country, year, and topic. 
#We can use that to create a plot of Vanuatu's voting, faceted by topic.

# Create vanuatu_by_country_year_topic
vanuatu_by_country_year_topic <- by_country_year_topic %>% filter(Country == "Vanuatu")

# Plot of percentage "yes" over time, faceted by topic
ggplot(vanuatu_by_country_year_topic, aes(x = year, y = percent_yes)) +
  geom_line() +
  facet_wrap(~topic)


