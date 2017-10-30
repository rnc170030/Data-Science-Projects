# INTRODUCTION TO dplyr AND tbls ----------------------------------------------------
# Load the dplyr package
library(dplyr)

# Load the hflights package
library(hflights)

# Call both head() and summary() on hflights
head(hflights)
summary(hflights)


# Convert the hflights data.frame into a hflights tbl
hflights <- tbl_df(hflights)

# Display the hflights tbl
hflights

# Create the object carriers, containing only the UniqueCarrier variable of hflights
carriers <- hflights$UniqueCarrier


# Use lut to translate the UniqueCarrier column of hflights and before doing so
# glimpse hflights to see the UniqueCarrier variablle
glimpse(hflights)

lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")
hflights$UniqueCarrier <- lut[hflights$UniqueCarrier]
# Now glimpse hflights to see the change in the UniqueCarrier variable
glimpse(hflights)

# Fill up empty entries of CancellationCode with 'E'
# To do so, first index the empty entries in CancellationCode
lut = c('A' = 'carrier', 'B' = 'weather', 'C' = 'FFA', 'D' = 'security', 
        'E' = 'not cancelled')
cancellationEmpty <- hflights$CancellationCode == ""
# Assign 'E' to the empty entries
hflights$CancellationCode[cancellationEmpty] <- 'E'

# Use the lookup table to create a vector of code labels. Assign the vector to the CancellationCode column of hflights
hflights$CancellationCode <- lut[hflights$CancellationCode]

# Inspect the resulting raw values of your variables
glimpse(hflights)

# SELECT() ----------------------------------------------------------------------------

hflights[c('ActualElapsedTime','ArrDelay','DepDelay')]
# Equivalently, using dplyr:
select(hflights, ActualElapsedTime, ArrDelay, DepDelay)

# Print out a tbl with the four columns of hflights related to delay
select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

# Print out hflights, nothing has changed!
hflights

# Print out the columns Origin up to Cancelled of hflights
select(hflights, Origin:Cancelled)

# Find the most concise way to select: columns Year up to and 
# including DayOfWeek, columns ArrDelay up to and including Diverted
# Answer to last question: be concise! 
# You may want to examine the order of hflight's column names before you 
# begin with names()
names(hflights)
select(hflights, -(DepTime:AirTime))

# Helper functions used with dplyr

# Print out a tbl containing just ArrDelay and DepDelay
select(hflights, ArrDelay, DepDelay)
# Use a combination of helper functions and variable names to print out 
# only the UniqueCarrier, FlightNum, TailNum, Cancelled, and CancellationCode 
# columns of hflights
select(hflights, UniqueCarrier, FlightNum, contains("Tail"), contains("Cancel"))

# Find the most concise way to return the following columns with select and its 
# helper functions: DepTime, ArrTime, ActualElapsedTime, AirTime, ArrDelay, 
# DepDelay. Use only helper functions
select(hflights, ends_with("Time"), ends_with("Delay"))

# Some comparisons to basic R
# both hflights and dplyr are available

ex1r <- hflights[c("TaxiIn","TaxiOut","Distance")]
ex1d <- select(hflights, TaxiIn, TaxiOut, Distance) 

ex2r <- hflights[c("Year","Month","DayOfWeek","DepTime","ArrTime")]
ex2d <- select(hflights, Year:ArrTime, -DayofMonth)

ex3r <- hflights[c("TailNum","TaxiIn","TaxiOut")]
ex3d <- select(hflights, TailNum, contains("Taxi"))

# MUTATE() ----------------------------------------------------------------------------

# Add the new variable ActualGroundTime to a copy of hflights and save the result as g1.
g1 <- mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime)

# Add the new variable GroundTime to a g1. Save the result as g2.
g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut)

# Add the new variable AverageSpeed to g2. Save the result as g3.
g3 <- mutate(g2, AverageSpeed = Distance / AirTime * 60)

# Print out g3
g3

# Add a second variable loss_percent to the dataset: m1
m1 <- mutate(hflights, loss = ArrDelay - DepDelay, 
             loss_percent = ((ArrDelay - DepDelay)/DepDelay)*100)

# mutate() allows you to use a new variable while creating a next variable 
# in the same call
# Copy and adapt the previous command to reduce redendancy: m2
m2 <- mutate(hflights, loss = ArrDelay - DepDelay, 
             loss_percent = (loss/DepDelay) * 100 )

# Add the three variables as described in the third instruction: m3
m3 <- mutate(hflights, TotalTaxi = TaxiIn + TaxiOut, 
             ActualGroundTime = ActualElapsedTime - AirTime, 
             Diff = TotalTaxi - ActualGroundTime)

# FILTER() --------------------------------------------------------------------------

# Print out all flights in hflights that traveled 3000 or more miles
filter(hflights, Distance > 3000)

# All flights flown by one of JetBlue, Southwest, or Delta
filter(hflights, UniqueCarrier %in% c('JetBlue', 'Southwest', 'Delta'))

# All flights where taxiing took longer than flying
filter(hflights, TaxiIn + TaxiOut > AirTime)

# Combining tests using boolean operators

# All flights that departed before 5am or arrived after 10pm
filter(hflights, DepTime < 500 | ArrTime > 2200 )

# All flights that departed late but arrived ahead of schedule
filter(hflights, DepDelay > 0 & ArrDelay < 0)

# All cancelled weekend flights
filter(hflights, DayOfWeek %in% c(6,7) & Cancelled == 1)

# All flights that were cancelled after being delayed
filter(hflights, Cancelled == 1, DepDelay > 0)

# Summarizing Exercise
# Select the flights that had JFK as their destination: c1
c1 <- filter(hflights, Dest == 'JFK')

# Combine the Year, Month and DayofMonth variables to create a Date column: c2
c2 <- mutate(c1, Date = paste(Year, Month, DayofMonth, sep = "-"))

# Print out a selection of columns of c2
select(c2, Date, DepTime, ArrTime, TailNum)

# How many weekend flights flew a distance of more than 1000 miles 
# but had a total taxiing time below 15 minutes?
nrow(filter(hflights, DayOfWeek %in% c(6,7), Distance > 1000, TaxiIn + TaxiOut < 15)) 


# ARRANGE() --------------------------------------------------------------------------

# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))

# Arrange dtc by departure delays
arrange(dtc, DepDelay)

# Arrange dtc so that cancellation reasons are grouped
arrange(dtc, CancellationCode)

# Arrange dtc according to carrier and departure delays
arrange(dtc, UniqueCarrier, DepDelay)

# Arrange according to carrier and decreasing departure delays
arrange(hflights, UniqueCarrier, desc(DepDelay))

# Arrange flights by total delay (normal order).
arrange(hflights, DepDelay + ArrDelay)

# Keep flights leaving to DFW before 8am and arrange according to decreasing AirTime 
arrange(filter(hflights, Dest == 'DFW', DepTime < 800), desc(AirTime))

# list containing only TailNum of flights that departed too late, sorted by 
# total taxiing time
# First select a threshold for departure delay by finding max DepDelay
max(filter(select(hflights, DepDelay), !is.na(DepDelay)))
select(arrange(filter(hflights, DepDelay > 360), TaxiIn + TaxiOut), TailNum)

# SUMMARISE() -----------------------------------------------------------------------

# Print out a summary with variables min_dist and max_dist
summarize(hflights, min_dist = min(Distance), max_dist = max(Distance))

# Print out a summary with variable max_div
summarize(filter(hflights, Diverted == 1), max_div = max(Distance))

# Remove rows that have NA ArrDelay: temp1
temp1 <- filter(hflights, !is.na(ArrDelay))

# Generate summary about ArrDelay column of temp1
summarise(temp1, earliest = min(ArrDelay), average = mean(ArrDelay), 
          latest = max(ArrDelay), sd = sd(ArrDelay))

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
temp2 <- filter(hflights, !is.na(TaxiIn), !is.na(TaxiOut))

# Print the maximum taxiing difference of temp2 with summarise()
summarise(temp2, max_taxi_diff = max(abs(TaxiIn - TaxiOut)))

# Generate summarizing statistics for hflights
summarise(hflights, n_obs = n(), n_carrier = n_distinct(UniqueCarrier), 
          n_dest = n_distinct(Dest), dest100 = nth(Dest, 100))

# Filter hflights to keep all American Airline flights: aa
aa <- filter(hflights, UniqueCarrier == "American")

# Generate summarizing statistics for aa 
summarise(aa, n_flights = n(), n_canc = sum(Cancelled), 
          p_canc = 100*(n_canc/n_flights), avg_delay = mean(ArrDelay, na.rm = TRUE))

# %>% OPERATOR ----------------------------------------------------------------------

# with %>% operator
hflights %>% 
  mutate(diff = TaxiOut - TaxiIn) %>% 
  filter(!is.na(diff)) %>% 
  summarise(avg = mean(diff))

# without %>% operator
# arguments get further and further apart
summarize(filter(mutate(hflights, diff = TaxiOut - TaxiIn),!is.na(diff)),
          avg = mean(diff))


# with %>% operator
d <- hflights %>% 
  select(Dest, UniqueCarrier, Distance, ActualElapsedTime) %>% 
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance/RealTime*60)

# without %>% operator
d <- mutate(select(hflights, Dest, UniqueCarrier, Distance, ActualElapsedTime),
            RealTime = ActualElapsedTime + 100, mph = Distance/RealTime*60)

# Filter and summarise d
d %>% 
  filter(!is.na(mph), mph < 70) %>% 
  summarise(n_less = n(), n_dest = n_distinct(Dest), 
            min_dist = min(Distance), max_dist = max(Distance))

# Let's define preferable flights as flights that are 150% faster than driving, 
# i.e. that travel 105 mph or greater in real time. Also, assume that cancelled or 
# diverted flights are less preferable than driving. 


# ADVANCED PIPING EXERCISES
# Use one single piped call to print a summary with the following variables:

# n_non - the number of non-preferable flights in hflights,
# p_non - the percentage of non-preferable flights in hflights,
# n_dest - the number of destinations that non-preferable flights traveled to,
# min_dist - the minimum distance that non-preferable flights traveled,
# max_dist - the maximum distance that non-preferable flights traveled

hflights %>% 
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance/RealTime*60) %>%
  filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
  summarise(n_non = n(), p_non = 100*n_non/nrow(hflights), n_dest = n_distinct(Dest),
            min_dist = min(Distance), max_dist = max(Distance))

# Use summarise() to create a summary of hflights with a single variable, n, 
# that counts the number of overnight flights. These flights have an arrival 
# time that is earlier than their departure time. Only include flights that have 
# no NA values for both DepTime and ArrTime in your count.

hflights %>%
  mutate(overnight = (ArrTime < DepTime)) %>%
  filter(overnight == TRUE) %>%
  summarise(n = n())

# group_by() -------------------------------------------------------------------------

# Generate a per-carrier summary of hflights with the following variables: n_flights, 
# the number of flights flown by the carrier; n_canc, the number of cancelled flights; 
# p_canc, the percentage of cancelled flights; avg_delay, the average arrival delay of 
# flights whose delay does not equal NA. Next, order the carriers in the summary from 
# low to high by their average arrival delay. Use percentage of flights cancelled to 
# break any ties. Which airline scores best based on these statistics?

hflights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(n_flights = n(), n_canc = sum(Cancelled), p_canc = 100*n_canc/n_flights, 
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>% arrange(avg_delay)

# Generate a per-day-of-week summary of hflights with the variable avg_taxi, 
# the average total taxiing time. Pipe this summary into an arrange() call such 
# that the day with the highest avg_taxi comes first.

hflights %>% 
  group_by(DayOfWeek) %>% 
  summarize(avg_taxi = mean(TaxiIn + TaxiOut, na.rm = TRUE)) %>% 
  arrange(desc(avg_taxi))

# Combine group_by with mutate-----

# First, discard flights whose arrival delay equals NA. Next, create a by-carrier 
# summary with a single variable: p_delay, the proportion of flights which are 
# delayed at arrival. Next, create a new variable rank in the summary which is a 
# rank according to p_delay. Finally, arrange the observations by this new rank
hflights %>% 
  filter(!is.na(ArrDelay)) %>% 
  group_by(UniqueCarrier) %>% 
  summarise(p_delay = sum(ArrDelay >0)/n()) %>% 
  mutate(rank = rank(p_delay)) %>% 
  arrange(rank) 

# n a similar fashion, keep flights that are delayed (ArrDelay > 0 and not NA). 
# Next, create a by-carrier summary with a single variable: avg, the average delay 
# of the delayed flights. Again add a new variable rank to the summary according to 
# avg. Finally, arrange by this rank variable.
hflights %>% 
  filter(!is.na(ArrDelay), ArrDelay > 0) %>% 
  group_by(UniqueCarrier) %>% 
  summarise(avg = mean(ArrDelay)) %>% 
  mutate(rank = rank(avg)) %>% 
  arrange(rank)

# Advanced group_by exercises-------------------------------------------------------

# Which plane (by tail number) flew out of Houston the most times? How many times?
# Name the column with this frequency n. Assign the result to adv1. To answer this 
# question precisely, you will have to filter() as a final step to end up with only 
# a single observation in adv1.
# Which plane (by tail number) flew out of Houston the most times? How many times? adv1
adv1 <- hflights %>% 
  group_by(TailNum) %>% 
  summarise(n = n()) %>%
  filter(n == max(n))

# How many airplanes only flew to one destination from Houston? adv2
# How many airplanes only flew to one destination from Houston? 
# Save the resulting dataset in adv2, that contains only a single column, 
# named nplanes and a single row.
adv2 <- hflights %>%
  group_by(TailNum) %>%
  summarise(n_dest = n_distinct(Dest)) %>%
  filter(n_dest == 1) %>%
  summarise(nplanes = n())

# Find the most visited destination for each carrier and save your solution to adv3. 
# Your solution should contain four columns:
# UniqueCarrier and Dest,
# n, how often a carrier visited a particular destination,
# rank, how each destination ranks per carrier. rank should be 1 for every row, 
# as you want to find the most visited destination for each carrier.

adv3 <- hflights %>%
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

# Find the carrier that travels to each destination the most: adv4
# For each destination, find the carrier that travels to that destination the most. 
# Store the result in adv4. Again, your solution should contain 4 columns: 
# Dest, UniqueCarrier, n and rank.

adv4 <- hflights %>%
  group_by(Dest, UniqueCarrier) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

