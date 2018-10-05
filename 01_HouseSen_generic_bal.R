#
# author: Alexander C. Hungerford
# 
# created: 04 October 2018
#
#
# summary:
#
#   Datacamp course: Analyzing Election and Polling Data in R
#   Chapter 2 - U.S. House and Senate Polling
#   dataset: generic_ballot.csv
#
# notes:
#   mutate() adds new variables and preserves existing
#   
#   dmy(), mdy(), ydm() etc. transform dates stored in character and numeric
#   vectors to Date or POSIXct objects  



library(dplyr)
library(zoo)
library(lubridate)

# load generic_ballot data
generic_ballot <- read.csv(file = "generic_ballot.csv", header = TRUE)

# view head of data
head(generic_ballot)

# filter election year to 2016, select columns: Date, Democrats, Republicans
filter(generic_ballot, ElecYear == 2016) %>%
  select(Date, Democrats, Republicans)


# Mutate a new variable called "Democratic.Margin" equal to the difference
# between Democrats' vote share and Republicans' vote share
democratic_lead <- generic_ballot %>%
  mutate(Democratic.Margin = Democrats - Republicans)

# look at Democratic.Margin
head(democratic_lead %>%
  select(Democratic.Margin))


# Group polls by month & year they were taken
# data %>% group_by(year, month)
# 
# # Create an average reading of the Democratic Margin that month
# data %>% group_by(year, month) %>%
# summarise(support = mean(support))
# 
# ggplot(data,aes(x=month,y=support)) +
#   geom_point() +
#   geom_smooth(span=0.2)
# 

# geom_smooth()
#   smoothing your trendline
#   span - how smooth you want the trend to be, range [0, 1]
#     higher numbers: smoother trend



# Group the generic ballot dataset by year and summarise
# an average of the Democratic.Margin variable
over_time <- democratic_lead %>%
  group_by(ElecYear) %>%
  summarize(Democratic.Margin = mean(Democratic.Margin))

# Explore the data.frame
head(over_time)


# Create a month and year variable for averaging polls by approximate date
timeSeries <- democratic_lead %>%
  mutate(Date = mdy(Date), month = month(Date), yr = year(Date))

head(timeSeries)

# mdy(), dmy(), ymd(), etc. transform dates stored in character and numeric
# vectors to Date or POSIXct objects 
# examples
d <- c("05/28/1987")
e <- c("28 May 1987")
f <- c("1987/May/28")

mdy(d)
dmy(e)
ymd(f)



# compare dataframes before and after date mutations
head(democratic_lead)
head(timeSeries)


?mutate
