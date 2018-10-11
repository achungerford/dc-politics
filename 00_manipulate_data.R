###############################################################################
# 
# author: Alexander C. Hungerford
# 
# course: Datacamp: Analyzing Election and Polling Data in R
# 
# date started: 03 October 2018
# 
# 
# 
# 
# Summary:
# 
# importing, filtering, grouping data
# moving averages
# date manipulation with lubridate
# 
# functions: rollmean(), pull(), ymd(), mdy(), dym()
# 
# 
# 
# 
# 
###############################################################################

# Importing the necessary libraries
library(dplyr)     # for working with data
library(lubridate) # for working with dates
library(zoo)       # for working with dates
library(ggplot2)   # for visualizations


# Importing data. All presidential approval polls from Gallup.
gallup <- read.csv("gallup approval polls.csv", header = TRUE)
head(gallup)


# Select the President, Date, and Approve columns. Filter "Trump" only.
Trump_Approval <- select(gallup, President, Date, Approve) %>%
  filter(President == "Trump")

# show Trump_Approval
head(Trump_Approval)

# get a vector of Trump's approval ratings
Trump_Approval_vector <-pull(Trump_Approval, Approve)


# Group data by president, get average approval ratings for each president
mean.approval.by.president <- gallup %>% 
  group_by(President) %>%
  summarize(meanApproval = mean(Approve))

# show data
mean.approval.by.president


# get a 10-poll moving average of Trump's approval ratings
Trump_Approval %>%
  mutate(Month = months(mdy(Date))) %>%
  group_by(Month) %>%
  summarize(Approve = mean(Approve))

# Save Donald Trump's approval polling chronologically for time series analysis
Trump_Approval_timeSeries <- select(gallup, President, Date, Approve) %>%
  filter(President == "Trump") %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date)

# view it
head(Trump_Approval_timeSeries)

# use rollmean() to get a moving average of the last 10 polls
movAvg_Trump <- Trump_Approval_timeSeries %>%
  mutate(avgApprove = rollmean(Approve, 10, na.pad = TRUE, align = "right"))

# plot the moving average
ggplot(movAvg_Trump, aes(x = Date, y = avgApprove)) +
  geom_line()


# Now plot every president's approval using moving averages
allApproval <- gallup %>%
  group_by(President) %>%
  mutate(avgApproval =
           rollmean(Approve, 10, na.pad = TRUE, align = "right"))

ggplot(allApproval, aes(x = Days, y = avgApproval, col = President)) +
  geom_line()


############################# NOTES ###########################################

# ------------------------------------------------------- Formatting dates in R
# library(lubridate)

# Universal format: "2018-01-01"
# Excel format:       "01/01/18"

# ymd()
# date <- ymd("2018-01-01")
#   month(date) # Equal to 1
#   month(date,label = T) # Equal to "Jan"



# ------------------------------------------------------------- Moving Averages
# library(zoo)

# arrange the data chronologically
# then average the most recent x-number of polls, then do next row

# rollmean(column, x-observations,
#          whether or not to fill the columns with NA,
#          whether or not to avg up or down the dataframe vector i.e. leading or lagging observations)
Trump_Approval %>%
  mutate(avgApprove = rollmean(Approve, 10, na.pad = TRUE, align = "right"))

x <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)

# moving average - align right
rollmean(x, 5, na.pad = TRUE, align = "right")
# moves across vector to the right (down the vector)


# moving average - align left
rollmean(x, 5, na.pad = TRUE, align = "left")
# moves across vector to the left (up the vector)



