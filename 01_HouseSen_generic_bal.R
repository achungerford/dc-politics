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
#   select() chooses columns
#   select(df, colName1, colName2, etc.)    -- method_1
#   df %>% select(colName1, colName2, etc.) -- method 2
#
#   mutate() adds new variables to df, but preserves existing
#   
#   summarize() reduces multiple values down to a single value
#   
#   dmy(), mdy(), ydm() etc. transform dates stored in character and numeric
#   vectors to Date or POSIXct objects  
#
#   geom_smooth() in ggplot()



library(dplyr)
library(lubridate)
library(ggplot2)

# load generic_ballot data. 
generic_ballot <- read.csv(file = "generic_ballot.csv", header = TRUE)

head(generic_ballot)


# Adding additional columns "dem.margin", "month", "yr"; reformat "Date" col
# dem.margin
generic_ballot_dates <-
  generic_ballot %>%
  mutate(dem.margin = Democrats - Republicans,
         Date = mdy(Date),
         month = month(Date),
         yr = year(Date))

# notice new columns and formats
head(generic_ballot)
head(generic_ballot_dates)


# new df with polls grouped by election year - 1 group
# polls in a single year get averaged
generic_ballot_group_elecYear <- 
  generic_ballot_dates %>%
  group_by(ElecYear) %>%
  summarize(dem.margin = mean(dem.margin))

head(generic_ballot)
head(generic_ballot_dates)
head(generic_ballot_group_elecYear)


# new df with polls grouped by year then month - 2 groups
generic_ballot_grouped <-
  generic_ballot_dates %>%
  group_by(yr, month) %>%
  summarise(dem.margin = mean(dem.margin))

# So what just happened?
# "yr" col is now used as the index - same year appears in multiple rows
#
# "month" col is secondary index - only one value for each month is displayed
# if there are multiple polls in the same month, the dem.margin is averaged

head(generic_ballot)                # original data
head(generic_ballot_dates)          # Date; dem.margin; month; yr
head(generic_ballot_group_elecYear) # 1 group  - ElecYear
head(generic_ballot_grouped)        # 2 groups - yr, mo


# new "time" column to use a date summary
### sprintf -- think of C-language
### returns a char vector/array containing a
### formatted combo of text and variable values
plot_df <- 
  generic_ballot_dates %>%
  mutate(time = sprintf("%s-%s-%s", yr, month, "01"))

head(plot_df)


###################### GOOD UP TO HERE ########################################




# Plot the dem.margin over time
ggplot(plot_df, aes(x = ymd(time), y = dem.margin)) +
  geom_line()

# Plot the dem.margin over time
ggplot(plot_df, aes(x = ymd(time), y = dem.margin)) +
  geom_line()

# Graph the trendline with geom_smooth()
ggplot(plot_df, aes(x = ymd(time), y = dem.margin)) +
  geom_point() +
  geom_smooth(span = 0.2)







####################### library(dplyr) notes #################################
#
#   select(df, colName1, colName2, etc.)    -- method_1
#   df %>% select(colName1, colName2, etc.) -- method 2



####################### library(lubridate) notes ##############################
#
# mdy(), dmy(), ymd(), etc. transform dates stored in character and numeric
# vectors to Date or POSIXct objects 

# examples
d <- c("05/28/1987")
mdy(d)

e <- c("28 May 1987")
dmy(e)

f <- c("1987/May/28")
ymd(f)


##################### ggplot notes ###########################################
#
# ggplot(df, aes(x = month, y = dem.margin)) +
#   geom_point() +
#   geom_smooth(span=0.2)
# 
#
# geom_smooth()
#   smoothing your trendline
#   span - how smooth you want the trend to be, range [0, 1]
#     higher numbers: smoother trend
#
###############################################################################