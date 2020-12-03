# File created by Angel Armenta, 
# Edits by Hannah, 12/1/20
# edit to test Github

library(tidyverse)
library(gtools)

Electiondata1 = read.delim("./4 Raw data/Election Study (Regular daily)_November 17, 2020_11.41.csv", sep=",", stringsAsFactors = F)
Electiondata2 = read.delim("./4 Raw data/Election Study (Extended daily)_November 17, 2020_11.42.csv", sep=",", stringsAsFactors = F)
Electiondata3 = read.delim("./4 Raw data/Election Study (Extended daily with prostalgia)_November 17, 2020_11.43.csv", sep=",", stringsAsFactors = F)


# 1. Merge data sets -----------------------------------------------------------------------------
# Note: This is a section header (can insert using Code->Insert Section), it helps with jumping around a big script

Electiondata1$Survey = "Daily"
Electiondata2$Survey = "Extended"
Electiondata3$Survey = "ExtendedProstalgia"

Electiondata <-merge(Electiondata1, Electiondata2, all = TRUE)
Electiondata <-merge(Electiondata, Electiondata3, all = TRUE)

# 2. Filter out unfinished surveys ------------------------------------------------------------
Electiondata = filter(Electiondata, Electiondata$Finished == "1")

# 3. Convert time variable --------------------------------------------------------------------
library(lubridate)

# split into date and time
Electiondata$Date = str_split_fixed(Electiondata$StartDate, " ", n = 2)[,1]
Electiondata$Time = str_split_fixed(Electiondata$StartDate, " ", n = 2)[,2]

# change to time class
Electiondata$Date = strptime(Electiondata$Date, format = "%m/%d/%y")
Electiondata$Time = strptime(Electiondata$Time, format = "%H:%M")

Electiondata$DateTime = strptime(Electiondata$StartDate, format = "%m/%d/%y %H:%M")

# take out rows that don't fall within study duration (study duration: 10/28 - 11/10)
startday = ymd("2020-10-28")
endday = ymd("2020-11-10")

Electiondata = filter(Electiondata, Date >= startday & Date <= endday)
unique(Electiondata$Date)

# take out times that don't fall within accepted windows (don't accept 2 am - 5:30 pm)
# technically window should open at 6, but some surveys were sent out a little early, so building in 
# a little wiggle room
# also separating date and time early made each time variable have the date of 2020-12-01
starttime = ymd_hms("2020-12-01-02:00:00", tz = "MST")
endtime = ymd_hms("2020-12-01-17:30:00", tz = "MST")

# make new variable so that I can manually check if it's doing the right thing
# 1 = in window, 0 = not in window
Electiondata$InWindow = ifelse(Electiondata$Time >= starttime & Electiondata$Time <= endtime, 0, 1)
head(select(Electiondata, Time, InWindow), 15)
Electiondata$Time[Electiondata$InWindow == 0] # looks like it's identifying the correct things that aren't in the window

Electiondata = filter(Electiondata, InWindow == 1)
unique(sort(Electiondata$Time)) # checks out, no values between 2 am and 5:30 pm

# 4. Add day in study ----------------------------------------------------------------------------

yday(startday) # first day of study is 302nd day of year
Electiondata$DayStudy = yday(Electiondata$Date) - 301

# need to adjust so that responses given between midnight and 2 am are counted on previous day of study
Electiondata$DayStudy.adj = ifelse(Electiondata$Time <= starttime, Electiondata$DayStudy-1, Electiondata$DayStudy)
head(Electiondata[Electiondata$Time <= starttime,], 20) # check looks good


# 5. Add day of week -----------------------------------------------------------------------------

Electiondata$DayWeek = NA
Electiondata$DayWeek[Electiondata$DayStudy.adj == 1|Electiondata$DayStudy.adj == 8] = "Wednesday"
Electiondata$DayWeek[Electiondata$DayStudy.adj == 2|Electiondata$DayStudy.adj == 9] = "Thursday"
Electiondata$DayWeek[Electiondata$DayStudy.adj == 3|Electiondata$DayStudy.adj == 10] = "Friday"
Electiondata$DayWeek[Electiondata$DayStudy.adj == 4|Electiondata$DayStudy.adj == 11] = "Saturday"
Electiondata$DayWeek[Electiondata$DayStudy.adj == 5|Electiondata$DayStudy.adj == 12] = "Sunday"
Electiondata$DayWeek[Electiondata$DayStudy.adj == 6|Electiondata$DayStudy.adj == 13] = "Monday"
Electiondata$DayWeek[Electiondata$DayStudy.adj == 7|Electiondata$DayStudy.adj == 14] = "Tuesday"

unique(Electiondata$DayWeek)

head(Electiondata[Electiondata$Time <= starttime,], 20) # check looks good: for responses after midnight, even if they happen on 
# a Wednesday, I want it to be marked as Tuesday since it's a continuation of Tuesday night


# 6. Take out weird sub IDs and withdrawn subs ------------------------------------------------

sort(unique(Electiondata$SubID)) # 1810 should be taken out
# 1011, 1028, 1111 withdrew, should be taken out

Electiondata = filter(Electiondata, !(SubID %in% c(1011, 1028, 1111, 1810)))
length(unique(Electiondata$SubID)) # n = 108

# 6. Look for multiple surveys per day -------------------------------------------------------------------------

check = count(Electiondata, SubID, DayStudy.adj) # counts number of rows for each level of SubID and DayStudy.adj
doubles = check[check$n >1,]

Electiondata[Electiondata$SubID == 1002 & Electiondata$DayStudy.adj == 14,]

# label responses so that we can filter second responses out
# loop goes through subs identified in doubles data frame and labels 1st and 2nd response
Electiondata$Resp = 1
for (i in 1:nrow(doubles)) {
  Electiondata$Resp[Electiondata$SubID == doubles[i,1] & Electiondata$DayStudy.adj == doubles[i,2]] = c(1:2)
}

# spot check
Electiondata[Electiondata$SubID == 1002 & Electiondata$DayStudy.adj == 14,]
Electiondata[Electiondata$SubID == 1056 & Electiondata$DayStudy.adj == 4,]
Electiondata[Electiondata$SubID == 1056 & Electiondata$DayStudy.adj == 14,]
Electiondata[Electiondata$SubID == 1104 & Electiondata$DayStudy.adj == 7,]

# take out second responses
Electiondata = filter(Electiondata, Resp == 1)

check = count(Electiondata, SubID, DayStudy.adj) # counts number of rows for each level of SubID and DayStudy.adj
check[check$n >1,] # nobody with doubles any more


# 7. Make aggregates --------------------------------------------------------------------------------

Electiondata$NA_agg = round(rowMeans(Electiondata[,19:28], na.rm =T), digits = 2)
Electiondata$PA_agg = round(rowMeans(Electiondata[,29:38], na.rm =T), digits = 2)
Electiondata$Dep_agg = round(rowMeans(Electiondata[,39:41], na.rm =T), digits = 2)
Electiondata$Anx_agg = round(rowMeans(Electiondata[,42:44], na.rm =T), digits = 2)


# 8. Export file ------------------------------------------------------------------------------

select(Electiondata, SubID:Anx_agg, -Resp, -InWindow, -DateTime, -DayStudy) %>% 
  write.table("./5 Clean data/ElectionStudy_MergedDailyDiary_clean.txt", row.names=F, sep="\t")

