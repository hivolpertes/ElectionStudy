library(tidyverse)

# read in daily diary
dat = read.delim("./5 Clean data/ElectionStudy_MergedDailyDiary_clean.txt")

# histograms for discrim
# in person
hist(dat$Discrim_1)
# online
hist(dat$Discrim_2)
# vicarious in person
hist(dat$Discrim_3)
# vicarious online
hist(dat$Discrim_4)
# negative comments/jokes in person
hist(dat$Discrim_5)
# negative comments/jokes online
hist(dat$Discrim_6)
# rumination
hist(dat$Discrim_7)

# look at person averages
personavg = select(dat, SubID, Discrim_1:Discrim_7) %>% 
  group_by(SubID) %>% 
  summarise_all(mean)
hist(personavg$Discrim_1)
hist(personavg$Discrim_2)
hist(personavg$Discrim_3)
hist(personavg$Discrim_4)
hist(personavg$Discrim_5)
hist(personavg$Discrim_6)
hist(personavg$Discrim_7)

# recode discrim as no = 0, yes = 1
newdat = select(dat, SubID, Discrim_1:Discrim_7, DailyNews, Survey, DMIL_1:DMIL_2, DayStudy.adj:Anx_agg)

newdat$Discrim_1.d = ifelse(newdat$Discrim_1 == 0, 0, 1)
newdat$Discrim_2.d = ifelse(newdat$Discrim_2 == 0, 0, 1)
newdat$Discrim_3.d = ifelse(newdat$Discrim_3 == 0, 0, 1)
newdat$Discrim_4.d = ifelse(newdat$Discrim_4 == 0, 0, 1)
newdat$Discrim_5.d = ifelse(newdat$Discrim_5 == 0, 0, 1)
newdat$Discrim_6.d = ifelse(newdat$Discrim_6 == 0, 0, 1)
newdat$Discrim_7.d = ifelse(newdat$Discrim_7 == 0, 0, 1)

newdat$Discrim_all.d = ifelse((newdat$Discrim_1.d + newdat$Discrim_2.d + newdat$Discrim_3.d + newdat$Discrim_4.d +
                                 newdat$Discrim_5.d + newdat$Discrim_6.d + newdat$Discrim_7.d) == 0, 0, 1)

newdat$Discrim_all_norum.d = ifelse((newdat$Discrim_1.d + newdat$Discrim_2.d + newdat$Discrim_3.d + newdat$Discrim_4.d +
                                       newdat$Discrim_5.d + newdat$Discrim_6.d) == 0, 0, 1)


# look at proportion of recoded yes/no
mean(newdat$Discrim_1.d, na.rm=T)
mean(newdat$Discrim_2.d, na.rm=T)
mean(newdat$Discrim_3.d, na.rm=T)
mean(newdat$Discrim_4.d, na.rm=T)
mean(newdat$Discrim_5.d, na.rm=T)
mean(newdat$Discrim_6.d, na.rm=T)
mean(newdat$Discrim_7.d, na.rm=T)
mean(newdat$Discrim_all.d, na.rm=T)
mean(newdat$Discrim_all_norum.d, na.rm=T)

# write data
write.table(newdat, "./5 Clean data/ElectionStudy_MergedDailyDiary_clean_recodedDiscrim.txt", sep="\t", row.names=F)


# Add lagged and previous days ----------------------------------------------------------------

newdat = read.delim("./5 Clean data/ElectionStudy_MergedDailyDiary_clean_recodedDiscrim.txt")

# add staggered values- first need to put in NAs for missing days
# figure out which subs are missing days
fullDays = c(1:14)
for (i in unique(newdat$SubID)) {
  if (nrow(newdat[newdat$SubID == i,]) < 14) {
    
    missDays = fullDays[!(fullDays %in% unique(newdat$DayStudy.adj[newdat$SubID == i]))]
    newdat = rbind(newdat,
                   data.frame(SubID = i,
                              Discrim_1 = NA, Discrim_2 = NA, Discrim_3 = NA, Discrim_4 = NA, Discrim_5 = NA, Discrim_6 = NA, Discrim_7 = NA,
                              DailyNews = 1, Survey = NA, DMIL_1 = NA, DMIL_2 = NA,
                              DayStudy.adj = missDays,
                              DayWeek = NA, # need to fix after
                              NA_agg = NA, PA_agg = NA, Dep_agg = NA, Anx_agg = NA,
                              Discrim_1.d = NA, Discrim_2.d = NA, Discrim_3.d = NA, Discrim_4.d = NA, Discrim_5.d = NA, Discrim_6.d = NA, Discrim_7.d = NA,
                              Discrim_all.d = NA, Discrim_all_norum.d = NA))
  }
}

# check
count(newdat, SubID)

# add in day of week
newdat$DayWeek[newdat$DayStudy.adj %in% c(1, 8)] = "Wednesday"
newdat$DayWeek[newdat$DayStudy.adj %in% c(2, 9)] = "Thursday"
newdat$DayWeek[newdat$DayStudy.adj %in% c(3, 10)] = "Friday"
newdat$DayWeek[newdat$DayStudy.adj %in% c(4, 11)] = "Saturday"
newdat$DayWeek[newdat$DayStudy.adj %in% c(5, 12)] = "Sunday"
newdat$DayWeek[newdat$DayStudy.adj %in% c(6, 13)] = "Monday"
newdat$DayWeek[newdat$DayStudy.adj %in% c(7, 14)] = "Tuesday"

# sort
newdat = newdat[order(newdat$DayStudy.adj),]
newdat = newdat[order(newdat$SubID),]

# add next day
for (i in unique(newdat$SubID)) {
  temp = newdat[newdat$SubID == i,]
  for (r in 1:13) {
    # next dat
    newdat$NA_agg_nextDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$NA_agg[temp$DayStudy.adj == r+1]
    newdat$PA_agg_nextDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$PA_agg[temp$DayStudy.adj == r+1]
    newdat$Dep_agg_nextDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$Dep_agg[temp$DayStudy.adj == r+1]
    newdat$Anx_agg_nextDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$Anx_agg[temp$DayStudy.adj == r+1]
  }
}

# add previous day
for (i in unique(newdat$SubID)) {
  temp = newdat[newdat$SubID == i,]
  for (r in 2:14) {
    # next dat
    newdat$NA_agg_prevDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$NA_agg[temp$DayStudy.adj == r-1]
    newdat$PA_agg_prevDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$PA_agg[temp$DayStudy.adj == r-1]
    newdat$Dep_agg_prevDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$Dep_agg[temp$DayStudy.adj == r-1]
    newdat$Anx_agg_prevDay[newdat$DayStudy.adj == r & newdat$SubID == i] = temp$Anx_agg[temp$DayStudy.adj == r-1]
  }
}

write.table(newdat, "./5 Clean Data/ElectionStudy_MergedDailyDiary_clean_recodedDiscrim_laggedDays.txt", sep="\t", row.names=F)
