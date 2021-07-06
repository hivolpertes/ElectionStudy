library(tidyverse)
library(lme4)
library(lmerTest)

dat = read.delim("./5 Clean data/ElectionStudy_MergedDailyDiary_clean_recodedDiscrim_laggedDays.txt")

# make day of week a factor (Monday = 0)
dat$DayWeek.d = NA
dat$DayWeek.d[dat$DayWeek == "Monday"] = 0
dat$DayWeek.d[dat$DayWeek == "Tuesday"] = 1
dat$DayWeek.d[dat$DayWeek == "Wednesday"] = 2
dat$DayWeek.d[dat$DayWeek == "Thursday"] = 3
dat$DayWeek.d[dat$DayWeek == "Friday"] = 4
dat$DayWeek.d[dat$DayWeek == "Saturday"] = 5
dat$DayWeek.d[dat$DayWeek == "Sunday"] = 6

dat$DayWeek.d = factor(dat$DayWeek.d)


# Unconditional ICC --------------------------------------------------------------------------

lmer(NA_agg ~ 1 + (1|SubID), data = dat) %>% summary()
.2567/(.2568+.3594) # ICC for subject is .42

lmer(PA_agg ~ 1 + (1|SubID), data = dat) %>% summary()
.4340/(.4340+.4136) # ICC for subject is .51

lmer(Anx_agg ~ 1 + (1|SubID), data = dat) %>% summary()
.3963/(.3963+.5622) # ICC for subject is .41

lmer(Dep_agg ~ 1 + (1|SubID), data = dat) %>% summary()
.5353/(.5353+.7611) # ICC for subject is .41


# Effect of discrim on same day ---------------------------------------------------------------------------------

# NA
m1 = lmer(NA_agg ~ Discrim_1.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(NA_agg ~ Discrim_2.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(NA_agg ~ Discrim_3.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(NA_agg ~ Discrim_4.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(NA_agg ~ Discrim_5.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(NA_agg ~ Discrim_6.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(NA_agg ~ Discrim_7.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(NA_agg ~ Discrim_all.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(NA_agg ~ Discrim_all_norum.d + DayWeek.d + Na_agg_prevDay + (1|SubID), data = dat) %>% summary()

NA_sameday = data.frame(Model = "Discrim_1",
                           Estimate = m1$coefficients[2,1],
                           P = round(m1$coefficients[2,5], digits=3)) %>% 
  rbind(data.frame(Model = "Discrim_2",
                   Estimate = m2$coefficients[2,1],
                   P = round(m2$coefficients[2,5], digits=3))) %>% 
  rbind(data.frame(Model = "Discrim_3",
                   Estimate = m3$coefficients[2,1],
                   P = round(m3$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_4",
                   Estimate = m4$coefficients[2,1],
                   P = round(m4$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_5",
                   Estimate = m5$coefficients[2,1],
                   P = round(m5$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_6",
                   Estimate = m6$coefficients[2,1],
                   P = round(m6$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_7",
                   Estimate = m7$coefficients[2,1],
                   P = round(m7$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all",
                   Estimate = m8$coefficients[2,1],
                   P = round(m8$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all_norum",
                   Estimate = m9$coefficients[2,1],
                   P = round(m9$coefficients[2,5], digits=3)))

# graded response for days with discrim
lmer(NA_agg ~ Discrim_1 + DayWeek.d + NA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_1.d == 1)) %>% summary()
lmer(NA_agg ~ Discrim_2 + DayWeek.d + NA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_2.d == 1)) %>% summary()
lmer(NA_agg ~ Discrim_3 + DayWeek.d + NA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_3.d == 1)) %>% summary()
lmer(NA_agg ~ Discrim_4 + DayWeek.d + NA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_4.d == 1)) %>% summary()
lmer(NA_agg ~ Discrim_5 + DayWeek.d + NA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_5.d == 1)) %>% summary()
lmer(NA_agg ~ Discrim_6 + DayWeek.d + NA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_6.d == 1)) %>% summary()
lmer(NA_agg ~ Discrim_7 + DayWeek.d + NA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_7.d == 1)) %>% summary()

# PA
m1 = lmer(PA_agg ~ Discrim_1.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(PA_agg ~ Discrim_2.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(PA_agg ~ Discrim_3.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(PA_agg ~ Discrim_4.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(PA_agg ~ Discrim_5.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(PA_agg ~ Discrim_6.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(PA_agg ~ Discrim_7.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(PA_agg ~ Discrim_all.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(PA_agg ~ Discrim_all_norum.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()

PA_sameday = data.frame(Model = "Discrim_1",
                           Estimate = m1$coefficients[2,1],
                           P = round(m1$coefficients[2,5], digits=3)) %>% 
  rbind(data.frame(Model = "Discrim_2",
                   Estimate = m2$coefficients[2,1],
                   P = round(m2$coefficients[2,5], digits=3))) %>% 
  rbind(data.frame(Model = "Discrim_3",
                   Estimate = m3$coefficients[2,1],
                   P = round(m3$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_4",
                   Estimate = m4$coefficients[2,1],
                   P = round(m4$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_5",
                   Estimate = m5$coefficients[2,1],
                   P = round(m5$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_6",
                   Estimate = m6$coefficients[2,1],
                   P = round(m6$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_7",
                   Estimate = m7$coefficients[2,1],
                   P = round(m7$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all",
                   Estimate = m8$coefficients[2,1],
                   P = round(m8$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all_norum",
                   Estimate = m9$coefficients[2,1],
                   P = round(m9$coefficients[2,5], digits=3)))

# graded response for days with discrim
lmer(PA_agg ~ Discrim_1 + DayWeek.d + PA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_1.d == 1)) %>% summary()
lmer(PA_agg ~ Discrim_2 + DayWeek.d + PA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_2.d == 1)) %>% summary()
lmer(PA_agg ~ Discrim_3 + DayWeek.d + PA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_3.d == 1)) %>% summary()
lmer(PA_agg ~ Discrim_4 + DayWeek.d + PA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_4.d == 1)) %>% summary()
lmer(PA_agg ~ Discrim_5 + DayWeek.d + PA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_5.d == 1)) %>% summary()
lmer(PA_agg ~ Discrim_6 + DayWeek.d + PA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_6.d == 1)) %>% summary()
lmer(PA_agg ~ Discrim_7 + DayWeek.d + PA_agg_prevDay + (1|SubID), data = filter(dat, Discrim_7.d == 1)) %>% summary()

# Anx
m1 = lmer(Anx_agg ~ Discrim_1.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(Anx_agg ~ Discrim_2.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(Anx_agg ~ Discrim_3.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(Anx_agg ~ Discrim_4.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(Anx_agg ~ Discrim_5.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(Anx_agg ~ Discrim_6.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(Anx_agg ~ Discrim_7.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(Anx_agg ~ Discrim_all.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(Anx_agg ~ Discrim_all_norum.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()

Anx_sameday = data.frame(Model = "Discrim_1",
                           Estimate = m1$coefficients[2,1],
                           P = round(m1$coefficients[2,5], digits=3)) %>% 
  rbind(data.frame(Model = "Discrim_2",
                   Estimate = m2$coefficients[2,1],
                   P = round(m2$coefficients[2,5], digits=3))) %>% 
  rbind(data.frame(Model = "Discrim_3",
                   Estimate = m3$coefficients[2,1],
                   P = round(m3$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_4",
                   Estimate = m4$coefficients[2,1],
                   P = round(m4$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_5",
                   Estimate = m5$coefficients[2,1],
                   P = round(m5$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_6",
                   Estimate = m6$coefficients[2,1],
                   P = round(m6$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_7",
                   Estimate = m7$coefficients[2,1],
                   P = round(m7$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all",
                   Estimate = m8$coefficients[2,1],
                   P = round(m8$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all_norum",
                   Estimate = m9$coefficients[2,1],
                   P = round(m9$coefficients[2,5], digits=3)))

# graded response for days with discrim
lmer(Anx_agg ~ Discrim_1 + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = filter(dat, Discrim_1.d == 1)) %>% summary()
lmer(Anx_agg ~ Discrim_2 + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = filter(dat, Discrim_2.d == 1)) %>% summary()
lmer(Anx_agg ~ Discrim_3 + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = filter(dat, Discrim_3.d == 1)) %>% summary()
lmer(Anx_agg ~ Discrim_4 + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = filter(dat, Discrim_4.d == 1)) %>% summary()
lmer(Anx_agg ~ Discrim_5 + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = filter(dat, Discrim_5.d == 1)) %>% summary()
lmer(Anx_agg ~ Discrim_6 + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = filter(dat, Discrim_6.d == 1)) %>% summary()
lmer(Anx_agg ~ Discrim_7 + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = filter(dat, Discrim_7.d == 1)) %>% summary()

# Dep
m1 = lmer(Dep_agg ~ Discrim_1.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(Dep_agg ~ Discrim_2.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(Dep_agg ~ Discrim_3.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(Dep_agg ~ Discrim_4.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(Dep_agg ~ Discrim_5.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(Dep_agg ~ Discrim_6.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(Dep_agg ~ Discrim_7.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(Dep_agg ~ Discrim_all.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(Dep_agg ~ Discrim_all_norum.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()

Dep_sameday = data.frame(Model = "Discrim_1",
                           Estimate = m1$coefficients[2,1],
                           P = round(m1$coefficients[2,5], digits=3)) %>% 
  rbind(data.frame(Model = "Discrim_2",
                   Estimate = m2$coefficients[2,1],
                   P = round(m2$coefficients[2,5], digits=3))) %>% 
  rbind(data.frame(Model = "Discrim_3",
                   Estimate = m3$coefficients[2,1],
                   P = round(m3$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_4",
                   Estimate = m4$coefficients[2,1],
                   P = round(m4$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_5",
                   Estimate = m5$coefficients[2,1],
                   P = round(m5$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_6",
                   Estimate = m6$coefficients[2,1],
                   P = round(m6$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_7",
                   Estimate = m7$coefficients[2,1],
                   P = round(m7$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all",
                   Estimate = m8$coefficients[2,1],
                   P = round(m8$coefficients[2,5], digits=3))) %>%
  rbind(data.frame(Model = "Discrim_all_norum",
                   Estimate = m9$coefficients[2,1],
                   P = round(m9$coefficients[2,5], digits=3)))

# graded response for days with discrim
lmer(Dep_agg ~ Discrim_1 + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = filter(dat, Discrim_1.d == 1)) %>% summary()
lmer(Dep_agg ~ Discrim_2 + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = filter(dat, Discrim_2.d == 1)) %>% summary()
lmer(Dep_agg ~ Discrim_3 + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = filter(dat, Discrim_3.d == 1)) %>% summary()
lmer(Dep_agg ~ Discrim_4 + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = filter(dat, Discrim_4.d == 1)) %>% summary()
lmer(Dep_agg ~ Discrim_5 + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = filter(dat, Discrim_5.d == 1)) %>% summary()
lmer(Dep_agg ~ Discrim_6 + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = filter(dat, Discrim_6.d == 1)) %>% summary()
lmer(Dep_agg ~ Discrim_7 + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = filter(dat, Discrim_7.d == 1)) %>% summary()

# model summaries
NA_sameday
PA_sameday
Anx_sameday
Dep_sameday

