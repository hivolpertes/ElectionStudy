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


# Effect of discrim on next day ---------------------------------------------------------------------------------

# NA
m1 = lmer(NA_agg_nextDay ~ Discrim_1.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(NA_agg_nextDay ~ Discrim_2.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(NA_agg_nextDay ~ Discrim_3.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(NA_agg_nextDay ~ Discrim_4.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(NA_agg_nextDay ~ Discrim_5.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(NA_agg_nextDay ~ Discrim_6.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(NA_agg_nextDay ~ Discrim_7.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(NA_agg_nextDay ~ Discrim_all.d + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(NA_agg_nextDay ~ Discrim_all_norum.d + DayWeek.d + Na_agg_prevDay + (1|SubID), data = dat) %>% summary()

NA_nextday = data.frame(Model = "Discrim_1",
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

# PA
m1 = lmer(PA_agg_nextDay ~ Discrim_1.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(PA_agg_nextDay ~ Discrim_2.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(PA_agg_nextDay ~ Discrim_3.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(PA_agg_nextDay ~ Discrim_4.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(PA_agg_nextDay ~ Discrim_5.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(PA_agg_nextDay ~ Discrim_6.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(PA_agg_nextDay ~ Discrim_7.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(PA_agg_nextDay ~ Discrim_all.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(PA_agg_nextDay ~ Discrim_all_norum.d + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat) %>% summary()

PA_nextday = data.frame(Model = "Discrim_1",
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

# Anx
m1 = lmer(Anx_agg_nextDay ~ Discrim_1.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(Anx_agg_nextDay ~ Discrim_2.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(Anx_agg_nextDay ~ Discrim_3.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(Anx_agg_nextDay ~ Discrim_4.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(Anx_agg_nextDay ~ Discrim_5.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(Anx_agg_nextDay ~ Discrim_6.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(Anx_agg_nextDay ~ Discrim_7.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(Anx_agg_nextDay ~ Discrim_all.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(Anx_agg_nextDay ~ Discrim_all_norum.d + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()

Anx_nextday = data.frame(Model = "Discrim_1",
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

# Dep
m1 = lmer(Dep_agg_nextDay ~ Discrim_1.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m2 = lmer(Dep_agg_nextDay ~ Discrim_2.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m3 = lmer(Dep_agg_nextDay ~ Discrim_3.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m4 = lmer(Dep_agg_nextDay ~ Discrim_4.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m5 = lmer(Dep_agg_nextDay ~ Discrim_5.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m6 = lmer(Dep_agg_nextDay ~ Discrim_6.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m7 = lmer(Dep_agg_nextDay ~ Discrim_7.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m8 = lmer(Dep_agg_nextDay ~ Discrim_all.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()
m9 = lmer(Dep_agg_nextDay ~ Discrim_all_norum.d + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat) %>% summary()

Dep_nextday = data.frame(Model = "Discrim_1",
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

# model summaries
NA_nextday
PA_nextday
Anx_nextday
Dep_nextday
