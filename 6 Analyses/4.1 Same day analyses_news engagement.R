library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)

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


# Separate between/within ---------------------------------------------------------------------

# Make subject means
dat = dat %>% 
  group_by(SubID) %>% 
  summarise(DailyNews.between = mean(DailyNews)) %>% 
  right_join(dat, by="SubID") %>% 
  as.data.frame()

# Make subject-centered variable
dat$DailyNews.within = dat$DailyNews - dat$DailyNews.between

# Effect of news consumption on same day ------------------------------------------------------

# NA
m1 = lmer(NA_agg ~ DailyNews.between + DailyNews.within + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat)
summary(m1)
confint(m1)
rest1 = lmer(NA_agg ~ DailyNews.within + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat)
rest2 = lmer(NA_agg ~ DailyNews.between + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat)
#effect size (between)
(r.squaredGLMM(m1)[1] - r.squaredGLMM(rest1)[1])/(1-r.squaredGLMM(m1)[1]) # fixed
(r.squaredGLMM(m1)[2] - r.squaredGLMM(rest1)[2])/(1-r.squaredGLMM(m1)[2]) # fixed and random
#effect size (within)
(r.squaredGLMM(m1)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(m1)[1]) # fixed
(r.squaredGLMM(m1)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(m1)[2]) # fixed and random

# PA
m2 = lmer(PA_agg ~ DailyNews.between + DailyNews.within + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat)
summary(m2)
confint(m2)
rest1 = lmer(PA_agg ~ DailyNews.within + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat)
rest2 = lmer(PA_agg ~ DailyNews.between + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat)
#effect size (between)
(r.squaredGLMM(m2)[1] - r.squaredGLMM(rest1)[1])/(1-r.squaredGLMM(m1)[1]) # fixed
(r.squaredGLMM(m2)[2] - r.squaredGLMM(rest1)[2])/(1-r.squaredGLMM(m1)[2]) # fixed and random
#effect size (within)
(r.squaredGLMM(m2)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(m1)[1]) # fixed
(r.squaredGLMM(m2)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(m1)[2]) # fixed and random

# Anx
m3 = lmer(Anx_agg ~ DailyNews.between + DailyNews.within + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat)
summary(m3)
confint(m3)
rest2 = lmer(Anx_agg ~ DailyNews.between + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat)
#effect size (within)
(r.squaredGLMM(m3)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(m1)[1]) # fixed
(r.squaredGLMM(m3)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(m1)[2]) # fixed and random

# Dep
m4 = lmer(Dep_agg ~ DailyNews.between + DailyNews.within + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat)
summary(m4)
confint(m4)
rest1 = lmer(Dep_agg ~ DailyNews.within + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat)
rest2 = lmer(Dep_agg ~ DailyNews.between + DayWeek.d + Dep_agg_prevDay + (1|SubID), data = dat)
#effect size (between)
(r.squaredGLMM(m4)[1] - r.squaredGLMM(rest1)[1])/(1-r.squaredGLMM(m4)[1]) # fixed
(r.squaredGLMM(m4)[2] - r.squaredGLMM(rest1)[2])/(1-r.squaredGLMM(m4)[2]) # fixed and random
#effect size (within)
(r.squaredGLMM(m4)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(m4)[1]) # fixed
(r.squaredGLMM(m4)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(m4)[2]) # fixed and random

# Effect of news consumption on next day ------------------------------------------------------

# NA
lmer(NA_agg_nextDay ~ DailyNews.within + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()

# PA
m2.nd = lmer(PA_agg_nextDay ~ DailyNews.within + DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat)
summary(m2.nd)
confint(m2.nd)
rest2 = lmer(PA_agg_nextDay ~ DayWeek.d + PA_agg_prevDay + (1|SubID), data = dat)
#effect size (within)
(r.squaredGLMM(m2.nd)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(m4)[1]) # fixed
(r.squaredGLMM(m2.nd)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(m4)[2]) # fixed and random

# Anx
lmer(Anx_agg_nextDay ~ DailyNews.within + DayWeek.d + Anx_agg_prevDay + (1|SubID), data = dat) %>% summary()

# Dep
lmer(Dep_agg_nextDay ~ DailyNews.within + DayWeek.d + NA_agg_prevDay + (1|SubID), data = dat) %>% summary()
