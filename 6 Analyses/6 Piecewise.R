library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)

dat = read.delim("./5 Clean data/ElectionStudy_MergedDailyDiary_clean_recodedDiscrim_laggedDays.txt")

# add variables for stages

# 3 stages:

# Stage 1 = 1-6
# Stage 2 = 7-10
# Stage 3 = 11-14

# A1-A3: Incremental slopes (not reported in manuscript)
dat$A1 = dat$DayStudy.adj - 1
dat$A2 = 0
dat$A2[dat$DayStudy.adj>7] = dat$DayStudy.adj[dat$DayStudy.adj>7] - 7
dat$A3 = 0
dat$A3[dat$DayStudy.adj>11] = dat$DayStudy.adj[dat$DayStudy.adj>11] - 11

# B1-B3: Separate slopes (reported in manuscript)
dat$B1 = dat$A1
dat$B1[dat$DayStudy.adj>7] = 6
dat$B2 = dat$A2
dat$B2[dat$DayStudy.adj>11] = 4
dat$B3 = dat$A3


# 1a. Fit models, no covariates -------------------------------------------------------------------------------

# NA
NA_mod_A = lmer(NA_agg ~ 1 + A1 + A2 + A3 + (1|SubID), data = dat) %>% 
  summary()
NA_mod_B = lmer(NA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% summary()
full = lmer(NA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat)
rest1 = lmer(NA_agg ~ 1 + B2 + B3 + (1|SubID), data = dat)
rest2 = lmer(NA_agg ~ 1 + B1 + B3 + (1|SubID), data = dat)
#effect size B1
(r.squaredGLMM(full)[1] - r.squaredGLMM(rest1)[1])/(1-r.squaredGLMM(full)[1]) # fixed
(r.squaredGLMM(full)[2] - r.squaredGLMM(rest1)[2])/(1-r.squaredGLMM(full)[2]) # fixed and random
#effect size B2
(r.squaredGLMM(full)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(full)[1]) # fixed
(r.squaredGLMM(full)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(full)[2]) # fixed and random
#CI
confint(full)

# PA
PA_mod_A = lmer(PA_agg ~ A1 + A2 + A3 + (1|SubID), data = dat) %>% 
  summary()
PA_mod_B = lmer(PA_agg ~ B1 + B2 + B3 + (1|SubID), data = dat) %>% 
  summary()
full = lmer(PA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat)
rest1 = lmer(PA_agg ~ 1 + B1 + B3 + (1|SubID), data = dat)
rest2 = lmer(PA_agg ~ 1 + B1 + B2 + (1|SubID), data = dat)
#effect size B2
(r.squaredGLMM(full)[1] - r.squaredGLMM(rest1)[1])/(1-r.squaredGLMM(full)[1]) # fixed
(r.squaredGLMM(full)[2] - r.squaredGLMM(rest1)[2])/(1-r.squaredGLMM(full)[2]) # fixed and random
#effect size B3
(r.squaredGLMM(full)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(full)[1]) # fixed
(r.squaredGLMM(full)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(full)[2]) # fixed and random
#CI
confint(full)

# Anx
Anx_mod_A = lmer(Anx_agg ~ A1 + A2 + A3 + (1|SubID), data = dat) %>% 
  summary()
Anx_mod_B = lmer(Anx_agg ~ B1 + B2 + B3 + (1|SubID), data = dat) %>% 
  summary()
full = lmer(Anx_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat)
rest1 = lmer(Anx_agg ~ 1 + B2 + B3 + (1|SubID), data = dat)
rest2 = lmer(Anx_agg ~ 1 + B1 + B3 + (1|SubID), data = dat)
#effect size B2
(r.squaredGLMM(full)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(full)[1]) # fixed
(r.squaredGLMM(full)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(full)[2]) # fixed and random
#CI
confint(full)

# Dep
Dep_mod_A = lmer(Dep_agg ~ A1 + A2 + A3 + (1|SubID), data = dat) %>% 
  summary()
Dep_mod_B = lmer(Dep_agg ~ B1 + B2 + B3 + (1|SubID), data = dat) %>% 
  summary()
full = lmer(Dep_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat)
rest1 = lmer(Dep_agg ~ 1 + B2 + B3 + (1|SubID), data = dat)
rest2 = lmer(Dep_agg ~ 1 + B1 + B3 + (1|SubID), data = dat)
#effect size B1
(r.squaredGLMM(full)[1] - r.squaredGLMM(rest1)[1])/(1-r.squaredGLMM(full)[1]) # fixed
(r.squaredGLMM(full)[2] - r.squaredGLMM(rest1)[2])/(1-r.squaredGLMM(full)[2]) # fixed and random
#effect size B2
(r.squaredGLMM(full)[1] - r.squaredGLMM(rest2)[1])/(1-r.squaredGLMM(full)[1]) # fixed
(r.squaredGLMM(full)[2] - r.squaredGLMM(rest2)[2])/(1-r.squaredGLMM(full)[2]) # fixed and random
#CI
confint(full)

# 1b. Plot, no covariates-------------------------------------------------------------------

# wide
wide = data.frame(Day = c(1:14),
                  NegAff = NA,
                  PosAff = NA,
                  Anx = NA,
                  Dep = NA)
# NA
wide$NegAff[wide$Day %in% c(1:6)] = 
  coef(NA_mod_A)[1,1] + # add intercept
  c(0:5)*coef(NA_mod_A)[2,1] # add stage 1 slope for days 1:6

wide$NegAff[wide$Day %in% c(7:10)] = 
  coef(NA_mod_A)[1,1] +  # add intercept
  c(6:9)*coef(NA_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(NA_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$NegAff[wide$Day %in% c(11:14)] = 
  coef(NA_mod_A)[1,1] +  # add intercept
  c(10:13)*coef(NA_mod_A)[2,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(NA_mod_A)[3,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(NA_mod_A)[4,1] # add stage 3 slope for days 11:14

# PA
wide$PosAff[wide$Day %in% c(1:6)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(0:5)*coef(PA_mod_A)[2,1] # add stage 1 slope for days 1:6

wide$PosAff[wide$Day %in% c(7:10)] = 
  coef(PA_mod_A)[1,1] +  # add intercept
  c(6:9)*coef(PA_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(PA_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$PosAff[wide$Day %in% c(11:14)] = 
  coef(PA_mod_A)[1,1] +  # add intercept
  c(10:13)*coef(PA_mod_A)[2,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(PA_mod_A)[3,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(PA_mod_A)[4,1] # add stage 3 slope for days 11:14

# Anx
wide$Anx[wide$Day %in% c(1:6)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Anx_mod_A)[2,1] # add stage 1 slope for days 1:6

wide$Anx[wide$Day %in% c(7:10)] = 
  coef(Anx_mod_A)[1,1] +  # add intercept
  c(6:9)*coef(Anx_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Anx_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$Anx[wide$Day %in% c(11:14)] = 
  coef(Anx_mod_A)[1,1] +  # add intercept
  c(10:13)*coef(Anx_mod_A)[2,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Anx_mod_A)[3,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Anx_mod_A)[4,1] # add stage 3 slope for days 11:14

# Dep
wide$Dep[wide$Day %in% c(1:6)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Dep_mod_A)[2,1] # add stage 1 slope for days 1:6

wide$Dep[wide$Day %in% c(7:10)] = 
  coef(Dep_mod_A)[1,1] +  # add intercept
  c(6:9)*coef(Dep_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Dep_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$Dep[wide$Day %in% c(11:14)] = 
  coef(Dep_mod_A)[1,1] +  # add intercept
  c(10:13)*coef(Dep_mod_A)[2,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Dep_mod_A)[3,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Dep_mod_A)[4,1] # add stage 3 slope for days 11:14

# plot wide
ggplot(wide, aes(Day, Dep)) +
  geom_point() +
  geom_line(group=1)

# wide to long format
long = gather(wide, facet, value, c(2:5))

# plot long

plot = 
  ggplot(long, aes(Day, value, color = facet)) +
  geom_point(size=3) +
  geom_line(size = 2) +
  geom_vline(xintercept=7) +
  geom_vline(xintercept=11)+
  scale_x_continuous(breaks=c(1:14)) +
  scale_y_continuous(limits = c(1, 3.5), breaks=c(1:4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

print(plot)

ggsave("./6 Analyses/3 Piecewise plots/PiecewisePlot_fullsample.jpg", plot, units="in", width=8, height=5.8)


# 2a. Fit models, voting intentions -----------------------------------------------------------------------

onboarding = read.delim("./5 Clean data/ElectionStudy_Onboarding_clean_fixedSubID_scalescores.txt")

dat = left_join(dat, select(onboarding, SubID, Voting.Intent_1), by="SubID")

# Voting intentions
# 1 = Trump
# 2 = Biden
# 3 = Other candidate
# 4 = Not voting
# 5 = Haven't decided

# Recode Trump vs all (Trump = 1, Other = 0) (reported in manuscript)
dat$Trump = 0
dat$Trump[dat$Voting.Intent_1 == 1] = 1
dat$Trump = factor(dat$Trump)

# Record Biden vs all (Biden = 1, Other = 0) (not reported in manuscript- see footnote)
dat$Biden = 0
dat$Biden[dat$Voting.Intent_1 == 2] = 1
dat$Biden = factor(dat$Biden)

# Trump vs. all -------------------------------------------------------------------------------

# NA
NA_tru_A = lmer(NA_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()
NA_tru_B = lmer(NA_agg ~ Trump + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat)

summary(NA_tru_B)
emtrends(NA_tru_B, pairwise ~ B1|Trump,  var="B1", adjust="none")$emtrends 
emtrends(NA_tru_B, pairwise ~ B2|Trump,  var="B2", adjust="none")$emtrends 
emtrends(NA_tru_B, pairwise ~ B3|Trump,  var="B3", adjust="none")$emtrends 

# PA
PA_tru_A = lmer(PA_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()
PA_tru_B = lmer(PA_agg ~ Trump + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat)

summary(PA_tru_B)
emtrends(PA_tru_B, pairwise ~ B1|Trump,  var="B1", adjust="none")$emtrends 
emtrends(PA_tru_B, pairwise ~ B2|Trump,  var="B2", adjust="none")$emtrends 
emtrends(PA_tru_B, pairwise ~ B3|Trump,  var="B3", adjust="none")$emtrends 

# Anx
Anx_tru_A = lmer(Anx_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()
Anx_tru_B = lmer(Anx_agg ~ Trump + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat)

summary(Anx_tru_B)
emtrends(Anx_tru_B, pairwise ~ B1|Trump,  var="B1", adjust="none")$emtrends 
emtrends(Anx_tru_B, pairwise ~ B2|Trump,  var="B2", adjust="none")$emtrends 
emtrends(Anx_tru_B, pairwise ~ B3|Trump,  var="B3", adjust="none")$emtrends 

# Dep
Dep_tru_A = lmer(Dep_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()
Dep_tru_B = lmer(Dep_agg ~ Trump + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat)

summary(Dep_tru_B)
emtrends(Dep_tru_B, pairwise ~ B1|Trump,  var="B1", adjust="none")$emtrends 
emtrends(Dep_tru_B, pairwise ~ B2|Trump,  var="B2", adjust="none")$emtrends 
emtrends(Dep_tru_B, pairwise ~ B3|Trump,  var="B3", adjust="none")$emtrends 

# effect sizes (Stage 1)
# NA
full = lmer(NA_agg ~ 1 + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(NA_agg ~ 1 + Trump + B1 + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed + random effects)

# PA
full = lmer(PA_agg ~ 1 + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(PA_agg ~ 1 + B1 + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed + random effects)

# Anx
full = lmer(Anx_agg ~ 1 + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Anx_agg ~ 1 + B1 + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed + random effects)

# Dep
full = lmer(Dep_agg ~ 1 + B1*Trump + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Dep_agg ~ 1 + B1 + B2*Trump + B3*Trump + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed + random effects)

# Biden vs. all -------------------------------------------------------------------------------

# NA
NA_bid_A = lmer(NA_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()
NA_bid_B = lmer(NA_agg ~ Biden + B1*Biden + B2*Biden + B3*Biden + (1|SubID), data = dat)

summary(NA_bid_B)
emtrends(NA_bid_B, pairwise ~ B1|Biden,  var="B1", adjust="none")$emtrends
emtrends(NA_bid_B, pairwise ~ B2|Biden,  var="B2", adjust="none")$emtrends 
emtrends(NA_bid_B, pairwise ~ B3|Biden,  var="B3", adjust="none")$emtrends 

# PA
PA_bid_A = lmer(PA_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()
PA_bid_B = lmer(PA_agg ~ Biden + B1*Biden + B2*Biden + B3*Biden + (1|SubID), data = dat)

summary(PA_bid_B)
emtrends(PA_bid_B, pairwise ~ B1|Biden,  var="B1", adjust="none")$emtrends
emtrends(PA_bid_B, pairwise ~ B2|Biden,  var="B2", adjust="none")$emtrends 
emtrends(PA_bid_B, pairwise ~ B3|Biden,  var="B3", adjust="none")$emtrends 

# Anx
Anx_bid_A = lmer(Anx_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()
Anx_bid_B = lmer(Anx_agg ~ Biden + B1*Biden + B2*Biden + B3*Biden + (1|SubID), data = dat)

summary(Anx_bid_B)
emtrends(Anx_bid_B, pairwise ~ B1|Biden,  var="B1", adjust="none")$emtrends
emtrends(Anx_bid_B, pairwise ~ B2|Biden,  var="B2", adjust="none")$emtrends 
emtrends(Anx_bid_B, pairwise ~ B3|Biden,  var="B3", adjust="none")$emtrends 

# Dep
Dep_bid_A = lmer(Dep_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()
Dep_bid_B = lmer(Dep_agg ~ Biden + B1*Biden + B2*Biden + B3*Biden + (1|SubID), data = dat)

summary(Dep_bid_B)
emtrends(Dep_bid_B, pairwise ~ B1|Biden,  var="B1", adjust="none")$emtrends
emtrends(Dep_bid_B, pairwise ~ B2|Biden,  var="B2", adjust="none")$emtrends 
emtrends(Dep_bid_B, pairwise ~ B3|Biden,  var="B3", adjust="none")$emtrends 

# 2b. Plot, voting intentions -------------------------------------------------------------

# Trump vs. all -------------------------------------------------------------------------------

# wide
wide.Trump = data.frame(Day = c(1:14),
                     NegAff.Trump = NA,
                     PosAff.Trump = NA,
                     Anx.Trump = NA,
                     Dep.Trump = NA,
                     
                     NegAff.Other = NA,
                     PosAff.Other = NA,
                     Anx.Other = NA,
                     Dep.Other = NA)
# NA (Other)
wide.Trump$NegAff.Other[wide.Trump$Day %in% c(1:6)] = 
  coef(NA_tru_A)[1,1] + # add intercept
  c(0:5)*coef(NA_tru_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$NegAff.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(NA_tru_A)[1,1] + # add intercept
  c(6:9)*coef(NA_tru_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(NA_tru_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$NegAff.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(NA_tru_A)[1,1] + # add intercept
  c(10:13)*coef(NA_tru_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(NA_tru_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(NA_tru_A)[5,1] # add stage 3 slope for days 11:14

# NA (Trump)
wide.Trump$NegAff.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(NA_tru_A)[1,1] + coef(NA_tru_A)[2,1] + # add intercept
  c(0:5)*(coef(NA_tru_A)[3,1]+coef(NA_tru_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$NegAff.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(NA_tru_A)[1,1] + coef(NA_tru_A)[2,1] + # add intercept
  c(6:9)*(coef(NA_tru_A)[3,1]+coef(NA_tru_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(NA_tru_A)[4,1]+coef(NA_tru_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$NegAff.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(NA_tru_A)[1,1] + coef(NA_tru_A)[2,1] + # add intercept
  c(10:13)*(coef(NA_tru_A)[3,1]+coef(NA_tru_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(NA_tru_A)[4,1]+coef(NA_tru_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(NA_tru_A)[5,1]+coef(NA_tru_A)[8,1]) # add stage 3 slope for days 11:14

# PA (Other)
wide.Trump$PosAff.Other[wide.Trump$Day %in% c(1:6)] = 
  coef(PA_tru_A)[1,1] + # add intercept
  c(0:5)*coef(PA_tru_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$PosAff.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(PA_tru_A)[1,1] + # add intercept
  c(6:9)*coef(PA_tru_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(PA_tru_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$PosAff.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(PA_tru_A)[1,1] + # add intercept
  c(10:13)*coef(PA_tru_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(PA_tru_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(PA_tru_A)[5,1] # add stage 3 slope for days 11:14

# PA (Trump)
wide.Trump$PosAff.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(PA_tru_A)[1,1] + coef(PA_tru_A)[2,1] + # add intercept
  c(0:5)*(coef(PA_tru_A)[3,1]+coef(PA_tru_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$PosAff.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(PA_tru_A)[1,1] + coef(PA_tru_A)[2,1] + # add intercept
  c(6:9)*(coef(PA_tru_A)[3,1]+coef(PA_tru_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(PA_tru_A)[4,1]+coef(PA_tru_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$PosAff.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(PA_tru_A)[1,1] + coef(PA_tru_A)[2,1] + # add intercept
  c(10:13)*(coef(PA_tru_A)[3,1]+coef(PA_tru_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(PA_tru_A)[4,1]+coef(PA_tru_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(PA_tru_A)[5,1]+coef(PA_tru_A)[8,1]) # add stage 3 slope for days 11:14

# Anx (Other)
wide.Trump$Anx.Other[wide.Trump$Day %in% c(1:6)] = 
  coef(Anx_tru_A)[1,1] + # add intercept
  c(0:5)*coef(Anx_tru_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$Anx.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(Anx_tru_A)[1,1] + # add intercept
  c(6:9)*coef(Anx_tru_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Anx_tru_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$Anx.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(Anx_tru_A)[1,1] + # add intercept
  c(10:13)*coef(Anx_tru_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Anx_tru_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Anx_tru_A)[5,1] # add stage 3 slope for days 11:14

# Anx (Trump)
wide.Trump$Anx.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(Anx_tru_A)[1,1] + coef(Anx_tru_A)[2,1] + # add intercept
  c(0:5)*(coef(Anx_tru_A)[3,1]+coef(Anx_tru_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$Anx.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(Anx_tru_A)[1,1] + coef(Anx_tru_A)[2,1] + # add intercept
  c(6:9)*(coef(Anx_tru_A)[3,1]+coef(Anx_tru_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Anx_tru_A)[4,1]+coef(Anx_tru_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$Anx.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(Anx_tru_A)[1,1] + coef(Anx_tru_A)[2,1] + # add intercept
  c(10:13)*(coef(Anx_tru_A)[3,1]+coef(Anx_tru_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Anx_tru_A)[4,1]+coef(Anx_tru_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Anx_tru_A)[5,1]+coef(Anx_tru_A)[8,1]) # add stage 3 slope for days 11:14

# Dep (Other)
wide.Trump$Dep.Other[wide.Trump$Day %in% c(1:6)] = 
  coef(Dep_tru_A)[1,1] + # add intercept
  c(0:5)*coef(Dep_tru_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$Dep.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(Dep_tru_A)[1,1] + # add intercept
  c(6:9)*coef(Dep_tru_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Dep_tru_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$Dep.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(Dep_tru_A)[1,1] + # add intercept
  c(10:13)*coef(Dep_tru_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Dep_tru_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Dep_tru_A)[5,1] # add stage 3 slope for days 11:14

# Dep (Trump)
wide.Trump$Dep.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(Dep_tru_A)[1,1] + coef(Dep_tru_A)[2,1] + # add intercept
  c(0:5)*(coef(Dep_tru_A)[3,1]+coef(Dep_tru_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$Dep.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(Dep_tru_A)[1,1] + coef(Dep_tru_A)[2,1] + # add intercept
  c(6:9)*(coef(Dep_tru_A)[3,1]+coef(Dep_tru_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Dep_tru_A)[4,1]+coef(Dep_tru_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$Dep.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(Dep_tru_A)[1,1] + coef(Dep_tru_A)[2,1] + # add intercept
  c(10:13)*(coef(Dep_tru_A)[3,1]+coef(Dep_tru_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Dep_tru_A)[4,1]+coef(Dep_tru_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Dep_tru_A)[5,1]+coef(Dep_tru_A)[8,1]) # add stage 3 slope for days 11:14

# gather wide to long

long.Trump = gather(wide.Trump, type, value, 2:9)

long.Trump$VI = NA
long.Trump$VI[grep("Trump", long.Trump$type)] = "Trump"
long.Trump$VI[grep("Other", long.Trump$type)] = "Other"

long.Trump$facet = NA
long.Trump$facet[grep("NegAff", long.Trump$type)] = "Neg. Affect"
long.Trump$facet[grep("PosAff", long.Trump$type)] = "Pos. Affect"
long.Trump$facet[grep("Anx", long.Trump$type)] = "Anxiety"
long.Trump$facet[grep("Dep", long.Trump$type)] = "Depression"

# plot

TrumpVIplot = 
  ggplot(long.Trump, aes(Day, value, color = facet)) +
  facet_wrap(~VI) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept=7) +
  geom_vline(xintercept=11)+
  scale_x_continuous(breaks=c(1:14)) +
  scale_y_continuous(limits = c(1, 3.5), breaks=c(1:4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16)) 

ggsave("./6 Analyses/3 Piecewise plots/PiecewisePlot_VIsplit_Trump.jpg", TrumpVIplot, units="in", width=10, height=6)

TrumpEmoplot = 
  ggplot(long.Trump, aes(Day, value, color = VI)) +
  facet_wrap(~facet) +
  geom_point(size=2.5) +
  geom_line(size=1.5) +
  geom_vline(xintercept=7) +
  geom_vline(xintercept=11)+
  scale_x_continuous(breaks=c(1:14)) +
  scale_y_continuous(limits = c(1, 3.5), breaks=c(1:4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 18)) +
  scale_color_manual(values=c("black", "gray60"))

print(TrumpEmoplot)

ggsave("./6 Analyses/3 Piecewise plots/PiecewisePlot_Emosplit_Trump.jpg", TrumpEmoplot, units="in", width=10, height=6)



# Biden vs. all -------------------------------------------------------------

# wide
wide.Biden = data.frame(Day = c(1:14),
                        NegAff.Biden = NA,
                        PosAff.Biden = NA,
                        Anx.Biden = NA,
                        Dep.Biden = NA,
                        
                        NegAff.Other = NA,
                        PosAff.Other = NA,
                        Anx.Other = NA,
                        Dep.Other = NA)
# NA (Other)
wide.Biden$NegAff.Other[wide.Biden$Day %in% c(1:6)] = 
  coef(NA_bid_A)[1,1] + # add intercept
  c(0:5)*coef(NA_bid_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$NegAff.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(NA_bid_A)[1,1] + # add intercept
  c(6:9)*coef(NA_bid_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(NA_bid_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$NegAff.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(NA_bid_A)[1,1] + # add intercept
  c(10:13)*coef(NA_bid_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(NA_bid_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(NA_bid_A)[5,1] # add stage 3 slope for days 11:14

# NA (Biden)
wide.Biden$NegAff.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(NA_bid_A)[1,1] + coef(NA_bid_A)[2,1] + # add intercept
  c(0:5)*(coef(NA_bid_A)[3,1]+coef(NA_bid_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$NegAff.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(NA_bid_A)[1,1] + coef(NA_bid_A)[2,1] + # add intercept
  c(6:9)*(coef(NA_bid_A)[3,1]+coef(NA_bid_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(NA_bid_A)[4,1]+coef(NA_bid_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$NegAff.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(NA_bid_A)[1,1] + coef(NA_bid_A)[2,1] + # add intercept
  c(10:13)*(coef(NA_bid_A)[3,1]+coef(NA_bid_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(NA_bid_A)[4,1]+coef(NA_bid_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(NA_bid_A)[5,1]+coef(NA_bid_A)[8,1]) # add stage 3 slope for days 11:14

# PA (Other)
wide.Biden$PosAff.Other[wide.Biden$Day %in% c(1:6)] = 
  coef(PA_bid_A)[1,1] + # add intercept
  c(0:5)*coef(PA_bid_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$PosAff.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(PA_bid_A)[1,1] + # add intercept
  c(6:9)*coef(PA_bid_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(PA_bid_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$PosAff.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(PA_bid_A)[1,1] + # add intercept
  c(10:13)*coef(PA_bid_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(PA_bid_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(PA_bid_A)[5,1] # add stage 3 slope for days 11:14

# PA (Biden)
wide.Biden$PosAff.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(PA_bid_A)[1,1] + coef(PA_bid_A)[2,1] + # add intercept
  c(0:5)*(coef(PA_bid_A)[3,1]+coef(PA_bid_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$PosAff.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(PA_bid_A)[1,1] + coef(PA_bid_A)[2,1] + # add intercept
  c(6:9)*(coef(PA_bid_A)[3,1]+coef(PA_bid_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(PA_bid_A)[4,1]+coef(PA_bid_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$PosAff.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(PA_bid_A)[1,1] + coef(PA_bid_A)[2,1] + # add intercept
  c(10:13)*(coef(PA_bid_A)[3,1]+coef(PA_bid_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(PA_bid_A)[4,1]+coef(PA_bid_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(PA_bid_A)[5,1]+coef(PA_bid_A)[8,1]) # add stage 3 slope for days 11:14

# Anx (Other)
wide.Biden$Anx.Other[wide.Biden$Day %in% c(1:6)] = 
  coef(Anx_bid_A)[1,1] + # add intercept
  c(0:5)*coef(Anx_bid_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$Anx.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(Anx_bid_A)[1,1] + # add intercept
  c(6:9)*coef(Anx_bid_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Anx_bid_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$Anx.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(Anx_bid_A)[1,1] + # add intercept
  c(10:13)*coef(Anx_bid_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Anx_bid_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Anx_bid_A)[5,1] # add stage 3 slope for days 11:14

# Anx (Biden)
wide.Biden$Anx.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(Anx_bid_A)[1,1] + coef(Anx_bid_A)[2,1] + # add intercept
  c(0:5)*(coef(Anx_bid_A)[3,1]+coef(Anx_bid_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$Anx.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(Anx_bid_A)[1,1] + coef(Anx_bid_A)[2,1] + # add intercept
  c(6:9)*(coef(Anx_bid_A)[3,1]+coef(Anx_bid_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Anx_bid_A)[4,1]+coef(Anx_bid_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$Anx.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(Anx_bid_A)[1,1] + coef(Anx_bid_A)[2,1] + # add intercept
  c(10:13)*(coef(Anx_bid_A)[3,1]+coef(Anx_bid_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Anx_bid_A)[4,1]+coef(Anx_bid_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Anx_bid_A)[5,1]+coef(Anx_bid_A)[8,1]) # add stage 3 slope for days 11:14

# Dep (Other)
wide.Biden$Dep.Other[wide.Biden$Day %in% c(1:6)] = 
  coef(Dep_bid_A)[1,1] + # add intercept
  c(0:5)*coef(Dep_bid_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$Dep.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(Dep_bid_A)[1,1] + # add intercept
  c(6:9)*coef(Dep_bid_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Dep_bid_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$Dep.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(Dep_bid_A)[1,1] + # add intercept
  c(10:13)*coef(Dep_bid_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Dep_bid_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Dep_bid_A)[5,1] # add stage 3 slope for days 11:14

# Dep (Biden)
wide.Biden$Dep.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(Dep_bid_A)[1,1] + coef(Dep_bid_A)[2,1] + # add intercept
  c(0:5)*(coef(Dep_bid_A)[3,1]+coef(Dep_bid_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$Dep.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(Dep_bid_A)[1,1] + coef(Dep_bid_A)[2,1] + # add intercept
  c(6:9)*(coef(Dep_bid_A)[3,1]+coef(Dep_bid_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Dep_bid_A)[4,1]+coef(Dep_bid_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$Dep.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(Dep_bid_A)[1,1] + coef(Dep_bid_A)[2,1] + # add intercept
  c(10:13)*(coef(Dep_bid_A)[3,1]+coef(Dep_bid_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Dep_bid_A)[4,1]+coef(Dep_bid_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Dep_bid_A)[5,1]+coef(Dep_bid_A)[8,1]) # add stage 3 slope for days 11:14

# gather wide to long

long.Biden = gather(wide.Biden, type, value, 2:9)

long.Biden$VI = NA
long.Biden$VI[grep("Biden", long.Biden$type)] = "Biden"
long.Biden$VI[grep("Other", long.Biden$type)] = "Other"

long.Biden$facet = NA
long.Biden$facet[grep("NegAff", long.Biden$type)] = "NegAff"
long.Biden$facet[grep("PosAff", long.Biden$type)] = "PosAff"
long.Biden$facet[grep("Anx", long.Biden$type)] = "Anx"
long.Biden$facet[grep("Dep", long.Biden$type)] = "Dep"

# plot

BidenVIplot = 
  ggplot(long.Biden, aes(Day, value, color = facet)) +
  facet_wrap(~VI) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept=7) +
  geom_vline(xintercept=11)+
  scale_x_continuous(breaks=c(1:14)) +
  scale_y_continuous(limits = c(1, 3.5), breaks=c(1:4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16))

ggsave("./6 Daily diary/3 Piecewise plots/PiecewisePlot_VIsplit_Biden.jpg", BidenVIplot, units="in", width=10, height=6)

BidenEmoplot = 
  ggplot(long.Biden, aes(Day, value, color = VI)) +
  facet_wrap(~facet) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept=7) +
  geom_vline(xintercept=11)+
  scale_x_continuous(breaks=c(1:14)) +
  scale_y_continuous(limits = c(1, 3.5), breaks=c(1:4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16))

ggsave("./6 Daily diary/3 Piecewise plots/PiecewisePlot_Emosplit_Biden.jpg", BidenEmoplot, units="in", width=10, height=6)


# 3a. Fit models, individual diff (full sample) ------------------------------------------

# add individual difference variables from onboarding
dat = left_join(dat, select(onboarding, SubID, MEIM_mean, MEIM_exp_mean, MEIM_com_mean, Nativity,
                            ICE_1, ICE_2, PHQ_sum, GAD_sum, MACVS_mean, MACVS_sup_mean, MACVS_ob_mean,
                            MACVS_ref_mean), by="SubID")
# Nativity -----------------------------------------------------------------------------

# NA
lmer(NA_agg ~ 1 + B1*Nativity + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Anx
lmer(Anx_agg ~ 1 + B1*Nativity + B2 + B3 + (1|SubID), data = dat) %>% summary()
confint(lmer(Anx_agg ~ 1 + B1*Nativity + B2 + B3 + (1|SubID), data = dat))
# Dep
lmer(Dep_agg ~ 1 + B1*Nativity + B2 + B3 + (1|SubID), data = dat) %>% summary()


# effect sizes
# NA
full = lmer(NA_agg ~ 1 + B1*Nativity + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(NA_agg ~ 1 + B1+Nativity + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed+random effects)
# Anx
full = lmer(Anx_agg ~ 1 + B1*Nativity + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Anx_agg ~ 1 + B1+Nativity + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed+random effects)
# Dep
full = lmer(Dep_agg ~ 1 + B1*Nativity + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Dep_agg ~ 1 + B1+Nativity + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed+random effects)

# Ethnic identity -----------------------------------------------------------------------------

# NA
lmer(NA_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
confint(lmer(NA_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat))

# Anx
lmer(Anx_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Dep
lmer(Dep_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
confint(lmer(Dep_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat))

# effect sizes
# NA
full = lmer(NA_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(NA_agg ~ 1 + B1+scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed+random effects)
# Anx
full = lmer(Anx_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Anx_agg ~ 1 + B1+scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed+random effects)
# Dep
full = lmer(Dep_agg ~ 1 + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Dep_agg ~ 1 + B1+scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[2] - rest[2])/(1-full[2]) # using marginal R^2 (fixed+random effects)

# Ethnic identity (plot) ----------------------------------------------------------

MEIM_mod_NA = lmer(NA_agg ~ scale(MEIM_mean) + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
estNA = MEIM_mod_NA$coefficients

MEIM_mod_PA = lmer(PA_agg ~ scale(MEIM_mean) + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
estPA = MEIM_mod_PA$coefficients

MEIM_mod_Anx = lmer(Anx_agg ~ scale(MEIM_mean) + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
estAnx = MEIM_mod_Anx$coefficients

MEIM_mod_Dep = lmer(Dep_agg ~ scale(MEIM_mean) + B1*scale(MEIM_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
estDep = MEIM_mod_Dep$coefficients

# compile intercept and slope values for +1 SD and -1 SD of MEIM
plotMEIM = rbind(
  data.frame(facet = "NegAff", SD = "-1SD",
             Intercept = estNA[1,1] + estNA[2,1]*-1,
             Slope = estNA[3,1] + estNA[6,1]*-1),
  data.frame(facet = "NegAff", SD = "+1SD",
             Intercept = estNA[1,1] + estNA[2,1]*1,
             Slope = estNA[3,1] + estNA[6,1]*1),  
  data.frame(facet = "PosAff", SD = "-1SD",
             Intercept = estPA[1,1] + estPA[2,1]*-1,
             Slope = estPA[3,1] + estPA[6,1]*-1),
  data.frame(facet = "PosAff", SD = "+1SD",
             Intercept = estPA[1,1] + estPA[2,1]*1,
             Slope = estPA[3,1] + estPA[6,1]*1),
  data.frame(facet = "Anx", SD = "-1SD",
             Intercept = estAnx[1,1] + estAnx[2,1]*-1,
             Slope = estAnx[3,1] + estAnx[6,1]*-1),
  data.frame(facet = "Anx", SD = "+1SD",
             Intercept = estAnx[1,1] + estAnx[2,1]*1,
             Slope = estAnx[3,1] + estAnx[6,1]*1),
  data.frame(facet = "Dep", SD = "-1SD",
             Intercept = estDep[1,1] + estDep[2,1]*-1,
             Slope = estDep[3,1] + estDep[6,1]*-1),
  data.frame(facet = "Dep", SD = "+1SD",
             Intercept = estDep[1,1] + estDep[2,1]*1,
             Slope = estDep[3,1] + estDep[6,1]*1)
)

plotMEIMvalues = NULL
for (f in unique(plotMEIM$facet)) {
  for (s in unique(plotMEIM$SD)) {
    frame = data.frame(Day = c(1:7))
    frame$facet = f
    frame$SD = s
    frame$value = plotMEIM$Intercept[plotMEIM$facet == f & plotMEIM$SD == s] +
      (frame$Day-1)*plotMEIM$Slope[plotMEIM$facet == f & plotMEIM$SD == s]
    plotMEIMvalues = rbind(plotMEIMvalues, frame)
  }
}

# plot
MEIMplot = 
  ggplot(plotMEIMvalues, aes(Day, value, color = SD)) +
  facet_wrap(~facet) +
  geom_line(size=1) +
  scale_x_continuous(breaks=c(0:7)) +
  scale_y_continuous(limits = c(1, 3.5), breaks=c(1:4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16))

print(MEIMplot)

ggsave("./6 Daily diary/3 Piecewise plots/Stage1_Emosplit_EthID.jpg", MEIMplot, units="in", width=10, height=6)



# Familism -----------------------------------------------------------------------------

# NA
lmer(NA_agg ~ 1 + B1*scale(MACVS_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(NA_agg ~ 1 + B1*scale(MACVS_sup_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(NA_agg ~ 1 + B1*scale(MACVS_ob_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(NA_agg ~ 1 + B1*scale(MACVS_ref_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Anx
lmer(Anx_agg ~ 1 + B1*scale(MACVS_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(Anx_agg ~ 1 + B1*scale(MACVS_sup_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
confint(lmer(Anx_agg ~ 1 + B1*scale(MACVS_sup_mean) + B2 + B3 + (1|SubID), data = dat))
lmer(Anx_agg ~ 1 + B1*scale(MACVS_ob_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(Anx_agg ~ 1 + B1*scale(MACVS_ref_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Dep
lmer(Dep_agg ~ 1 + B1*scale(MACVS_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(Dep_agg ~ 1 + B1*scale(MACVS_sup_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(Dep_agg ~ 1 + B1*scale(MACVS_ob_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()
lmer(Dep_agg ~ 1 + B1*scale(MACVS_ref_mean) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# effect sizes
# NA
full = lmer(NA_agg ~ 1 + B1*scale(MACVS_sup_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(NA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Anx
full = lmer(Anx_agg ~ 1 + B1*scale(MACVS_sup_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Anx_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Dep
full = lmer(Dep_agg ~ 1 + B1*scale(MACVS_sup_mean) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Dep_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)

# Attitudes linking Trump and ICE -----------------------------------------------------------------------------

# NA
lmer(NA_agg ~ 1 + B1*scale(ICE_1) + B2 + B3 + (1|SubID), data = dat) %>% summary()

lmer(NA_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# PA
lmer(PA_agg ~ 1 + B1*scale(ICE_1) + B2 + B3 + (1|SubID), data = dat) %>% summary()

lmer(PA_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Anx
lmer(Anx_agg ~ 1 + B1*scale(ICE_1) + B2 + B3 + (1|SubID), data = dat) %>% summary()

lmer(Anx_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Dep
lmer(Dep_agg ~ 1 + B1*scale(ICE_1) + B2 + B3 + (1|SubID), data = dat) %>% summary()

lmer(Dep_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# effect sizes
# NA
full = lmer(NA_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(NA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# PA
full = lmer(PA_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(PA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Anx
full = lmer(Anx_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Anx_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Dep
full = lmer(Dep_agg ~ 1 + B1*scale(ICE_2) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Dep_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)


# Trait anxiety -------------------------------------------------------------------------------

# NA
lmer(NA_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# PA
lmer(PA_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Anxiety
lmer(Anx_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Depression
lmer(Dep_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# effect sizes
# NA
full = lmer(NA_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(NA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# PA
full = lmer(PA_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(PA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Anx
full = lmer(Anx_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Anx_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Dep
full = lmer(Dep_agg ~ 1 + B1*scale(GAD_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Dep_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)


# Trait depression -------------------------------------------------------------------------------

# NA
lmer(NA_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# PA
lmer(PA_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Anxiety
lmer(Anx_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# Depression
lmer(Dep_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% summary()

# effect sizes
# NA
full = lmer(NA_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(NA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# PA
full = lmer(PA_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(PA_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Anx
full = lmer(Anx_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Anx_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
# Dep
full = lmer(Dep_agg ~ 1 + B1*scale(PHQ_sum) + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
rest = lmer(Dep_agg ~ 1 + B1 + B2 + B3 + (1|SubID), data = dat) %>% r.squaredGLMM()
(full[1] - rest[1])/(1-full[1]) # using conditional R^2 (only fixed effects)
