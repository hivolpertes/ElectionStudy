library(tidyverse)
library(lme4)
library(lmerTest)

dat = read.delim("./5 Clean data/ElectionStudy_MergedDailyDiary_clean_recodedDiscrim_laggedDays.txt")

# add variables for stages

# 3 stages:

# A1 = 1-6
# A2 = 7-10
# A3 = 11-14

dat$A1 = dat$DayStudy.adj - 1
dat$A2 = 0
dat$A2[dat$DayStudy.adj>7] = dat$DayStudy.adj[dat$DayStudy.adj>7] - 7
dat$A3 = 0
dat$A3[dat$DayStudy.adj>11] = dat$DayStudy.adj[dat$DayStudy.adj>11] - 11

dat$B1 = dat$DayStudy.adj - 1
dat$B2 = 0
dat$B2[dat$DayStudy.adj>7] = dat$DayStudy.adj[dat$DayStudy.adj>7] - 7

dat$C1 = dat$DayStudy.adj - 1
dat$C2 = 0
dat$C2[dat$DayStudy.adj>11] = dat$DayStudy.adj[dat$DayStudy.adj>11] - 11


# Fit models, no covariates -------------------------------------------------------------------------------

# NA
NA_mod_A = lmer(NA_agg ~ 1 + A1 + A2 + A3 + (A1+A2+A3|SubID), data = dat) %>% summary()
NA_mod_B = lmer(NA_agg ~ B1 + B2 + (1|SubID), data = dat) %>% summary()
NA_mod_C = lmer(NA_agg ~ C1 + C2 + (1|SubID), data = dat) %>% summary()
     
# PA
PA_mod_A = lmer(PA_agg ~ A1 + A2 + A3 + (A1+A2+A3|SubID), data = dat) %>% summary()
PA_mod_B = lmer(PA_agg ~ B1 + B2 + (1|SubID), data = dat) %>% summary()
PA_mod_C = lmer(PA_agg ~ C1 + C2 + (1|SubID), data = dat) %>% summary()

# Anx
Anx_mod_A = lmer(Anx_agg ~ A1 + A2 + A3 + (A1+A2+A3|SubID), data = dat) %>% summary()
Anx_mod_B = lmer(Anx_agg ~ B1 + B2 + (1|SubID), data = dat) %>% summary()
Anx_mod_C = lmer(Anx_agg ~ C1 + C2 + (1|SubID), data = dat) %>% summary()

# Dep
Dep_mod_A = lmer(Dep_agg ~ A1 + A2 + A3 + (A1+A2+A3|SubID), data = dat) %>% summary()
Dep_mod_B = lmer(Dep_agg ~ B1 + B2 + (1|SubID), data = dat) %>% summary()
Dep_mod_C = lmer(Dep_agg ~ C1 + C2 + (1|SubID), data = dat) %>% summary()

# Plot no covariates (Split A) -------------------------------------------------------------------

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
  coef(NA_mod_A)[1,1] + + # add intercept
  c(6:9)*coef(NA_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(NA_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$NegAff[wide$Day %in% c(11:14)] = 
  coef(NA_mod_A)[1,1] + + # add intercept
  c(10:13)*coef(NA_mod_A)[2,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(NA_mod_A)[3,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(NA_mod_A)[4,1] # add stage 3 slope for days 11:14

# PA
wide$PosAff[wide$Day %in% c(1:6)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(0:5)*coef(PA_mod_A)[2,1] # add stage 1 slope for days 1:6

wide$PosAff[wide$Day %in% c(7:10)] = 
  coef(PA_mod_A)[1,1] + + # add intercept
  c(6:9)*coef(PA_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(PA_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$PosAff[wide$Day %in% c(11:14)] = 
  coef(PA_mod_A)[1,1] + + # add intercept
  c(10:13)*coef(PA_mod_A)[2,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(PA_mod_A)[3,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(PA_mod_A)[4,1] # add stage 3 slope for days 11:14

# Anx
wide$Anx[wide$Day %in% c(1:6)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Anx_mod_A)[2,1] # add stage 1 slope for days 1:6

wide$Anx[wide$Day %in% c(7:10)] = 
  coef(Anx_mod_A)[1,1] + + # add intercept
  c(6:9)*coef(Anx_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Anx_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$Anx[wide$Day %in% c(11:14)] = 
  coef(Anx_mod_A)[1,1] + + # add intercept
  c(10:13)*coef(Anx_mod_A)[2,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Anx_mod_A)[3,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Anx_mod_A)[4,1] # add stage 3 slope for days 11:14

# Dep
wide$Dep[wide$Day %in% c(1:6)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Dep_mod_A)[2,1] # add stage 1 slope for days 1:6

wide$Dep[wide$Day %in% c(7:10)] = 
  coef(Dep_mod_A)[1,1] + + # add intercept
  c(6:9)*coef(Dep_mod_A)[2,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Dep_mod_A)[3,1] # add stage 2 slope for days 7:10

wide$Dep[wide$Day %in% c(11:14)] = 
  coef(Dep_mod_A)[1,1] + + # add intercept
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
  geom_point() +
  geom_line() +
  geom_vline(xintercept=7) +
  geom_vline(xintercept=11)+
  scale_x_continuous(breaks=c(1:14)) +
  scale_y_continuous(limits = c(1, 3.5), breaks=c(1:4)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggsave("./6 Daily diary/3 Piecewise plots/PiecewisePlot_fullsample.jpg", plot, units="in", width=5.5, height=6)


# Add voting intentions -----------------------------------------------------------------------

onboarding = read.delim("./5 Clean data/ElectionStudy_Onboarding_clean_fixedSubID.txt")

dat = left_join(dat, select(onboarding, SubID, Voting.Intent_1))

# Voting intentions
# 1 = Trump
# 2 = Biden
# 3 = Other candidate
# 4 = Not voting
# 5 = Haven't decided

# Recode Trump vs all (Trump = 1, Other = 0)
dat$Trump = 0
dat$Trump[dat$Voting.Intent_1 == 1] = 1
dat$Trump = factor(dat$Trump)

# Record Biden vs all (Biden = 1, Other = 0)
dat$Biden = 0
dat$Biden[dat$Voting.Intent_1 == 2] = 1
dat$Biden = factor(dat$Biden)

# Fit models, voting intentions (Trump) -------------------------------------------------------------------------------

# NA
NA_mod_A = lmer(NA_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()

# PA
PA_mod_A = lmer(PA_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()

# Anx
Anx_mod_A = lmer(Anx_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()

# Dep
Dep_mod_A = lmer(Dep_agg ~ Trump + A1*Trump + A2*Trump + A3*Trump + (1|SubID), data = dat) %>% summary()


# Plot, voting intentions (Trump) -------------------------------------------------------------

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
  coef(NA_mod_A)[1,1] + # add intercept
  c(0:5)*coef(NA_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$NegAff.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(NA_mod_A)[1,1] + # add intercept
  c(6:9)*coef(NA_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(NA_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$NegAff.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(NA_mod_A)[1,1] + # add intercept
  c(10:13)*coef(NA_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(NA_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(NA_mod_A)[5,1] # add stage 3 slope for days 11:14

# NA (Trump)
wide.Trump$NegAff.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(NA_mod_A)[1,1] + coef(NA_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(NA_mod_A)[3,1]+coef(NA_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$NegAff.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(NA_mod_A)[1,1] + coef(NA_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(NA_mod_A)[3,1]+coef(NA_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(NA_mod_A)[4,1]+coef(NA_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$NegAff.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(NA_mod_A)[1,1] + coef(NA_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(NA_mod_A)[3,1]+coef(NA_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(NA_mod_A)[4,1]+coef(NA_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(NA_mod_A)[5,1]+coef(NA_mod_A)[8,1]) # add stage 3 slope for days 11:14

# PA (Other)
wide.Trump$PosAff.Other[wide.Trump$Day %in% c(1:6)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(0:5)*coef(PA_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$PosAff.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(6:9)*coef(PA_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(PA_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$PosAff.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(10:13)*coef(PA_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(PA_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(PA_mod_A)[5,1] # add stage 3 slope for days 11:14

# PA (Trump)
wide.Trump$PosAff.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(PA_mod_A)[1,1] + coef(PA_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(PA_mod_A)[3,1]+coef(PA_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$PosAff.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(PA_mod_A)[1,1] + coef(PA_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(PA_mod_A)[3,1]+coef(PA_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(PA_mod_A)[4,1]+coef(PA_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$PosAff.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(PA_mod_A)[1,1] + coef(PA_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(PA_mod_A)[3,1]+coef(PA_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(PA_mod_A)[4,1]+coef(PA_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(PA_mod_A)[5,1]+coef(PA_mod_A)[8,1]) # add stage 3 slope for days 11:14

# Anx (Other)
wide.Trump$Anx.Other[wide.Trump$Day %in% c(1:6)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Anx_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$Anx.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(6:9)*coef(Anx_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Anx_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$Anx.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(10:13)*coef(Anx_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Anx_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Anx_mod_A)[5,1] # add stage 3 slope for days 11:14

# Anx (Trump)
wide.Trump$Anx.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(Anx_mod_A)[1,1] + coef(Anx_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(Anx_mod_A)[3,1]+coef(Anx_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$Anx.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(Anx_mod_A)[1,1] + coef(Anx_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(Anx_mod_A)[3,1]+coef(Anx_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Anx_mod_A)[4,1]+coef(Anx_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$Anx.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(Anx_mod_A)[1,1] + coef(Anx_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(Anx_mod_A)[3,1]+coef(Anx_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Anx_mod_A)[4,1]+coef(Anx_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Anx_mod_A)[5,1]+coef(Anx_mod_A)[8,1]) # add stage 3 slope for days 11:14

# Dep (Other)
wide.Trump$Dep.Other[wide.Trump$Day %in% c(1:6)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Dep_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Trump$Dep.Other[wide.Trump$Day %in% c(7:10)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(6:9)*coef(Dep_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Dep_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Trump$Dep.Other[wide.Trump$Day %in% c(11:14)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(10:13)*coef(Dep_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Dep_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Dep_mod_A)[5,1] # add stage 3 slope for days 11:14

# Dep (Trump)
wide.Trump$Dep.Trump[wide.Trump$Day %in% c(1:6)] = 
  coef(Dep_mod_A)[1,1] + coef(Dep_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(Dep_mod_A)[3,1]+coef(Dep_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Trump$Dep.Trump[wide.Trump$Day %in% c(7:10)] = 
  coef(Dep_mod_A)[1,1] + coef(Dep_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(Dep_mod_A)[3,1]+coef(Dep_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Dep_mod_A)[4,1]+coef(Dep_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Trump$Dep.Trump[wide.Trump$Day %in% c(11:14)] = 
  coef(Dep_mod_A)[1,1] + coef(Dep_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(Dep_mod_A)[3,1]+coef(Dep_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Dep_mod_A)[4,1]+coef(Dep_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Dep_mod_A)[5,1]+coef(Dep_mod_A)[8,1]) # add stage 3 slope for days 11:14

# gather wide to long

long.Trump = gather(wide.Trump, type, value, 2:9)

long.Trump$VI = NA
long.Trump$VI[grep("Trump", long.Trump$type)] = "Trump"
long.Trump$VI[grep("Other", long.Trump$type)] = "Other"

long.Trump$facet = NA
long.Trump$facet[grep("NegAff", long.Trump$type)] = "NegAff"
long.Trump$facet[grep("PosAff", long.Trump$type)] = "PosAff"
long.Trump$facet[grep("Anx", long.Trump$type)] = "Anx"
long.Trump$facet[grep("Dep", long.Trump$type)] = "Dep"

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
        axis.text = element_text(size = 14),,
        strip.text = element_text(size = 16))

ggsave("./6 Daily diary/3 Piecewise plots/PiecewisePlot_VIsplit_Trump.jpg", TrumpVIplot, units="in", width=10, height=6)

# Fit models, voting intentions (Biden) -------------------------------------------------------------------------------

# NA
NA_mod_A = lmer(NA_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()

# PA
PA_mod_A = lmer(PA_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()

# Anx
Anx_mod_A = lmer(Anx_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()

# Dep
Dep_mod_A = lmer(Dep_agg ~ Biden + A1*Biden + A2*Biden + A3*Biden + (1|SubID), data = dat) %>% summary()


# Plot, voting intentions (Biden) -------------------------------------------------------------

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
  coef(NA_mod_A)[1,1] + # add intercept
  c(0:5)*coef(NA_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$NegAff.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(NA_mod_A)[1,1] + # add intercept
  c(6:9)*coef(NA_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(NA_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$NegAff.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(NA_mod_A)[1,1] + # add intercept
  c(10:13)*coef(NA_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(NA_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(NA_mod_A)[5,1] # add stage 3 slope for days 11:14

# NA (Biden)
wide.Biden$NegAff.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(NA_mod_A)[1,1] + coef(NA_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(NA_mod_A)[3,1]+coef(NA_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$NegAff.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(NA_mod_A)[1,1] + coef(NA_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(NA_mod_A)[3,1]+coef(NA_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(NA_mod_A)[4,1]+coef(NA_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$NegAff.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(NA_mod_A)[1,1] + coef(NA_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(NA_mod_A)[3,1]+coef(NA_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(NA_mod_A)[4,1]+coef(NA_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(NA_mod_A)[5,1]+coef(NA_mod_A)[8,1]) # add stage 3 slope for days 11:14

# PA (Other)
wide.Biden$PosAff.Other[wide.Biden$Day %in% c(1:6)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(0:5)*coef(PA_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$PosAff.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(6:9)*coef(PA_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(PA_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$PosAff.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(PA_mod_A)[1,1] + # add intercept
  c(10:13)*coef(PA_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(PA_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(PA_mod_A)[5,1] # add stage 3 slope for days 11:14

# PA (Biden)
wide.Biden$PosAff.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(PA_mod_A)[1,1] + coef(PA_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(PA_mod_A)[3,1]+coef(PA_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$PosAff.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(PA_mod_A)[1,1] + coef(PA_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(PA_mod_A)[3,1]+coef(PA_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(PA_mod_A)[4,1]+coef(PA_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$PosAff.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(PA_mod_A)[1,1] + coef(PA_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(PA_mod_A)[3,1]+coef(PA_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(PA_mod_A)[4,1]+coef(PA_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(PA_mod_A)[5,1]+coef(PA_mod_A)[8,1]) # add stage 3 slope for days 11:14

# Anx (Other)
wide.Biden$Anx.Other[wide.Biden$Day %in% c(1:6)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Anx_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$Anx.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(6:9)*coef(Anx_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Anx_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$Anx.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(Anx_mod_A)[1,1] + # add intercept
  c(10:13)*coef(Anx_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Anx_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Anx_mod_A)[5,1] # add stage 3 slope for days 11:14

# Anx (Biden)
wide.Biden$Anx.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(Anx_mod_A)[1,1] + coef(Anx_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(Anx_mod_A)[3,1]+coef(Anx_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$Anx.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(Anx_mod_A)[1,1] + coef(Anx_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(Anx_mod_A)[3,1]+coef(Anx_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Anx_mod_A)[4,1]+coef(Anx_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$Anx.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(Anx_mod_A)[1,1] + coef(Anx_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(Anx_mod_A)[3,1]+coef(Anx_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Anx_mod_A)[4,1]+coef(Anx_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Anx_mod_A)[5,1]+coef(Anx_mod_A)[8,1]) # add stage 3 slope for days 11:14

# Dep (Other)
wide.Biden$Dep.Other[wide.Biden$Day %in% c(1:6)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(0:5)*coef(Dep_mod_A)[3,1] # add stage 1 slope for days 1:6

wide.Biden$Dep.Other[wide.Biden$Day %in% c(7:10)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(6:9)*coef(Dep_mod_A)[3,1] + # add stage 1 slope for days 7:10
  c(0:3)*coef(Dep_mod_A)[4,1] # add stage 2 slope for days 7:10

wide.Biden$Dep.Other[wide.Biden$Day %in% c(11:14)] = 
  coef(Dep_mod_A)[1,1] + # add intercept
  c(10:13)*coef(Dep_mod_A)[3,1] + # add stage 1 slope for days 11:14
  c(4:7)*coef(Dep_mod_A)[4,1] + # add stage 2 slope for days 11:14
  c(0:3)*coef(Dep_mod_A)[5,1] # add stage 3 slope for days 11:14

# Dep (Biden)
wide.Biden$Dep.Biden[wide.Biden$Day %in% c(1:6)] = 
  coef(Dep_mod_A)[1,1] + coef(Dep_mod_A)[2,1] + # add intercept
  c(0:5)*(coef(Dep_mod_A)[3,1]+coef(Dep_mod_A)[6,1]) # add stage 1 slope for days 1:6

wide.Biden$Dep.Biden[wide.Biden$Day %in% c(7:10)] = 
  coef(Dep_mod_A)[1,1] + coef(Dep_mod_A)[2,1] + # add intercept
  c(6:9)*(coef(Dep_mod_A)[3,1]+coef(Dep_mod_A)[6,1]) + # add stage 1 slope for days 7:10
  c(0:3)*(coef(Dep_mod_A)[4,1]+coef(Dep_mod_A)[7,1]) # add stage 2 slope for days 7:10

wide.Biden$Dep.Biden[wide.Biden$Day %in% c(11:14)] = 
  coef(Dep_mod_A)[1,1] + coef(Dep_mod_A)[2,1] + # add intercept
  c(10:13)*(coef(Dep_mod_A)[3,1]+coef(Dep_mod_A)[6,1]) + # add stage 1 slope for days 11:14
  c(4:7)*(coef(Dep_mod_A)[4,1]+coef(Dep_mod_A)[7,1]) + # add stage 2 slope for days 11:14
  c(0:3)*(coef(Dep_mod_A)[5,1]+coef(Dep_mod_A)[8,1]) # add stage 3 slope for days 11:14

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

