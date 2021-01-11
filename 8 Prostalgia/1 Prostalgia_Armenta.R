# Created by Angel Armenta
# Revised by Hannah, 12/11

library(tidyverse)
library(gtools)
library(psych)
library(psy)
library(ppcor)

##############################################################################################################################
#reading in data

Onboardingdata = read.delim("./5 Clean data/ElectionStudy_Onboarding_clean_fixedSubID.txt", header=TRUE)
Onboardingdata$Survey = "Onboarding"
Extendeddata = read.delim("./5 Clean data/ElectionStudy_MergedDailyDiary_Clean.txt", header=TRUE)

##############################################################################################################################

#creating mean scores for each scale
#national nostalgia time 1
Onboardingdata$nnostalgia = round(rowMeans(Onboardingdata[,34:39], na.rm =T), digits = 2)

#chronbach for nostalgia time 1
cronbach(data.frame(Onboardingdata$NN_1, Onboardingdata$NN_2, Onboardingdata$NN_3, Onboardingdata$NN_4, 
                    Onboardingdata$NN_5, Onboardingdata$NN_6))

##############################################################################################################################

#national prostalgia time 1
Onboardingdata$nProstalgia = round(rowMeans(Onboardingdata[,40:45], na.rm =T), digits = 2)
#Chronbach for prostalgia time 1
cronbach(data.frame(Onboardingdata$NP_1, Onboardingdata$NP_2, Onboardingdata$NP_3, Onboardingdata$NP_4, 
                    Onboardingdata$NP_5, Onboardingdata$NP_6))


##############################################################################################################################

#national nostalgia time 2
Extendeddata$nnostalgia = round(rowMeans(Extendeddata[,66:71], na.rm =T), digits = 2)
#Chronbach for nostalgia time 2
cronbach(data.frame(Extendeddata$NN_1, Extendeddata$NN_2, Extendeddata$NN_3, Extendeddata$NN_4, 
                    Extendeddata$NN_5, Extendeddata$NN_6))


##############################################################################################################################

#prostalgia time 2
Extendeddata$nProstalgia = round(rowMeans(Extendeddata[,72:77], na.rm =T), digits = 2)
#Chronbach for prostalgia time 1
cronbach(data.frame(Extendeddata$NP_1, Extendeddata$NP_2, Extendeddata$NP_3, Extendeddata$NP_4, 
                    Extendeddata$NP_5, Extendeddata$NP_6))


##############################################################################################################################
#Optimism items 2-4 need to be recoded
Onboardingdata$OP_2 = recode(Onboardingdata$OP_2, '4' = 1, '3' =2, '2' =3, '1' =4)
Onboardingdata$OP_3 = recode(Onboardingdata$OP_3, '4' = 1, '3' =2, '2' =3, '1' =4)
Onboardingdata$OP_4 = recode(Onboardingdata$OP_4, '4' = 1, '3' =2, '2' =3, '1' =4)
Onboardingdata$Optimism = (round(rowMeans(Onboardingdata[,46:50], na.rm =T), digits = 2))
                           
#chronbach for optimism time 1

cronbach(data.frame(Onboardingdata$OP_1, Onboardingdata$OP_2, Onboardingdata$OP_3, Onboardingdata$OP_4, Onboardingdata$OP_5))

##############################################################################################################################
#Optimism items 2-4 need to be recoded
Extendeddata$OP_2 = recode(Extendeddata$OP_2, '4' = 1, '3' =2, '2' =3, '1' =4)
Extendeddata$OP_3 = recode(Extendeddata$OP_3, '4' = 1, '3' =2, '2' =3, '1' =4)
Extendeddata$OP_4 = recode(Extendeddata$OP_4, '4' = 1, '3' =2, '2' =3, '1' =4)
Extendeddata$Optimism = round(rowMeans(Extendeddata[,78:82], na.rm =T), digits = 2)

#chronbach for optimism time 2

cronbach(data.frame(Extendeddata$OP_1, Extendeddata$OP_2, Extendeddata$OP_3, Extendeddata$OP_4, Extendeddata$OP_5))


##############################################################################################################################
#Recode so that Trump = 1 and Other = 0
#filter out those who are not voting or are undecided
select.Onboarding = filter(Onboardingdata, Onboardingdata$Voting.Intent_1 %in% c(1, 2, 3))
select.Onboarding$VoteTrump = recode(select.Onboarding$Voting.Intent_1, '1'=1, '2'=0, '3'=0)
select.Onboarding$VoteTrump = as.factor(select.Onboarding$VoteTrump)

select.Onboarding$VoteBiden = recode(select.Onboarding$Voting.Intent_1, '1'=0, '2'=1, '3'=0)
select.Onboarding$VoteBiden = as.factor(select.Onboarding$VoteBiden)

select.Onboarding$Gender.e = recode(select.Onboarding$Gender, '1'=-1, '2'=1, '3'=0)

#getting partial correlations to compute Rsquared and then run a post-hoc poweranalysis...
pcor.test(select.Onboarding$VoteTrump, select.Onboarding$nProstalgia, select.Onboarding$nnostalgia)
pcor.test(select.Onboarding$VoteTrump, select.Onboarding$nnostalgia, select.Onboarding$nProstalgia)

#running logistic regression
#Both are "marginally" significant......
#we are only about 50-60% powered... we needed more people :( cries
model = glm(VoteTrump ~ nnostalgia + nProstalgia + Optimism, data = select.Onboarding, family = "binomial")
summary(model)

model = glm(VoteBiden ~ nnostalgia + nProstalgia + Optimism, data = select.Onboarding, family = "binomial")
summary(model)

# try without filtering out
Onboardingdata$VoteTrump = recode(Onboardingdata$Voting.Intent_1, '1'=1, '2'=0, '3'=0, '4'=0, '5'=0)
Onboardingdata$VoteTrump = as.factor(Onboardingdata$VoteTrump)

Onboardingdata$VoteBiden = recode(Onboardingdata$Voting.Intent_1, '1'=0, '2'=1, '3'=0, '4'=0, '5'=0)
Onboardingdata$VoteBiden = as.factor(Onboardingdata$VoteBiden)

glm(VoteTrump ~ nnostalgia + nProstalgia + Optimism , data = Onboardingdata, family = "binomial") %>% 
  summary()

glm(VoteBiden ~ nnostalgia + nProstalgia + Optimism, data = Onboardingdata, family = "binomial") %>% 
  summary()

# add covariates:
# gender (male = -1, female = 1, trans/nonbinary = 0)
# age
# conservatism (overall, social, economic)
# whether people associate ICE with trump
Onboardingdata$Gender.e = recode(Onboardingdata$Gender, '1'=-1, '2'=1, '3'=0)

glm(VoteTrump ~ nnostalgia + nProstalgia + Optimism + Gender.e + Age + 
      scale(Conserv_1) + scale(ICE_2), data = Onboardingdata, family = "binomial") %>% 
  summary()

cor(select(Onboardingdata, Conserv_1, Conserv_2, Conserv_3, ICE_1, ICE_2, nnostalgia, nProstalgia, Optimism))
# associating Trump with ICE is related to conservatism (both economic and social), but not completely

glm(VoteBiden ~ nnostalgia + nProstalgia + Optimism + Gender.e + Age + 
      scale(Conserv_1) + scale(ICE_2), data = Onboardingdata, family = "binomial") %>% 
  summary()

##############################################################################################################################

#higher national Prostalgia = higher Ethnic identity
#nostalgia doesnt produce anything...
Onboardingdata$EthnicIden = round(rowMeans(Onboardingdata[,55:60], na.rm =T), digits = 2)

#Chronbach for ethnic identity all together
cronbach(data.frame(Onboardingdata$MEIM_1, Onboardingdata$MEIM_2, Onboardingdata$MEIM_3, Onboardingdata$MEIM_4, Onboardingdata$MEIM_5, Onboardingdata$MEIM_6))

#ethnic identity overall without making separate composites predicted by nostalgia and prostalgia
lm(EthnicIden ~  nnostalgia + nProstalgia, data = Onboardingdata) %>% 
  summary()

#manually making composites for ethnic identity subgroups
Onboardingdata$ethnicexploration = (Onboardingdata$MEIM_1 + Onboardingdata$MEIM_4 + Onboardingdata$MEIM_5)/3
Onboardingdata$ethnicommitment = (Onboardingdata$MEIM_2 + Onboardingdata$MEIM_3 + Onboardingdata$MEIM_6)/3

#doesnt change the results lol. 
lm(ethnicexploration ~  nnostalgia + nProstalgia, data = Onboardingdata) %>% 
  summary()
lm(ethnicommitment ~  nnostalgia + nProstalgia, data = Onboardingdata) %>% 
  summary()


##############################################################################################################################
#prostalgia and discrimination- wrong. Need to redo
#Extendeddataset only AKA time 2
Extendeddata$Discrimination = round(rowMeans(Extendeddata[,28:34], na.rm =T), digits = 2)
#discrimination vigilance  
cronbach(data.frame(Extendeddata$Discrim_1, Extendeddata$Discrim_2, Extendeddata$Discrim_3, 
                    Extendeddata$Discrim_4, Extendeddata$Discrim_5, Extendeddata$Discrim_6,
                    Extendeddata$Discrim_7))
#prostalgia and nostalgia preidcting discrimination vigilance
fit = lm(Extendeddata$Discrimination ~  Extendeddata$nnostalgia + Extendeddata$nProstalgia, data = Extendeddata)
summary(fit)











#exploratory stuff
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################

#depression
Onboardingdata$depression= round(rowMeans(Onboardingdata[,19:26], na.rm =T), digits = 2)

fit = lm(Onboardingdata$depression ~  Onboardingdata$nnostalgia + Onboardingdata$nProstalgia, data = Onboardingdata)
summary(fit)



#perceptions of cultural dynamism
#higher numers mean more cultural change
Onboardingdata$Dyn_2 = recode(Onboardingdata$Dyn_2, "5=1;4=2;3=3;2=4;1=5;")
Onboardingdata$Dyn_3 = recode(Onboardingdata$Dyn_3, "5=1;4=2;3=3;2=4;1=5;")
Onboardingdata$Change = round(rowMeans(Onboardingdata[,51:53], na.rm =T), digits = 2)

#views on change predicted by nostalgia and prostalgia
fit = lm(Onboardingdata$Change ~  Onboardingdata$nnostalgia + Onboardingdata$nProstalgia, data = Onboardingdata)
summary(fit)

#nostalgia predicted by views of change
fit = lm(Onboardingdata$nnostalgia ~ Onboardingdata$Change, data = Onboardingdata)
summary(fit)

#voting intentions predicted by views on change
model = glm(Onboardingdata$Vote ~ Onboardingdata$Change, data = Onboardingdata, family = "binomial")
summary(model)



#Trump and ICE
Onboardingdata$ICE = round(rowMeans(Onboardingdata[,129:130], na.rm =T), digits = 2)

fit = lm(Onboardingdata$ICE_2 ~  Onboardingdata$nnostalgia + Onboardingdata$nProstalgia, data = Onboardingdata)
summary(fit)


#Meaning with Life
Onboardingdata$Meaning = round(rowMeans(Onboardingdata[,101:106], na.rm =T), digits = 2)

fit = lm(Onboardingdata$Meaning ~  Onboardingdata$nnostalgia + Onboardingdata$nProstalgia, data = Onboardingdata)
summary(fit)


#Valuing and engagement with Hispanic Culture
Onboardingdata$Values = round(rowMeans(Onboardingdata[,63:90], na.rm =T), digits = 2)

fit = lm(Onboardingdata$Values ~  Onboardingdata$nnostalgia + Onboardingdata$nProstalgia, data = Onboardingdata)
summary(fit)



#test retest reliability of prostalgia
#doesnt work
#cor(Onboardingdata$nProstalgia, Extendeddata$nProstalgia)
