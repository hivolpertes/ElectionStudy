library(tidyverse)

dat = read.delim("./5 Clean data/ElectionStudy_MergedDailyDiary_clean.txt", stringsAsFactors=F)

# put into long form
long = select(dat, SubID, NA_1:Dep_3, DayStudy.adj) %>% 
  gather("Emotion", "value", 2:27)

# Add emotion words
long$Emotion[long$Emotion == "NA_1"] = "Afraid"
long$Emotion[long$Emotion == "NA_2"] = "Scared"
long$Emotion[long$Emotion == "NA_3"] = "Nervous"
long$Emotion[long$Emotion == "NA_4"] = "Jittery"
long$Emotion[long$Emotion == "NA_5"] = "Irritable"
long$Emotion[long$Emotion == "NA_6"] = "Hostile"
long$Emotion[long$Emotion == "NA_7"] = "Guilty"
long$Emotion[long$Emotion == "NA_8"] = "Ashamed"
long$Emotion[long$Emotion == "NA_9"] = "Upset"
long$Emotion[long$Emotion == "NA_10"] = "Distressed"

long$Emotion[long$Emotion == "PA_1"] = "Active"
long$Emotion[long$Emotion == "PA_2"] = "Alert"
long$Emotion[long$Emotion == "PA_3"] = "Attentive"
long$Emotion[long$Emotion == "PA_4"] = "Determined"
long$Emotion[long$Emotion == "PA_5"] = "Enthusiastic"
long$Emotion[long$Emotion == "PA_6"] = "Excited"
long$Emotion[long$Emotion == "PA_7"] = "Inspired"
long$Emotion[long$Emotion == "PA_8"] = "Interested"
long$Emotion[long$Emotion == "PA_9"] = "Proud"
long$Emotion[long$Emotion == "PA_10"] = "Strong"

long$Emotion[long$Emotion == "Anx_1"] = "Anxious"
long$Emotion[long$Emotion == "Anx_2"] = "Worried"
long$Emotion[long$Emotion == "Anx_3"] = "Restless"

long$Emotion[long$Emotion == "Dep_1"] = "Depressed"
long$Emotion[long$Emotion == "Dep_2"] = "Sad"
long$Emotion[long$Emotion == "Dep_3"] = "Downhearted"

# add emotion category
NegEmo = c("Afraid", "Scared", "Nervous", "Jittery", "Irritable", "Hostile", "Guilty", "Ashamed", "Upset", "Distressed")
PosEmo = c("Active", "Alert", "Attentive", "Determined", "Enthusiastic", "Excited", "Inspired", "Interested", "Proud", "Strong")
Dep = c("Depressed", "Sad", "Downhearted")
Anx = c("Anxious", "Worried", "Restless")

long$EmoCategory = NA
long$EmoCategory[long$Emotion %in% NegEmo] = "Negative Affect"
long$EmoCategory[long$Emotion %in% PosEmo] = "Positive Affect"
long$EmoCategory[long$Emotion %in% Dep] = "Depression"
long$EmoCategory[long$Emotion %in% Anx] = "Anxiety"


# 1. Plot for each individual (faceting by emotion category) ----------------------------------

for (i in unique(long$SubID)) {
  temp = long[long$SubID == i,]
  
  ggplot(temp, aes(DayStudy.adj, value, color = Emotion))+
    facet_wrap(~EmoCategory) +
    geom_point() +
    geom_smooth(method="loess",se=F) +
    #scale_x_continuous(breaks = c((0:17)*5)) +
    geom_vline(xintercept=0) +
    # add vertical line for Election day
    geom_vline(xintercept=7, linetype = "dashed") +
    # add vertical line for day Biden announced winner
    geom_vline(xintercept=11, linetype = "dashed", color = "blue") +
    ggtitle(paste("Subject", i, sep=" ")) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(paste("./6 Daily diary/1 Individual plots/Subject", i,"_facetbyEmotionCategory.jpg", sep=""), width = 14, height = 7, units="in")
  
}

# 2. Plot for each emotion (all subs together) ----------------------------------

long$SubID = factor(long$SubID)

for (i in unique(long$Emotion)) {
  temp = long[long$Emotion == i,]
  
  ggplot(temp, aes(DayStudy.adj, value, color = SubID))+
    #geom_point() +
    geom_smooth(method="loess",se=F, alpha = .1) +
    geom_smooth(method="loess",se=F, color = "black") +
    geom_vline(xintercept=0) +
    # add vertical line for Election day
    geom_vline(xintercept=7, linetype = "dashed") +
    # add vertical line for day Biden announced winner
    geom_vline(xintercept=11, linetype = "dashed", color = "blue") +
    ggtitle(i) + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  ggsave(paste("./6 Daily diary/2 Emotion plots/AllSubs_", i,".jpg", sep=""), width = 14, height = 7, units="in")
  
}
