## ----setup, include=FALSE-----------------------------------------------
#r setup

knitr::opts_chunk$set(echo = FALSE)
library(readr)        # Reading CSV file
library(dplyr)        # Majorly used for data cleaning and manipulation 
library(ggplot2)      # For creating better visualization
library(ggcorrplot)   # Visualize a correlation matrix
library(gridExtra)    # Plot ggplot side-by-side
library(lubridate)    # Extract date parts from date column
library(knitr)
library(glmnet)
library(xgboost)
library(caret) 
library(pROC)
library(tidyverse)
library(PRROC)
library(ROCR)



## -----------------------------------------------------------------------
#reading in the data

session1 <- readRDS("C:/Users/nicol/Downloads/sessions/session1.rds")
session2 <- readRDS("C:/Users/nicol/Downloads/sessions/session2.rds")
session3 <- readRDS("C:/Users/nicol/Downloads/sessions/session3.rds")
session4 <- readRDS("C:/Users/nicol/Downloads/sessions/session4.rds")
session5 <- readRDS("C:/Users/nicol/Downloads/sessions/session5.rds")
session6 <- readRDS("C:/Users/nicol/Downloads/sessions/session6.rds")
session7 <- readRDS("C:/Users/nicol/Downloads/sessions/session7.rds")
session8 <- readRDS("C:/Users/nicol/Downloads/sessions/session8.rds")
session9 <- readRDS("C:/Users/nicol/Downloads/sessions/session9.rds")
session10 <- readRDS("C:/Users/nicol/Downloads/sessions/session10.rds")
session11 <- readRDS("C:/Users/nicol/Downloads/sessions/session11.rds")
session12 <- readRDS("C:/Users/nicol/Downloads/sessions/session12.rds")
session13 <- readRDS("C:/Users/nicol/Downloads/sessions/session13.rds")
session14 <- readRDS("C:/Users/nicol/Downloads/sessions/session14.rds")
session15 <- readRDS("C:/Users/nicol/Downloads/sessions/session15.rds")
session16 <- readRDS("C:/Users/nicol/Downloads/sessions/session16.rds")
session17 <- readRDS("C:/Users/nicol/Downloads/sessions/session17.rds")
session18 <- readRDS("C:/Users/nicol/Downloads/sessions/session18.rds")

all_sessions <- list(session1, session2, session3, session4, session5, session6, session7, session8, session9, session10, session11, session12, session13, session14, session15, session16, session17, session18)

# test data

test1 <- readRDS("C:/Users/nicol/Downloads/test/test1.rds")
test18 <- readRDS("C:/Users/nicol/Downloads/test/test2.rds")

all_tests <- list(test1, test18)



## ---- eval=FALSE--------------------------------------------------------
## for (i in seq_along(all_sessions)) {
##   brain_area <- all_sessions[[i]]$brain_area
##   print(unique(brain_area))
##   print(length(unique(brain_area)))
## }
## 


## -----------------------------------------------------------------------

all_summaries = list()

average_spike_area<-function(i.t,this_session){
  spk.trial = this_session$spks[[i.t]]
  area= this_session$brain_area
  spk.count=apply(spk.trial,1,sum)
  spk.average.tapply=tapply(spk.count, area, mean)
  return(spk.average.tapply)
}

# s1piespks <- list()
# s2piespks <- list()
# s3piespks <- list()
# s4piespks <- list()
# s5piespks <- list()
# s6piespks <- list()
# s7piespks <- list()
# s8piespks <- list()
# s9piespks <- list()
# s10piespks <- list()
# s11piespks <- list()
# s12piespks <- list()
# s13piespks <- list()
# s14piespks <- list()
# s15piespks <- list()
# s16piespks <- list()
# s17piespks <- list()
# s18piespks <- list()

all_sessions_bar <- list()
all_sessions_bar2 <- list()
all_sessions_bar3 <- list()
all_sessions_bar4 <- list()
all_sessions_bar5 <- list()
all_sessions_bar6 <- list()

corr1 <- list()

corr4 <- list()

trial.lms <- list()

for (i in seq_along(all_sessions)) {
  #getting data frame (using disc code lol)
  rows = length(all_sessions[[i]]$spks)
  trial.summary <- matrix(nrow=rows, ncol=40)
  i.s = i
  i.t = 1
  spk.trial = all_sessions[[i.s]]$spks[[i.t]]
  area=all_sessions[[i.s]]$brain_area
  spk.count=apply(spk.trial,1,sum)
  tmp <- data.frame(
    area = area, 
    spikes = spk.count
  )
  spk.average.dplyr =tmp %>%
  group_by(area) %>%
  summarize(mean= mean(spikes))
  average_spike_area(1,this_session = all_sessions[[i.s]])
  n.trial=length(all_sessions[[i.s]]$feedback_type)
  n.area=length(unique(all_sessions[[i.s]]$brain_area ))
  trial.summary =matrix(nrow=n.trial,ncol= n.area+1+2+1)
  for(i.t in 1:n.trial) {
    trial.summary[i.t,]= c(
      average_spike_area(i.t,this_session = all_sessions[[i.s]]),  
      all_sessions[[i.s]]$feedback_type[i.t],
      all_sessions[[i.s]]$contrast_left[i.t],
      all_sessions[[i.s]]$contrast_right[i.t], 
      i.t) 
    }
  colnames(trial.summary)=c(
    names(average_spike_area(i.t,
                             this_session = all_sessions[[i.s]])), 
                            'feedback', 
                            'left contr.',
                            'right contr.',
                            'id' )
  trial.summary <- as_tibble(trial.summary)
  
  all_summaries[[i]] <- trial.summary
  
  #subsetting by success 
  #subset each trial as successful or unsuccessful, take average of average spike of each neuron from success and unsuccess, compare (maybe as pie chart?)
  # i <3 pie charts
  
  feedback_type <- all_sessions[[i]]$feedback_type
  
  trial.summary2 <- cbind(feedback_type,trial.summary)
  

  succ_df <- trial.summary2[trial.summary2$feedback_type == 1,]
  unsucc_df <- trial.summary2[trial.summary2$feedback_type == -1,]
  
  
  succ_m <- as.data.frame(colMeans(succ_df))
  succ_m <- succ_m[-1, ,drop=FALSE]
  succ_m <- head(succ_m,-4)
  names(succ_m) <- c("Average_Spikes")
  succ_m$Brain_Area <- rownames(succ_m)
  
  
  unsucc_m <- as.data.frame(colMeans(unsucc_df))
  unsucc_m <- unsucc_m[-1, ,drop=FALSE]
  unsucc_m <- head(unsucc_m,-4)
  names(unsucc_m) <- c("Average_Spikes")
  unsucc_m$Brain_Area <- rownames(unsucc_m)
  
  succunsucc <- data.frame(
    BrainArea = succ_m$Brain_Area,
    SuccessSpks = succ_m$Average_Spikes,
    FailureSpks = unsucc_m$Average_Spikes
  )
  
  succunsucc_long <- tidyr::pivot_longer(succunsucc,
                                         cols=c(SuccessSpks,
                                                FailureSpks), 
                                         names_to = "Feedback", 
                                         values_to = "AverageSpikes")
  
  succunsucc_bar <- ggplot(succunsucc_long, 
                           aes(x= BrainArea, 
                               y=AverageSpikes, 
                               fill=Feedback)) +
    geom_bar(stat="identity", position = "dodge") +
    labs(title=paste("Average Spikes by Brain Area by Feedback Type, Session",i), x= "Brain Area", 
         y= "Average Spikes")
  
  if (i <= 3) {
    all_sessions_bar[[i]] <- succunsucc_bar
  }
  else if (i <= 6) {
    all_sessions_bar2[[i-3]] <- succunsucc_bar
  }
  else if (i <= 9) {
    all_sessions_bar3[[i-6]] <- succunsucc_bar
  }
  else if (i <= 12) {
    all_sessions_bar4[[i-9]] <- succunsucc_bar
  }
  else if (i <= 15) {
    all_sessions_bar5[[i-12]] <- succunsucc_bar
  }
  else if (i <= 18) {
    all_sessions_bar6[[i-15]] <- succunsucc_bar
  }
  
  # succ_pie <- ggplot(succ_m, aes(x="", 
  #                                      y=Average_Spikes, 
  #                                      fill=Brain_Area)) +
  #   geom_col() +
  #   coord_polar(theta = "y") + 
  #   labs(title= paste("Successes, Session",i), 
  #        fill ="Brain_Area", x= "", y="") +
  #   geom_text(aes(label=round(Average_Spikes,2)), 
  #             position = position_stack(vjust=0.5)) +
  #   theme_void()
  # 
  # unsucc_pie <- ggplot(unsucc_m, aes(x="", 
  #                                      y=Average_Spikes, 
  #                                      fill=Brain_Area)) +
  #   geom_col() +
  #   coord_polar(theta = "y") + 
  #   labs(title= paste("Failures, Session",i), 
  #        fill ="Brain_Area", x= "", y="") +
  #   geom_text(aes(label=round(Average_Spikes,2)), 
  #             position = position_stack(vjust=0.5)) +
  #   theme_void()
  # 
  # if (i == 1) {
  #   s1piespks[[1]] <- succ_pie
  #   s1piespks[[2]] <- unsucc_pie
  # }
  # if (i == 2) {
  #   s2piespks[[1]] <- succ_pie
  #   s2piespks[[2]] <- unsucc_pie
  # }
  # if (i == 3) {
  #   s3piespks[[1]] <- succ_pie
  #   s3piespks[[2]] <- unsucc_pie
  # }
  # if (i == 4) {
  #   s4piespks[[1]] <- succ_pie
  #   s4piespks[[2]] <- unsucc_pie
  # }
  # if (i == 5) {
  #   s5piespks[[1]] <- succ_pie
  #   s5piespks[[2]] <- unsucc_pie
  # }
  # if (i == 6) {
  #   s6piespks[[1]] <- succ_pie
  #   s6piespks[[2]] <- unsucc_pie
  # }
  # if (i == 7) {
  #   s7piespks[[1]] <- succ_pie
  #   s7piespks[[2]] <- unsucc_pie
  # }
  # if (i == 8) {
  #   s8piespks[[1]] <- succ_pie
  #   s8piespks[[2]] <- unsucc_pie
  # }
  # if (i == 9) {
  #   s9piespks[[1]] <- succ_pie
  #   s9piespks[[2]] <- unsucc_pie
  # }
  # if (i == 10) {
  #   s10piespks[[1]] <- succ_pie
  #   s10piespks[[2]] <- unsucc_pie
  # }
  # if (i == 11) {
  #   s11piespks[[1]] <- succ_pie
  #   s11piespks[[2]] <- unsucc_pie
  # }
  # if (i == 12) {
  #   s12piespks[[1]] <- succ_pie
  #   s12piespks[[2]] <- unsucc_pie
  # }
  # if (i == 13) {
  #   s13piespks[[1]] <- succ_pie
  #   s13piespks[[2]] <- unsucc_pie
  # }
  # if (i == 14) {
  #   s14piespks[[1]] <- succ_pie
  #   s14piespks[[2]] <- unsucc_pie
  # }
  # if (i == 15) {
  #   s15piespks[[1]] <- succ_pie
  #   s15piespks[[2]] <- unsucc_pie
  # }
  # if (i == 16) {
  #   s16piespks[[1]] <- succ_pie
  #   s16piespks[[2]] <- unsucc_pie
  # }
  # if (i == 17) {
  #   s17piespks[[1]] <- succ_pie
  #   s17piespks[[2]] <- unsucc_pie
  # }
  # if (i == 18) {
  #   s18piespks[[1]] <- succ_pie
  #   s18piespks[[2]] <- unsucc_pie
  # }
  
  #correlation matrix:
  
  trial.summary$feedback[trial.summary$feedback == -1] <- 0
  
  corr2 <- trial.summary[, -c((ncol(trial.summary)-2):ncol(trial.summary))] %>%
    cor()
  
  
  corrplot <- ggcorrplot(corr2, type="lower", lab = TRUE) +
    labs(title = paste("Brain Area, S", i))
  
  corr1[[i]] <- corrplot
  
  corr3 <- trial.summary %>%
    select(feedback, `left contr.`, `right contr.`) %>%
    cor()
  
  corrplot2 <- ggcorrplot(corr3, type = "lower", lab = TRUE) +
    labs(title = paste("L/R Contrast, S", i))
  
  corr4[[i]] <- corrplot2
  
  trial.summary3 = subset(trial.summary, select = -c(feedback, `left contr.`, `right contr.`, id))
  
  trial.lm = glm(trial.summary$feedback ~ ., family = "binomial", data = trial.summary3)
  
  trial.lms[[i]] = trial.lm
  
}

# grid.arrange(grobs=all_sessions_bar, ncol=1)
# grid.arrange(grobs=all_sessions_bar2, ncol=1)
# grid.arrange(grobs=all_sessions_bar3, ncol=1)
# grid.arrange(grobs=all_sessions_bar4, ncol=1)
# grid.arrange(grobs=all_sessions_bar5, ncol=1)
# grid.arrange(grobs=all_sessions_bar6, ncol=1)

#  grid.arrange(grobs=s1piespks,ncol=2)
#  grid.arrange(grobs=s2piespks,ncol=2)
#  grid.arrange(grobs=s3piespks,ncol=2)
#  grid.arrange(grobs=s4piespks,ncol=2)
#  grid.arrange(grobs=s5piespks,ncol=2)
#  grid.arrange(grobs=s6piespks,ncol=2)
#  grid.arrange(grobs=s7piespks,ncol=2)
#  grid.arrange(grobs=s8piespks,ncol=2)
#  grid.arrange(grobs=s9piespks,ncol=2)
#  grid.arrange(grobs=s10piespks,ncol=2)
#  grid.arrange(grobs=s11piespks,ncol=2)
#  grid.arrange(grobs=s12piespks,ncol=2)
#  grid.arrange(grobs=s13piespks,ncol=2)
#  grid.arrange(grobs=s14piespks,ncol=2)
#  grid.arrange(grobs=s15piespks,ncol=2)
#  grid.arrange(grobs=s16piespks,ncol=2)
#  grid.arrange(grobs=s17piespks,ncol=2)
#  grid.arrange(grobs=s18piespks,ncol=2)


## -----------------------------------------------------------------------
#pies

all_sessions_feed_pie1 <- list()
all_sessions_feed_pie2 <- list()
all_sessions_feed_pie3 <- list()

all_sessions_feed_vec <- c()
session_number <- c()

for (i in seq_along(all_sessions)) {
  feedback_type <- all_sessions[[i]]$feedback_type
  feed <- data.frame(x=as.factor(feedback_type)) 
  feed_counts = feed %>% group_by(x) %>% count() %>% 
    ungroup() %>%
    mutate(perc = `n` / sum(`n`) ) %>%
    mutate(labels = scales::percent(perc))
  names(feed_counts) <- c("Feedback", "Count", "Perc", "Labels")
  feed_plot <- ggplot(feed_counts, aes(x="", y=Count, fill=Feedback)) +
    geom_col() +
    geom_text(aes(label=Labels), position = position_stack(vjust=0.5)) + 
    coord_polar(theta = "y") + 
    labs(title= paste("Feedback in Session",i), fill ="Feedback", x= "", y="") +
    theme_void()
  if (i <= 6) {
    all_sessions_feed_pie1[[i]] <- feed_plot
  }
  else if (i <= 12) {
    all_sessions_feed_pie2[[i-6]] <- feed_plot
  }
  else if (i <= 18) {
    all_sessions_feed_pie3[[i-12]] <- feed_plot
  }
  all_sessions_feed_vec <- c(all_sessions_feed_vec, as.character(feed_counts[2,4]))
}

sessions <- c()
dates <- c()
for (i in seq_along(all_sessions)) {
  session <- i
  date <- all_sessions[[i]]$date_exp
  sessions <- c(sessions, session)
  dates <- c(dates, date)
}

# neurons
# visual cortex neurons
VIS = c(0.3, 0.17, 0.17, 0.21, 0.24, 0.2, 0.11, -0.01, 0.26, 0.29, 0.28, 0.18, 0.27, 0.03, 0.09, 0.12, 0.21, 0.2, 0.12, -0.2)
VIS.m = mean(VIS)
# frontal cortex = RSP, ACA, MOS, PL, ILA, ORB, MOp, SSp
FR = c(0.33, 0.32, 0.12, 0.06, 0.09, 0.01, -0.05, 0, 0.06, 0.11, 0.23, 0.09, 0.21, 0.14, 0.06, -0.02, 0.08, 0.08, 0.1, -0.14, 0.24, 0.13, 0.21, 0.04)
FR.m = mean(FR)
# hippocampus = DG, CA3, CA1, POST, SUB
HIP = c(0.17, 0.28, 0.25, 0.18, 0.17, 0.18, 0.19, 0.05, 0.14, 0.27, 0.09, 0.2, 0.13, 0.05, 0.18, 0.28, 0.2, 0.07, 0.29, 0.27, 0.19, 0.12, 0.07, 0.07, 0.22, 0.17, 0.15, 0.05, 0.03, 0.23, 0.09, 0.05, 0.07)
HIP.m = mean(HIP)
# olfactory bulb = OLF, BLA
OLF = c(0.05, 0.09)
OLF.m = mean(OLF)
# basal ganglia = CP, GPe, SNr, ACB , LS
BG = c(0.09, 0.21, 0.14, 0.19, 0.14, 0.32, 0.11, 0.06, 0.12, 0.14, 0.21)
BG.m = mean(BG)
# thalamus = LGd, LP, LD, POL, MD, VPL, PO, VPM, RT, MG
TH = c(0.23, 0.29, 0.11, 0.05, 0.07, 0.02, 0.32, 0.22, 0.3, 0.16, 0.07, 0.28, 0.2, 0.12, 0.27, -0.04, 0.32, 0.22, 0.14, 0.03, 0.22)
TH.m = mean(TH)
# motor-related superior colliculus = SCs, SCm, MRN, APN, PAG
SC = c(0.16, 0.11, 0.16, 0.17, 0.01, 0.05, 0.14, 0.25, 0.01, 0.21, 0.02)
SC.m = mean(SC)

# corr1[[1]]
# corr1[[2]]
# corr1[[3]]
# corr1[[4]]
# corr1[[5]]
# corr1[[6]]
# corr1[[7]]
# corr1[[8]]
# corr1[[9]]
# corr1[[10]]
# corr1[[11]]
# corr1[[12]]
# corr1[[13]]
# corr1[[14]]
# corr1[[15]]
# corr1[[16]]
# corr1[[17]]
# corr1[[18]]





## -----------------------------------------------------------------------
corr1[[1]]
corr1[[18]]


## -----------------------------------------------------------------------
# grid.arrange(grobs=all_sessions_bar, ncol=1)
# grid.arrange(grobs=all_sessions_bar2, ncol=1)
# grid.arrange(grobs=all_sessions_bar3, ncol=1)
# grid.arrange(grobs=all_sessions_bar4, ncol=1)
# grid.arrange(grobs=all_sessions_bar5, ncol=1)
# grid.arrange(grobs=all_sessions_bar6, ncol=1)

 n.session=18

# in library tidyverse
 meta <- tibble(
   mouse_name = rep('name',n.session),
   date_exp =rep('dt',n.session),
   n_brain_area = rep(0,n.session),
   n_neurons = rep(0,n.session),
   n_trials = rep(0,n.session),
   success_rate = rep(0,n.session)
 )
#
 for(i in 1:18){
   tmp = all_sessions[[i]];
   meta[i,1]=tmp$mouse_name;
   meta[i,2]=tmp$date_exp;
   meta[i,3]=length(unique(tmp$brain_area));
   meta[i,4]=dim(tmp$spks[[1]])[1];
   meta[i,5]=length(tmp$feedback_type);
   meta[i,6]=mean(tmp$feedback_type+1)/2;
 }


spk.means = c()
feeds = c()
for (i in seq_along(session1$spks)) {
  #calc average spikes per neuron per session
  spk = session1$spks[[i]]
  spk.mean = (sum(as.vector(spk)))/734
  
  #extract succ/failure
  feed = session1$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s1.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                          aes(x=feedback, 
                              y=avg.spks, 
                              fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S1, 0-0.4s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") +
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

spk.means = c()
feeds = c()
for (i in seq_along(session18$spks)) {
  #calc average spikes per neuron per session
  spk = session18$spks[[i]]
  spk.mean = (sum(as.vector(spk)))/1090
  
  #extract succ/failure
  feed = session18$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s18.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                           aes(x=feedback, 
                               y=avg.spks, 
                               fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S18, 0-0.4s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") + 
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

s1.s18.avg.spks.plots1 = list(s1.avg.spks.plot, s18.avg.spks.plot)

# grid.arrange(grobs=s1.s18.avg.spks.plots1, ncol=1)



## -----------------------------------------------------------------------
spk.means = c()
feeds = c()
for (i in seq_along(session1$spks)) {
  #calc average spikes per neuron per session
  spk = session1$spks[[i]]
  spk2 = spk[, 1:20]
  spk.mean = (sum(as.vector(spk2)))/734
  
  #extract succ/failure
  feed = session1$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s1.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                          aes(x=feedback, 
                              y=avg.spks, 
                              fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S1, 0-0.2s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") +
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

spk.means = c()
feeds = c()
for (i in seq_along(session18$spks)) {
  #calc average spikes per neuron per session
  spk = session18$spks[[i]]
  spk2 = spk[, 1:20]
  spk.mean = (sum(as.vector(spk2)))/1090
  
  #extract succ/failure
  feed = session18$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s18.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                           aes(x=feedback, 
                               y=avg.spks, 
                               fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S18, 0-0.2s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") + 
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

s1.s18.avg.spks.plots2 = list(s1.avg.spks.plot, s18.avg.spks.plot)

# grid.arrange(grobs=s1.s18.avg.spks.plots2, ncol=1)


## -----------------------------------------------------------------------
spk.means = c()
feeds = c()
for (i in seq_along(session1$spks)) {
  #calc average spikes per neuron per session
  spk = session1$spks[[i]]
  spk2 = spk[, 20:40]
  spk.mean = (sum(as.vector(spk2)))/734
  
  #extract succ/failure
  feed = session1$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s1.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                          aes(x=feedback, 
                              y=avg.spks, 
                              fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S1, 0.2-0.4s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") +
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

spk.means = c()
feeds = c()
for (i in seq_along(session18$spks)) {
  #calc average spikes per neuron per session
  spk = session18$spks[[i]]
  spk2 = spk[, 20:40]
  spk.mean = (sum(as.vector(spk2)))/1090
  
  #extract succ/failure
  feed = session18$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s18.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                           aes(x=feedback, 
                               y=avg.spks, 
                               fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S18, 0.2-0.4s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") + 
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

s1.s18.avg.spks.plots3 = list(s1.avg.spks.plot, s18.avg.spks.plot)

# grid.arrange(grobs=s1.s18.avg.spks3.plots, ncol=1)


## -----------------------------------------------------------------------
spk.means = c()
feeds = c()
for (i in seq_along(session1$spks)) {
  #calc average spikes per neuron per session
  spk = session1$spks[[i]]
  spk2 = spk[, 10:30]
  spk.mean = (sum(as.vector(spk2)))/734
  
  #extract succ/failure
  feed = session1$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s1.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                          aes(x=feedback, 
                              y=avg.spks, 
                              fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S1, 0.1-0.3s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") +
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

spk.means = c()
feeds = c()
for (i in seq_along(session18$spks)) {
  #calc average spikes per neuron per session
  spk = session18$spks[[i]]
  spk2 = spk[, 10:30]
  spk.mean = (sum(as.vector(spk2)))/1090
  
  #extract succ/failure
  feed = session18$feedback_type[[i]]
  feed2 = (feed+1)/2
  if (feed2 == 1) {
    feed3 = "Success"
  }
  else if (feed2 == 0) {
    feed3 = "Failure"
  }
  
  #add to vectors
  spk.means = c(spk.means, spk.mean)
  feeds = c(feeds, feed3)
}
avg.spks.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

success_mean <- avg.spks.df %>%
  filter(feedback == "Success") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

failure_mean <- avg.spks.df %>%
  filter(feedback == "Failure") %>%
  pull(avg.spks) %>%
  mean(na.rm = TRUE)

means.s1.spks = c(success_mean,failure_mean)
col_names = c("Success", "Failure")

means.s1.spks.df = data.frame(
  avg.spks = means.s1.spks,
  feedback = col_names
)

s18.avg.spks.plot = ggplot(data=means.s1.spks.df, 
                           aes(x=feedback, 
                               y=avg.spks, 
                               fill=feedback)) +
  geom_bar(stat="identity") +
  labs(title = "Average Spikes per Neuron, S18, 0.1-0.3s", 
       x = "Feedback Type",
       y = "Average Spikes per Neuron") + 
  geom_text(aes(label= round(avg.spks,2)), vjust= 3)

s1.s18.avg.spks.plots4 = list(s1.avg.spks.plot, s18.avg.spks.plot)

grid.arrange(grobs=s1.s18.avg.spks.plots4, ncol=1)


## ---- message = FALSE---------------------------------------------------

get_trail_data <- function(session_id, trail_id){
  spikes <- all_sessions[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }

  #trail_tibble <- as_tibble(spikes) %>% set_names(binename) %>%  add_column("brain_area" = session[[session_id]]$brain_area ) %>% group_by(brain_area) %>% summarize( "sum_spikes" =across(everything(),sum),.groups = "drop") 
  trail_tibble <- tibble("neuron_spike" = rowSums(spikes))  %>%  add_column("brain_area" = all_sessions[[session_id]]$brain_area ) %>% group_by(brain_area) %>% summarize( region_sum_spike = sum(neuron_spike), region_count = n(),region_mean_spike = mean(neuron_spike)) 
  trail_tibble  = trail_tibble%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= all_sessions[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= all_sessions[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= all_sessions[[session_id]]$feedback_type[trail_id])
  trail_tibble
}

get_session_data <- function(session_id){
  n_trail <- length(all_sessions[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail) {
    trail_tibble <- get_trail_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- do.call(rbind, trail_list)
  session_tibble <- session_tibble %>% 
    add_column("mouse_name" = all_sessions[[session_id]]$mouse_name) %>% 
    add_column("date_exp" = all_sessions[[session_id]]$date_exp) %>% 
    add_column("session_id" = session_id) 
  session_tibble
}


session_list = list()
for (session_id in 1:18) {
  session_list[[session_id]] <- get_session_data(session_id)
}

full_tibble <- do.call(rbind, session_list)
full_tibble$success <- full_tibble$feedback_type == 1
full_tibble$success <- as.numeric(full_tibble$success)
full_tibble$contrast_diff <- abs(full_tibble$contrast_left-full_tibble$contrast_right)

binename <- paste0("bin", as.character(1:40))

get_trail_functional_data <- function(session_id, trail_id){
  spikes <- all_sessions[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }

  trail_bin_average <- matrix(colMeans(spikes), nrow = 1)
  colnames(trail_bin_average) <- binename
  trail_tibble  = as_tibble(trail_bin_average)%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= all_sessions[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= all_sessions[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= all_sessions[[session_id]]$feedback_type[trail_id])
  
  trail_tibble
}

get_session_functional_data <- function(session_id){
  n_trail <- length(all_sessions[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_functional_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- as_tibble(do.call(rbind, trail_list))
  session_tibble <- session_tibble %>% add_column("mouse_name" = all_sessions[[session_id]]$mouse_name) %>% add_column("date_exp" = all_sessions[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

session_list = list()
for (session_id in 1: 18){
  session_list[[session_id]] <- get_session_functional_data(session_id)
}
full_functional_tibble <- as_tibble(do.call(rbind, session_list))
full_functional_tibble$session_id <- as.factor(full_functional_tibble$session_id )
full_functional_tibble$contrast_diff <- abs(full_functional_tibble$contrast_left-full_functional_tibble$contrast_right)

full_functional_tibble$success <- full_functional_tibble$feedback_type == 1
full_functional_tibble$success <- as.numeric(full_functional_tibble$success)

col_names <-names(full_functional_tibble)
region_sum_subset <- col_names[grep("^region_sum", col_names)]
region_mean_subset <- col_names[grep("^region_mean", col_names)]

# average_spike <- full_tibble %>% group_by( session_id,trail_id) %>% summarise(mean_spike = mean(region_mean_spike))
average_spike <- full_tibble %>% group_by( session_id,trail_id) %>% summarise(mean_spike = sum(region_sum_spike)/sum(region_count))

average_spike$mouse_name <- full_functional_tibble$mouse_name
average_spike$contrast_diff <- full_functional_tibble$contrast_diff
average_spike$success <- full_functional_tibble$success

ggplot(average_spike, aes(x = trail_id, y = mean_spike)) + 
  geom_line()+
  geom_smooth(method = "loess")+  # Fit a smooth spline

  facet_wrap(~session_id)


## -----------------------------------------------------------------------

diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session1$contrast_left)) {
  left = session1$contrast_left[[i]]
  right = session1$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session1$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

cor1 = ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) +
  labs(title="L/R Difference, S1")



## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session18$contrast_left)) {
  left = session18$contrast_left[[i]]
  right = session18$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session18$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

cor2 = ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference, S18")

corplots = list(cor1, cor2)

grid.arrange(grobs=corplots, ncol=2)


## -----------------------------------------------------------------------
sessionsvsdate <- data.frame(Session = sessions, Date = dates, Success_Rate = all_sessions_feed_vec)
kable(sessionsvsdate, format="html", table.attr ="class='table table striped'")


## -----------------------------------------------------------------------

sessions.num <- as.numeric(sessions)

success.num <- as.numeric(sub("%", "", all_sessions_feed_vec)) / 100

failure.num <- 1 - success.num

sessionsbysuccess <- data.frame(session = sessions.num, success = success.num, failure = failure.num)

corr = sessionsbysuccess %>%
  cor

ggcorrplot(corr, type= "lower", hc.order = TRUE, lab = TRUE)



## -----------------------------------------------------------------------

cori <- c()
forssmann <- c()
hench <- c()
lederberg <- c()

names <- c()

for (i in seq_along(all_sessions)) {
  name <- all_sessions[[i]]$mouse_name
  if (name == "Cori") {
    cori <- c(cori, success.num[i])
  }
  else if (name == "Forssmann") {
    forssmann <- c(forssmann, success.num[i])
  }
  else if (name == "Hench") {
    hench <- c(hench, success.num[i])
  }
  else if (name == "Lederberg") {
    lederberg <- c(lederberg, success.num[i])
  }
  names <- c(names, name)
}

cori.m = mean(cori)
fors.m = mean(forssmann)
hench.m = mean(hench)
leder.m = mean(lederberg)

mouse.m = c(cori.m, fors.m, hench.m, leder.m)
mouse = c("Cori", "Forssmann", "Hench", "Lederberg")
trial.lab = c(paste("n =",length(cori)), paste("n =",length(forssmann)), paste("n =",length(hench)), paste("n =",length(lederberg)))
trials = c(length(cori), length(forssmann), length(hench), length(lederberg))
mouse.ms = data.frame(name = mouse, mean = mouse.m, trials = trials)

ggplot(data = mouse.ms, aes(x = name, y = mean, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title=paste("Average Success Rate by Mouse"), 
       x= "Mouse Name", 
       y= "Average Success Rate") +
  geom_text(aes(label = trial.lab), vjust= 3)



## ---- message = FALSE---------------------------------------------------

binename <- paste0("bin", as.character(1:40))

get_trail_functional_data <- function(session_id, trail_id){
  spikes <- all_sessions[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }

  trail_bin_average <- matrix(colMeans(spikes), nrow = 1)
  colnames(trail_bin_average) <- binename
  trail_tibble  = as_tibble(trail_bin_average)%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= all_sessions[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= all_sessions[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= all_sessions[[session_id]]$feedback_type[trail_id])
  
  trail_tibble
}

get_session_functional_data <- function(session_id){
  n_trail <- length(all_sessions[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_functional_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- as_tibble(do.call(rbind, trail_list))
  session_tibble <- session_tibble %>% add_column("mouse_name" = all_sessions[[session_id]]$mouse_name) %>% add_column("date_exp" = all_sessions[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

session_list = list()
for (session_id in 1: 18){
  session_list[[session_id]] <- get_session_functional_data(session_id)
}
full_functional_tibble <- as_tibble(do.call(rbind, session_list))
full_functional_tibble$session_id <- as.factor(full_functional_tibble$session_id )
full_functional_tibble$contrast_diff <- abs(full_functional_tibble$contrast_left-full_functional_tibble$contrast_right)

full_functional_tibble$success <- full_functional_tibble$feedback_type == 1
full_functional_tibble$success <- as.numeric(full_functional_tibble$success)


bin_feed <- c("feedback_type" ,binename)
bin_feed_df = full_functional_tibble[bin_feed]

bins <- c(binename)
bin_df = full_functional_tibble[bins]

# bin_feed_df = functional tibble for prediction model

label <- as.numeric(full_functional_tibble$success)
X <- model.matrix(~., bin_feed_df)

bin_long = tidyr::pivot_longer(bin_feed_df, cols = c(all_of(binename)), names_to = "bin", values_to = "avg spikes")

bin_long$bin = sub("^bin", "", bin_long$bin)

bin_long$bin = as.numeric(bin_long$bin)
bin_long$bin = (bin_long$bin)/100

bin_long$feedback_type[bin_long$feedback_type == -1] = "Failure"
bin_long$feedback_type[bin_long$feedback_type == 1] = "Success"

ggplot(bin_long, aes(x=bin, y=`avg spikes`, color=feedback_type)) +
  geom_point(alpha=0.1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Average spike rate per time bin by feedback type",
       x = "Time (s)",
       y = "Average spike rate per neuron") +
  scale_color_discrete(name = "Feedback type")


## -----------------------------------------------------------------------
lr.diffs = c()
feeds = c()
for (i in 1:18) {
  left = all_sessions[[i]]$contrast_left
  right = all_sessions[[i]]$contrast_right
  feed = all_sessions[[i]]$feedback_type
  for (j in seq_along(left)) {
    #left right difference
    l = left[[j]]
    r = right[[j]]
    diff = abs(l - r)
    lr.diffs = c(lr.diffs, diff)
    
    #feedback type
    feed.trial = feed[[j]]
    feeds = c(feeds, feed.trial)
  }
}
all.lr.diffs.df = data.frame(
  lr.diffs = lr.diffs,
  feedback = feeds
)


all.lr.diffs.df$feedback[all.lr.diffs.df$feedback == -1] = "Failure"
all.lr.diffs.df$feedback[all.lr.diffs.df$feedback == 1] = "Success"

ggplot(all.lr.diffs.df, aes(x = lr.diffs, fill= feedback)) + 
  geom_histogram(binwidth = 0.25, alpha = 0.7) + 
  facet_wrap(~feedback, scales = "free") + 
  labs(title = "Difference in L/R Contrast by Feedback Type, All Sessions", 
       x = "Feedback Type", 
       y = "Frequency") +
  geom_vline(data = subset(all.lr.diffs.df, feedback == "Success"), 
             aes(xintercept = median(lr.diffs)), color = "blue", linetype = "dashed") +
  geom_vline(data = subset(all.lr.diffs.df, feedback == "Failure"), 
             aes(xintercept = median(lr.diffs)), color = "red", linetype = "dashed")


## -----------------------------------------------------------------------
# kable(meta, format = "html", table.attr = "class='table table-striped'",digits=2) 


## -----------------------------------------------------------------------
# lm

best_coeffs = list()
best_coeffs_values = list()
all_coeffs = list()
for (i in 1:18) {
  model = trial.lms[[i]]
  coeff = coef(model)[-1]
  best_coeff = names(coeff)[which(coeff > 0)]
  best_coeffs[[i]] = best_coeff
  best_coeffs_values[[i]] = coeff[best_coeff]
  all_coeffs[[i]] = coeff
}

#can't standardize across sessions :(
#knn for distance between two trials
#multiple imputation?



## -----------------------------------------------------------------------

spk.means = c()
feeds = c()
for (i in 1:18) {
  spks = all_sessions[[i]]$spks 
  feed = all_sessions[[i]]$feedback_type
  n.neurons = length(all_sessions[[i]]$brain_area)
  for (j in seq_along(spks)) {
    #spk means
    spks2 = spks[[j]] 
    spks3 = spks2[,10:30]
    spk.mean = (sum(spks3))/n.neurons
    spk.means = c(spk.means,spk.mean)
    
    #feedback type
    feed.trial = feed[[j]]
    feeds = c(feeds, feed.trial)
  }
}
all.spk.means.df = data.frame(
  avg.spks = spk.means,
  feedback = feeds
)

all.spk.means.df$feedback = factor(all.spk.means.df$feedback)

ggplot(all.spk.means.df, aes(x = avg.spks, fill= feedback)) + 
  geom_histogram(binwidth = 0.025, alpha = 0.7) + 
  facet_wrap(~feedback, scales = "free") + 
  labs(title = "Average Spikes per Neuron by Feedback Type, All Sessions", 
       x = "Feedback Type: -1 = Failure, 1 = Success", 
       y = "Frequency") +
  geom_vline(data = subset(all.spk.means.df, feedback == 1), 
             aes(xintercept = median(avg.spks)), color = "blue", linetype = "dashed") +
  geom_vline(data = subset(all.spk.means.df, feedback == -1), 
             aes(xintercept = median(avg.spks)), color = "red", linetype = "dashed")


## -----------------------------------------------------------------------

all.avg = c()
for (i in 1:18) {
  spks.trial=all_sessions[[i]]$spks[[1]]
  total.spikes=apply(spks.trial,1,sum)
  avg.spikes=mean(total.spikes)
  all.avg = c(all.avg, avg.spikes)
}

di.table1 = data.frame(
  `Mouse Name` = meta$mouse_name,
  `Session Date` = meta$date_exp,
  `Success Rate` = meta$success_rate,
  `Average Spikes per Neuron` = all.avg
)

# kable(di.table1, format = "html", table.attr = "class='table table-striped'",digits=2)



## -----------------------------------------------------------------------
# data frame of average brain spikes per brain area of session 1

s1.avg.spks = all_summaries[[1]]

s1.avg.predict = s1.avg.spks[,-c(ncol(s1.avg.spks) - 2:0)]

s1.avg.spks$feedback[s1.avg.spks$feedback == -1] = "Failure"
s1.avg.spks$feedback[s1.avg.spks$feedback == 1] = "Success"

s1_long = tidyr::pivot_longer(s1.avg.spks, cols = c(ACA, CA3, DG, LS, MOs, root, SUB, VISp), names_to = "brain area")

s1.average = aggregate(value ~ `brain area` + feedback, data = s1_long, FUN = mean)

ggplot(s1.average, aes(x = `brain area`, y = value, fill = feedback)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.7) +
  labs(title = "Average Spikes per Brain Area by Success and Failure, Session 1",
       x = "Brain Area", y = "Average Spikes") +
  theme_minimal()



## -----------------------------------------------------------------------
# data frame of average brain spikes per brain area of session 18

s18.avg.spks = all_summaries[[18]]

s18.avg.predict = s18.avg.spks[,-c(ncol(s18.avg.spks) - 2:0)]

s18.avg.spks$feedback[s18.avg.spks$feedback == -1] = "Failure"
s18.avg.spks$feedback[s18.avg.spks$feedback == 1] = "Success"

s18_long = tidyr::pivot_longer(s18.avg.spks, cols = c(ACB, CA3, CP, LGd, OT, root, SI, SNr, TH, ZI), names_to = "brain area")

s18.average = aggregate(value ~ `brain area` + feedback, data = s18_long, FUN = mean)

ggplot(s18.average, aes(x = `brain area`, y = value, fill = feedback)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.7) +
  labs(title = "Average Spikes per Brain Area by Success and Failure, Session 18",
       x = "Brain Area", y = "Average Spikes") +
  theme_minimal()



## ---- include = FALSE---------------------------------------------------
set.seed(22324)

# all.spk.means.df

# linear model

# split data
n_obs = nrow(all.spk.means.df)
sample <- sample.int(n = n_obs, size = floor(.8 * n_obs), replace = F)
train <- all.spk.means.df[sample, ]
test  <- all.spk.means.df[-sample, ]

# logistic regression
fit1 <- glm(feedback ~ avg.spks, data = train, family="binomial")

# prediction model
pred1 <- predict(fit1, test %>% select(-feedback), type = 'response')
prediction1 <- as.numeric(ifelse(pred1 > 0.5, 1, -1))
prediction1_factor <- factor(ifelse(prediction1 >0.5, 1, -1), levels = c(1,-1))
actual_factor <- factor(test$feedback, levels = c(1,-1))
accuracy_mA = mean(prediction1_factor == actual_factor)

# confusion matrix
conf_matrix <- table(actual_factor, prediction1_factor)

true_pos = conf_matrix[1]
false_pos = conf_matrix[2]
true_neg = conf_matrix[4]
false_neg = conf_matrix[3]

all_neg = true_neg + false_neg
all_pos = true_pos + false_pos

#auc value
auc_value1 = roc(test$feedback,pred1)$auc



## ---- include = FALSE---------------------------------------------------
set.seed(22324) #reproducibility

label <- as.numeric(full_functional_tibble$success)
X <- model.matrix(~., bin_df)

trainIndex <- createDataPartition(label, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_df <- bin_df[trainIndex, ]
train_X <- X[trainIndex,]
test_df <- bin_df[-trainIndex, ]
test_X <- X[-trainIndex,]

train_label <- label[trainIndex]
test_label <- label[-trainIndex]

xgb_model <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nround = 10)



## -----------------------------------------------------------------------
predictions <- predict(xgb_model, newdata = test_X)
predicted_labels <- as.numeric(ifelse(predictions > 0.5, 1, 0))
accuracy_mB <- mean(predicted_labels == test_label)

conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))

true_neg = conf_matrix$table[1]
false_neg = conf_matrix$table[2]
true_pos = conf_matrix$table[4]
false_pos = conf_matrix$table[3]

all_neg = true_neg + false_neg
all_pos = true_pos + false_pos



## -----------------------------------------------------------------------

pr_curve_data <- pr.curve(scores.class0 = predicted_labels, weights.class0 = test_label, curve = TRUE)

plot(pr_curve_data, main = "Precision-Recall Curve, Model B")


## ---- include = FALSE---------------------------------------------------

set.seed(22324)

# all.lr.diffs.df

all.lr.diffs.df$feedback[all.lr.diffs.df$feedback == "Success"] = 1
all.lr.diffs.df$feedback[all.lr.diffs.df$feedback == "Failure"] = -1
all.lr.diffs.df$feedback = as.factor(all.lr.diffs.df$feedback)

# anova? linear model prob

# split data
n_obs = nrow(all.lr.diffs.df)
sample <- sample.int(n = n_obs, size = floor(.8 * n_obs), replace = F)
train <- all.lr.diffs.df[sample, ]
test  <- all.lr.diffs.df[-sample, ]

# logistic regression
fit2 <- glm(feedback ~ lr.diffs, data = train, family="binomial")

# prediction model
pred2 <- predict(fit2, test %>% select(-feedback), type = 'response')
prediction2 <- as.numeric(ifelse(pred2 > 0.5, 1, -1))
prediction2_factor <- factor(ifelse(prediction2 >0.5, 1, -1), levels = c(1,-1))
actual_factor <- factor(test$feedback, levels = c(1,-1))
accuracy_mC = mean(prediction2_factor == actual_factor)

# confusion matrix
conf_matrix <- table(actual_factor, prediction2_factor)

true_pos = conf_matrix[1]
false_pos = conf_matrix[2]
true_neg = conf_matrix[4]
false_neg = conf_matrix[3]

all_neg = true_neg + false_neg
all_pos = true_pos + false_pos

#auc value
auc_value2 = roc(test$feedback,pred2)$auc




## ---- include = FALSE---------------------------------------------------

# s1.avg.predict

s1.pred = s1.avg.predict[, -ncol(s1.avg.predict)]

# xgboost

# possible problem: overfitting

set.seed(40123) #reproducibility

label = as.numeric(session1$feedback_type)
label[label == -1] = 0

X <- model.matrix(~., s1.pred)

trainIndex <- createDataPartition(label, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_df <- s1.pred[trainIndex, ]
train_X <- X[trainIndex,]
test_df <- s1.pred[-trainIndex, ]
test_X <- X[-trainIndex,]

train_label <- label[trainIndex]
test_label <- label[-trainIndex]

xgb_model1 <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nround = 10)



## -----------------------------------------------------------------------
predictions <- predict(xgb_model1, newdata = test_X)
predicted_labels <- as.numeric(ifelse(predictions > 0.5, 1, 0))
accuracy_mD1 <- mean(predicted_labels == test_label)

conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))

true_neg = conf_matrix$table[1]
false_neg = conf_matrix$table[2]
true_pos = conf_matrix$table[4]
false_pos = conf_matrix$table[3]

all_neg = true_neg + false_neg
all_pos = true_pos + false_pos



## -----------------------------------------------------------------------
pr_curve_data <- pr.curve(scores.class0 = predicted_labels, weights.class0 = test_label, curve = TRUE)

plot(pr_curve_data, main = "Precision-Recall Curve, Model D, Session 1")


## ---- include = FALSE---------------------------------------------------

# s1.avg.predict

s18.pred = s18.avg.predict[, -ncol(s18.avg.predict)]

# xgboost

# possible problem: overfitting

set.seed(22324) #reproducibility

label = as.numeric(session18$feedback_type)
label[label == -1] = 0

X <- model.matrix(~., s18.pred)

trainIndex <- createDataPartition(label, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_df <- s18.pred[trainIndex, ]
train_X <- X[trainIndex,]
test_df <- s18.pred[-trainIndex, ]
test_X <- X[-trainIndex,]

train_label <- label[trainIndex]
test_label <- label[-trainIndex]

xgb_model2 <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nround = 10)



## -----------------------------------------------------------------------
predictions <- predict(xgb_model2, newdata = test_X)
predicted_labels <- as.numeric(ifelse(predictions > 0.5, 1, 0))
accuracy_mD2 <- mean(predicted_labels == test_label)

conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))

true_neg = conf_matrix$table[1]
false_neg = conf_matrix$table[2]
true_pos = conf_matrix$table[4]
false_pos = conf_matrix$table[3]

all_neg = true_neg + false_neg
all_pos = true_pos + false_pos



## -----------------------------------------------------------------------
pr_curve_data <- pr.curve(scores.class0 = predicted_labels, weights.class0 = test_label, curve = TRUE)

plot(pr_curve_data, main = "Precision-Recall Curve, Model D, Session 18")


## -----------------------------------------------------------------------
AUC = c(0.6030, 0.7205, 0.5947, 0.8385, 0.8047)
labels = c("A", "B", "C", "D, session 1", "D, session 18")

df = data.frame(
  Model = labels,
  `AUC` = AUC
)

kable(df, format = "html", table.attr ="class='table table striped'")


## -----------------------------------------------------------------------
# formatting test data

average_spike_area<-function(i.t,this_session){
  spk.trial = this_session$spks[[i.t]]
  area= this_session$brain_area
  spk.count=apply(spk.trial,1,sum)
  spk.average.tapply=tapply(spk.count, area, mean)
  return(spk.average.tapply)
}

test_summaries <- list()

for (i in seq_along(all_tests)) {
  #getting data frame (using disc code lol)
  rows = length(all_tests[[i]]$spks)
  trial.summary <- matrix(nrow=rows, ncol=40)
  i.s = i
  i.t = 1
  spk.trial = all_tests[[i.s]]$spks[[i.t]]
  area=all_tests[[i.s]]$brain_area
  spk.count=apply(spk.trial,1,sum)
  tmp <- data.frame(
    area = area, 
    spikes = spk.count
  )
  spk.average.dplyr =tmp %>%
  group_by(area) %>%
  summarize(mean= mean(spikes))
  average_spike_area(1,this_session = all_tests[[i.s]])
  n.trial=length(all_tests[[i.s]]$feedback_type)
  n.area=length(unique(all_tests[[i.s]]$brain_area ))
  trial.summary =matrix(nrow=n.trial,ncol= n.area+1+2+1)
  for(i.t in 1:n.trial) {
    trial.summary[i.t,]= c(
      average_spike_area(i.t,this_session = all_tests[[i.s]]),  
      all_tests[[i.s]]$feedback_type[i.t],
      all_tests[[i.s]]$contrast_left[i.t],
      all_tests[[i.s]]$contrast_right[i.t], 
      i.t) 
    }
  colnames(trial.summary)=c(
    names(average_spike_area(i.t,
                             this_session = all_tests[[i.s]])), 
                            'feedback', 
                            'left contr.',
                            'right contr.',
                            'id' )
  trial.summary <- as_tibble(trial.summary)
  
  test_summaries[[i]] <- trial.summary
}


## ---- include = FALSE---------------------------------------------------
# retraining data

Y <- model.matrix(~., s1.pred)

train_df <- s1.pred
train_X <- Y

train_label <- session1$feedback_type
train_label[train_label == -1] = 0

xgb_model1 <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nround = 10)

# test data

test1.sum = test_summaries[[1]]

test_label = test1.sum$feedback
test_label[test_label == -1] = 0

test1.sum = test1.sum[,-c(ncol(test1.sum) - 3:0)]

X <- model.matrix(~., test1.sum)

predictions <- predict(xgb_model1, newdata = X)
predicted_labels <- as.numeric(ifelse(predictions > 0.5, 1, 0))
accuracy_mD1 <- mean(predicted_labels == test_label)

conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))

true_neg = conf_matrix$table[1]
false_neg = conf_matrix$table[2]
true_pos = conf_matrix$table[4]
false_pos = conf_matrix$table[3]

all_neg = true_neg + false_neg
all_pos = true_pos + false_pos

biased_labels = rep(1, times = 100)
accuracy_bias <- mean(biased_labels == test_label)


## -----------------------------------------------------------------------
pr_curve_data <- pr.curve(scores.class0 = predicted_labels, weights.class0 = test_label, curve = TRUE)

plot(pr_curve_data, main = "Precision-Recall Curve, Model D, Session 1")


## ---- include = FALSE---------------------------------------------------
# retraining data

Y <- model.matrix(~., s18.pred)

train_df <- s18.pred
train_X <- Y

train_label <- session18$feedback_type
train_label[train_label == -1] = 0

xgb_model2 <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nround = 10)

# test data

test18.sum = test_summaries[[2]]

test_label = test18.sum$feedback
test_label[test_label == -1] = 0

test18.sum = test18.sum[,-c(ncol(test18.sum) - 3:0)]

X <- model.matrix(~., test18.sum)

predictions <- predict(xgb_model2, newdata = X)
predicted_labels <- as.numeric(ifelse(predictions > 0.5, 1, 0))
accuracy_mD18 <- mean(predicted_labels == test_label)

conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))

true_neg = conf_matrix$table[1]
false_neg = conf_matrix$table[2]
true_pos = conf_matrix$table[4]
false_pos = conf_matrix$table[3]

all_neg = true_neg + false_neg
all_pos = true_pos + false_pos

biased_labels = rep(1, times = 100)
accuracy_bias <- mean(biased_labels == test_label)


## -----------------------------------------------------------------------
pr_curve_data <- pr.curve(scores.class0 = predicted_labels, weights.class0 = test_label, curve = TRUE)

plot(pr_curve_data, main = "Precision-Recall Curve, Model D, Session 18")


## -----------------------------------------------------------------------

corr1[[1]]
corr1[[2]]
corr1[[3]]
corr1[[4]]
corr1[[5]]
corr1[[6]]
corr1[[7]]
corr1[[8]]
corr1[[9]]
corr1[[10]]
corr1[[11]]
corr1[[12]]
corr1[[13]]
corr1[[14]]
corr1[[15]]
corr1[[16]]
corr1[[17]]
corr1[[18]]



## -----------------------------------------------------------------------

grid.arrange(grobs=s1.s18.avg.spks.plots1, ncol=1)
grid.arrange(grobs=s1.s18.avg.spks.plots2, ncol=1)
grid.arrange(grobs=s1.s18.avg.spks.plots3, ncol=1)
grid.arrange(grobs=s1.s18.avg.spks.plots4, ncol=1)



## -----------------------------------------------------------------------
grid.arrange(grobs=list(corr4[[1]],corr4[[2]]), ncol = 2)
grid.arrange(grobs=list(corr4[[3]],corr4[[4]]), ncol = 2)
grid.arrange(grobs=list(corr4[[5]],corr4[[6]]), ncol = 2)
grid.arrange(grobs=list(corr4[[7]],corr4[[8]]), ncol = 2)
grid.arrange(grobs=list(corr4[[9]],corr4[[10]]), ncol = 2)
grid.arrange(grobs=list(corr4[[11]],corr4[[12]]), ncol = 2)
grid.arrange(grobs=list(corr4[[13]],corr4[[14]]), ncol = 2)
grid.arrange(grobs=list(corr4[[15]],corr4[[16]]), ncol = 2)
grid.arrange(grobs=list(corr4[[17]],corr4[[18]]), ncol = 2)



## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session1$contrast_left)) {
  left = session1$contrast_left[[i]]
  right = session1$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session1$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 1")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session2$contrast_left)) {
  left = session2$contrast_left[[i]]
  right = session2$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session2$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 2")


## -----------------------------------------------------------------------

diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session3$contrast_left)) {
  left = session3$contrast_left[[i]]
  right = session3$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session3$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 3")



## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session4$contrast_left)) {
  left = session4$contrast_left[[i]]
  right = session4$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session4$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 4")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session5$contrast_left)) {
  left = session5$contrast_left[[i]]
  right = session5$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session5$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 5")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session6$contrast_left)) {
  left = session6$contrast_left[[i]]
  right = session6$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session6$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 6")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session7$contrast_left)) {
  left = session7$contrast_left[[i]]
  right = session7$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session7$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 7")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session8$contrast_left)) {
  left = session8$contrast_left[[i]]
  right = session8$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session8$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 8")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session9$contrast_left)) {
  left = session9$contrast_left[[i]]
  right = session9$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session9$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 9")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session10$contrast_left)) {
  left = session10$contrast_left[[i]]
  right = session10$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session10$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 10")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session11$contrast_left)) {
  left = session11$contrast_left[[i]]
  right = session11$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session11$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 11")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session12$contrast_left)) {
  left = session12$contrast_left[[i]]
  right = session12$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session12$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 12")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session13$contrast_left)) {
  left = session13$contrast_left[[i]]
  right = session13$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session13$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE)+ 
  labs(title = "L/R Difference in Contrast, Session 13")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session14$contrast_left)) {
  left = session14$contrast_left[[i]]
  right = session14$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session14$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 14")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session15$contrast_left)) {
  left = session15$contrast_left[[i]]
  right = session15$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session15$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 15")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session16$contrast_left)) {
  left = session16$contrast_left[[i]]
  right = session16$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session16$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 16")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session17$contrast_left)) {
  left = session17$contrast_left[[i]]
  right = session17$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session17$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 17")


## -----------------------------------------------------------------------
diffs = c()
feeds = c()
feeds.num = c()
for (i in seq_along(session18$contrast_left)) {
  left = session18$contrast_left[[i]]
  right = session18$contrast_right[[i]]
  abs_diff = abs(left - right)
  feed = session18$feedback_type[[i]]
  if (feed == 1) {
    feed2 = "Success"
  }
  else if (feed == -1) {
    feed2 = "Failure"
  }
  
  diffs = c(diffs, abs_diff)
  feeds = c(feeds, feed2)
  feeds.num = c(feeds.num, feed)
}
diff.feed.df = data.frame(
  lr.diff = diffs,
  feedback = feeds, 
  feedback.num = feeds.num
)
diff.feed.num.df = data.frame(
  lr.diff = diffs,
  feedback = feeds.num
)

diff.corr = diff.feed.num.df %>%
  cor

ggcorrplot(diff.corr, type= "lower", hc.order = TRUE, lab = TRUE) + 
  labs(title = "L/R Difference in Contrast, Session 18")


## -----------------------------------------------------------------------

grid.arrange(grobs=all_sessions_feed_pie1,ncol=3)
grid.arrange(grobs=all_sessions_feed_pie2,ncol=3)
grid.arrange(grobs=all_sessions_feed_pie3,ncol=3)



## ----ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE---------------
## NA

