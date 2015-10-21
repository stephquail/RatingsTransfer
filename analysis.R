##Data Extraction and Analysis for the post-training causal ratings and ratings transfer of the Conditioned Inhibition task

#To Extract:
#   Initial Ratings for [Cue Presented: Outcome Presented vs. Outcome Associated with in Training]
#     - Trained Individual Excitors: Same Outcome
#     - Trained Individual Excitors: Different Outcome
#     - Trained Individual Inhibitors: Same Outcome
#     - Trained Individual Inhibitors: Different Outcome
#
#
#   Novel Compound Ratings for [Type of Compound: Outcome Presented vs. Outcome Predicted: Location of Inhibitor in Compound]
#     - Congruent Compounds: Same Outcome: Consistent Location
#     - Congruent Compounds: Same Outcome: Inconsistent Location
#     - Congruent Compounds: Different Outcome: Consistent Location
#     - Congruent Compounds: Different Outcome: Inconsistent Location
#     - Incongruent Compounds: Same Outcome: Consistent Location
#     - Incongruent Compounds: Same Outcome: Inconsistent Location
#     - Incongruent Compounds: Different Outcome: Consistent Location
#     - Incongruent Compounds: Different Outcome: Inconsistent Location

source("R/functions.R") # loads functions

#load libraries needed 
library(ggplot2)
library(plyr)
library(grid)
library(reshape2)
library(stringr)

#Vectors of participant data paths, and IDs created in functions
#Create a list of participant info vectors

ID <- list(CI101, CI102, CI103, CI104, CI105, CI106, CI107, CI108, CI109, CI110, CI111, CI112, CI113, CI114, CI115, CI116, CI117, CI118, CI119, CI120, CI121, CI122)

#Create Empty Vectors
#These empty vectors will be filled in with individual participant information as the analysis loops through each participant
#The third item in each participant vector corresponds to the row that partcipants' data will be inserted into the empty vectors. 

participant <- character(length = length(ID)) # creates vectors with as many elements in them as the number of participantin in the ID vector

#Trained Cues
excitors.same <- numeric(length = length(ID))
excitors.diff <- numeric(length = length(ID))
inhibitors.same <- numeric(length = length(ID))
inhibitors.diff <- numeric(length = length(ID))

#Ratings Transfer
#  - Individual Novel Cues
novel.same <- numeric(length = length(ID))
novel.diff <- numeric(length = length(ID))

#  - Novel Compounds
congruent.same.con <- numeric(length = length(ID))
congruent.same.incon <- numeric(length = length(ID))
congruent.diff.con <- numeric(length = length(ID))
congruent.diff.incon <- numeric(length = length(ID))
incongruent.same.con <- numeric(length = length(ID))
incongruent.same.incon <- numeric(length = length(ID))
incongruent.diff.con <- numeric(length = length(ID))
incongruent.diff.incon <- numeric(length = length(ID))


#Loop through each participant in the ID list for extraction of rating data

for(i in ID){
  data <- read.csv(i[[1]])
  
  o1 <- "M&M.png" #Outcome 1: M&Ms
  o2 <- "BBQ.png" #Outcome 2: BBQ Shapes
  
  #Presentation of S1, paired with O1/O2
  v1o1 <- data$rating.response[data$testCS=="v1.png" & data$testSnack == o1]
  v1o2 <- data$rating.response[data$testCS=="v1.png" & data$testSnack == o2]
  
  #Presentation of S2, paired with O1/O2
  v2o1 <- data$rating.response[data$testCS=="v2.png" & data$testSnack == o1]
  v2o2 <- data$rating.response[data$testCS=="v2.png" & data$testSnack == o2]
  
  #Presentation of S3, paired with O1/O2
  v3o1 <- data$rating.response[data$testCS=="v3.png" & data$testSnack == o1]
  v3o2 <- data$rating.response[data$testCS=="v3.png" & data$testSnack == o2]
  
  #Presentation of S4, paired with O1/O2
  v4o1 <- data$rating.response[data$testCS=="v4.png" & data$testSnack == o1]
  v4o2 <- data$rating.response[data$testCS=="v4.png" & data$testSnack == o2]
  
  
  #Collapse individual cue/outcome pairings across appropriate conditions
  #S1/S2: Excitors
  #S3/S4: Inhibitors
  
  
  i.excitors.same <- mean(v1o1, v2o2) #causal ratings for SAME outcome paired with cue during training
  i.excitors.diff <- mean(v1o2, v2o1) #causal ratings for DIFFERENT outcome (i.e. outcome not paired with cue during training)
  
  i.inhibitors.same <- mean(v3o1, v4o2) # causal ratings for SAME outcome cue predicted was absent during training
  i.inhibitors.diff <- mean(v3o2, v4o1)
  
  #Extract ratings for Novel Cues
  # N1 --> O2  #### BE CAREFUL OF THIS - THIS IS NOT THE CUE-OUTCOME PAIRING THAT WOULD BE EXPECTED ####
  # N2 --> O1
  
  n1o1 <- data$rating_2.response[data$testCS=="vend_n1.png" & data$testSnack == o1]
  n1o2 <- data$rating_2.response[data$testCS == "vend_n1.png" & data$testSnack == o2]
  
  n2o1 <- data$rating_2.response[data$testCS == "vend_n2.png" & data$testSnack == o1]
  n2o2 <- data$rating_2.response[data$testCS == "vend_n2.png" & data$testSnack == o2]
  
  i.novel.same <- mean(n1o2, n2o1)
  i.novel.diff <- mean(n1o1, n2o2)
  
  #Extract ratings for Novel Compounds
  # Compounds of nXvXoX are location consistent compounds (the inhibitor is presented in the same location as in training)
  # Compounds of vXnXoX are location inconsitent compounds (the inhibitor is presented where the excitors were during training)
  
  n1v3o1 <- data$rating_2.response[data$testCS=="n1v3.png" & data$testSnack == o1]
  n1v3o2 <- data$rating_2.response[data$testCS=="n1v3.png" & data$testSnack == o2]
  
  v3n1o1 <- data$rating_2.response[data$testCS=="v3n1.png" & data$testSnack == o1]
  v3n1o2 <- data$rating_2.response[data$testCS=="v3n1.png" & data$testSnack == o2]
  
  n1v4o1 <- data$rating_2.response[data$testCS=="n1v4.png" & data$testSnack == o1]
  n1v4o2 <- data$rating_2.response[data$testCS=="n1v4.png" & data$testSnack == o2]
  
  v4n1o1 <- data$rating_2.response[data$testCS=="v4n1.png" & data$testSnack == o1]
  v4n1o2 <- data$rating_2.response[data$testCS=="v4n1.png" & data$testSnack == o2]
  
  n2v3o1 <- data$rating_2.response[data$testCS=="n2v3.png" & data$testSnack == o1]
  n2v3o2 <- data$rating_2.response[data$testCS=="n2v3.png" & data$testSnack == o2]
  
  v3n2o1 <- data$rating_2.response[data$testCS=="v3n2.png" & data$testSnack == o1]
  v3n2o2 <- data$rating_2.response[data$testCS=="v3n2.png" & data$testSnack == o2]
  
  n2v4o1 <- data$rating_2.response[data$testCS=="n2v4.png" & data$testSnack == o1]
  n2v4o2 <- data$rating_2.response[data$testCS=="n2v4.png" & data$testSnack == o2]
  
  v4n2o1 <- data$rating_2.response[data$testCS=="v4n2.png" & data$testSnack == o1]
  v4n2o2 <- data$rating_2.response[data$testCS=="v4n2.png" & data$testSnack == o2]
  
  # Collapse ratings across condition pairs
  # Congruent Compounds: Novel excitors and Inhibitors both associated with the same outcome
  # Incongruent Compounds: Novel excitor predicts delivery of different outcome to what the Inhibitor predicts is absent
  
  # Same Pairing: Outcome asked to make predictions about is the same outcome the novel excitor predicts the delivery of
  # Different Pairing: Outcome asked to make predictions about is the different outcome to what the novel excitor predicts the delivery of
  
  i.congruent.same.con <- mean(n1v4o2, n2v3o1)
  i.congruent.same.incon <- mean(v4n1o2, v3n2o1)
  
  i.congruent.diff.con <- mean(n1v4o1, n2v3o2)
  i.congruent.diff.incon <- mean(v4n1o1, v3n2o2)
  
  i.incongruent.same.con <- mean(n1v3o2, n2v4o1)
  i.incongruent.same.incon <- mean(v3n1o2, v4n2o1)
  
  i.incongruent.diff.con <- mean(n1v3o1, n2v4o2)
  i.incongruent.diff.incon <- mean(v3n1o1, v4n2o2)
  
  
  #Insert individual participant rating scores into empty vectors created earlier
  # participant vectors containt the number of the element
  participant[as.numeric(i[3])] <- i[[2]]
  excitors.same[as.numeric(i[3])] <- i.excitors.same
  excitors.diff[as.numeric(i[3])] <- i.excitors.diff
  inhibitors.same[as.numeric(i[3])] <- i.inhibitors.same
  inhibitors.diff[as.numeric(i[3])] <- i.inhibitors.diff
  novel.same[as.numeric(i[3])] <- i.novel.same
  novel.diff[as.numeric(i[3])] <- i.novel.diff
  congruent.same.con[as.numeric(i[3])] <- i.congruent.same.con
  congruent.same.incon[as.numeric(i[3])] <- i.congruent.same.incon
  congruent.diff.con[as.numeric(i[3])] <- i.congruent.diff.con
  congruent.diff.incon[as.numeric(i[3])] <- i.congruent.diff.incon
  incongruent.same.con[as.numeric(i[3])] <- i.incongruent.same.con
  incongruent.same.incon[as.numeric(i[3])] <- i.incongruent.same.incon
  incongruent.diff.con[as.numeric(i[3])] <- i.incongruent.diff.con
  incongruent.diff.incon[as.numeric(i[3])] <- i.incongruent.diff.incon
  
} #end looping through individual participants

#Create a wide data frame of ratings, with all conditions as columns

group.df <- data.frame(participant, excitors.same, excitors.diff, inhibitors.same, inhibitors.diff, novel.same, novel.diff, congruent.same.con, congruent.same.incon, congruent.diff.con, congruent.diff.incon, incongruent.same.con, incongruent.same.incon, incongruent.diff.con, incongruent.diff.incon)

##Export group data to excel file
dir.output <- "R/output/data" # sets output folder
write.csv(group.df, file = file.path(dir.output, "group_ratingtransfer.csv"), row.names = FALSE) 


#Graph Data 

#Convert dataframe from wide to long format
long.rating.df <- melt(group.df,
                       id.vars = "participant",
                       variable.name = "cue.presented",
                       value.name = "causal.rating")
#Add Factors to Data
#CUE: Excitors, Inhibitors, Novel, Congruent, Incongruent
#OUTCOME: Same, Different
#LOCATION: Consistent, Inconsistent

long.rating.df$factor <- str_split_fixed(long.rating.df$cue.presented, "\\.", 3) # splits individual type labels at "." points
long.rating.df$cues <- as.factor(long.rating.df$factor[,1])
long.rating.df$outcome <- as.factor(long.rating.df$factor[,2])
long.rating.df$location <- as.factor(long.rating.df$factor[,3])
long.rating.df$factor <- NULL #drops redundant factor 

#Give the levels meaningful names
levels(long.rating.df$cues)[levels(long.rating.df$cues)=="excitors"] <- "Excitor"
levels(long.rating.df$cues)[levels(long.rating.df$cues)=="inhibitors"] <- "Inhibitor"
levels(long.rating.df$cues)[levels(long.rating.df$cues)=="novel"] <- "Novel"
levels(long.rating.df$cues)[levels(long.rating.df$cues)=="congruent"] <- "Congruent"
levels(long.rating.df$cues)[levels(long.rating.df$cues)=="incongruent"] <- "Incongruent"


levels(long.rating.df$outcome)[levels(long.rating.df$outcome)=="same"] <- "Same"
levels(long.rating.df$outcome)[levels(long.rating.df$outcome)=="diff"] <- "Different"

levels(long.rating.df$location)[levels(long.rating.df$location)=="con"] <- "Consistent"
levels(long.rating.df$location)[levels(long.rating.df$location)=="incon"] <- "Inconsistent"

#Order the Outcome Levels
long.rating.df$outcome <- ordered(long.rating.df$outcome, levels=c("Same", "Different"))

#Split long.df into New and Old Cue presentations

old.rating.df <- long.rating.df[long.rating.df$cues == "Excitor" | long.rating.df$cues == "Inhibitor",]
old.rating.df <- droplevels(old.rating.df)
new.rating.df <- long.rating.df[long.rating.df$cues == "Novel",]
new.rating.df <- droplevels(new.rating.df)
transfer.rating.df <- long.rating.df[long.rating.df$cues != "Excitor" & long.rating.df$cues != "Inhibitor" & long.rating.df$cues != "Novel",]
transfer.rating.df <- droplevels(transfer.rating.df)
#Calculate the summary data to graph
#Old Cues
#Within Subjects

oldcues_summWS <- summarySEwithin(old.rating.df, measurevar="causal.rating", withinvars = c("cues", "outcome"), idvar = "participant", na.rm=FALSE, conf.interval=.95)

novelcues_summWS <- summarySEwithin(new.rating.df, measurevar="causal.rating", withinvars = "outcome", idvar = "participant", na.rm=FALSE, conf.interval=.95)

transfercues_summWS <- summarySEwithin(transfer.rating.df, measurevar="causal.rating", withinvars =c("cues", "outcome", "location"), idvar = "participant", na.rm=FALSE, conf.interval=.95)

#Graph causal ratings

columncols <- c("red2", "dodgerblue")
#For individual cues
oldcues_graph <- ggplot(oldcues_summWS, aes(x=cues, y=causal.rating, fill = outcome)) +
  geom_bar(position=position_dodge(), stat = "identity") +
  scale_fill_manual(values = columncols) +
  geom_errorbar(aes(ymin = causal.rating - se, ymax = causal.rating +se),
                size=.3,
                width=.2,
                position=position_dodge(.9)) +
  xlab("Cue") +
  ylab("Causal Rating")

#For novel cues

novelcues_graph <- ggplot(novelcues_summWS, aes(x=outcome, y=causal.rating, fill = outcome)) +
  geom_bar(position=position_dodge(), stat = "identity") +
  scale_fill_manual(values = columncols) +
  geom_errorbar(aes(ymin = causal.rating - se, ymax = causal.rating + se),
                size=.3,
                width=.2,
                position=position_dodge(.9))+
  xlab("Outcome") +
  ylab("Causal Rating")

#For compound cues transfer scores
transfercues_graph <- ggplot(transfercues_summWS, aes(x=location, y=causal.rating, fill=outcome)) +
  geom_bar(position=position_dodge(), stat = "identity") +
  scale_fill_manual(values = columncols) +
  geom_errorbar(aes(ymin = causal.rating - se, ymax = causal.rating + se),
                size = .3,
                width=.2,
                position=position_dodge(.9)) +
  facet_grid(cues ~.) +
  ylab("Causal Rating") + 
  xlab("Inhibitor Cue Location")

pdf("R/output/figures/oldcues.pdf")
print(oldcues_graph)
dev.off()

pdf("R/output/figures/novelcues.pdf")
print(novelcues_graph)
dev.off()

pdf("R/output/figures/transfercues.pdf")
print(transfercues_graph)
dev.off()


#Statistical Tests of Causal Ratings of Trained Cues

# old.rating.df is the data frame used
# (2) x (2) ANOVA
# Cue: Excitor, Inhibitor
# Outcome: Same, Different

old.rating.aov <- aov(causal.rating ~ cues*outcome + Error(participant/(cues*outcome)), data = old.rating.df)

pairedTTest(inhibitors.same, inhibitors.diff)

t.test(inhibitors.same, mu = 0)
t.test(inhibitors.diff, mu = 0)
t.test(excitors.same, mu = 0)
t.test(excitors.diff, mu = 0)

#Statistical Test of Novel cues

#new.rating.df is the data frame used

pairedTTest(novel.same, novel.diff)

# Statistical Tests of Causal Ratings Transfer

# transfer.rating.df is the data frame used

#(2) x (2) x (2) Within-Subjects ANOVA
# Compound Type: Congruent, Incongruent
# Outcome: Same, Different
# Inhibitor Location: Consistnent, Inconsistent

transfer.rating.aov <- aov(causal.rating ~ cues*outcome*location + Error(participant/(cues*outcome*location)), data = transfer.rating.df)

summary(transfer.rating.aov)
model.tables(transfer.rating.aov, "means")


