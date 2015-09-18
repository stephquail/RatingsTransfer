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

#Vectors of participant data paths, and IDs created in functions
#Create a list of participant info vectors

ID <- list(CI101, CI102, CI103, CI104, CI105, CI106, CI107, CI108, CI109)

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
dir.output <- "R/output" # sets output folder
write.csv(group.df, file = file.path(dir.output, "group_ratingtransfer.csv"), row.names = FALSE) 

