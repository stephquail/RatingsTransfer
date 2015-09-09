##Functions used for the analysis of the ratings transfer data for the Conditioned Inhibition task.
#
# Create vectors for each participant
# Each ID Vector consists of:
#   - Path to data file
#   - ID code
#   - participant number (to insert data into appropriate row when looping through participants for analysis)

CI101 <- c("R/data/CI101.csv", "CI101", 1)


##Adds factors to measured data
# Factor 1: charater vector with participant ID codes
# Factor 2 & 3: measurement ID variables
addFactor <- function(factor1, factor2, factor3, x){
  df <-data.frame(factor1, c(rep(factor2, len = length(x))), c(rep(factor3, len = length(x))), x)
  colnames(df) <- c("ID", "Factor1", "Factor2", "X")
  df
}