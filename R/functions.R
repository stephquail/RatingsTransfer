##Functions used for the analysis of the ratings transfer data for the Conditioned Inhibition task.
#
# Create vectors for each participant
# Each ID Vector consists of:
#   - Path to data file
#   - ID code
#   - participant number (to insert data into appropriate row when looping through participants for analysis)

CI101 <- c("R/data/CI101.csv", "CI101", 1)
CI102 <- c("R/data/CI102.csv", "CI102", 2)
CI103 <- c("R/data/CI103.csv", "CI103", 3)
CI104 <- c("R/data/CI104.csv", "CI104", 4)
CI105 <- c("R/data/CI105.csv", "CI105", 5)
CI106 <- c("R/data/CI106.csv", "CI106", 6)
CI107 <- c("R/data/CI107.csv", "CI107", 7)
CI108 <- c("R/data/CI108.csv", "CI108", 8)
CI109 <- c("R/data/CI109.csv", "CI109", 9)
CI110 <- c("R/data/CI110.csv", "CI110", 10)
CI111 <- c("R/data/CI111.csv", "CI111", 11)
CI112 <- c("R/data/CI112.csv", "CI112", 12)
CI113 <- c("R/data/CI113.csv", "CI113", 13)
CI114 <- c("R/data/CI114.csv", "CI114", 14)
CI115 <- c("R/data/CI115.csv", "CI115", 15)
CI116 <- c("R/data/CI116.csv", "CI116", 16)
CI117 <- c("R/data/CI117.csv", "CI117", 17)
CI118 <- c("R/data/CI118.csv", "CI118", 18)
CI119 <- c("R/data/CI119.csv", "CI119", 19)
CI120 <- c("R/data/CI120.csv", "CI120", 20)
CI121 <- c("R/data/CI121.csv", "CI121", 21)
CI122 <- c("R/data/CI122.csv", "CI122", 22)

##Adds factors to measured data
# Factor 1: charater vector with participant ID codes
# Factor 2 & 3: measurement ID variables
addFactor <- function(factor1, factor2, factor3, x){
  df <-data.frame(factor1, c(rep(factor2, len = length(x))), c(rep(factor3, len = length(x))), x)
  colnames(df) <- c("ID", "Factor1", "Factor2", "X")
  df
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


# paired t-test with equal variance
pairedTTest <- function(x,y){
  t.test(
    x,
    y,
    paired = TRUE, var.equal = TRUE)}

#Adds factors to measured data
#ID: charater vector with participant ID codes
#Factor 1, 2 & 3: measurement ID variables
#X: measured data
add3Factor <- function(id, factor1, factor2, factor3, x){
  df <-data.frame(id, c(rep(factor1, len = length(x))), c(rep(factor2, len = length(x))), c(rep(factor3, len = length(x))), x)
  colnames(df) <- c("ID", "Factor1", "Factor2", "Factor3", "X")
  df
}

#Adds two factors to measured data
add2Factor <- function(factor1, factor2, factor3, x){
  df <-data.frame(factor1, c(rep(factor2, len = length(x))), c(rep(factor3, len = length(x))), x)
  colnames(df) <- c("ID", "Factor1", "Factor2", "X")
  df
}

