

###########################################################################
# Libraries to load -------------------------------------------------------
###########################################################################


library(data.table) # for data manipulation
library(magrittr) # for piping
library(ggplot2) # for graphs
library(Hmisc) # for stats


###########################################################################
# Custom Functions --------------------------------------------------------
###########################################################################

# Function 1: replace zeros with NAs
zero2na <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == 0) {
      x[i] <- NA_real_
    }
  }
  return(x)
}



# Function 2: fully automated function to calculate AR

# the INPUTS are: 
# (1) the data in data.table format and 
# (2) the column name in character format

# The OUTPUT is a value of AR
# AR = 1: perfect model
# AR = 0: not better than a random model
# AR < 0: the model is worse than random

calcAR <- function(data, varname) {
  # subsetting the data and removing the NAs
  dt_sub <- data[!is.na(get(varname)), .(type, get(varname))]
  colnames(dt_sub) <- c("type", "var1")
  
  # count bad and good companies
  bad <- dt_sub[type=="D", .N]
  good <- dt_sub[type=="G",.N]
  
  # sort the data for empirical CDF 
  dt_sub <- dt_sub[order(var1)]
  
  # calculate the empirical CDFs of random model, perfect model, and our data
  dt_sub[, `:=`(xval = seq(0,1,length.out = dt_sub[,.N]),
             r_cdf = seq(0,1,length.out = dt_sub[,.N]),
             p_cdf = cumsum(c(rep(1/bad,bad), rep(0, good))),
             e_cdf = 0
  )]
  for (i in 1:dt_sub[,.N]) {
    dt_sub$e_cdf[i] <- dt_sub[1:i, sum(type == "D")/bad]
  }
  
  # Calculating the AR
  CAP <- 0
  for (i in 1:dt_sub[,.N]) {
    CAP <- CAP + dt_sub$e_cdf[i]/dt_sub[, .N]
  }
  AR <- (2*CAP - 1)/(1 - bad/(bad + good))
  
  return(AR)
}

###########################################################################
# Load the data -----------------------------------------------------------
###########################################################################

# path to the data (you may customize this)
path <- paste0(getwd(),"/data/data_new/1_1_type.csv")

# and for other files
# path <- paste0(getwd(),"/data/data_new/3_1_1_type.csv")
# path <- paste0(getwd(),"/data/data_new/7_1_type.csv")




# Read the data into variable dt
dt <- fread(file = path, sep = ";")


###########################################################################
# Transform the data ------------------------------------------------------
###########################################################################

# replace zeros with NAs in the data (if neccessary)
dt <- dt[, lapply(.SD, zero2na)]



###########################################################################
# Example of AR calculation for the first day: 01.01.2015 -----------------
###########################################################################

# subset the data while removing NA (in case we have created them)
dt_sub <- dt[!is.na(`01.01.2015`), .(type, `01.01.2015`)]

# some stats on good companies
dt_sub[type=="G", describe(`01.01.2015`)]

# stats on bad companies
dt_sub[type=="D", describe(`01.01.2015`)]

# count bad and good companies
bad <- dt_sub[type=="D", .N]
good <- dt_sub[type=="G",.N]

# sort the data for empirical CDF 
dt_sub <- dt_sub[order(`01.01.2015`)]

# calculate the empirical CDFs of random model, perfect model, and our data
dt_sub[, `:=`(xval = seq(0,1,length.out = dt_sub[,.N]),
           r_cdf = seq(0,1,length.out = dt_sub[,.N]),
           p_cdf = cumsum(c(rep(1/bad,bad), rep(0, good))),
           e_cdf = 0)]
for (i in 1:dt_sub[,.N]) {
  dt_sub$e_cdf[i] <- dt_sub[1:i, sum(type == "D")/bad]
}


# Plot the cdfs
dt_sub %>% ggplot() + 
  geom_line(aes(x = xval, y = e_cdf), color = "red", lwd = 1.5) +
  geom_line(aes(x = xval, y = r_cdf), color = "black") + 
  geom_line(aes(x = xval, y = p_cdf), color = "blue") +
  labs(y = "Proportion of defaults", x = "Proportion of all debtors", 
       title = "CAP: Blue - perfect; Red - current; Black - random") +
  theme(axis.text.x  = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "lightgray"),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        axis.title = element_text(size = 14),
        title = element_text(size = 12))


# Calculating the result

# first way
CAP <- -0.5
PERF <- -0.5
for (i in 1:dt_sub[,.N]) {
  CAP <- CAP + dt_sub$e_cdf[i]/dt_sub[, .N]
  PERF <- PERF + dt_sub$p_cdf[i]/dt_sub[, .N]
}
AR1 <- CAP/PERF

# Second way (as in the request)
CAP <- 0
for (i in 1:dt_sub[,.N]) {
  CAP <- CAP + dt_sub$e_cdf[i]/dt_sub[, .N]
}
AR2 <- (2*CAP - 1)/(1 - bad/(bad + good))

# their difference
AR1 - AR2


###########################################################################
# Use Function to calculate AR for the whole data set ---------------------
###########################################################################

# create a table to store results
AR_results <- data.table(day = rep(NA_character_, (dim(dt)[2] -2 ) ), NA_count = NA_integer_, AR = NA_real_)

# fill this table
for (i in 1:AR_results[,.N]) {
  AR_results$day[i] <- names(dt)[i+2]
  AR_results$NA_count[i] <- dt[, .(get(names(dt)[i+2]))][V1==0 | is.na(V1), .N]
  AR_results$AR[i] <- calcAR(dt, AR_results$day[i])
}

# look at the results
AR_results



