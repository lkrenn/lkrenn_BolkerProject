#######################################################################################################

# Welcome!  This RStudio Amazon AMI contains RStudio Server version 1.1.456,
# running R 3.5.1 on Ubuntu 16.04 LTS.
# Includes support for Shiny (add /shiny/rstudio to URL).
# NEW: experimental support for CUDA 9.0 (incl. cuDNN 7.2.1) and Magma 2.4.0
#      enabling use of GPU packages in R and higher performance for deep
#      learning frameworks such as TensorFlow.
# AMI created by Louis Aslett (http://www.louisaslett.com/).  If you've
# any comments or suggestions please mail louis.aslett@durham.ac.uk

# NOTE: It is *highly* recommended that you immediately change the
# default password for logging into RStudio, which you can do by logging
# in via SSH (recommended) in the usual EC2 fashion.  Alternatively,
# since this AMI was created to make RStudio Server accessible to those
# who are less comfortable with Linux commands you can follow the
# instructions below to change it without touching Linux.

# There is now a mini package where functions to manipulate the server will be
# placed.  This includes a function to change the password.  First load the
# package:
library("RStudioAMI")

# Now you can change the password by just running the following function.  It
# will prompt you to provide the existing password (just the instance ID) and
# then type in a new password
passwd()

# It is strongly recommended that you clear the console so that the password is
# not visible after running the function.  Either press Ctrl+L or go to
# Edit -> Clear Console within the RStudio interface.

# There is also a function to assist with linking to a Dropbox account to
# make loading scripts/data on and off the server much easier.  Just run the
# following function once you are ready to link to your Dropbox and follow the
# instructions
linkDropbox()

# Once Dropbox is linked you will notice a new folder called Dropbox appear in
# the Files pane on the right ===>
# This will begin syncing immediately.  If you have a large Dropbox then it is
# strongly recommended that you selectively sync only what you need.  Use the
# excludeSyncDropbox and includeSyncDropbox functions for this.
#######################################################################################################
library(dplyr)
library(tidyr)
library(stringr)
library(MARSS)
library(ggplot2)

# Import the data
a_data <- read.csv("Data_Annually.csv")

# We want to see all the possible unique series, so we can determine which may be left out
unique(a_data$Subject)

# 825 series! Way too many to look at every combination, so we will need to know which are
# the most important

# We want to see how many unique countries we are dealing with
unique(a_data$Country)

# 63 levels! While not every "country" has 825 series, there is still way too many here
# (42368 rows in the excel file). Luckily, there are also groups that we can work with 
# i.e. 
#
# Major 5 Asia
# BRIICS
# Euro area
# EU
# G20
# Big Four Euro
# G7
# NAFTA
# OECD total with different flavours
# SDR _ "SDRs (Special Drawing Rights) are international reserve assets created by the 
#       International Monetary Fund and allocated to its members to supplement existing 
#       reserve assets."
#
# So we will definitely need to break these down into smaller pieces and figure out how
# many we will need 


# Remove the colnames that are NA (i.e. the flags)
a_data <- a_data[,!grepl("FLAGS",  colnames(a_data))]

# I think there are redundant columns here i.e. Series.code vs Series.code.1 so we will
# check those quickly and if they are the exact same, we can remove them. These will be 
# commented out because it's a lot of repetiton

##########################################################################################

sum(a_data$Series.code == a_data$Series.code.1) - length(a_data$Series.code)
# Evaluates to 0 aka no FALSEs in this series. Therefore one of the two can be removed

a_data$Series.code.1 <- NULL

# Do the same for the others

# LOCATION
sum(a_data$LOCATION == a_data$LOCATION.1) - length(a_data$LOCATION)
# Evaluates to 0, can remove
a_data$LOCATION.1 <- NULL

# Country
sum(a_data$Country == a_data$Country.1) - length(a_data$Country)
# Evaluates to 0, can remove
a_data$Country.1 <- NULL

# Subject
# Cannot make sense of the SUBJECT column, so I will remove it for clarity
a_data$SUBJECT <- NULL

# Measure
sum(a_data$MEASURE == a_data$Measure) - length(a_data$MEASURE)
a_data$MEASURE <- NULL

# Units
a_data$UNIT.CODE <- NULL

# Exponent of Units
a_data$POWERCODE <- NULL

# Reference Period
a_data$REFERENCE.PERIOD.CODE <- NULL

##########################################################################################

# We will use stringr to count the number of categories in each subject, and then separate 
# them into that many individual columns

countofstr <- str_count(a_data$Subject, ">")

max(countofstr)

# There are at most 3 levels of granularity which means at most 4 categories. We will split
# them into general colnames and see if we can be more specific with colnames afterwards

a_data2 <- separate(a_data, Subject, c("Subject1", "Subject2", "Subject3", "Subject4"), sep =" > ")

# First Subject category
unique(a_data2$Subject1)

unique(a_data2$Subject2)

unique(a_data2$Subject3)

unique(a_data2$Subject4)

# To begin, we will look for just the annual GDP numbers for each country i.e. we want to choose
# the rows where Subject2 is "Gross Domestic Product by Expenditure"

a_data_GDP <- a_data2[a_data2$Subject2 == "GDP by Expenditure",]

# We can see here that there is further granularity, so we will also need to specify that Subject4
# has to be "Gross Domestic Product - Total" 

a_data_GDP <- a_data_GDP[a_data_GDP$Subject4 == "Gross Domestic Product - Total",]

# To further granularize, we will also filter based on the Measure, we want it to be Growth rate 
# previous period. s.a.

a_data_GDP <- a_data_GDP[a_data_GDP$Measure == "Growth rate previous period. s.a.",]

cols <- c(colnames(a_data_GDP))

View(a_data_GDP[,12:49])

# Create a dataframe with everything specified above
y_model = data.frame(a_data_GDP$Country, a_data_GDP[,12:length(colnames(a_data_GDP))])

# Get rid of the X that begins each column's name
colnames(y_model) <- substring(names(y_model), 2)

# Before we do anything too fancy, we will first work with a univariate model. I hand-picked
# Australia's GDP series because it was dense with values. This is a test to make sure that 
# the specifications I choose are correct. 

aus_test <- data.matrix(y_model[1,2:length(colnames(y_model))])


mod.mat = list(B = matrix(1), U = matrix(0), Q = matrix("q"), 
                  Z = matrix(1), A = matrix(0), R = matrix("r"), x0 = matrix("mu"), 
                  tinitx = 0)


MARSS(mat, model=mod.mat, control=list(minit=15, maxit=1000, abstol=0.001, trace=0, 
                                       sparse=FALSE, safe=FALSE, allow.degen=TRUE, 
                                       min.degen.iter=50, degen.lim=1.0e-04, 
                                       min.iter.conv.test=15, conv.test.deltaT=9, 
                                       conv.test.slope.tol= 0.5, demean.states=FALSE))


# It worked! I'll have to come back to analyze further. Let's see if we can get the same thing going
# but for all the variables now:

mod.list.0 <- list(B = matrix(1), U = matrix("u"), Q = matrix("q"), 
                   Z = matrix(1,49,1), A = "scaling", R = "diagonal and unequal", 
                   x0 = matrix("mu"), tinitx = 0)

y_full <- data.matrix(y_model[,2:length(colnames(y_model))])
rownames(y_full) <- y_model$`_data_GDP.Country`


marss.full <- MARSS(y_full, model=mod.list.0, control=list(minit=15, maxit=1000, abstol=0.001, trace=0, 
                                       sparse=FALSE, safe=FALSE, allow.degen=TRUE, 
                                       min.degen.iter=50, degen.lim=1.0e-04, 
                                       min.iter.conv.test=15, conv.test.deltaT=9, 
                                       conv.test.slope.tol= 0.5, demean.states=FALSE))

# AIC or loglikelihood between the two models


# B is the autoregressive coefficient

# U is the trend

# Q is the noise/innovation

# Vector of Y's that are calculated using:

# Z means a 49x1 matrix of just ones 

# A is a column of intercepts

# R is the noise in Y which is a diagonal but unequal matrix

# AIC between other models 

# methods(class="marssMLE")
#   we did autoplot
#   fitted vals
#   AICs
#   sort countries based on variance
