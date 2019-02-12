library(dplyr)

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

# Transpose
t<- data.frame(t(a_data))

# Remove all rows that have FLAGS in it, thus leaving only year indices
t <- t[!grepl("FL", row.names(t)),]

# Check by looking at the row names of the column
View(t$X1)

# Put all the major countries/groups into their own dataframes
z <- t["Country",]

# Change the column names in our whole dataset to make the columns less
# ambiguous
colnames(t) <- c(t(t(z)))

# Prepare variables for loop
countries <- unique(colnames(t))

# Create separate variables for each country/group
for (c in countries){
  df <- t[colnames(t)==c]
  assign(c, df)
}


