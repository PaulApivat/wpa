### Update: May 9, 2020 ###
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock" 
# RStudio version: 1.2.5042

# Navigate and Load data
#setwd("/Users/paulapivat/Desktop")
#load(file = "finalsession.RData")

# libraries
library(tidyverse)
library(reshape2)
library(waffle)
library(patchwork)
library(moderndive)
library(networkD3)
library(rmarkdown)
library(tinytex)



### Question 1:

### Question 2:

### Question 3: Causality

# dataframe 'b' contain distinct staff_id (n = 462)
# contain age_bracket individual belongs in 
# at time of first assignment (assignment_number==1)
# check correlation btwn age_bracket and num_medco

# n = 462
get_correlation(data = b, age_bracket ~ num_medco, na.rm = FALSE)

#         cor
# -0.02291633

# n = 462
get_correlation(data = b, age_bracket ~ total_num_assign, na.rm = FALSE)

#          cor
# -0.05038205

### Run point-biserial correlation between sex and num_medco / total_num_assign
# install new package
library(ltm)

# ERROR: 'x' must be a numeric vector
cor.test(b$sex, b$num_medco)  # base r regular correlation
biserial.cor(b$sex,b$num_medco, level = 1)   # ltm package, point biserial corr

# save 'b' to new dataframe



