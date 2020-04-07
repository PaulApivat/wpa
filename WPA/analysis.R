# Software
# RStudio version: 1.2.5033
# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night”

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

# load data
df <- read.csv("/Users/paulapivat/Desktop/wpa/DataVizCompetition2020_Feb12/WPAC_Final.csv")

#######--------- Data Manipulation ---------########

# filter for all MedCo assignments (n = 946)
df_medco <- df %>% filter(pool=='MEDCO')

# create data frame containing number of MedCo assignments for all staff_id
multiple_medco <- df_medco %>%
    group_by(staff_id) %>%
    tally(sort = TRUE)

# change column names
colnames(multiple_medco)[2] <- 'num_medco'

# join df_medco with multiple_medco
df_medco_join <- df_medco %>%
    inner_join(multiple_medco, by = 'staff_id')

# create column for assignment number of first MedCo assignment
# by getting distinct (first) staff_id from df_medco_join
# currently df_medco_join has n=946 MedCo assignments (but some staff_id has multiple assignments)
# should result in n=462 staff_id who did at least one medco
assign_first_medco <- distinct(df_medco_join, staff_id, .keep_all = TRUE)

# create column for assignment number of first MedCo assignment - assign_num_first_medco
assign_first_medco <- assign_first_medco %>%
    mutate(assign_num_first_medco = assignment_number)

# create column for assignment number of LAST MedCo
# there's probably a faster way; this is just a way I hacked together

# step 1 (base R)
# subset dataframe to x (n = 946)
# use rev() for the first time to reverse order
x <- data.frame("rev_staff_id" = df_medco_join$staff_id, "rev_assign_num" = df_medco_join$assignment_number)
x$rev_staff_id <- rev(x$rev_staff_id)
x$rev_assign_num <- rev(x$rev_assign_num)

# step 2 subset further to get only unique staff_id
# at this point, larger staff_id numbers facing up
y <- distinct(x, rev_staff_id, .keep_all = TRUE)

# step 3 use rev() AGAIN to reverse BACK 
# so smaller  staff_id numbers facing up
y$rev_staff_id <- rev(y$rev_staff_id)
y$rev_assign_num <- rev(y$rev_assign_num)

# change column names before performing inner_join
colnames(y)[1] <- "staff_id"
colnames(y)[2] <- "assign_num_last_medco"



#------------------------------- ggplot2 way slightly easier
reverse_staff <- df_medco_join %>%
    select(staff_id, assignment_number)

# use rev() to reverse both staff_id and assignment_number 
# so assignment number of last medco is facing toward the top
reverse_staff$staff_id <- rev(reverse_staff$staff_id)
reverse_staff$assignment_number <- rev(reverse_staff$assignment_number)

# get only distinct staff_id (n = 462)
# time time assignment number of last medco is facing toward the top
reverse_staff <- distinct(reverse_staff, staff_id, .keep_all = TRUE)

# use rev() AGAIN to reverse BACK
reverse_staff$staff_id <- rev(reverse_staff$staff_id)
reverse_staff$assignment_number <- rev(reverse_staff$assignment_number)

# change column name of only assignment_number column
colnames(reverse_staff)[2] <- 'assign_num_last_medco'

# ---------------------

