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


