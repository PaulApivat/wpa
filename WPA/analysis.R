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

