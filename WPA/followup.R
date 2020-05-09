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

## Try point biserial corr btwn age_bracket and num_medco
## Error: age_bracket not dichotomous variable

## pearson's product-moment correlation in base R same as ModernDive
cor.test(b_alt$age_bracket, b_alt$num_medco, method = c('pearson'))
# sample estimates:
#        cor 
# -0.02291633 

# 95 percent confidence interval:
# -0.11390711  0.06845571

## total_num_assign
cor.test(b_alt$age_bracket, b_alt$total_num_assign, method = c('pearson'))
# sample estimates:
#        cor 
# -0.05038205 

# 95 percent confidence interval:
#  -0.14096306  0.04103549


### Run point-biserial correlation between sex and num_medco / total_num_assign
# install new package
library(ltm)

# ERROR: 'x' must be a numeric vector
cor.test(b$sex, b$num_medco)  # base r regular correlation
biserial.cor(b$sex,b$num_medco, level = 1)   # ltm package, point biserial corr

# save 'b' to new dataframe: b_alt
b_alt <- b

# convert sex into numeric variable
# insert new column .before = first_departure
b_alt <- add_column(b_alt, sex2 = NA, .after = 'sex')

# run ifelse loop to fill 0 = F, 1 = M
b_alt$sex2 <- ifelse(b_alt$sex=='F', 0, 1)

# re-run point biserial corr btwn gender and num_medco
biserial.cor(b_alt$num_medco,b_alt$sex2, level = 1)
# [1] 0.01904766


# re-run point biserial corr btwn gender and total_num_assign
biserial.cor(b_alt$total_num_assign,b_alt$sex2, level = 1)
# [1] 0.01596403

#### Check that point biserial corr between 'consecutive status' is stronger
#### with num_medco and total_num_assign

# add consecutive2 column
b_alt[,'consecutive2'] <- NA

# check if =='consecutive', enter 2, else 1
# did not register 'NA' as 'else'
# had to do is.na()
b_alt$consecutive2 <- ifelse(b_alt$consecutive=='consecutive', 2,1)
b_alt$consecutive2 <- ifelse(is.na(b_alt$consecutive), 1, b_alt$consecutive2)

# run point biserial corr btwn 'consecutive2' status and num_medco
# moderately high correlation (proving my point)
biserial.cor(b_alt$num_medco,b_alt$consecutive2, level = 1)
# [1] 0.655591

# run point biserial corr btwn consecutive2 and total_num_assign
# moderately high correlation (proving my point)
biserial.cor(b_alt$total_num_assign,b_alt$consecutive2, level = 1)
# [1] 0.5021832

#### Basic scatter plots

# gender
ggplot(data = b_alt, mapping = aes(x=sex2, y=num_medco)) + geom_point() + geom_jitter()
ggplot(data = b_alt, mapping = aes(x=sex2, y=total_num_assign)) + geom_point() + geom_jitter()

# age bracket
# NOTE: visual does seem to show that age of first assignment appears to matter
ggplot(data = b_alt, mapping = aes(x=age_bracket, y=num_medco)) + geom_point() + geom_jitter()
ggplot(data = b_alt, mapping = aes(x=age_bracket, y=total_num_assign)) + geom_point() + geom_jitter()

# original point: consecutive status impact num_medco and total_num_assign
ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) + geom_point() + geom_jitter()
ggplot(data = b_alt, mapping = aes(x=consecutive2, y=total_num_assign)) + geom_point() + geom_jitter()

