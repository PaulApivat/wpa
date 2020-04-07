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

# create variable for length of assignment; diff_date
df_medco_join$diff_date <- as.Date(as.character(df_medco_join$return_date), format = "%m/%d/%Y")-as.Date(as.character(df_medco_join$departure_date), format = "%m/%d/%Y")

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

# create new data frame, all_assign_medco by inner_join y and assign_first_medco
all_assign_medco <- assign_first_medco %>%
    inner_join(y, by = "staff_id")

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

####### Explore both Medco AND Non-Medco Career Paths #######

# df is the original data set with all staff_id doing multiple assignments 
# (even some who did NOT do a MedCo assignment)
# this join allow us to see only staff_id who did at least one MedCo assignment
# different from all MedCo assignments (n=946)
# different from unique staff_id who did one MedCo (n=462)
# this one is showing all MedCo assignments for all staff_id (n=3569)


df_join <- df %>%
    inner_join(multiple_medco, by = "staff_id")

# create vector for total number of assignments (including NON-Medco assignments)
total_number_assignment <- df_join %>% 
    group_by(staff_id) %>% 
    tally(sort = TRUE)

colnames(total_number_assignment)[2] <- 'total_num_assign'

# join all_assign_medco and total_number_assignment
# this gets all distinct staff_id but with following info
# num_medco - number of MedCo assignments
# assignment_num_first_medco
# assignment_num_last_medco
# total_num_assign (including non-Medco)
# for unique staff_id (n = 462)
all_assignment_join <- all_assign_medco %>% 
    inner_join(total_number_assignment, by = "staff_id")

# create same ALTERNATE version of all_assignment_join 
# not just distinct staff_id, but multiple assignments for every staff_id
# (n = 3569)

all_assignment_join2 <- all_assignment_join %>%
    select(staff_id, assign_num_first_medco, assign_num_last_medco, total_num_assign)

# create new data frame (n = 3569) with all: num_medco, assign_num_first_medco, assign_num_last_medco, total_num_assign
# all assignments for all staff_id who did at least one MedCo
df_join_data <- left_join(df_join, all_assignment_join2, by = 'staff_id')

#### Accounting for Gaps - Consecutive & Non-Consecutive MedCo Assignments ####

# create dataframe a (n = 3569)
# determine if a person did consecutive MedCo assignments
a <- df_join_data
a[,'consecutive'] <- NA 
a$consecutive <- if_else(a$assign_num_last_medco-a$assign_num_first_medco==(a$num_medco-1), 'consecutive', NULL)


# create dataframe b (n = 462)
# only distinct staff_id
b <- distinct(a, staff_id, .keep_all = TRUE)


# create dataframe c (n = 112)
# for all NON-consecutive
c <- b[is.na(b$consecutive),]


#########-------------- PLOT(s)-------------- ############
#### Description of Assignments, Number of People

# temp: tally number of assignments in each pool
temp <- df %>%
    group_by(pool) %>%
    tally(sort = TRUE)

# plot
# counting frequency of assignments in each Pool
plot <- ggplot(data = temp, mapping = aes(x=reorder(pool,n), y=n, fill = if_else(temp$pool=="MEDCO", "#e9222a", "#0d0d0d"))) 
+ geom_bar(stat = "identity") 
+ coord_flip() 
+ geom_text(aes(label = n), vjust = 1.0, hjust = -0.5, color = "black") 
+ theme(legend.position = "none") 
+ labs(y = "Number of Assignments", x = "Pools", title = "Total Assignments in each Pool") 
+ scale_fill_manual(values = c("#6c6c6c", "#e9222a")) 
+ theme(panel.background = element_blank())

# plot2
# side-by-side bar chart of Number of Assignments and Number of People per each Pool

temp2 <- temp

# find number of people for EACH pool (manual process)
df %>%
    filter(pool=="MIDWIFE") %>%      # repeat for ALL pool categories 
    summarise(num_people = n_distinct(staff_id))

# add new column to temp2 to store 'num_people' with 11 observations (see above)
temp2[,"num_people"] <- c(4186, 1511, 1657, 1582, 1985, 1947, 1328, 462, 414, 246, 139)
temp3 <- select(temp2, 'pool', 'num_assignments', 'num_people')

# use rehape package to melt
temp4 <- melt(temp3, id.vars = 'pool')

plot2 <- ggplot(data = temp4, mapping = aes(x=reorder(pool, value), y=value, fill=variable)) 
+ geom_bar(stat = "identity", position = "dodge") 
+ labs(x="Pool", y = "Numbers", title = "Assignments and People", fill = "Variables") 
+ scale_fill_manual(labels=c("Number of Assignments", "Number of People"), values = c("#6c6c6c", "#e9222a")) 
+ theme(panel.background = element_blank())

# plot3
# create data frame listing frequency of medco assignments (1-15) and how many people per frequency
num_medco_by_people <- all_assignment_join %>%
    group_by(num_medco) %>%
    tally(sort = TRUE)

# change column name to num_of_people
colnames(num_medco_by_people)[2] <- "num_of_people"

# create new column with factor levels 
num_medco_by_people[,"num_medco_factor"] <- factor(c("one", "more than one", "more than one", "more than one", "more than one", "more than one", "more than one", "more than one", "more than one", "more than one", "more than one", "more than one"), levels = c("one", "more than one"))

# plot3
plot3 <- ggplot(data = num_medco_by_people, mapping = aes(x=num_medco, y=num_of_people, fill=num_medco_factor)) 
+ geom_col() 
+ geom_text(aes(label = num_of_people), vjust = -1.0) 
+ scale_fill_manual(values = c("#e9222a", "#6c6c6c")) 
+ labs(x = "Number of Medco Assignments", y = "Number of People", fill = "MedCo Assignments") 
+ theme(panel.background = element_blank())

# plot4
# remove/delete row from num_medco_by_people by subsetting
# remove/delete rows 3-to-12 from num_medco_by_people
num_medco_by_people2 <- num_medco_by_people[-c(3:12),]

# change second observation in row 2 from 98 to 200
num_medco_by_people2$num_of_people[2] <- 200


# plot4
plot4 <- ggplot(data = num_medco_by_people2, mapping = aes(x=num_medco, y=num_of_people, fill=num_medco_factor)) 
+ geom_col() 
+ scale_fill_manual(values = c("#e9222a", "#6c6c6c")) 
+ labs(x = "One vs More than one", y = "Number of People", fill = "MedCo Assignments") 
+ theme(panel.background = element_blank())

#### Understanding Shortage: One-Timer vs Multi-Timers
# plot7 (will return to plot5 and plot6)

# starting point data frames ONLY ‘b’  (compared 1 medco, n=262, vs >1 medco, n=200)
# first calculated mean and standard deviation, then you’ll use those mean to construct the graphs

# b (n = 262) those who did only one medco

b %>% 
    filter(num_medco==1) %>%
    summarize(avg_position_first_medco = mean(assign_num_first_medco), sd = sd(assign_num_first_medco))

  avg_position_first_medco       sd
1                 4.381679 	3.678576

# b (n = 200) those who did more than one medco
b %>%
    filter(num_medco > 1) %>%
    summarize(avg_position_first_medco = mean(assign_num_first_medco), sd = sd(assign_num_first_medco))

avg_position_first_medco       sd
1                    4.895 		3.944843

# create two dataframe, then use rbind() as precursor
# distributions generated from rnorm() which uses *actual* means and standard deviations to simulate random variate under normal distribution
one_medco <- data.frame(type = "one_medco", assign_num_first_medco = rnorm(n=262, mean = 4.381679, sd = 3.678576))
more_than_one_medco <- data.frame(type = "more_than_one_medco", assign_num_first_medco = rnorm(n=200, mean = 4.895, sd = 3.944843))
overlap2_df <- rbind(one_medco, more_than_one_medco)

# plot7
plot7 <- ggplot(data = overlap2_df, mapping = aes(x=assign_num_first_medco, fill=type)) 
+ geom_histogram(alpha = .8, binwidth = .5, position = "identity") 
+ theme_classic() 
+ scale_fill_manual(labels=c("One", "More than one"), values = c("#e9222a", "#6c6c6c")) 
+ xlim(0,15) 
+ ylim(0,15) 
+ labs(x = "Average Position of First Medco Assignments", y = "Number of People", title = "Difference in Average Position of First Medco", fill = "Number of MedCo")

# Walch Two Sample t-test (not significantly different)
t.test(assign_num_first_medco ~ type, data = overlap2_df)

### plot7a 

# starting point data frames ONLY ‘b’  (compared 1 medco, n=262, vs >1 medco, n=200)

# first calculated mean and standard deviation, then you’ll use those mean to construct the graphs
# b (n = 262)

b %>%
    filter(num_medco==1) %>%
    summarize(avg_num_assignment = mean(total_num_assign), sd = sd(total_num_assign))

  avg_num_assignment       sd
1           5.675573 		5.158567

# b (n = 200)

b %>%
    filter(num_medco > 1) %>%
    summarize(avg_num_assignment = mean(total_num_assign), sd = sd(total_num_assign))

  avg_num_assignment       sd
1              10.41 		6.917453

# create two dataframe, then use rbind() as precursor

one_medco_a <- data.frame(type = "one_medco", total_num_assign = rnorm(n=262, mean = 5.675573, sd = 5.158567))
more_than_one_medco_a <- data.frame(type = "more_than_one_medco", total_num_assign = rnorm(n=200, mean = 10.41, sd = 6.917453))
overlap2_df_a <- rbind(one_medco_a, more_than_one_medco_a)

# plot7a
plot7a <- ggplot(data = overlap2_df_a, mapping = aes(x=total_num_assign, fill=type)) 
+ geom_histogram(alpha = .8, binwidth = 1, position = "identity") 
+ theme_classic() 
+ scale_fill_manual(labels=c("One", "More than one"), values = c("#e9222a", "#6c6c6c")) 
+ xlim(0,30) 
+ ylim(0,30) 
+ labs(x = "Average Number of Assignments", y = "Number of People", title = "Difference in Average Number of Assignments", fill = "Number of MedCo")

# t.test (significant difference e in average number of (total) assignments)
t.test(total_num_assign ~ type, data = overlap2_df_a)


