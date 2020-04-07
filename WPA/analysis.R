# minor edit
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

# plot7b

## The correct calculation for this graph starts with data frame “a” because we want to account for ALL Medco assignments
# first create diff_date for 'a' using difftime() to find difference between two dates
a$diff_date <- difftime(a$return_date, a$departure_date, units = c('days'))

# n = 262 (only one-timers)
a %>%
    filter(num_medco==1) %>%
    filter(pool=="MEDCO") %>%
    summarize(average_length_medco = mean(diff_date), sd = sd(diff_date))

  average_length_medco       sd
1        267.6908 days 	263.7288

# n = 684 (staffers go on more than one MedCo)
a %>%
    filter(num_medco > 1) %>%
    filter(pool=="MEDCO") %>%
    summarize(average_length_medco = mean(diff_date), sd = sd(diff_date))

  average_length_medco       sd
1        206.0249 days 	226.0477


# create two dataframe, then use rbind() as precursor

one_medco_b <- data.frame(type = "one_medco", diff_date = rnorm(n=262, mean = 229.084, sd = 239.6649))
more_than_one_medco_b <- data.frame(type = "more_than_one_medco", diff_date = rnorm(n=200, mean = 190.3, sd = 136.8582))
overlap2_df_b <- rbind(one_medco_b, more_than_one_medco_b)

# plot 7b
plot7b <- ggplot(data = overlap2_df_b, mapping = aes(x=diff_date, fill=type)) 
+ geom_histogram(alpha = .8, binwidth = 25, position = "identity") 
+ theme_classic() 
+ scale_fill_manual(labels=c("One", "More than one"), values = c("#e9222a", "#6c6c6c")) 
+ xlim(0,1000) 
+ ylim(0,40) 
+ labs(x = "Length of MedCo Assignments", y = "Number of People", title = "Difference in Average Length of MedCo Assignments", fill = "Number of MedCo")

# t.test
t.test(diff_date ~ type, data = overlap2_df_b)

#### PLOT12

# question: Does length of assignment matter for first Medco assignments?
# context: There are 93 people whose first assignment was MedCo, 
# - 66 only did one MedCo
# - 27 did more than one

# create a93 data frame from 'a'
a %>% filter(assignment_number==1) %>% 
    filter(pool=="MEDCO") %>% 
    group_by(num_medco, first_departure) %>% 
    tally(sort = TRUE) -> a93

# reverse factor-level of a93x  (check str(a93) vs. str(a93x))
# reverse factor-level helps with CLEARER visualization
a93x <- a93
a93x$first_departure <- factor(a93x$first_departure, levels = rev(levels(a93x$first_departure)))

# plot using a93x data frame
plot12 <- ggplot(data = a93x, mapping = aes(x=num_medco, y=n, fill=first_departure)) 
+ geom_col() 
+ labs(x="Number of Medco", y = "Number of People", title = "93 people whose first assignment was Medco", fill = "Departure Status") 
+ scale_fill_manual(values = c("black", "#6c6c6c", "#e9222a")) 
+ scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) 
+ theme(panel.background = element_blank())

#### PLOT11b
# note: to see if relationship between Number of Assignments and Position of First MedCo differs between Departure Status

# subset only variables of interest
b_mod <- b %>% select(staff_id, num_medco, assign_num_first_medco, first_departure)

# plot
plot11b <- ggplot(data = b_mod, mapping = aes(x=num_medco, y=assign_num_first_medco, color = first_departure)) 
+ geom_point(size = 5, alpha = .5) 
+ geom_smooth(method = lm, se = FALSE) 
+ labs(x = "Number of MedCo Assignments", y = "Position of First MedCo Assignment", color = "Departure Status") 
+ scale_color_manual(values = c("#EE0000", "#FF9393", "#000000")) 
+ theme_classic()

# correlation between num_medco and assign_num_first_medco by departure status

# r = -0.008226777
b_mod %>% filter(first_departure=="First departure") %>% get_correlation(formula = num_medco ~ assign_num_first_medco)
# r = -0.00596063
b_mod %>% filter(first_departure=="First departure, relevant experience") %>% get_correlation(formula = num_medco ~ assign_num_first_medco)
# r = 0.2270808
b_mod %>% filter(first_departure=="Not first departure") %>% get_correlation(formula = num_medco ~ assign_num_first_medco)

##### Understanding Gaps: Consecutive vs Non-Consecutive
library(waffle)

### PLOT5
# count one-timer vs multi-timer (consecutive & non-consecutive)
# note: backticks
parts2 <- c(`One assignment` = (462-88-112), `Consecutive assignments` = 88, `Non-consecutive assignments` = 112)
waffle(parts2, rows = 8, size = 1, colors = c("#000000", "#EE0000", "#FF9393"), legend_pos = "bottom")

plot5 <- waffle(parts2, rows = 8, size = 1, colors = c("#000000", "#EE0000", "#FF9393"), legend_pos = "bottom")

### PLOT6
# examine if there are differences in "total number of MedCo assignments" between 'consecutive' vs. 'non-consecutive'
# starting point are data frames 'b' and 'c'
# first calculated mean and standard deviation, then you’ll use those mean to construct the graphs


b %>%
    filter(consecutive=="consecutive") %>%
    filter(num_medco > 1) %>%
    summarize(avg_num_medco_assignment = mean(num_medco), sd = sd(num_medco))

#avg_num_medco_assignment        sd
#1                 2.488636 0.8840128

c %>%
    summarize(avg_num_medco_assignment = mean(num_medco), sd = sd(num_medco))

#avg_num_medco_assignment       sd
#1                 4.151786 2.428101



# create overlap_df data frame
consecutive_set <- data.frame(type = "consecutive", num_medco = rnorm(n=88, mean = 2.488636, sd = 0.8840128))
non_consecutive_set <- data.frame(type = "non-consecutive", num_medco = rnorm(n=112, mean = 4.151786, sd = 2.428101))

overlap_df <- rbind(consecutive_set, non_consecutive_set)

# plot
plot6 <- ggplot(overlap_df, aes(x=num_medco, fill=type)) 
+ geom_histogram(alpha = .8, binwidth = .5, position = "identity") 
+ theme_classic() 
+ scale_fill_manual(labels=c("Consecutive", "Non-Consecutive (Gap)"), values = c("#EE0000", "#000000")) 
+ labs(x = "Number of Medco Assignments", y = "Number of People", title = "Differences in Number of MedCo Assignments", fill = "MedCo Assignments") 
+ xlim(0,15)

# t-test
t.test(num_medco ~ type, data = overlap_df)

### PLOT6b
# note: similar to previous plot, but see differences in TOTAL number of assignments
# starting point are data frames 'b' and 'c'
# calculate mean and standard deviation

# create overlap_df_b data frame
consecutive_set_b <- data.frame(type = "consecutive", total_num_assign = rnorm(n=88, mean = 6.579545, sd = 3.737728))
non_consecutive_set_b <- data.frame(type = "non-consecutive", total_num_assign = rnorm(n=112, mean = 13.41964, sd = 7.352928))
overlap_df_b <- rbind(consecutive_set_b, non_consecutive_set_b)

# plot
plot6b <- ggplot(data = overlap_df_b, mapping = aes(x=total_num_assign, fill=type)) 
+ geom_histogram(alpha = .8, binwidth = 1, position = "identity") 
+ theme_classic() 
+ scale_fill_manual(labels=c("Consecutive", "Non-Consecutive (Gap)"), values = c("#EE0000", "#000000")) 
+ xlim(0,30) 
+ labs(x = "Number of Assignments", y = "Number of People", title = "Differences in Number of (Total) Assignments", fill = "MedCo Pattern")


### PLOT6a
# note to see if there are differences between 'consecutive' vs 'non-consecutive' in
# WHEN their first MedCo assignment was
# differences in how long it takes to 'get ready'

# calculating average number of assignments
# consecutive
b %>% filter(consecutive=="consecutive") %>% filter(num_medco > 1) %>% summarize(avg_position_first_medco = mean(assign_num_first_medco), sd = sd(assign_num_first_medco))
# non-consecutive
c %>% summarize(avg_position_first_medco = mean(assign_num_first_medco), sd = sd(assign_num_first_medco))


# create overlap_df_a
consecutive_set_a <- data.frame(type = "consecutive", assign_num_first_medco = rnorm(n=88, mean = 4, sd = 2.95561))
non_consecutive_set_a <- data.frame(type = "non-consecutive", assign_num_first_medco = rnorm(n=112, mean = 5.598214, sd = 4.462981))
overlap_df_a <- rbind(consecutive_set_a, non_consecutive_set_a)

# plot
plot6a <- ggplot(data = overlap_df_a, mapping = aes(x=assign_num_first_medco, fill=type)) 
+ geom_histogram(alpha = .8, binwidth = .5, position = "identity") 
+ theme_classic() 
+ scale_fill_manual(labels=c("Consecutive", "Non-Consecutive (Gap)"), values = c("#EE0000", "#000000")) 
+ xlim(0,17) 
+ ylim(0,17) 
+ labs(x = "Assignment Number of first MedCo", y = "Number of People", title = "When did people do their first MedCo assignment?", fill = "MedCo Pattern")

# t-test
t.test(assign_num_first_medco ~ type, data = overlap_df_a)

### PLOT11a
# create b_cor data frame
b_cor <- b %>% select(staff_id, num_medco, total_num_assign, consecutive)

# plot
plot11a <- ggplot(data = b_cor, mapping = aes(x=num_medco, y=total_num_assign, color = consecutive)) 
+ geom_point(size = 5, alpha = .5) 
+ geom_smooth(method = lm, se = FALSE) 
+ labs(x = "Number of Medco Assignments", y = "Number of Total Assignments", color = "MedCo Pattern") 
+ scale_color_manual(values = c("#EE0000", "#000000")) 
+ theme_classic()

##### PLOT8b
# What do people do BEFORE MedCo?
# answer with stacked bar chart

# create before_medco_df_b
a %>% 
    filter(assign_num_first_medco==5) %>% 
    filter(assignment_number < 5) %>% 
    count(assignment_number, pool) -> before_medco_df_b

# filter out 'unknown'
before_medco_df_b <- before_medco_df_b %>% 
    filter(pool != "Unknown")

# plot
plot8b <- ggplot(before_medco_df_b, aes(fill=pool, y=n, x=assignment_number)) 
+ geom_bar(position = "fill", stat = "identity") 
+ scale_fill_manual(values = c("#FFE5E5", "#FF9393", "#EE0000", "#F5F5F5", "#DCDCDC", "#C0C0C0", "#8B8989", "#000000")) 
+ labs(x = "Assignment Number", y = "Percentage", title = "Pools represented in first four assignments before first MedCo", fill = "Pools") 
+ theme_classic()

###### SANKEY ######
## note: if the average assignment number of the 1st MedCo is 5, what did people do on assignments 1 through 4

# start with data frame “a”, filter for those who had first MedCo on fifth assignment, filter for assignments 1-4
sankey5 <- a %>% 
    filter(assign_num_first_medco==5) %>% 
    filter(assignment_number < 5)

# filter for each assignment, then paste into new data frame resulting in side by side column(s) of pool, each representing a different assignment
sankey5 %>%
    filter(assignment_number==1) %>%
    select(staff_id, pool) -> sankey1


sankey5 %>%
    filter(assignment_number==2) %>%
    select(pool) -> sankey1$pool2


sankey5 %>%
    filter(assignment_number==3) %>%
    select(pool) -> sankey1$pool3


sankey5 %>%
    filter(assignment_number==4) %>%
    select(pool) -> sankey1$pool4

# without having to change column name later
sankey5 %>%
    filter(assignment_number==5) %>%
    select(pool) -> sankey1[,"assignment_5"]

# change column names

colnames(sankey1)[2] <- "assignment_1"
colnames(sankey1)[3] <- "assignment_2"
colnames(sankey1)[4] <- "assignment_3"
colnames(sankey1)[5] <- "assignment_4"

#note: columns 3-5 are “data.frames” for some reason
#changing from data frame to character (temporary solution)

sankey1$assignment_2 <- as.vector(unlist(sankey1['assignment_2']))
sankey1$assignment_3 <- as.vector(unlist(sankey1['assignment_3']))
sankey1$assignment_4 <- as.vector(unlist(sankey1['assignment_4']))

#### IMPORTANT: add string of number to vectors— THIS WAS KEY

#source: https://stackoverflow.com/questions/6984796/how-to-paste-a-string-on-each-element-of-a-vector-of-strings-using-apply-in-r
sankey1$assignment_2 <- paste(sankey1$assignment_2, "2", sep = "")
sankey1$assignment_3 <- paste(sankey1$assignment_3, "3", sep = "")
sankey1$assignment_4 <- paste(sankey1$assignment_4, "4", sep = "")

# turn characters into factors in order to use group_by() and tally() functions
#source: https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors

sankey1$assignment_2 <- as.factor(sankey1$assignment_2)
sankey1$assignment_3 <- as.factor(sankey1$assignment_3)
sankey1$assignment_4 <- as.factor(sankey1$assignment_4)

# mixture of using group_by() and tally() plus manually creating a new data frame for Sankey diagram

# assignment 1 -> 2 (save to links1)
sankey1 %>%
    group_by(assignment_1, assignment_2) %>%
    tally(sort = TRUE) -> links1

# assignment 2 -> 3 (links2)
sankey1 %>%
    group_by(assignment_2, assignment_3) %>%
    tally(sort = TRUE) -> links2

# assignment 3 -> 4 (links3)
sankey1 %>%
    group_by(assignment_3, assignment_4) %>%
    tally(sort = TRUE) -> links3

# assignment 4 -> 5 (links4)
sankey1 %>%
    group_by(assignment_4, assignment_5) %>%
    tally(sort = TRUE) -> links4

# change column names for links1, links2, links3, link4

colnames(links1)[1] <- "source"
colnames(links1)[2] <- "target"
colnames(links1)[3] <- "value"

colnames(links2)[1] <- "source"
colnames(links2)[2] <- "target"
colnames(links2)[3] <- "value"

colnames(links3)[1] <- "source"
colnames(links3)[2] <- "target"
colnames(links3)[3] <- "value"

colnames(links4)[1] <- "source"
colnames(links4)[2] <- "target"
colnames(links4)[3] <- "value"

# create a node data frame, listing all entities involved in the flow

nodes1 <- data.frame(name=c(as.character(links1$source), as.character(links1$target)) %>% unique())
nodes2 <- data.frame(name=c(as.character(links2$source), as.character(links2$target)) %>% unique())
nodes3 <- data.frame(name=c(as.character(links3$source), as.character(links3$target)) %>% unique())
nodes4 <- data.frame(name=c(as.character(links4$source), as.character(links4$target)) %>% unique())

# with network3D , connection must be provided using ID, so need to reformat

links1$IDsource <- match(links1$source, nodes1$name)-1
links1$IDtarget <- match(links1$target, nodes1$name)-1

links2$IDsource <- match(links2$source, nodes2$name)-1
links2$IDtarget <- match(links2$target, nodes2$name)-1

links3$IDsource <- match(links3$source, nodes3$name)-1
links3$IDtarget <- match(links3$target, nodes3$name)-1

links4$IDsource <- match(links4$source, nodes4$name)-1
links4$IDtarget <- match(links4$target, nodes4$name)-1

# plot sankey diagram

p1 <- sankeyNetwork(Links = links1, Nodes = nodes1, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", sinksRight = FALSE)
p2 <- sankeyNetwork(Links = links2, Nodes = nodes2, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", sinksRight = FALSE)
p3 <- sankeyNetwork(Links = links3, Nodes = nodes3, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", sinksRight = FALSE)
p4 <- sankeyNetwork(Links = links4, Nodes = nodes4, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", sinksRight = FALSE)
p5 <- sankeyNetwork(Links = links5, Nodes = nodes5, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", sinksRight = FALSE)

## next step: combine all Sankey into one ##

# save rows into new data frames (should be able to do with ‘links1’ etc)

one <- links1[1:16,]
two <- links2[1:14,]
three <- links3[1:18,]
four <- links4[1:9,]

# rbind to merge dataframe by rows

links1_a <- bind_rows(one, two, three, four)

links5 <- links1_a

## need to re-create this, this cannot be copied from previous four data frames

nodes5 <- data.frame(name=c(as.character(links5$source), as.character(links5$target)) %>% unique())

### CUSTOMIZATION: Color each flow (Sankey Diagram)
# source: https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html

# create new dataframe
links6 <- links5
nodes6 <- nodes5

# classify each LINK into a ‘type’ and create a new column “group” (all 57 links must be classified)

links6$group <- as.factor(c("md_type", "md_type", "md_type", "every_type", "every_type", "every_type", "every_type", "every_type", "md_type", "mtl_type", "mtl_type", "mtl_type", "every_type", "pc_type", "every_type", "every_type", "md_type", "pc_type", "md_type", "every_type", "every_type", "md_type", "mtl_type", "pc_type", "pc_type", "md_type", "mtl_type", "mtl_type", "nonmed_type", "every_type", "pc_type", "md_type", "pc_type", "pc_type", "md_type", "md_type", "mtl_type", "every_type", "every_type", "every_type", "md_type", "mtl_type", "every_type", "every_type", "pc_type", "every_type", "every_type", "every_type", "pc_type", "md_type", "mtl_type", "nonmed_type", "deputy_type", "every_type", "every_type", "every_type", "every_type"))

# do the same thing for each NODE (in this case, i’ve decided all nodes will be the same color, emphasizing the color of flow instead); create new “group” column

nodes6$group <- as.factor(c("my_unique_group"))

# create color variable that matches each “type” (including node) with a “color”

my_color <- 'd3.scaleOrdinal() .domain(["md_type", "pc_type", "mtl_type", "nonmed_type", "deputy_type", "every_type", "my_unique_group"]) .range(["#EE0000", "#FF4141", "#FF9393", "#FFE5E5", "#C0C0C0", "#DCDCDC", "#8B8989"])'

##### Try out the different sankey flow colors

# MD flow
my_color_md <- 'd3.scaleOrdinal() .domain(["md_type", "pc_type", "mtl_type", "nonmed_type", "deputy_type", "every_type", "my_unique_group"]) .range(["#EE0000", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#8B8989"])'

# PC flow
my_color_pc <- 'd3.scaleOrdinal() .domain(["md_type", "pc_type", "mtl_type", "nonmed_type", "deputy_type", "every_type", "my_unique_group"]) .range(["#DCDCDC", "#000000", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#8B8989"])'

# MTL flow
my_color_mtl <- 'd3.scaleOrdinal() .domain(["md_type", "pc_type", "mtl_type", "nonmed_type", "deputy_type", "every_type", "my_unique_group"]) .range(["#DCDCDC", "#DCDCDC", "#800000", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#8B8989"])'

# other flows
my_color_every <- 'd3.scaleOrdinal() .domain(["md_type", "pc_type", "mtl_type", "nonmed_type", "deputy_type", "every_type", "my_unique_group"]) .range(["#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#FF9393", "#8B8989"])'

# Messy flow (includes everything) 
my_color_2a <- 'd3.scaleOrdinal() .domain(["md_type", "pc_type", "mtl_type", "nonmed_type", "deputy_type", "every_type", "my_unique_group"]) .range(["#EE0000", "#000000", "#800000", "#00FF00", "#00BFFF", "#FF9393", "#8B8989"])'

# SankeyNetwork function (change “colourScale” to see different flows)
sankeyNetwork(Links = links6, Nodes = nodes6, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale = my_color_2a, LinkGroup = "group", NodeGroup = "group")

