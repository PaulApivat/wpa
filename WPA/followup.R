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

# new libraries
library(ltm)


### Question 1: HISTOGRAMS

# prior context
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

## ggplot() implementation of back-to-back histogram 
## use existing overlap2_df
## source: http://tagteam.harvard.edu/hub_feeds/1981/feed_items/1011661
## source: https://www.r-bloggers.com/making-back-to-back-histograms/

############### When is it appropriate to use Back-to-Back Histograms ##########
# 1 clear difference between two distribution
# 2 clear difference between x-axis variable

##### One-timer vs Multi-timer

# Plot 7: Diff avg position 1st medco - NO SIGNIFICANT DIFFERENCE
# 95 percent confidence interval:
# -1.2747961  0.1304898

# Plot 7a Diff avg Number of Assignments - YES SIGNIFICANT DIFFERENCE
# 95 percent confidence interval:
# -5.499777 -3.239905

# Plot 7b Diff Length of MedCo Assignment - YES SIGNIFICANT DIFFERENCE
# 95 percent confidence interval:
#   8.191753 109.060236

### One-Timer vs Multi-Timer (Individual Difference - Gender, Age)

## Are there GENDER differences between One-Timer vs Multi-Timer? - NO RELATIONSHIP
# [1] -0.004597443

## Are there AGE differences between One-Timer vs Multi-Timer? - NO RELATIONSHIP
# [1] 0.07349357


###### Consecutive vs Non-consecutive

# Plot 6: Diff Number of MedCo - YES SIGNIFICANT DIFFERENCES
# 95 percent confidence interval:
# -2.511473 -1.577169

# Plot 6b: Diff Total Number of Assignments - YES SIGNIFICANT DIFFERENCES
# 95 percent confidence interval:
# -8.980970 -5.386363

# Plot 6a

## Are there GENDER differences between Consecutive vs Non-Consecutive status?
## Are there AGE differences between Consecutive vs Non-Consecutive status?

############ -------- Back-to-Back HISTOGRAMS  ---------- #############
############ -------- 90 Degree HISTOGRAMS  ---------- #############
## NOTE: need to set the y-axis limit (i.e., a negative assign_num_first_medco OR total_num_assign does NOT make sense)
# key is subset() and y=-..density..

#### Alternative plot7 (see overlap2_df)
ggplot() 
    + geom_histogram(data = subset(overlap2_df, type=='one_medco'), aes(x=assign_num_first_medco, fill='one_medco', y=..count..), binwidth = diff(range(overlap2_df$assign_num_first_medco))/30, fill='blue') 
    + geom_histogram(data = subset(overlap2_df, type=='more_than_one_medco'), aes(x=assign_num_first_medco, fill='more_than_one_medco', y=-..count..), binwidth = diff(range(overlap2_df$assign_num_first_medco))/30, fill='green') 
    + scale_fill_hue('Type')
    + xlim(0,20)

# 90-degree rotation
+ coord_flip()

#### FINAL plot7_alt (coord_flip())
plot7_alt <- ggplot() 
    + geom_histogram(data = subset(overlap2_df, type=='one_medco'), aes(x=assign_num_first_medco, fill='one_medco', y=..count..), binwidth = diff(range(overlap2_df$assign_num_first_medco))/30, fill='#e9222a') 
    + geom_histogram(data = subset(overlap2_df, type=='more_than_one_medco'), aes(x=assign_num_first_medco, fill='more_than_one_medco', y=-..count..), binwidth = diff(range(overlap2_df$assign_num_first_medco))/30, fill='#6c6c6c') 
    + theme_classic() 
    + labs(x = "Average Position of First Medco Assignments", y = "Number of People", title = "Difference in Average Position of First Medco", fill = "Number of MedCo") 
    + xlim(0,20) 
    + coord_flip()


#### Alternative plot7a (see overlap2_df_a)
ggplot() 
    + geom_histogram(data = subset(overlap2_df_a, type=='one_medco'), aes(x=total_num_assign, fill='one_medco', y=..count..), binwidth = diff(range(overlap2_df_a$total_num_assign))/30, fill='orange') 
    + geom_histogram(data = subset(overlap2_df_a, type=='more_than_one_medco'), aes(x=total_num_assign, fill='more_than_one_medco', y=-..count..), binwidth = diff(range(overlap2_df_a$total_num_assign))/30, fill='green') 
    + scale_fill_hue('Type')

# 90-degree rotation
+ coord_flip()

#### FINAL plot7a_alt (two lines - show mean difference)
plot7a_alt <- ggplot() 
    # geom_vlines compare mean of two distributions
    + geom_histogram(data = subset(overlap2_df_a, type=='one_medco'), aes(x=total_num_assign, fill='one_medco', y=..count..), binwidth = diff(range(overlap2_df_a$total_num_assign))/50, fill='#e9222a') 
    # can choose which rows to calculate mean [1:262] or [263:462]
    + geom_vline(xintercept = mean(overlap2_df_a$total_num_assign[1:262]), color="#6c6c6c", linetype="dotted", size=1.5) 
    + geom_histogram(data = subset(overlap2_df_a, type=='more_than_one_medco'), aes(x=total_num_assign, fill='more_than_one_medco', y=-..count..), binwidth = diff(range(overlap2_df_a$total_num_assign))/50, fill='#6c6c6c') 
    + geom_vline(xintercept = mean(overlap2_df_a$total_num_assign[263:462]), color="#e9222a", linetype="dotted", size=1.5) 
    + theme_classic() 
    + xlim(0,30) 
    + scale_fill_manual(labels=c("One", "More than one"), values = c("#e9222a", "#6c6c6c")) 
    + labs(x = "Average Number of Assignments", y = "Number of People", title = "Difference in Average Number of Assignments", fill = "Number of MedCo")



#### Alternative plot7b (see overlap2_df_b)
ggplot() 
    + geom_histogram(data = subset(overlap2_df_b, type=='one_medco'), aes(x=diff_date, fill='one_medco', y=..count..), binwidth = diff(range(overlap2_df_b$diff_date))/30, fill='purple') 
    + geom_histogram(data = subset(overlap2_df_b, type=='more_than_one_medco'), aes(x=diff_date, fill='more_than_one_medco', y=-..count..), binwidth = diff(range(overlap2_df_b$diff_date))/30, fill='green') 
    + scale_fill_hue('Type')

# 90-degree rotation
+ coord_flip()


#### FINAL plot7b_alt (two lines - show mean difference)
plot7b_alt <- ggplot() 
    + geom_histogram(data = subset(overlap2_df_b_alt, type=='one_medco'), aes(x=diff_date, fill='one_medco', y=..count..), binwidth = diff(range(overlap2_df_b_alt$diff_date))/50, fill='#e9222a') 
    + geom_histogram(data = subset(overlap2_df_b_alt, type=='more_than_one_medco'), aes(x=diff_date, fill='more_than_one_medco', y=-..count..), binwidth = diff(range(overlap2_df_b_alt$diff_date))/50, fill='#6c6c6c') 
    + theme_classic() 
    + xlim(0,1000) 
    + scale_fill_manual(labels=c("One", "More than one"), values = c("#e9222a", "#6c6c6c")) 
    + labs(x = "Length of MedCo Assignments", y = "Number of People", title = "Difference in Average Length of MedCo Assignments", fill = "Number of MedCo") 
    + scale_y_continuous(labels = abs)

## to add vertical lines
+ geom_vline(xintercept = mean(overlap2_df_b_alt$diff_date[1:262]), color="#6c6c6c", linetype="dotted", size=1.5)
+ geom_vline(xintercept = mean(overlap2_df_b$diff_date[263:462]), color="#e9222a", linetype="dotted", size=1.5)

## 90-degre rotation (NOT RECOMMENDED)
+ coord_flip()




#### Alternative plot6 (see overlap_df)
ggplot() 
    + geom_histogram(data = subset(overlap_df, type=='consecutive'), aes(x=num_medco, fill='consecutive', y=..count..), binwidth = diff(range(overlap_df$num_medco))/30, fill='pink') 
    + geom_histogram(data = subset(overlap_df, type=='non-consecutive'), aes(x=num_medco, fill='non-consecutive', y=-..count..), binwidth = diff(range(overlap_df$num_medco))/30, fill='green') 
    + scale_fill_hue('Type')

# 90-degree rotation
+ coord_flip()

##### FINAL plot6_alt (absolute value on y-axis)
plot6_alt <- ggplot() 
    + geom_histogram(data = subset(overlap_df, type=='consecutive'), aes(x=num_medco, fill='consecutive', y=..count..), binwidth = .5, fill='#EE0000') 
    + geom_vline(xintercept = mean(overlap_df$num_medco[1:88]), color='#000000', linetype='dotted', size=1.5) 
    + geom_histogram(data = subset(overlap_df, type=='non-consecutive'), aes(x=num_medco, fill='non-consecutive', y=-..count..), binwidth = .5, fill='#000000') 
    + geom_vline(xintercept = mean(overlap_df$num_medco[89:200]), color='#EE0000', linetype='dotted', size=1.5) + xlim(0,15) 
    + theme_classic() 
    + scale_fill_manual(labels=c("Consecutive", "Non-Consecutive (Gap)"), values = c("#EE0000", "#000000")) 
    + labs(x = "Number of Medco Assignments", y = "Number of People", title = "Differences in Number of MedCo Assignments", fill = "MedCo Assignments") 
    + scale_y_continuous(labels = abs)


#### Alternative plot6b (see overlap_df_b)

ggplot() 
    + geom_histogram(data = subset(overlap_df_b, type=='consecutive'), aes(x=total_num_assign, fill='consecutive', y=..count..), binwidth = diff(range(overlap_df_b$total_num_assign))/30, fill='gray') 
    + geom_histogram(data = subset(overlap_df_b, type=='non-consecutive'), aes(x=total_num_assign, fill='non-consecutive', y=-..count..), binwidth = diff(range(overlap_df_b$total_num_assign))/30, fill='green') 
    + scale_fill_hue('Type')
    

# 90-degree rotation
+ coord_flip()

##### FINAL plot6b_alt (absolute value on y-axis)
plot6b_alt <- ggplot() 
    + geom_histogram(data = subset(overlap_df_b, type=='consecutive'), aes(x=total_num_assign, fill='consecutive', y=..count..), binwidth = 1, fill='#EE0000') 
    + geom_vline(xintercept = mean(overlap_df_b$total_num_assign[1:88]), color='#000000', linetype='dotted', size=1.5) 
    + geom_histogram(data = subset(overlap_df_b, type=='non-consecutive'), aes(x=total_num_assign, fill='non-consecutive', y=-..count..), binwidth = 1, fill='#000000') 
    + geom_vline(xintercept = mean(overlap_df_b$total_num_assign[89:200]), color='#EE0000', linetype='dotted', size=1.5) + xlim(0,35) + theme_classic() 
    + scale_fill_manual(labels=c("Consecutive", "Non-Consecutive (Gap)"), values = c("#EE0000", "#000000")) 
    + labs(x = "Number of Assignments", y = "Number of People", title = "Differences in Number of (Total) Assignments", fill = "MedCo Pattern") 
    + scale_y_continuous(labels = abs)


#### Alternative plot6a (see overlap_df_a)
# Note: xlim(0,17)
ggplot() 
    + geom_histogram(data = subset(overlap_df_a, type=='consecutive'), aes(x=assign_num_first_medco, fill='consecutive', y=..count..), binwidth = diff(range(overlap_df_a$assign_num_first_medco))/30, fill='dodgerblue') 
    + geom_histogram(data = subset(overlap_df_a, type=='non-consecutive'), aes(x=assign_num_first_medco, fill='non-consecutive', y=-..count..), binwidth = diff(range(overlap_df_a$assign_num_first_medco))/30, fill='green') 
    + scale_fill_hue('Type') 
    + xlim(0,17)

# 90-degree rotation
+ coord_flip()

##### FINAL plot6a_alt (absolute value on y-axis)
plot6a_alt <- ggplot() 
    + geom_histogram(data = subset(overlap_df_a, type=='consecutive'), aes(x=assign_num_first_medco, fill='consecutive', y=..count..), binwidth = .5, fill='#EE0000') 
    + geom_vline(xintercept = mean(overlap_df_a$assign_num_first_medco[1:88]), color='#000000', linetype='dotted', size=1.5) 
    + geom_histogram(data = subset(overlap_df_a, type=='non-consecutive'), aes(x=assign_num_first_medco, fill='non-consecutive', y=-..count..), binwidth = .5, fill='#000000') 
    + geom_vline(xintercept = mean(overlap_df_a$assign_num_first_medco[89:200]), color='#EE0000', linetype='dotted', size=1.5) 
    + xlim(0,17) 
    + theme_classic() 
    + scale_fill_manual(labels=c("Consecutive", "Non-Consecutive (Gap)"), values = c("#EE0000", "#000000")) + labs(x = "Assignment Number of first MedCo", y = "Number of People", title = "When did people do their first MedCo assignment?", fill = "MedCo Pattern") 
    + scale_y_continuous(labels = abs)


##### Alternative to HISTOGRAM #####
### geom_density()
ggplot() 
    + geom_density(data = subset(overlap_df_a, type=='consecutive'), aes(x=assign_num_first_medco, fill='consecutive', y=..count..), binwidth = diff(range(overlap_df_a$assign_num_first_medco))/30, fill='dodgerblue') 
    + geom_density(data = subset(overlap_df_a, type=='non-consecutive'), aes(x=assign_num_first_medco, fill='non-consecutive', y=-..count..), binwidth = diff(range(overlap_df_a$assign_num_first_medco))/30, fill='green') 
    + scale_fill_hue('Type') + xlim(0,17)

## see also geom_freqpoly() but ignores 'fill' (warning: No colors)


### Question 2: SANKEY DIAGRAMS

#### Prior Context
# start with data frame “a”, filter for those who had first MedCo on fifth assignment, filter for assignments 1-4
# sankey5 has staff_id where assign_num_first_medco==5 and list assignment_number 1-4
# assumption is medco is assignment_number==5 
# avg position of people's first Medco is 5th assignment
sankey5 <- a %>% 
    filter(assign_num_first_medco==5) %>% 
    filter(assignment_number < 5)

### use sankey5 to create sankey1
### use sankey1 to create links1, links2, links3, links4
### use create corresponding nodes1, nodes2, nodes3, nodes4
### INITIAL (separate) sankey plots p1, p2, p3, p4, p5
### links5 and nodes5
### links6 and nodes6 (color customization)


### NOTE: 'value' (THICKNESS of sankey line) determined by:
# repeated for assignment_1 -> assignment_2,
# THEN assignment_2 -> assignment_3, 
# THEN assignment_3 -> assignment_4,
# THEN assignment_4 -> assignment_5,
sankey1 %>%
    group_by(assignment_1, assignment_2) %>%
    tally(sort = TRUE)

## Best way to communicate COMMONALITY of career path is to use colors to convey popular paths
## well travel paths are denoted by "value" in the links data frame
## instead of color being matched to group-type, create color scheme to match group-value
## instead of having 10 colors, just have 5 - easier to communicate (i.e., paths of values 1-2 are grouped together under 'whitesmoke')
## cannot simply arrange by desc(value) becauses the flows get sorted out of place (flows links6_alt are OUT of order)

links6_alt <- links6 %>%
+ arrange(desc(value))

# instead do
links6_alt <- links6


## change group column from 'type-based' to 'value-based' (goes from 6 -> 5 clusters)
links6_alt$group <- ifelse(links6_alt$value==13, 'level5', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==12, 'level5', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==9, 'level4', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==7, 'level4', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==6, 'level3', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==5, 'level3', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==4, 'level2', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==3, 'level2', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==2, 'level1', links6_alt$group)
links6_alt$group <- ifelse(links6_alt$value==1, 'level1', links6_alt$group)



# need to create new color scheme gradient by VALUE
# create new color gradient based on value, NOT 'type' (i.e., pc, md, mtl, nonmed) or source 

# Colors based on value (colorbrewer2.org - sequential multi-hue 5-class BuGn)
## Use GREEN to contrast with original plots that are using 'on-brand' colors
my_color_alt <- 'd3.scaleOrdinal() .domain(["level5", "level4", "level3", "level2", "level1", "my_unique_group"]) .range(["#006d2c", "#2ca25f", "#66c2a4", "#b2e2e2", "#edf8fb", "#8B8989"])'

## colorbrewer 5-class RdPu (Red Purple)
my_color_alt2 <- 'd3.scaleOrdinal() .domain(["level5", "level4", "level3", "level2", "level1", "my_unique_group"]) .range(["#7a0177", "#c51b8a", "#f768a1", "#fbb4b9", "#feebe2", "#8B8989"])'


## NEW sankey common career paths sequentially organized by color with 5-shades to minimize cognitive load
# note retain nodes6
sankeyNetwork(Links = links6_alt, Nodes = nodes6, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale = my_color_alt, LinkGroup = "group", NodeGroup = "group")


## Try setting iterations = 0 and re-ordering nodes6
## source: https://stackoverflow.com/questions/52229334/fixing-the-order-of-a-sankey-flow-graph-in-r-networkd3-package

sankeyNetwork(Links = links6_alt, Nodes = nodes6, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale = my_color_alt, LinkGroup = "group", NodeGroup = "group", iterations = 0)

## IMPORTANT - visually helpful to add nodePadding, nodeWidth and fontSize
sankeyNetwork(Links = links6_alt, Nodes = nodes6, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale = my_color_alt, LinkGroup = "group", NodeGroup = "group", iterations = 0, sinksRight = TRUE, nodePadding = 30, nodeWidth = 15, fontSize = 15)
## same sankey with Red Purple color shades
sankeyNetwork(Links = links6_alt, Nodes = nodes6, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale = my_color_alt2, LinkGroup = "group", NodeGroup = "group", iterations = 0, sinksRight = TRUE, nodePadding = 30, nodeWidth = 15, fontSize = 15)





### Question 3: Causality

# main data frames
b
b_alt

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


######## Run point-biserial correlation between sex and num_medco / total_num_assign #########
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

#### Check that point biserial corr between 'One-Timer vs Multi-Timer'
#### While we've run correlations between two continuous variables num_medco with Gender/Age
#### What if we CODED num_medco as dichotomous variable? (one-timer vs multi-timer) ??
#### Then check differences in Gender and Age btwn One-Timer vs Multi-Timer

b_alt <- add_column(b_alt, num_medco_fct = NA, .after = 'first_departure')
b_alt$num_medco_fct <- ifelse(b_alt$num_medco==1, 1,2)

## Is there a Relationship Gender and One-Timer vs Multi-Timer?
biserial.cor(b_alt$num_medco_fct,b_alt$sex2, level = 1)
# [1] -0.004597443

## Is there a Relationship btween Age Bracket and One-Timer vs Multi-Timer?
biserial.cor(b_alt$age_bracket, b_alt$num_medco_fct, level = 1)
# [1] 0.07349357


#### Check that point biserial corr between 'consecutive status' is stronger
#### with num_medco and total_num_assign

# add consecutive2 column
b_alt <- add_column(b_alt, consecutive2 = NA, .after = 'consecutive')

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

## Gender: F = 0, M = 1
# gender and num_medco: point biserial corr 0.01904766
ggplot(data = b_alt, mapping = aes(x=sex2, y=num_medco)) + geom_point() + geom_jitter()

# FINAL Gender and Number of Medco - Scatter
gender_num_medco <- ggplot(data = b_alt, mapping = aes(x=sex2, y=num_medco)) 
    + geom_point(aes(col=as.factor(sex2)), position = 'jitter') 
    + theme_classic() 
    + theme(axis.text.x = element_blank()) 
    + scale_color_manual(values = c('#e9222a', '#6c6c6c'), labels = c("Female", "Male"))  
    + labs(title = "Differences between Gender on Number of MedCo Assignments", y = "Number of MedCo Assignments", x = "Gender", color = 'Gender')


# gender and total_num_assign: point biserial corr 0.01596403
ggplot(data = b_alt, mapping = aes(x=sex2, y=total_num_assign)) + geom_point() + geom_jitter()


# Final Gender and Total Number of Assignments - Scatter
gender_total_num_assign <- ggplot(data = b_alt, mapping = aes(x=sex2, y=total_num_assign)) 
    + geom_point(aes(col=as.factor(sex2)), position = 'jitter') 
    + theme_classic() 
    + theme(axis.text.x = element_blank()) 
    + scale_color_manual(values = c('#e9222a', '#6c6c6c'), labels = c("Female", "Male"))  
    + labs(title = "Differences between Gender on Total Number of Assignments", y = "Total Number of Assignments", x = "Gender", color = 'Gender')



#### Age bracket
# NOTE: visual does seem to show that age of first assignment appears to matter

# Age_bracket and num_medco: pearson corr -0.02291633
ggplot(data = b_alt, mapping = aes(x=age_bracket, y=num_medco)) + geom_point() + geom_jitter()


# Final Age Bracket and Number of Medco Assignments
age_bracket_num_medco <- ggplot(data = b_alt, mapping = aes(x=age_bracket, y=num_medco)) 
    + geom_point(aes(col=as.factor(age_bracket)), position = 'jitter') 
    + geom_vline(xintercept = c(30,40,50,60), color = '#6c6c6c', linetype='dotted') 
    + theme_classic() 
    # shades of alizarin red
    + theme(legend.position = 'none') + scale_color_manual(values = c('#F6A4A8', '#F48D92', '#F17177', '#ED4E55', '#E9222A')) 
    + labs(title = 'Differences between Age Bracket and Number of MedCo Assignments', y = 'Number of MedCo Assignments', x = 'Age Brackets')


# Age_bracket and total_num_assign: pearson corr -0.05038205
ggplot(data = b_alt, mapping = aes(x=age_bracket, y=total_num_assign)) + geom_point() + geom_jitter()


# Final Age Bracket and Total Number of Assignments
age_bracket_total_num_assign <- ggplot(data = b_alt, mapping = aes(x=age_bracket, y=total_num_assign)) 
    + geom_point(aes(col=as.factor(age_bracket)), position = 'jitter') 
    + geom_vline(xintercept = c(30,40,50,60), color = '#6c6c6c', linetype='dotted') 
    + theme_classic() 
    + theme(legend.position = 'none') 
    # darker shades of red
    + scale_color_manual(values = c('#E9222A', '#BA1B22', '#95161B', '#771216', '#5F0E12')) 
    + labs(title = 'Differences between Age Bracket and Total Number of Assignments', y = 'Total Number of Assignments', x = 'Age Brackets')


### original point: consecutive status impact num_medco and total_num_assign
# Consecutive_status and Number of MedCo Assignments: point biserial corr 0.655591 (moderately high)
ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) + geom_point() + geom_jitter()

# Final Status (consecutive v non-consecutive) and Number of MedCo Assignments
# horiztonal line = avg num_medco for 'consecutive' status = 1.38
b_alt %>% filter(consecutive2==2) %>% summarize(mean_num_medco = mean(num_medco))
# horizontal line = avg num_medco for 'non-consecutive' status = 4.15
b_alt %>% filter(consecutive2==1) %>% summarize(mean_num_medco = mean(num_medco))


# Final Status and Number of Medco Assignments
status_num_medco <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) 
    + geom_point(aes(col=as.factor(consecutive2)), position = 'jitter') 
    + geom_hline(yintercept = c(1.38, 4.15), color = 'red', linetype='dashed') 
    + theme_classic() 
    + theme(axis.text.x = element_blank()) 
    + scale_color_manual(values = c('grey', 'black'), labels = c('Non-Consecutive', 'Consecutive')) 
    + labs(title = 'Differences in Number of MedCo Between Consecutive vs. Non-Consecutive', color = 'Status', y = 'Number of MedCo Assignments', x = 'Status')

# Alternative White
status_num_medco_alt <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) 
    + geom_point(aes(col=as.factor(consecutive2)), position = 'jitter') 
    + geom_hline(yintercept = c(1.38, 4.15), color = 'white', linetype='dashed', size=1) 
    + theme_classic() 
    + theme(axis.text.x = element_blank(), panel.background = element_rect(fill = '#E9222A', color = '#E9222A')) 
    + scale_color_manual(values = c('grey', 'black'), labels = c('Non-Consecutive', 'Consecutive')) 
    + labs(title = 'Differences in Number of MedCo Between Consecutive vs. Non-Consecutive', color = 'Status', y = 'Number of MedCo Assignments', x = 'Status')


# Alternative Red
status_num_medco_alt2 <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) 
    + geom_point(aes(col=as.factor(consecutive2)), position = 'jitter') 
    + geom_hline(yintercept = c(1.38, 4.15), color = 'white', linetype='dashed', size=1) 
    + theme_classic() 
    + theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(color = 'white'), 
        axis.title.y = element_text(color = 'white'), 
        axis.text.y = element_text(color = 'white'), 
        plot.title = element_text(color = 'white'), 
        panel.background = element_rect(fill = '#E9222A', color = '#E9222A'), 
        panel.border = element_rect(color = '#E9222A', fill = NA), 
        plot.background = element_rect(fill = '#E9222A'), 
        legend.background = element_rect(fill = '#E9222A'), 
        legend.title = element_text(color = 'white'), 
        legend.text = element_text(color = 'white')) 
    + scale_color_manual(values = c('grey', 'black'), labels = c('Non-Consecutive', 'Consecutive')) 
    + labs(title = 'Differences in Number of MedCo Between Consecutive vs. Non-Consecutive', color = 'Status', y = 'Number of MedCo Assignments', x = 'Status')

######

# Consecutive_status and Total Number of Assignments: point biserial corr 0.5021832 (moderate)
ggplot(data = b_alt, mapping = aes(x=consecutive2, y=total_num_assign)) + geom_point() + geom_jitter()

# Final Status (consecutive v non-consecutive) and Total Number of Assignments
# horiztonal line = avg total_num_assign for 'non-consecutive' status = 13.42
b_alt %>% filter(consecutive2==1) %>% summarize(mean_total_num_assign = mean(total_num_assign))
# horiztonal line = avg total_num_assign for 'consecutive' status = 5.90
b_alt %>% filter(consecutive2==2) %>% summarize(mean_total_num_assign = mean(total_num_assign))


# Final Status and Total Number of Assignments
status_total_num_assign <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=total_num_assign)) 
    + geom_point(aes(col=as.factor(consecutive2)), position = 'jitter') 
    + geom_hline(yintercept = c(5.90, 13.42), color = 'red', linetype='dashed') 
    + theme_classic() 
    + theme(axis.text.x = element_blank()) 
    + scale_color_manual(values = c('grey', 'black'), labels = c('Non-Consecutive', 'Consecutive')) 
    + labs(title = 'Differences in Total Number of Assignments Between Consecutive vs. Non-Consecutive', color = 'Status', y = 'Total Number of Assignments', x = 'Status')

# Red and Dark Red
status_total_num_assign1 <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=total_num_assign)) 
    + geom_point(aes(col=as.factor(consecutive2)), position = 'jitter') 
    + geom_hline(yintercept = c(5.90), color = 'black', size=1) 
    + geom_hline(yintercept = c(13.42), color = 'grey', size=1) 
    + theme_classic() 
    + theme(axis.text.x = element_blank()) 
    + scale_color_manual(values = c('#5F0E12', '#E9222A'), labels = c('Non-Consecutive', 'Consecutive')) 
    + labs(title = 'Differences in Total Number of Assignments Between Consecutive vs. Non-Consecutive', color = 'Status', y = 'Total Number of Assignments', x = 'Status')


# Status and Total Number of Assignments (Alternative - RED)
status_total_num_assign_alt <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=total_num_assign)) 
    + geom_point(aes(col=as.factor(consecutive2)), position = 'jitter') 
    + geom_hline(yintercept = c(5.90), color = 'black', size=1) 
    + geom_hline(yintercept = c(13.42), color = 'grey', size=1) 
    + theme_classic() 
    + theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(color = 'white'), 
        axis.title.y = element_text(color = 'white'), 
        axis.text.y = element_text(color = 'white'), 
        plot.title = element_text(color = 'white'), 
        panel.background = element_rect(fill = '#E9222A', color = '#E9222A'), 
        panel.border = element_rect(color = '#E9222A', fill = NA), 
        plot.background = element_rect(fill = '#E9222A'), 
        legend.background = element_rect(fill = '#E9222A'), 
        legend.title = element_text(color = 'white'), 
        legend.text = element_text(color = 'white')) 
    + scale_color_manual(values = c('grey', 'black'), labels = c('Non-Consecutive', 'Consecutive')) 
    + labs(title = 'Differences in Total Number of Assignments Between Consecutive vs. Non-Consecutive', color = 'Status', y = 'Total Number of Assignments', x = 'Status')


# basic scatter plot consecutive vs non-consecutive with color factor by age_bracket
# (may not be suitable IF age DOES have an effect, see age_bracket scatter plot)
ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) 
    + geom_point(aes(col=as.factor(age_bracket)), position = 'jitter') 


# Status Num Medco filled with Age
# to show that all Age brackets are represented in Non-Consecutive and Consecutive groups
status_num_medco_age <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) 
+ geom_point(aes(col=as.factor(age_bracket)), position = 'jitter') 
+ geom_hline(yintercept = c(1.38, 4.15), color = 'black', linetype='dashed') 
+ theme_classic() 
+ theme(axis.text.x = element_blank()) 
+ scale_color_manual(values = c('#ffffcc', '#a1dab4', '#41b6c4', '#2c7fb8', '#253494'), 
    labels = c('25 - 34 yrs', '35 - 44 yrs', '45 - 54 yrs', '55 - 64 yrs', '65 - 74 yrs')) 
+ labs(title = 'Differences in Number of MedCo Between Consecutive vs. Non-Consecutive', 
    color = 'Age Bracket', y = 'Number of MedCo Assignments', x = 'Status') 
+ theme(axis.text.x = element_blank(), 
    axis.title.x = element_text(color = 'white'), 
    axis.title.y = element_text(color = 'white'), 
    axis.text.y = element_text(color = 'white'), 
    plot.title = element_text(color = 'white'), 
    panel.background = element_rect(fill = '#65737e', color = '#65737e'), 
    panel.border = element_rect(color = '#65737e', fill = NA), 
    plot.background = element_rect(fill = '#65737e'), 
    legend.background = element_rect(fill = '#65737e'), 
    legend.title = element_text(color = 'white'), 
    legend.text = element_text(color = 'white'))


# basic scatter plot consecutive vs non-consecutive with color factor by GENDER
ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) 
    + geom_point(aes(col=sex), position = 'jitter') 

status_num_medco_gender <- ggplot(data = b_alt, mapping = aes(x=consecutive2, y=num_medco)) 
    + geom_point(aes(col=sex), position = 'jitter') 
    + geom_hline(yintercept = c(1.38, 4.15), color = 'black', linetype='dashed') 
    + theme_classic() 
    + theme(axis.text.x = element_blank()) 
    + scale_color_manual(values = c('#e41a1c', '#4daf4a'), labels = c('Female', 'Male')) 
    + labs(title = 'Differences in Number of MedCo Between Consecutive vs. Non-Consecutive', color = 'Gender', y = 'Number of MedCo Assignments', x = 'Status') 
    + theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(color = 'white'), 
        axis.title.y = element_text(color = 'white'), 
        axis.text.y = element_text(color = 'white'), 
        plot.title = element_text(color = 'white'), 
        panel.background = element_rect(fill = '#65737e', color = '#65737e'), 
        panel.border = element_rect(color = '#65737e', fill = NA), 
        plot.background = element_rect(fill = '#65737e'), 
        legend.background = element_rect(fill = '#65737e'), 
        legend.title = element_text(color = 'white'), 
        legend.text = element_text(color = 'white'))

##### RE-DO Plot7b
##### NOTE: Original plot7b compares One-Timer vs Multi-Timer on LENGTH of MedCo Assignment
##### Original analysis flawed because there are only 200 multi-timers, not 684

############## Original, Flawed Analysis ##############
## plot7b
## The correct calculation for this graph starts with data frame “a” because we want to account for ALL Medco assignments
## first create diff_date for 'a' using difftime() to find difference between two dates
## a$diff_date <- difftime(a$return_date, a$departure_date, units = c('days'))

# n = 262 (only one-timers)
# a %>%
#    filter(num_medco==1) %>%
#    filter(pool=="MEDCO") %>%
#    summarize(average_length_medco = mean(diff_date), sd = sd(diff_date))

#  average_length_medco       sd
#1        267.6908 days 	263.7288

# n = 684 (staffers go on more than one MedCo)
# a %>%
#    filter(num_medco > 1) %>%
#    filter(pool=="MEDCO") %>%
#    summarize(average_length_medco = mean(diff_date), sd = sd(diff_date))

#  average_length_medco       sd
#1        206.0249 days 	226.0477


###################################

# NEW ANALYSIS plot7b_alt
b_alt <- b
b_alt[,'diff_date'] <- NA
b_alt$diff_date <- difftime(b_alt$return_date, b_alt$departure_date, units = c('days'))

# Average Length Medco Assignement for ONE-Timer (n = 262)
b_alt %>% filter(num_medco==1) %>% filter(pool=='MEDCO') %>% summarize(average_length_medco = mean(diff_date), sd = sd(diff_date))
  average_length_medco       sd
1        331.0909 days      359.4093

# Average Length Medco Assignment for MULTI-Timer (n = 200)
 b_alt %>% filter(num_medco > 1) %>% filter(pool=='MEDCO') %>% summarize(average_length_medco = mean(diff_date), sd = sd(diff_date))
  average_length_medco       sd
1        265.0741 days      220.6975

# create two dataframe, then use rbind() as precursor

one_medco_b_alt <- data.frame(type = "one_medco", diff_date = rnorm(n=262, mean = 331.0909, sd = 359.4093))
more_than_one_medco_b_alt <- data.frame(type = "more_than_one_medco", diff_date = rnorm(n=200, mean = 265.0741, sd = 220.6975))
overlap2_df_b_alt <- rbind(one_medco_b_alt, more_than_one_medco_b_alt)

# NEW plot7b
new_plot7b <- ggplot(data = overlap2_df_b_alt, mapping = aes(x=diff_date, fill=type)) 
    + geom_histogram(alpha = .8, binwidth = 25, position = "identity") 
    + theme_classic() 
    + scale_fill_manual(labels=c("One", "More than one"), values = c("#e9222a", "#6c6c6c")) 
    + xlim(0,1000) 
    + ylim(0,20) 
    + labs(x = "Length of MedCo Assignments", y = "Number of People", title = "Difference in Average Length of MedCo Assignments", fill = "Number of MedCo")

# t.test difference for NEW plot7b
t.test(diff_date ~ type, data = overlap2_df_b_alt)

# True difference in means is not equal to 0
# 95 percent confidence interval:
#   8.191753 109.060236

# NEW ANALYSIS plot6b

new_consecutive_set_b <- data.frame(type = "consecutive", total_num_assign = rnorm(n=88, mean = 5.902857, sd = 4.851579))
new_non_consecutive_set_b <- data.frame(type = "non-consecutive", total_num_assign = rnorm(n=112, mean = 13.41964, sd = 7.352928))
new_overlap_df_b <- rbind(new_consecutive_set_b, new_non_consecutive_set_b)

# t-test (YES, Significantly different)
# 95 percent confidence interval:
# -8.980970 -5.386363

# NEW plot6b
new_plot6b <- ggplot(data = new_overlap_df_b, mapping = aes(x=total_num_assign, fill=type)) 
    + geom_histogram(alpha = .8, binwidth = 1, position = "identity") 
    + theme_classic() 
    + scale_fill_manual(labels=c("Consecutive", "Non-Consecutive (Gap)"), values = c("#EE0000", "#000000")) 
    + xlim(0,30) 
    + labs(x = "Number of Assignments", y = "Number of People", title = "Differences in Number of (Total) Assignments", fill = "MedCo Pattern")

