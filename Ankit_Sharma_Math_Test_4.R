library(readxl)
library(psych)
library(exact2x2)

df = as.data.frame(read_excel("C:/Users/ankit/OneDrive - St. Lawrence College/Summer 2023/MATH5001- Introduction to Statistical Analysis 1/swim-Q.xlsx", sheet = "Data"))
df

#Question 1, On average, did female swimmers have higher training costs than male swimmers in 2019?
df1 = df
# H0:μ1 femalecost <= μ1 malecost
# H1:μ1femalecost  > μ1 malecost

# make a group for male and female 

femalecost = df$COST2019[df2$SEX == "F"]
malecost = df1$COST2019[df2$SEX == "M"]

# outliers

outfemale = boxplot(femalecost)$out # no outliers in female
outmale = boxplot(malecost)$out # outlier in male 

# removing outliers 

malecost = malecost[!(malecost %in% outmale)]
boxplot(malecost)

# histogram
hist(femalecost)
hist(malecost)

# checking the normality
shapiro.test(femalecost)  # normality assumption is met
shapiro.test(malecost)  

#  homogeneity Check of variance
var.test(femalecost, malecost, alternative="two.sided")  # the homogeneity of variance assumption is met.


#test

t.test(femalecost, malecost, paired = FALSE, var.equal = TRUE, alternative = "greater" )



#Question 2 -  Were male and female swimmers equally likely to sustain a serious injury in 2014?

df2 = df

# H0:male = female

# H1:male != female

t = table(df2$SEX,df2$INJURY2014)
t

n = rowSums(t)
n
# sample size less than 50
# will use fisher test

prop.table(t,1)


fisher.exact(t, alternative = "two.sided")


#Question 3 

df3 = df

# H0 : There is no association 
# Ha : There is an association

# contingency table

injury_table = table(df3$COUNTRY, df3$INJURY2019)


# Perform chi-squared test fir independence

chisq.test(injury_table)



#Question 4
#Did the average training cost for Canadian swimmers increase from 2014 to 2019?

df4 = df
# H0: μd <= 0
# H1: μd > 0

# data frame Canadian swimmers

df4 = subset(df4, df4$COUNTRY=="CA")

#difference i.e.change in training costs
Cost_change = df4$costChange
Cost_2019 = df4$COST2019
Cost_2014 = df4$COST2014

Cost_change = Cost_2019 - Cost_2014
Cost_change


# outlier in change in training cost

outcostchange = boxplot(Cost_change)$out  # no outlier is found in the dataset 


hist(Cost_change)

#normality

shapiro.test(Cost_change)  #normality assumption is met

describe(Cost_2019)
describe(Cost_2014)
summary(Cost_2019)
summary(Cost_2014)

t.test(Cost_2019, Cost_2014, paired=TRUE, alternative ="greater")


#through chatgpt, because i am not able to figure out why describe funtion is not working :)

# Calculate standard deviation for Cost_2019 and Cost_2014
sd_cost_2019 <- sd(Cost_2019)
sd_cost_2014 <- sd(Cost_2014)

# Display the standard deviations
cat("Standard Deviation for Cost_2019:", sd_cost_2019, "\n")
cat("Standard Deviation for Cost_2014:", sd_cost_2014, "\n")


