library(readxl)
dfHR <- as.data.frame(read_excel("C:/Users/ankit/OneDrive/Desktop/eoyHR.xlsx", sheet = "Data"))



#Question 2, Blanks
df = dfHR

ppOut<-boxplot(df$BeneCurr)$out
shapiro.test(df$BeneCurr)

wilcox.test(df$BeneCurr, mu = 3000, alternative="two.sided")

#Question 3

df2 = dfHR
View(df2)

sample_mean = mean(df2$BeneCurr)
sample_mean

sample_sd = sd(df2$BeneCurr)
sample_sd

n = length(df2$BeneCurr)
n
confidence_level = 0.9
alpha = 1 - 0.95 # 1 - confidence level
dif = n - 1
tcrit = qt(alpha/2, dif, lower.tail=FALSE) # population SD is unknown, use t distribution for critical value
ci_min = sample_mean - tcrit*sample_mean/sqrt(n) # CI lower bound
ci_min


#Question 5
df3 = dfHR
df3 = subset(dfHR, City %in% 2)
View(df3)

pdCounts<-table(df3$Education)
pdCounts
n = sum(pdCounts)
n
x = pdCounts["Secondary"]
x



test_result <- binom.test(x, n, p = 0.40, alternative = "two.sided")

# Extract the p-value from the test result
p_value <- test_result$p.value

# Print the p-value
print(p_value)





#Question6
df4<-dfHR

# Check for outliers
ppOut<-boxplot(df$PerfPrev)$out
# 2 outliers identified, remove outliers
df5<-df4[!(df$PerfPrev %in% ppOut),]

# Re-check for outliers
boxplot(df5$PerfPrev)


shapiro.test(df5$PerfPrev)
# Normality assumption is met
library (psych)

# Descriptive statistics
describe(df5$PerfPrev)
# Perform 2-tailed test
t.test(df5$PerfPrev, mu = 7.15, alternative="greater")



#Question 7
df6 = dfHR
df6 = subset(dfHR, City %in% c(1, 2))
df7 = subset(df6, City %in% c(1, 2))
View(df7)
pdCounts<-table(df7$ProfDev) 
pdCounts

n = sum(pdCounts)
n  # sample size < 50, use binom test
x = pdCounts["Yes"] # x = number of successes = number of Yes 
x

prop.test(x,n,p = 0.15, alternative="less", correct=TRUE) 


#Question 9
Z <- qnorm(0.995) # For a 99% confidence level
sigma <- 12       # Population standard deviation
E <- 5            # Desired margin of error

# Calculate the required sample size
n <- ceiling((Z^2 * sigma^2) / E^2)

# Print the result
print(n)