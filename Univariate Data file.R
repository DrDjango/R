library(readxl)
dfHR <- as.data.frame(read_excel("C:/Users/ankit/Downloads/eoyHR.xlsx", sheet = "Data"))
head(dfHR)
View(dfHR)
#1 ACME's CEO boasts that they have one of the highest paid sales force in the nation.  He claims that average sales representative is paid over $28 per hour.  Test this claim at a 10% level of significance.

myDF = dfHR
hist(mydf$Income)
incomeOutliers <- boxplot(myDF$Income)$out
myDF2 <- myDF[!(myDF$Income %in% incomeOutliers),]
boxplot(myDF2$Income)

H0: mu <= 28
H1: mu > 28



myDF3 = dfHR
hist(myDF3$HrPay)
incomeOutliers = boxplot(myDF$HrPay)$out
myDF3 = myDF[!(myDF$HrPay %in% incomeOutliers),]
boxplot(myDF3$HrPay)

install.packages("psych")
library(psych)
describe(myDF3$HrPay)

shapiro.test(myDF3$HrPay)
wilcox.test(myDF3$HrPay, mu = 28, alternative = "greater" )

t.test(myDF3$HrPay, mu = 28, alternative="greater")


#






