# SERGIO ELOLA GARCÍA AND OLGA GONZÁLEZ FARIÑAS

# Read the data file as a data.frame:
Datos = read.csv("Datos.csv",sep = ",")
attach(Datos)
names(Datos)

# QUALITATIVE VARIABLES: GENDER and REASON

## GENDER

# Absolute frequency table:
table(GENDER)
# Sample size
n = length(GENDER)
# Relative frequency table: 57% are female and 43% are male
table(GENDER)/n
# Bar plots:
barplot(table(GENDER))
# Pie chart
pie(table(GENDER))
 
## REASON

# Absolute frequency table:
table(REASON)
# Sample size
n = length(REASON)
# Relative frequency table:
table(REASON)/n
# Bar plots:
barplot(table(REASON))
# Pie chart
pie(table(REASON))


# DISCRETE VARIABLES: AGE
# Absolute and relative frequency table:
table(AGE)
n = length(AGE)
table(AGE)/n
# Absolute cumulative frequency table:
cumsum(table(AGE))
# Relative cumulative frequency table:
cumsum(table(AGE)/n)
# Bar plots
barplot(table(AGE))
barplot(cumsum(table(AGE)))
# Empirical Cumulative Distribution Function (ECDF): 
F=ecdf(AGE)
plot(ecdf(AGE))

# CONTINUOUS VARIABLES: PRICE and MONTHS

## PRICE

# Absolute frequency histogram
hist(PRICE.EUROS)
# Breakpoints between classes
hist(PRICE.EUROS)$breaks
# Absolute and relative frequencies
hist(PRICE.EUROS)$counts
n = length(PRICE.EUROS)
hist(PRICE.EUROS)$counts/n
# Cumulative absolute and relative frequencies
cumsum(hist(PRICE.EUROS)$counts)
cumsum(hist(PRICE.EUROS)$counts/n)
# Histogram of a total area of one
hist(PRICE.EUROS, freq = FALSE)
# Changing the number of classes:
hist(PRICE.EUROS, freq = FALSE, nclass = 8)
hist(PRICE.EUROS, freq = FALSE, nclass = 3)
# Empirical Cumulative Distribution Function (ECDF):
plot(ecdf(PRICE.EUROS))

## TIME.MONTHS

# Absolute frequency histogram
hist(TIME.MONTHS)
# Breakpoints between classes
hist(TIME.MONTHS)$breaks
# Absolute and relative frequencies
hist(TIME.MONTHS)$counts
n = length(TIME.MONTHS)
hist(TIME.MONTHS)$counts/n
# Cumulative absolute and relative frequencies
cumsum(hist(TIME.MONTHS)$counts)
cumsum(hist(TIME.MONTHS)$counts/n)
# Histogram of a total area of one
hist(TIME.MONTHS, freq = FALSE)
# Changing the number of classes:
hist(TIME.MONTHS, freq = FALSE, nclass = 50)
hist(TIME.MONTHS, freq = FALSE, nclass = 3)
# Empirical Cumulative Distribution Function (ECDF):
plot(ecdf(TIME.MONTHS))

# DESCRIPTIVE STADISTICS

## TIME.MONTHS
mean(TIME.MONTHS)
median(TIME.MONTHS)
boxplot(TIME.MONTHS) # WE HAVE OUTLIERS

min(TIME.MONTHS)
max(TIME.MONTHS)
max(TIME.MONTHS) - min(TIME.MONTHS) # RANGE
Q1 = quantile(TIME.MONTHS,probs = 0.25)
Q2 = quantile(TIME.MONTHS,probs = 0.50)
Q3 = quantile(TIME.MONTHS,probs = 0.75)
Q1-1.5*(Q3-Q1) # Lower and upper limits
Q3+1.5*(Q3-Q1)  # WE HAVE OUTLIERS

var(TIME.MONTHS)
sd(TIME.MONTHS)
# Coefficient of variation
sd(TIME.MONTHS)/mean(TIME.MONTHS)
# Median absolute deviation
mad(TIME.MONTHS)
median(abs(TIME.MONTHS-median(TIME.MONTHS)))
# Skewness coefficient
library(e1071) 
skewness(TIME.MONTHS)
##
## PRICE.EUROS
mean(PRICE.EUROS) # It is very sensitive to outliers
median(PRICE.EUROS)
boxplot(PRICE.EUROS) # WE HAVE OUTLIERS

min(PRICE.EUROS)
max(PRICE.EUROS)
max(PRICE.EUROS) - min(PRICE.EUROS) #RANGE
Q1 = quantile(PRICE.EUROS,probs = 0.25)
Q2 = quantile(PRICE.EUROS,probs = 0.50)
Q3 = quantile(PRICE.EUROS,probs = 0.75)
Q1-1.5*(Q3-Q1) # Lower and upper limits
Q3+1.5*(Q3-Q1) # WE HAVE OUTLIERS

var(PRICE.EUROS)
sd(PRICE.EUROS)

# Coefficient of variation
sd(PRICE.EUROS)/mean(PRICE.EUROS)
# Median absolute deviation
mad(PRICE.EUROS)
median(abs(PRICE.EUROS-median(PRICE.EUROS)))
# Skewness coefficient
library(e1071) 
skewness(PRICE.EUROS)
##
## AGE
mean(AGE)
median(AGE)
boxplot(AGE)

min(AGE)
max(AGE)
max(AGE) - min(AGE) #RANGE
Q1 = quantile(AGE,probs = 0.25)
Q2 = quantile(AGE,probs = 0.50)
Q3 = quantile(AGE,probs = 0.75)
Q1-1.5*(Q3-Q1) # Lower and upper limits
Q3+1.5*(Q3-Q1)

var(AGE)
sd(AGE)

# Coefficient of variation
sd(AGE)/mean(AGE)

# Median absolute deviation
mad(AGE)
median(abs(AGE-median(AGE)))
# Skewness coefficient
library(e1071) 
skewness(AGE) 


# TWO QUALITATIVE VARIABLES: GENDER AND REASON

# Absolute joint frequency table
table(REASON,GENDER)
# Relative joint frequency table
n = length(REASON)
table(REASON,GENDER)/n
# Marginal absolute frequency tables
table(REASON)
table(GENDER)
# Marginal relative frequency tables
table(REASON)/n
table(GENDER)/n
# Conditional frequencies of REASON for MEN
table(REASON[GENDER=="Male"])
nM=sum(GENDER=="Male")
table(REASON[GENDER=="Male"])/nM
# Conditional frequencies of GENDER for people which REASON = 0
table(GENDER[REASON=="0"])
nS=sum(REASON=="0")
table(GENDER[REASON=="0"])/nS
# Group bar plots
tableNS = table(REASON,GENDER)
barplot(tableNS,beside=TRUE,legend = rownames(tableNS))
tableSN = table(GENDER,REASON)
barplot(tableSN,beside=TRUE,legend = rownames(tableSN))

# ONE QUANTITATIVE VARIABLE AND ONE QUALITATIVE VARIABLE.

# Months - Gender (No differences, maybe the women's phone last a little bit longer)
# Boxplots of TIME.MONTHS for each gender.
boxplot(TIME.MONTHS ~ GENDER)
# Histogram of TIME.MONTHS for each gender.
par(mfrow=c(2,1))
hist(TIME.MONTHS [GENDER =="Male"])
hist(TIME.MONTHS [GENDER =="Female"])
par(mfrow=c(1,1))
# Summary statistics comparison
summary(TIME.MONTHS[GENDER=="Male"])
summary(TIME.MONTHS[GENDER=="Female"])

# Price - Gender  (No differences)
boxplot(PRICE.EUROS ~ GENDER)
# Histogram
par(mfrow=c(2,1))
hist(PRICE.EUROS [GENDER =="Male"])
hist(PRICE.EUROS [GENDER =="Female"])
par(mfrow=c(1,1))
# Summary statistics comparison
summary(PRICE.EUROS[GENDER=="Male"])
summary(PRICE.EUROS[GENDER=="Female"])

# Age - Reason (maybe older people change their phones because of reason 1 more than young people)

boxplot(AGE ~ REASON)
# Histogram
par(mfrow=c(2,1))
hist(AGE [REASON =="0"])
hist(AGE [REASON =="1"])
par(mfrow=c(1,1))
# Summary statistics comparison
summary(AGE[REASON=="0"])
summary(AGE[REASON=="1"])

# Months - Reason (yes, we'll analyze this relation in the report)

boxplot(TIME.MONTHS ~ REASON)
# Histogram
par(mfrow=c(2,1))
hist(TIME.MONTHS [REASON =="0"])
hist(TIME.MONTHS [REASON =="1"])
par(mfrow=c(1,1))
# Summary statistics comparison
summary(TIME.MONTHS[REASON=="0"])
summary(TIME.MONTHS[REASON=="1"])


# Price - Reason (yes, we'll analyze this relation in the report)

boxplot(PRICE.EUROS ~ REASON)
# Histogram
par(mfrow=c(2,1))
hist(PRICE.EUROS [REASON =="0"])
hist(PRICE.EUROS [REASON =="1"])
par(mfrow=c(1,1))
# Summary statistics comparison
summary(PRICE.EUROS[REASON=="0"])
summary(PRICE.EUROS[REASON=="1"])

# TWO QUANTITATIVE VARIABLES

# Scatter plot
plot(PRICE.EUROS,TIME.MONTHS)
cov(PRICE.EUROS,TIME.MONTHS)
cor(PRICE.EUROS,TIME.MONTHS)
plot(PRICE.EUROS,AGE)
cov(PRICE.EUROS,AGE)
cor(PRICE.EUROS,AGE)
plot(AGE,TIME.MONTHS)
cov(AGE,TIME.MONTHS)
cor(AGE,TIME.MONTHS)

#INFERENCE ANALYSIS

# Load the library to fit distribution models
library(fitdistrplus)

# Fit a Gaussian for PRICE.EUROS using the method of moments

fitdist(PRICE.EUROS,"norm",method="mme")
fit = fitdist(PRICE.EUROS,"norm",method="mme")
fit$estimate
hist(PRICE.EUROS, freq = FALSE, nclass = 8)
grid=seq(1,1000,1)
lines(grid,dnorm(grid,fit$estimate[1],fit$estimate[2]))

# Fit a Gamma for PRICE.EUROS using the method of moments

fit2 = fitdist(PRICE.EUROS,"gamma",method="mme")
fit2$estimate
hist(PRICE.EUROS, freq = FALSE, nclass = 8)
grid=seq(1,1000,1)
lines(grid,dgamma(grid,fit2$estimate[1],fit2$estimate[2]))

# Fit an Exponential for PRICE.EUROS using the method of moments

fit3 = fitdist(PRICE.EUROS,"exp",method="mme")
fit3$estimate
hist(PRICE.EUROS, freq = FALSE, nclass = 8)
grid=seq(1,1000,1)
lines(grid,dexp(grid,fit3$estimate[1]),col="blue")

# All distributions together
hist(PRICE.EUROS, freq = FALSE, nclass = 8)
lines(grid,dnorm(grid,fit$estimate[1],fit$estimate[2]))
lines(grid,dgamma(grid,fit2$estimate[1],fit2$estimate[2]),col="red")
lines(grid,dexp(grid,fit3$estimate[1]),col="blue")

# Compare Gaussian, Gamma and Exp ecdf with empirical cdf
plot(ecdf(PRICE.EUROS))
lines(grid,pnorm(grid,fit$estimate[1],fit$estimate[2]))
lines(grid,pgamma(grid,fit2$estimate[1],fit2$estimate[2]),col="red")
lines(grid,pexp(grid,fit3$estimate[1]),col="blue")

# Fit a Gaussian, Gamma and Exponential for PRICE.EUROS using MLE

fitmle_n = fitdist(PRICE.EUROS,"norm",method="mle")
fitmle_g = fitdist(PRICE.EUROS,"gamma",method="mle")
fitmle_e = fitdist(PRICE.EUROS,"exp",method="mle")
hist(PRICE.EUROS, freq = FALSE, nclass = 8)
lines(grid,dnorm(grid,fitmle_n$estimate[1],fitmle_n$estimate[2]))
lines(grid,dgamma(grid,fitmle_g$estimate[1],fitmle_g$estimate[2]),col="red")
lines(grid,dexp(grid,fitmle_e$estimate[1]),col="blue")

plot(ecdf(PRICE.EUROS))
lines(grid,pnorm(grid,fitmle_n$estimate[1],fitmle_n$estimate[2]))
lines(grid,pgamma(grid,fitmle_g$estimate[1],fitmle_g$estimate[2]),col="red")
lines(grid,pexp(grid,fitmle_e$estimate[1]),col="blue")

# Model comparison using AIC
fitmle_n$aic     # maximum likelihood normal
fit$aic  # moments normal
fitmle_g$aic     # maximum likelihood gamma   ("better" fit)
fit2$aic  # moments gamma
fitmle_e$aic     # maximum likelihood exponential
fit3$aic # moments exponential

# Central Limit Theorem to obtain the confidence interval
# confidence interval 95% for the mean of PRICE.EUROS (population mean)
t.test(PRICE.EUROS)
# predictive interval for PRICE.EUROS will be do a qgamma(0.025...), qgamma(0.975....)

# H_0: mean = 272???
# H_1: mean < 272???

t.test(PRICE.EUROS, mu=272, alternative = "l")

# No stadistical evidences about that H_1 is true

# Confidence intervals 95% for the mean of TIME.MONTHS and AGE (population mean)
t.test(TIME.MONTHS)
t.test(AGE)

# H_0: mean = 20.5
# H_1: mean > 20.5

t.test(TIME.MONTHS, mu=20.5, alternative = "greater")

# Inference two variables

# Chi-Test
chisq.test(GENDER, REASON)
chisq.test(GENDER, REASON)$observed
chisq.test(GENDER, REASON)$expected


Chi = chisq.test(GENDER,REASON)$statistic
N = length(GENDER)
L = min(length(unique(GENDER)),length(unique(REASON)))
V = sqrt(Chi/(N*(L-1)))
V

# Dependency 1 quantitative and 1 qualitative with two groups 
Y1 = PRICE.EUROS[REASON=="0"]
Y2 = PRICE.EUROS[REASON=="1"]
boxplot(Y1,Y2)    # "descriptive analysis" looking the boxplot it seems that  
# the variances are different

# As we have seen before, Price.Euros doesn't follow a Guassian distribution
# but as the sample sizes are large, we can relax the Gaussian assumption thanks to the CLT (Central Limit Theorem)

#H_0: var_reason0 = var_reason1
#H_1: var_reason0 != var_reason1
var.test(Y1,Y2)
# p-valor too small => H_1 proved, we have found statistical evidences about that H_1 is true

#H_0: mu_reason0 = mu_reason1
#H_1: mu_reason0 != mu_reason1
t.test(Y1,Y2,var.equal = F)   # test for checking if their mean is the same, taking into account that the variances are different
# p-valor too small => H_1 proved, we have found statistical evidences about that H_1 is true


#H_0: mu_reason0 = mu_reason1
#H_1: mu_reason0 < mu_reason1
t.test(Y1,Y2,var.equal = F, alternative = "less")


# Dependence Analysis
# H_0: p=0
# H_1: p!=0
plot(PRICE.EUROS,TIME.MONTHS)
cor.test(PRICE.EUROS,TIME.MONTHS)

lm(TIME.MONTHS~PRICE.EUROS)
abline(lm(TIME.MONTHS~PRICE.EUROS))

