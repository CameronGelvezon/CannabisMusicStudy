### Cannabis Habits and Music Listening: Cameron Gelvezon


# Get Libraries
library('psych')
library('arm')
library('boot')



## Part 1


# Load the Data
muca <- read.csv('~/Desktop/PSYCH 101/Final Project/music_cannabis.csv',
                 stringsAsFactors=TRUE)

head(muca) #First 6 Rows

nrow(muca) #Num Individuals

names(muca) #Var Names


# Statistics for Each Var

describe(muca$age) #age Stats

summary(muca$sex) #sex Stats

describe(muca$lsn) #lsn Stats

describe(muca$csm) #csm Stats

summary(muca$prm) #prm Stats


# Models of Vars
hist(muca$age, main = 'Frequency of Age', xlab = 'Age', ylab = 'Freq')

plot(muca$sex, main = 'Gender of Participants')

hist(muca$lsn, main = 'Frequency of Time Spent Listening to Music',
     xlab = 'Time Spent Listening to Music (hrs/week)', ylab = 'Freq')

hist(muca$csm, main = 'Frequency of Cannabis Consumed',
     xlab = 'Cannabis Consumed (days/week)', ylab = 'Freq')

plot(muca$prm, main = 'Cannabis Consumption Method')


# Clean the Data
muca$prm <- as.numeric(muca$prm) - 1 #Change prm to binary


# Standardized Linear Models

mod1 <- lm(scale(lsn) ~ scale(csm), data = muca) #Model 1

mod2 <- lm(scale(lsn) ~ scale(prm), data = muca) #Model 2

mod3 <- lm(scale(lsn) ~ scale(csm) + scale(prm), data = muca) #Model 3


# Summaries of Models

#Model 1

round(coef(mod1), 4) #Slope and Intercept

round(summary(mod1)$coefficients[,3], 4) #T-Value

round(summary(mod1)$coefficients[,4], 4) #P-Value

round(summary(mod1)$r.squared, 4) #R-Squared

round(summary(mod1)$r.squared^0.5, 4) #Correlation

round(summary(mod1)$fstatistic,4 ) #F-Stat

#Model 2

round(coef(mod2), 4) #Slope and Intercept

round(summary(mod2)$coefficients[,3], 4) #T-Value

round(summary(mod2)$coefficients[,4], 4) #P-Value

round(summary(mod2)$r.squared, 4) #R-Squared

round(summary(mod2)$r.squared^0.5, 4) #Correlation

round(summary(mod2)$fstatistic, 4) #F-Stat

#Model 3

round(coef(mod3), 4) #Slope and Intercept

round(summary(mod3)$coefficients[,3], 4) #T-Value

round(summary(mod3)$coefficients[,4], 4) #P-Value

round(summary(mod3)$r.squared, 4) #R-Squared

round(summary(mod3)$r.squared^0.5, 4) #Correlation

round(summary(mod3)$fstatistic, 4) #F-Stat


# Plot the Models

#Model 1
plot(scale(lsn) ~ scale(csm), data = muca,
     main = 'Time Spent Listening to Music vs Cannabis Consumed',
     xlab = 'Cannabis Consumed (z)',
     ylab = 'Time Spent Listening to Music (z)')
abline(mod1, lwd = 3, col = 'purple') #Line of Best Fit

#Model 2
plot(scale(lsn) ~ scale(prm), data = muca,
     main = 'Time Spent Listening to Music vs Smoker',
     xlab = 'Smoker (z)',
     ylab = 'Time Spent Listening to Music (z)')
abline(mod2, lwd = 3, col = 'purple') #Line of Best Fit


## Part 2


# Bootstrapping
bucket1 <- array()

bucket2 <- array()

bucket3 <- array()

for(i in c(1:1000)){
  boot.muca <- muca[sample(1:nrow(muca), nrow(muca), replace = T),]
  boot.mod1 <- lm(scale(lsn) ~ scale(csm), data = boot.muca)
  boot.mod2 <- lm(scale(lsn) ~ scale(prm), data = boot.muca)
  boot.mod3 <- lm(scale(lsn) ~ scale(csm) + scale(prm), data = boot.muca)
  bucket1[i] <- coef(boot.mod1)[2]
  bucket2[i] <- coef(boot.mod2)[2]
  bucket3[i] <- coef(boot.mod3)[2]
}


# Histogram of Distributions

hist(bucket1) #Model 1

hist(bucket2) #Model 2

hist(bucket3) #Model 3


# Percent of Slopes in Same Direction

(sum(bucket1 > 0)/length(bucket1)) * 100 #Model 1

(sum(bucket2 > 0)/length(bucket2)) * 100 #Model 2

(sum(bucket3 < 0)/length(bucket3)) * 100 #Model 3


# Sampling Errors of Dist

sd(bucket1) #Model 1

sd(bucket2) #Model 2

sd(bucket3) #Model 3


# 95% CI

#Model 1

round(coef(mod1)[2] - (1.96 * sd(bucket1)), 4) #Lower csm Bound

round(coef(mod1)[2] + (1.96 * sd(bucket1)), 4) #Upper csm Bound

#Model 2

round(coef(mod2)[2] - (1.96 * sd(bucket2)), 4) #Lower prm Bound

round(coef(mod2)[2] + (1.96 * sd(bucket2)), 4) #Upper prm Bound

#Model 3

round(coef(mod3)[2] - (1.96 * sd(bucket3)), 4) #Lower csm Bound

round(coef(mod3)[2] + (1.96 * sd(bucket3)), 4) #Upper csm Bound

round(coef(mod3)[3] - (1.96 * sd(bucket3)), 4) #Lower prm Bound

round(coef(mod3)[3] + (1.96 * sd(bucket3)), 4) #Upper prm Bound
