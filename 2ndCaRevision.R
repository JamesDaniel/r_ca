
# Exercise 1
# a)
library(readr)
setwd("~/Desktop")
cardiology <- read_csv("C:/Users/Finbar/Desktop/r_ca/cardiology.csv")
# b)
View(cardiology)

# c)
hist(cardiology$age,
     xlab = "Age Groups (5yr bins)",
     main = "Patient Ages",
     ylab = "Number of Patients")

# d)
cardiology.sick <- subset(cardiology, cardiology$class == "Sick")
cardiology.healthy <- subset(cardiology, cardiology$class == "Healthy")
summary(cardiology.sick$age)
summary(cardiology.healthy$age)
sd(cardiology.healthy$age)
sd(cardiology.sick$age)

# thisis how to find mode bitches
install.packages('modeest')
library(modeest)
mfv(cardiology.healthy$age, method = "mfv")

# e)
boxplot(cardiology.healthy$age,
        main = "Healthy Patients BoxPlot",
        ylab = "Age")

boxplot(cardiology.sick$age,
        main = "Sick Patients BoxPlot",
        ylab = "Age")


boxplot(cardiology.sick$age,
        cardiology.healthy$age,
        main = "Patients BoxPlot",
        ylab = "Age",
        names = c("Sick", "Healthy"))

# f)

hist(cardiology.healthy$age,
     xlab = "Age Groups (5yr bins)",
     main = "Healthy Patient Ages",
     ylab = "Number of Patients",
     ylim = c(0, 25))

hist(cardiology.sick$age,
     xlab = "Age Groups (5yr bins)",
     main = "Sick Patient Ages",
     ylab = "Number of Patients")

# looking at the historgram, we see that sick patients are generally older
#  the IQR for healthy people is larger indicating a larger variance in patient age

# G)