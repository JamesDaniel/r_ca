library(readr)
# Exercise 1
# a)
setwd("~/Desktop")
cardiology <-
  read_csv("C:/Users/t00175569/Desktop/r_ca/cardiology.csv")
# b)
View(cardiology)

# c)
hist(cardiology$age,
     xlab = "Age Groups (5yr bins)",
     main = "Patient Ages",
     ylab = "Number of Patients")

# d)
cardiology.sick <- subset(cardiology, cardiology$class == "Sick")
cardiology.healthy <-
  subset(cardiology, cardiology$class == "Healthy")
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


boxplot(
  cardiology.sick$age,
  cardiology.healthy$age,
  main = "Patients BoxPlot",
  ylab = "Age",
  names = c("Sick", "Healthy")
)

# f)

hist(
  cardiology.healthy$age,
  xlab = "Age Groups (5yr bins)",
  main = "Healthy Patient Ages",
  ylab = "Number of Patients"
)

hist(cardiology.sick$age,
     xlab = "Age Groups (5yr bins)",
     main = "Sick Patient Ages",
     ylab = "Number of Patients")

# The sicker patients are older and that the healthy patients had
# a larger IQR indicating that greater age spread of patients (variance)

# g)

# i) 
x <- cardiology$age
y <- cardiology$`blood-pressure`
relation <- lm(y ~ x)
plot(x,
     y,
     xlab = "Age",
     ylab = "Blood pressure",
     abline(relation, col = "red"))
# ii)
cor(x, y)
# from https://www.andrews.edu/~calkins/math/edrm611/edrm05.htm
# Correlation coefficients whose magnitude are 
# between 0.9 and 1.0 indicate variables which can be considered very highly correlated
# between 0.7 and 0.9 indicate variables which can be considered highly correlated. 
# between 0.5 and 0.7 indicate variables which can be considered moderately correlated. 
# between 0.3 and 0.5 indicate variables which have a low correlation
# 
# the low slope and high spread of data points indicate low correlation between age and blood pressure


## Exercise 2

#1)
cardiology.factors <-
  read.csv("C:/Users/t00175569/Desktop/r_ca/cardiology.csv", stringsAsFactors = T)

View(cardiology.factors)
kc <- kmeans(cardiology.factors[,-14],2)

kc <- kmeans(cardiology.factors[,c(1,4,5,8,10,12)],2)
kc
x <- cardiology.factors[,c(1,4)]
plot(x[c("age","blood.pressure")],
     col = kc$cluster)
cardiology.factors$
class(cardiology.factors$sex)
plot(x[c("age","blood.pressure")], col = kc$cluster)
##TODO -COME BACK TO THIS!!!


# 2)

install.packages("party")
library(party)
output <- ctree(cardiology.factors$class ~ age + sex,
                data = cardiology.factors)
plot(output)


#try this
df <- cardiology[-14]
View(df)
set.seed(11)

library(rpart)
dtree <- rpart(class ~ .,
               data = cardiology.factors,
               method = "class",
               parms = list(split = "information")
               )
dtree$cptable
plotcp(dtree)

dtree.pruned <- prune(dtree, cp = 0.399)

plotcp(dtree.pruned)
install.packages('rpart.plot')
library(rpart.plot)
cardiology.factors$class <- factor(cardiology.factors$class)
prp(dtree.pruned,
    type = 2,
    extra = 104)
