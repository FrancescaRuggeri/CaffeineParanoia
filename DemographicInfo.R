
setwd("~/Desktop/CaffeineData/")
library(tidyverse)
data <- read_csv("CaffeineHabits.csv")
CH2 <- read_csv("CaffeineHabits2.csv")
Dictator <- read_csv("Dictator.csv")



DATA <- plyr::join(data, CH2, by = 'ID')

master <- plyr::join(DATA, Dictator, by = 'ID')

write_csv(master, "master.csv")

head(data)

table(master$Treatment, master$Caffeine)


#LastCaff_Quant
data$LastCaff_Quant <- as.factor(data$LastCaff_Quant)
class(data$LastCaff_Quant)

(t <- table(master1$LastCaff_Quant, exclude = F))
round(prop.table(t)*100, 2)

(t <- table(master1$LastCaff_Quant[master1$Caffeine == "A"], exclude = F))
round(prop.table(t)*100, 2)

(t <- table(master1$LastCaff_Quant[master1$Caffeine == "B"], exclude = F))
round(prop.table(t)*100, 2)


#Coffee_day
median(data$Coffee_day)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

Mode(data$Coffee_day)
#Tea_day
data$Tea_day <- as.numeric(data$Tea_day)
class(data$Tea_day)

median(data$Tea_day, exclude = F)
Mode(data$Tea_day)

#OtherCaff_quant
data$OtherCaff_Quant <- as.factor(data$OtherCaff_Quant)
(t <- table(data$OtherCaff_Quant, exclude = F))
round(prop.table(t)*100, 2)


summary(master$ResponseHI)

#Gender
summary(master1$Gender)
(t <- table(master1$Gender, exclude = F))
round(prop.table(t)*100, 2)

(t <- table(master1$Gender[master1$Caffeine == "A"], exclude = F))
round(prop.table(t)*100, 2)

(t <- table(master1$Gender[master1$Caffeine == "B"], exclude = F))
round(prop.table(t)*100, 2)

#Age
(t <- table(master$Age_quant, exclude = F))
round(prop.table(t)*100, 2)
summary(master1$Age_quant)

(t <- table(master1$Age_quant[master1$Caffeine == "A"], exclude = F))
round(prop.table(t)*100, 2)

(t <- table(master1$Age_quant[master1$Caffeine == "B"], exclude = F))
round(prop.table(t)*100, 2)

#Sleep
(t <- table(master1$Sleep, exclude = F))
round(prop.table(t)*100, 2)
summary(master1$Sleep)

mean(master1$Sleep[master1$Caffeine == "A"], na.rm = T)
sd(master1$Sleep[master1$Caffeine == "A"], na.rm = T)
mean(master1$Sleep[master1$Caffeine == "B"], na.rm = T)
sd(master1$Sleep[master1$Caffeine == "B"], na.rm = T)



#Smoking
master$Smoker <- factor(master$Smoker)
(t <- table(master$Smoker, exclude = F))
round(prop.table(t)*100, 2)

(t <- table(master1$Smoker[master1$Caffeine == "A"], exclude = F))
round(prop.table(t)*100, 2)

(t <- table(master1$Smoker[master1$Caffeine == "B"], exclude = F))
round(prop.table(t)*100, 2)

#Alcohol
master$Smoker <- factor(master$Smoker)
(t <- table(master$Smoker, exclude = F))
round(prop.table(t)*100, 2)

mean(master1$Alcohol_week[master1$Caffeine == "A"], na.rm = T)
sd(master1$Alcohol_week[master1$Caffeine == "A"], na.rm = T)
mean(master1$Alcohol_week[master1$Caffeine == "B"], na.rm = T)
sd(master1$Alcohol_week[master1$Caffeine == "B"], na.rm = T)
