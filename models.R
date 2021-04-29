master$Sleep <- as.factor(master$Sleep)
master$Task_Name <- factor(master$Task_Name)
library(tidyverse)
#response HI

summary(master$ResponseHI)
summary(master$ResponseSI)

chisq.test(master$Caffeine, master$Gender)
table(master$Caffeine, master$Gender)
master$Caffeine <- factor(master$Caffeine)


#linear models

model4 <- lm(ResponseHI ~ Caffeine + Age_quant + Gender + Sleep, data = master1)
summary(model4)
confint(model4)


model5 <- lm(ResponseHI ~ Caffeine * Task_Name + Age_quant + Gender + Sleep, data = master1)
summary(model5)
confint(model5)

#Task predicts HI, but the interaction between Caffeine and HI does not
model8 <- lm(ResponseHI ~ Caffeine * Task_Name, data = master)
summary(model8)

model9 <- lm(ResponseHI ~ Task_Name, data = master)
summary(model9)

#Sleep doesn't predict HI
model10 <- lm(ResponseHI ~ Sleep * Task_Name, data = master1)
summary(model10)

#lm for SI
model19 <- lm(ResponseSI ~ Caffeine + Age_quant + Gender + Sleep, data = master1)
summary(model19)
confint(model19)

model20 <- lm(ResponseSI ~ Caffeine * Task_Name + Age_quant + Gender + Sleep, data = master1)
summary(model20)
confint(model20)

#ANOVA - Compute the analysis of variance
res.aov <- aov(ResponseHI ~ Caffeine, data = master1)
# Summary of the analysis
summary(res.aov)

res.aov2 <- aov(ResponseHI ~ Task_Name, data = master)
summary(res.aov2)

#plotting


master1 <- na.omit(master)

master1$Task_Name <- factor(master1$Task_Name)
levels(master1$Task_Name) <- c("Giving Fair", "Giving Stingy", "Taking Fair", "Taking Stingy")


#Harmful intent, Caffeine group, Dictator style

ggplot(data = master1) +
  
  geom_boxplot(aes(Caffeine, ResponseHI, fill = Task_Name), color = '#000000') +
  
  scale_fill_manual(name = 'Dictator Style', values = c('#DAF7A6', '#FFC300', '#FF5733','#C70039')) +
  
  labs(x = "Caffeine", y = "Harmful Intent", title = "Harmful Intent in Caffeine and Control across Task Type")


ggplot(data = master1) +
  
  geom_boxplot(aes(Caffeine, ResponseSI, fill = Task_Name), color = '#000000') +
  
  scale_fill_manual(name = 'Dictator Style', values = c('#DAF7A6', '#FFC300', '#FF5733','#C70039')) +
  
  labs(x = "Caffeine", y = "Self Interest", title = "Self Interest in Caffeine and Control Across Task Type")

#HI, Dictator Style
ggplot(data = master1) +
  
  geom_boxplot(aes(Task_Name, ResponseHI, fill = Task_Name), color = '#000000') +
  
  scale_fill_manual(name = 'Task Type', values = c('#DAF7A6', '#FFC300', '#FF5733','#C70039')) +
  
  labs(x = "Task Type", y = "Harmful Intent", title = "Harmful Intent across Task Type") + theme(legend.position = "none")

#SI, DIctator Style
ggplot(data = master1) +
  
  geom_boxplot(aes(Task_Name, ResponseSI, fill = Task_Name), color = '#000000') +
  
  scale_fill_manual(name = 'Task Type', values = c('#DAF7A6', '#FFC300', '#FF5733','#C70039')) +
  
  labs(x = "Task Type", y = "Self Interest", title = "Self Interest across Task Type") + theme(legend.position = "none")

#Combined Dictator style Boxplot 

master1$Dictator <- ifelse(master1$Task_Name == "Giving Fair" | master1$Task_Name == "Taking Fair", "Fair", "Stingy")

ggplot(data = master1) +
  
  geom_boxplot(aes(Caffeine, ResponseHI, fill = Dictator), color = '#000000') +
  
  scale_fill_manual(name = 'Dictator Style', values = c('#6495ED', '#9FE2BF')) +
  
  labs(x = "Caffeine", y = "Harmful Intent", title = "Harmful Intent in Caffeine and Control With Fair and Stingy Dictator")

ggplot(data = master1) +
  
  geom_boxplot(aes(Caffeine, ResponseSI, fill = Dictator), color = '#000000') +
  
  scale_fill_manual(name = 'Dictator Style', values = c('#6495ED', '#9FE2BF')) +
  
  labs(x = "Caffeine", y = "Self Interest", title = "Self Interest in Caffeine and Control With Fair and Stingy Dictator")

#Dictator style vs HI and SI
ggplot(data = master1) +
  
  geom_boxplot(aes(Dictator, ResponseHI, fill = Dictator), color = '#000000') + 
  
  scale_fill_manual(values = c('#6495ED', '#9FE2BF')) +
  
  labs(x = "Dictator Style", y = "Harmful Intent", title = "Harmful Intent with Fair and Stingy Dictators") +
  
  theme(legend.position = "none")

ggplot(data = master1) +
  
  geom_boxplot(aes(Dictator, ResponseSI, fill = Dictator), color = '#000000') + 
  
  scale_fill_manual(values = c('#6495ED', '#9FE2BF')) +
  
  labs(x = "Dictator Style", y = "Harmful Intent", title = "Self Interest with Fair and Stingy Dictators") +
  
  theme(legend.position = "none")

#t-test HI fair vs. stingy (group A p=0.001913, group B p=0.07163)

t.test(master1$ResponseHI[master1$Caffeine == "A"] ~(master1$Dictator[master1$Caffeine == "A"]))

t.test(master1$ResponseHI[master1$Caffeine == "B"] ~(master1$Dictator[master1$Caffeine == "B"]))

#t-test SI fair vs. stingy 
t.test(master1$ResponseSI[master1$Caffeine == "A"] ~(master1$Dictator[master1$Caffeine == "A"]))

t.test(master1$ResponseSI[master1$Caffeine == "B"] ~(master1$Dictator[master1$Caffeine == "B"]))

#t-test HI and SI fair vs stingy overall
t.test(master1$ResponseHI ~(master1$Dictator))
t.test(master1$ResponseSI ~(master1$Dictator))


#Caffeine group boxplot
ggplot(data = master1) +
  
  geom_boxplot(aes(Caffeine, ResponseHI, fill = Caffeine), color = '#000000') + 
  
  scale_fill_manual(values = c('#E53935', '#FF9800')) +
  
  labs(x = "Caffeine", y = "Harmful Intent", title = "Harmful Intent in Caffeine and Control") +
  
  theme(legend.position = "none")


ggplot(data = master1) +
  
  geom_boxplot(aes(Caffeine, ResponseSI, fill = Caffeine), color = '#000000') +
  
  scale_fill_manual(values = c('#E53935', '#FF9800')) +
  
  labs(x = "Caffeine", y = "Self Interest", title = "Self Interest in Caffeine and Control") +
  
  theme(legend.position = "none")


#correlations between sleep and daily caffeine intake
master1$Tea_day <- as.numeric(master1$Tea_day)
master1$Age_quant <-as.factor(master1$Age_quant)
CaffeineUnits <- (master1$Coffee_day + master1$Tea_day + master1$EnDrinks + master1$RedBull + master1$FizDrinks)
  
ggplot(data = master1) + geom_boxplot(aes(CaffeineUnits, Sleep), method = 'lm')
ggplot(data = master1) + geom_smooth(aes(CaffeineUnits, Sleep), method = 'lm') +
  labs(x = "Caffeine Units per Day", y = "Sleep", 
       title = "Correlation between Sleep and Caffeine Units Consumed per Day") + theme_minimal()


model13 <-lm(CaffeineUnits ~ Sleep + Age_quant + Gender, data = master1)
summary(model13)
confint(model13)


#info about caffeineunits
master1$CaffeineUnits <- CaffeineUnits

summary(CaffeineUnits)
(t <- table(CaffeineUnits, exclude = F))
round(prop.table(t)*100, 2)

mean(master1$CaffeineUnits[master1$Caffeine == "A"], na.rm = T)
median(master1$CaffeineUnits[master1$Caffeine == "A"], na.rm = T)
sd(master1$CaffeineUnits[master1$Caffeine == "A"], na.rm = T)

mean(master1$CaffeineUnits[master1$Caffeine == "B"], na.rm = T)
median(master1$CaffeineUnits[master1$Caffeine == "B"], na.rm = T)
sd(master1$CaffeineUnits[master1$Caffeine == "B"], na.rm = T)

#tukey test task type

master$Caffeine <- factor(master$Caffeine)
master$Task_Name <- factor(master$Task_Name)

master1 <- na.omit(master) # can't have NAs in the Tukey test

########################################################
# HI

model=lm(ResponseHI ~ Task_Name, data = master1)
ANOVA=aov(model)

summary(ANOVA)

# Tukey test to study each pair of task type :
tuk <- TukeyHSD(ANOVA, conf.level=0.95)
tuk # results of tukey test 

########################################################
# SI

model=lm(ResponseSI ~ Task_Name, data = master1)
ANOVA=aov(model)

summary(ANOVA)

# Tukey test to study each pair of task type :
tuk <- TukeyHSD(ANOVA, conf.level=0.95)
tuk # results of tukey test 