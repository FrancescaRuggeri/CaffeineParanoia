setwd("~/Desktop/CaffeineData/")
master <- read_csv("master.csv")

library(tidyverse)

head(master)
master$Caffeine <- factor(master$Caffeine)
master$Task_Name <- factor(master$Task_Name)



a <- master %>%
  select(Task_Name, Caffeine, ResponseHI, ResponseSI)

# overall medians
b <- a %>% group_by(Task_Name)
b_medians <- b %>% summarise(
  HI = median(ResponseHI),
  SI = median(ResponseSI)
)
b_medians

giving_fair <- b %>% filter(Task_Name == "Giving_Fair")
gf <- giving_fair %>% group_by(Caffeine) %>% summarise(
  HI = median(ResponseHI),
  SI = median(ResponseSI)
)
gf$Task_Name <- c(rep("Giving_Fair"))

# within task name groups 
giving_stingy <- b %>% filter(Task_Name == "Giving_Stingy")
gs <- giving_stingy %>% group_by(Caffeine) %>% summarise(
  HI = median(ResponseHI),
  SI = median(ResponseSI)
)
gs$Task_Name <- c(rep("Giving_Stingy"))


taking_fair <- b %>% filter(Task_Name == "Taking_Fair")
tf <- taking_fair %>% group_by(Caffeine) %>% summarise(
  HI = median(ResponseHI),
  SI = median(ResponseSI)
)
tf$Task_Name <- c(rep("Taking_Fair"))

taking_stingy <- b %>% filter(Task_Name == "Taking_Stingy")
ts <- taking_stingy %>% group_by(Caffeine) %>% summarise(
  HI = median(ResponseHI),
  SI = median(ResponseSI)
)
ts$Task_Name <- c(rep("Taking_Stingy"))

gf <- data.frame(gf)
tf <- data.frame(tf)
gs <- data.frame(gs)
ts <- data.frame(ts)

merged <- rbind(gf, ts, gs, tf)


# split into caffeine groups
A <- master %>% filter(Caffeine == "A")
B <- master %>% filter(Caffeine == "B")

# task summaries A HI
quantile(A$ResponseHI[A$Task_Name == "Giving_Fair"], na.rm = T)
quantile(A$ResponseHI[A$Task_Name == "Taking_Fair"], na.rm = T)
quantile(A$ResponseHI[A$Task_Name == "Taking_Stingy"], na.rm = T)
quantile(A$ResponseHI[A$Task_Name == "Giving_Stingy"], na.rm = T)

# task summaries A SI
quantile(A$ResponseSI[A$Task_Name == "Giving_Fair"], na.rm = T)
quantile(A$ResponseSI[A$Task_Name == "Taking_Fair"], na.rm = T)
quantile(A$ResponseSI[A$Task_Name == "Taking_Stingy"], na.rm = T)
quantile(A$ResponseSI[A$Task_Name == "Giving_Stingy"], na.rm = T)

# task summaries B HI
quantile(B$ResponseHI[B$Task_Name == "Giving_Fair"], na.rm = T)
quantile(B$ResponseHI[B$Task_Name == "Taking_Fair"], na.rm = T)
quantile(B$ResponseHI[B$Task_Name == "Taking_Stingy"], na.rm = T)
quantile(B$ResponseHI[B$Task_Name == "Giving_Stingy"], na.rm = T)

# task summaries B SI
quantile(B$ResponseSI[B$Task_Name == "Giving_Fair"], na.rm = T)
quantile(B$ResponseSI[B$Task_Name == "Taking_Fair"], na.rm = T)
quantile(B$ResponseSI[B$Task_Name == "Taking_Stingy"], na.rm = T)
quantile(B$ResponseSI[B$Task_Name == "Giving_Stingy"], na.rm = T)


#####################################################################

