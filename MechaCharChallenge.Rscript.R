library(tidyverse)
MechaCar_table <- read.csv(file='MechaCar_mpg.csv')

lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table) #generate multiple linear regression model
lm(mpg ~ vehicle.length,data=MechaCar_table) #generate single linear regression model
lm(mpg ~ ground.clearance,data=MechaCar_table) #generate single linear regression model

summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table)) #generate summary statistics
summary(lm(mpg ~ vehicle.length,data=MechaCar_table)) #generate summary statistics for vehicle.length
summary(lm(mpg ~ ground.clearance,data=MechaCar_table)) #generate summary statistics for ground.clearance

Suspension_table <- read.csv(file='Suspension_Coil.csv',sep=",", header = T)
#Suspension_summary <- sd(Suspension_table$PSI)
Suspension_table %>%
  group_by(Manufacturing_Lot) %>%
  summarise(PSI_mean= mean(PSI), PSI_sd = sd(PSI), PSI_median = median(PSI), PSI_variance = var(PSI))

sample_table <- Suspension_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- Suspension_table %>% sample_n(50) #generate another 50 randomly sampled data points

t.test(sample_table$PSI,sample_table2$PSI) #compare means of two samples
t.test(sample_table$PSI, mu = 1500)
t.test(sample_table2$PSI, mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

?mpg()
?mtcars()

lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars) #generate multiple linear regression model
lm(mpg ~ hp,data=mtcars) #generate single linear regression model

summary_table_mtcars<-summary(lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars)) #generate multiple linear regression model
summary_table_mtcars

summary(lm(mpg ~ hp,data=mtcars)) #generate single linear regression model
summary(lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars)) #generate multiple linear regression model

mtcars %>%
  summarise(hp_mean= mean(hp), hp_sd = sd(hp), hp_median = median(hp), hp_variance = var(hp))
sample_table_mtcars <- mtcars %>% sample_n(16) #generate 16 randomly sampled data points
t.test(sample_table_mtcars$hp, mu = 146)
