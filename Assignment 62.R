train <- read.csv("C:/Users/Mathew K/Desktop/Mathew's/ACGILD Data Analytics/Acad Assignment 6/train.csv")
View(train)
# a. Is there any difference in fares by different class of tickets? Note - Show a boxplot displaying the distribution of fares by class

b <- boxplot(Fare~Pclass, data = train, main = "Fare Data", xlab = "Class", ylab = "Fare")
tapply(train$Fare, train$Pclass, mean)

#yes, the fares are highier for class 1 and decreases as class 2 and 3. The lowest mean fare is for class3

# b. Is there any association with Passenger class and gender? Note - Show a stacked bar chart

library(dplyr)
library(ggplot2)
summ <- train %>%
  group_by(Pclass,Sex) %>%
  summarise(count = n())

summ1<- train %>%
  group_by(Pclass) %>%
  summarise(count = n())

summ$count[summ$Sex == "male"] <- summ$count[summ$Sex == "male"] *-1
summ$ypos <-summ$count/2

ggplot(data = summ, aes(Pclass,count,fill=Sex))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = abs(count), y=ypos),vjust = -0.2,size=5)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  geom_hline(yintercept = 0)

summ2 <- merge(summ, summ1, by="Pclass")
summ2$perc <- (summ2$count.x/summ2$count.y)*100
summ2$ypos <-summ2$perc/2

ggplot(data = summ2, aes(Pclass,perc,fill=Sex))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste(round(abs(perc),2),"%"), y=ypos),vjust = -0.2,size=5)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  geom_hline(yintercept = 0)

#Yes, there is an association with Passenger Class with gender. As the Passenger Class (value) increases the propotion of Females decreases and males increases