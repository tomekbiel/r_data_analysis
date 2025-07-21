#library(readr)
#data <- read_csv("data/ResearchInformation3.csv")
#data <- read.csv("data/ResearchInformation3.csv", stringsAsFactors = FALSE)
#install.packages('tidyverse', dependencies=TRUE, type="source")
library(tidyverse)
url <- "https://raw.githubusercontent.com/tomekbiel/r_data_analysis/refs/heads/master/data/ResearchInformation3.csv"

data <- read.csv(url)

(head(data[, c(8, 16)], 10))

(mean <- round(sapply(split(data$Overall, data$Preparation),mean),3))

(var <- round(sqrt(sapply(split(data$Overall, data$Preparation),var)),3))

boxplot(split(data$Overall,data$Preparation),
        xlab='Preparation',ylab='Overall',col='green')

attach(data)

#model construct
m1 <- lm(data$Overall~data$Preparation)
(anova(m1))
(detach(data))


'''
ANOVA summary 
Hypothesis (H₀):
The mean Overall score is the same for all levels of Preparation time.
In other words, students’ average scores do not differ depending on how much time they spend preparing.


Results:
  
  The ANOVA shows an F-statistic = 50.093 with a p-value < 2.2e-16, which is extremely small.

This means there is strong evidence to reject the null hypothesis (H₀).

Therefore, we conclude that at least one group’s mean Overall score is significantly different from the others.

In practical terms: the amount of preparation time has a statistically significant effect on students’ average scores.
'''
anova(m1)
post_m1 <- aov(data$Overall~data$Preparation)
overall_hsd <- TukeyHSD(post_m1, which='data$Preparation')
(overall_hsd)
plot(overall_hsd)
(detach(data))

'''
The one-way ANOVA tested the null hypothesis that the mean Overall scores do not differ across the three levels of Preparation.
The ANOVA result (p < 2.2e-16) strongly rejects the null hypothesis.
Post-hoc Tukey HSD shows that students who study 2–3 hours or more than 3 hours score significantly higher than those who study 0–1 hour.
However, there is no significant difference between the 2–3 hours and >3 hours groups.
'''
# two factors anova
data$Attendance <- as.factor(data$Attendance)
data$Preparation <- as.factor(data$Preparation)
attach(data)
('means')
(means <- round(matrix(sapply(split(data$Overall, interaction(data$Attendance, data$Preparation)),mean),ncol=4),3))
('variances')
(vars <- round(matrix(sapply(split(data$Overall, interaction(data$Attendance, data$Preparation)),var),ncol=4),3)) 
( table(data$Attendance, data$Preparation))

boxplot(split(data$Overall, interaction(data$Attendance, data$Preparation)), xlab='Preparation', ylab='Overall', col='red')


# visual interaction

attach(data)

par(mfrow=c(2,1))
#par(mar=c(4.2,4,0.8,1.1))
interaction.plot(data$Attendance, data$Preparation, data$Overall, ylim = c(3,7),lty=c(1,5,10),
                 ylab = 'overall',xlab = 'Attendance',lwd=2,trace.label = 'Preparation')
interaction.plot( data$Preparation,data$Attendance, data$Overall, ylim = c(3,7),lty=c(1,10),
                 ylab = 'overall',xlab = 'Preparation',lwd=2,trace.label = 'Attendance')
detach(data)

# 2 factors model

attach(data)

m1 <- lm(data$Overall~data$Preparation+data$Attendance)
m2 <- anova(m1)
(m2)
detach(data)

'''
Both preparation time and attendance have a statistically significant effect on students
overall performance (p < 0.001). However, to fully understand if their effects are independent
or if they interact, a two-way ANOVA including an interaction term is required.
'''
attach(data)
post_m1 <- aov(data$Overall~data$Attendance+data$Preparation)
overall_hsd <- TukeyHSD(post_m1, which=c("data$Attendance","data$Preparation"))
(overall_hsd)
par(mfrow=c(2,1))
#par(mar=c(4,3,3,1.5))
plot(overall_hsd)
detach(data)

'''
What do the Tukey post-hoc test results tell us?
For Attendance:
Most pairwise comparisons are statistically significant (adjusted p-values close to 0).

For example, the difference in means between the "Below 40%" attendance group and the "80%-100%" group is large and negative (-1.8), indicating that students with the lowest attendance have significantly lower scores compared to those with the highest attendance.

Similarly, the "Below 40%" group scores significantly lower than all other attendance groups.

This clearly shows that attendance has a strong impact on overall scores.

For Preparation:
The difference between "2-3 Hours" and "0-1 Hour" preparation time is significant and positive (~0.31), meaning more preparation time leads to higher scores.

The difference between "More than 3 Hours" and "0-1 Hour" is not significant (p = 0.36).

Interestingly, the difference between "More than 3 Hours" and "2-3 Hours" is significant and negative (-0.22), suggesting that students preparing more than 3 hours score slightly lower than those preparing 2-3 hours.

This unexpected result might indicate a diminishing return or less efficient study time beyond 3 hours.
'''

m_int <- aov(Overall ~ Attendance * Preparation, data=data)
summary(m_int)

'''
Both Attendance and Preparation significantly affect overall scores. 
The significant interaction shows that the effect of preparation depends on attendance level.
This means the two factors influence scores together, not just separately
'''
















