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








