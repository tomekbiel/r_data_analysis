str(data)
# Konwersja na faktory:
data$Department <- as.factor(data$Department)
data$Gender <- as.factor(data$Gender)
data$Income <- as.factor(data$Income)
data$Hometown <- as.factor(data$Hometown)
data$Gaming <- as.factor(data$Gaming)
data$Job <- as.factor(data$Job)
data$Extra <- as.factor(data$Extra)
data$Semester <- as.factor(data$Semester)  # ewentualnie ordered factor, jeśli ma sens

# Model regresji liniowej:
model <- lm(Overall ~ Preparation + Attendance + Gaming + Income + English + Extra + Semester + Last + Job + Gender + Department + HSC + SSC + Computer, data = data)

summary(model)


'''
Multiple Linear Regression Summary:

The model explains 87.46% of the variance in the overall score (Overall) — indicating a very good fit.

Significant predictors of the overall score include:

Preparation time (2-3 Hours): positively associated with higher scores compared to 0-1 hour.

11th Semester: negatively associated with scores compared to the reference semester.

Last score (Last): has a strong positive effect on the overall score.

Department: Electrical and Electronic Engineering: associated with lower scores compared to the reference department.

Many other variables (gender, job status, income levels, gaming time, etc.) were not statistically significant.

Overall, the model suggests preparation time, recent performance, semester, and department play important roles in predicting students' overall results.
'''