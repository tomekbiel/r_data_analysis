library(car)
library(corrplot)

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

plot(model)

'''

Multiple Linear Regression Summary:

The model explains 87.46% of the variance in the overall score (Overall) — indicating a very good fit.

Significant predictors of the overall score include:

Preparation time (2-3 Hours): positively associated with higher scores compared to 0-1 hour.

11th Semester: negatively associated with scores compared to the reference semester.

Last score (Last): has a strong positive effect on the overall score.

Department: Electrical and Electronic Engineering: associated with lower scores compared to the reference department.

Many other variables (gender, job status, income levels, gaming time, etc.) were not statistically significant.

Overall, the model suggests preparation time, recent performance, semester,
and department play important roles in predicting student overall results.

'''


# Wybieramy tylko zmienne typu numerycznego
numeric_vars <- data[, sapply(data, is.numeric)]

# Sprawdźmy jakie kolumny zostały wybrane
names(numeric_vars)

# Macierz korelacji z zaokrągleniem
cor_matrix <- cor(numeric_vars, use = "complete.obs")
round(cor_matrix, 2)

install.packages("corrplot")  # tylko raz
library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")
# Stwórz dummy zmienne (predictors bez interceptu)
X <- model.matrix(Overall ~ ., data = data)[, -1]  # predictors

# Połącz z y (zmienna zależna)
df_vif <- data.frame(Overall = data$Overall, X)

# Regresja
model <- lm(Overall ~ ., data = df_vif)


vif(model)

######################## Regresja po wstępnym oczyszczeniu

# Usunięcie kolumn 'Last' i 'Department'
data_clean <- data[, !(names(data) %in% c("Last", "Department"))]

# Jeśli używasz wcześniej zakodowanych danych (dummy variables), np. data_encoded:
# data_encoded_clean <- data_encoded[, !(names(data_encoded) %in% c("Last", grep("^Department", names(data_encoded), value = TRUE)))]

# Regresja liniowa na oczyszczonych danych
model_clean <- lm(Overall ~ ., data = data_clean)

# Podsumowanie modelu
summary(model_clean)

'''
The linear regression model explaining Overall student performance based on various predictors (gender, HSC, SSC scores, study time, gaming, attendance, semester, etc.) accounts for about 59.3% of the variance (Multiple R-squared = 0.5928). The adjusted R-squared is 56.3%, indicating a solid fit after adjusting for the number of predictors.

Significant predictors (p < 0.05):
  
  GenderMale: negative effect – male students perform slightly worse overall.

HSC: positive – better HSC scores are linked to better overall performance.

Computer: positively associated with higher Overall scores.

Preparation2-3 Hours: strong positive influence.

GamingMore than 3 Hours: negative effect – longer gaming correlates with worse outcomes.

Attendance: higher attendance (especially 80–100%) significantly improves scores, while <40% reduces them.
                               
JobYes: having a job negatively impacts performance.
                               
English: better English scores increase Overall.
                               
ExtraYes: participating in extracurricular activities helps.
                               
 Non-significant predictors:
                                 
 Most income levels and semester levels showed no significant impact.
                               
  SSC and semester indicators had minimal or no influence.
                               
     📌 Conclusions:
Lifestyle, study habits, and academic engagement (attendance, preparation, extracurriculars) strongly affect academic success.
                               
Gaming and employment may interfere with performance.
                               
Scores in HSC, computer studies, and English are key indicators of academic performance.
   
'''


                               
