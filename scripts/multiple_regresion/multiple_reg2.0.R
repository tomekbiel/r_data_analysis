# 1. Naprawa zmiennej Income (usunięcie niespójności formatu)
data <- data %>%
  mutate(
    Income = case_when(
      grepl("^High", Income) ~ "High",
      grepl("^Upper middle", Income) ~ "Upper middle",
      grepl("^Lower middle", Income) ~ "Lower middle",
      grepl("^Low", Income) ~ "Low",
      TRUE ~ NA_character_
    )
  )

# 2. Tworzenie nowej ramki danych z usuniętą kolumną Last
data_new <- data %>% 
  select(-Last)

# 3. Konwersja zmiennych kategorycznych bez porządku:
data_new <- data_new %>%
  mutate(
    Department = as.factor(Department),
    Gender = as.factor(Gender),
    Hometown = as.factor(Hometown),
    Job = as.factor(Job),
    Extra = as.factor(Extra)
  )

# 4. Konwersja zmiennych porządkowych:
data_new <- data_new %>%
  mutate(
    Income = factor(Income, 
                    levels = c("Low", "Lower middle", "Upper middle", "High"),
                    ordered = TRUE),
    Gaming = factor(Gaming, 
                    levels = c("0-1 Hour", "2-3 Hours", "More than 3 Hours"),
                    ordered = TRUE),
    Semester = factor(Semester,
                      levels = c("1st", "2nd", "3rd", "4th", "5th", 
                                 "6th", "7th", "8th", "9th", "10th"),
                      ordered = TRUE),
    Attendance = factor(Attendance,
                        levels = c("Below 40%", "40%-59%", "60%-79%", "80%-100%"),
                        ordered = TRUE),
    Preparation = factor(Preparation,
                         levels = c("0-1 Hour", "2-3 Hours", "More than 3 Hours"),
                         ordered = TRUE)
  )

# 5. Konwersja zmiennych liczbowych - zgodnie z Twoimi wytycznymi:
# HSC i SSC - pozostawiamy jako ciągłe (numeryczne)
# Computer i English - zamieniamy na uporządkowane czynniki
data_new <- data_new %>%
  mutate(
    Computer = factor(Computer, levels = 1:5, ordered = TRUE),
    English = factor(English, levels = 1:5, ordered = TRUE)
  )

# 6. Sprawdzenie struktury danych
str(data_new)

# 7. Weryfikacja poziomów dla zmiennych uporządkowanych
cat("\nPoziomy dla zmiennych porządkowych:\n")
cat("Computer:", levels(data_new$Computer), "\n")
cat("English:", levels(data_new$English), "\n")
cat("Income:", levels(data_new$Income), "\n")
cat("Gaming:", levels(data_new$Gaming), "\n")
cat("Semester:", levels(data_new$Semester), "\n")
cat("Attendance:", levels(data_new$Attendance), "\n")
cat("Preparation:", levels(data_new$Preparation), "\n")

# 8. Podsumowanie nowej ramki danych
summary(data_new)

model <- lm(Overall ~ ., data = data_new)

summary(model)

plot(model)


# Wybierz tylko zmienne numeryczne (HSC, SSC, Overall)
numeric_vars <- data_new %>% 
  select(where(is.numeric))  # bezpieczniejsze niż sapply(is.numeric)

# Macierz korelacji
cor_matrix <- cor(numeric_vars, use = "complete.obs")
round(cor_matrix, 2)

# Wizualizacja
library(corrplot)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         addCoef.col = "black",
         title = "Korelacje między zmiennymi numerycznymi")


# Model z wszystkimi zmiennymi (używając data_new)
model_full <- lm(Overall ~ ., data = data_new)

# Oblicz VIF
vif_results <- vif(model_full)
print(vif_results)

# Interpretacja:
# - VIF > 5 = umiarkowana multikolinearność
# - VIF > 10 = poważny problem

summary.lm(model_full)$coef
alfa  <- 0.05
uf <- confint(model_full,level = 1-alfa)
## z prawdopodobieństwem ...

sw <- shapiro.test(model_full$residuals)
print(sw)
library(lmtest)
gq <- gqtest(model_full)
print(gq)

library(car)
DW_test <- durbinWatsonTest(model_full, alternative = "positive")
print(DW_test)
library(lmtest)
BG <- bgtest(model_full,order=1,type = c("Chisq"))
print(BG)