# ----------------------------
# 0. Przygotowanie danych (już wykonane)
# ----------------------------
# Usuń brakujące dane (ważne dla niektórych testów)
data_clean <- na.omit(data_new)

# ----------------------------
# 1. Estymacja parametrów
# ----------------------------
model <- lm(Overall ~ ., data = data_clean)

# Alternatywnie: Model bez nieistotnych zmiennych
model_reduced <- step(model, direction = "both", trace = 0)

# ----------------------------
# 2. Tablica ANOVA
# ----------------------------
anova_table <- anova(model)
print(anova_table)

# ----------------------------
# 3. Podstawowe charakterystyki
# ----------------------------
summary(model)
AIC_model <- AIC(model)
BIC_model <- BIC(model)
cat("AIC:", AIC_model, "BIC:", BIC_model, "\n")

# ----------------------------
# 4. Diagnostyka modelu
# ----------------------------
# Wpływowe obserwacje
influence_measures <- influence.measures(model)
plot(cooks.distance(model), type = "h")

# Wykresy diagnostyczne
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

# ----------------------------
# 5. Współliniowość (już sprawdzone)
# ----------------------------
vif(model) # Wszystkie < 5 → OK

# ----------------------------
# 6. Testowanie liniowości
# ----------------------------
# Test RESET na nieliniowość
library(lmtest)
resettest(model, power = 2:3, type = "fitted")

# Wizualizacja zależności
ggplot(data_clean, aes(x = as.numeric(Computer), y = Overall)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_smooth(method = "loess", color = "red")

# ----------------------------
# 7. Normalność reszt
# ----------------------------
# Transformacja Box-Cox
library(car)
bc <- powerTransform(data_clean$Overall)
lambda <- bc$lambda
data_clean$Overall_bc <- bcPower(data_clean$Overall, lambda)

model_bc <- lm(Overall_bc ~ ., data = data_clean %>% select(-Overall))
shapiro.test(residuals(model_bc)) # Sprawdź poprawę

# Jeśli transformacja nie pomaga → używaj testów odpornych
library(lmtest)
coeftest(model, vcov = vcovHC(model)) # Huber-White SE

# ----------------------------
# 8. Heteroskedastyczność
# ----------------------------
# Test Breusch-Pagan
bptest(model)

# Korekta:
library(sandwich)
coeftest(model, vcov = vcovHC(model, type = "HC3"))

# ----------------------------
# 9. Autokorelacja (pomijamy - nie ma danych czasowych)
# ----------------------------
# Test Durbina-Watsona tylko dla formalności
durbinWatsonTest(model) # Wynik interpretuj ostrożnie

# ----------------------------
# 10. Predykcja
# ----------------------------
# Przykładowa predykcja
new_data <- data.frame(
  Department = "Computer Science and Engineering",
  Gender = "Male",
  HSC = 4.5,
  SSC = 4.8,
  Income = "Upper middle",
  Hometown = "City",
  Computer = factor(4, levels = 1:5, ordered = TRUE),
  Preparation = "2-3 Hours",
  Gaming = "2-3 Hours",
  Attendance = "80%-100%",
  Job = "No",
  English = factor(4, levels = 1:5, ordered = TRUE),
  Extra = "Yes",
  Semester = "3rd"
)

predict(model, newdata = new_data, interval = "confidence")