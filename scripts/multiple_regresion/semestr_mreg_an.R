#library(ggplot2)

# Boxplot: Overall wg semestru
ggplot(data_new, aes(x = Semester, y = Overall, fill = Semester)) +
  geom_boxplot() +
  labs(title = "Rozkład ocen Overall w zależności od semestru",
       x = "Semestr",
       y = "Ocena Overall") +
  theme_minimal()

# Linia trendu (średnia Overall wg semestru)
data_new %>%
  group_by(Semester) %>%
  summarise(Mean_Overall = mean(Overall, na.rm = TRUE)) %>%
  ggplot(aes(x = Semester, y = Mean_Overall, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Średnia ocena Overall w kolejnych semestrach",
       x = "Semestr",
       y = "Średnia Overall") +
  theme_minimal()

# ANOVA: Czy średnie Overall różnią się między semestrami?
anova_result <- aov(Overall ~ Semester, data = data_new)
summary(anova_result)

# Testy post-hoc (jeśli ANOVA istotna)
TukeyHSD(anova_result)  # Pokazuje, które semestry różnią się istotnie

# Przykład: Średnia frekwencja wg semestru
data_new %>%
  group_by(Semester) %>%
  summarise(Mean_Attendance = mean(as.numeric(Attendance), na.rm = TRUE)) %>%
  ggplot(aes(x = Semester, y = Mean_Attendance, group = 1)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Średnia frekwencja w kolejnych semestrach",
       x = "Semestr",
       y = "Średni poziom frekwencji (1=Below 40%, 4=80%-100%)")

# Czy studenci z wyższych semestrów częściej pracują (Job)?
data_new %>%
  group_by(Semester, Job) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Semester, y = Count, fill = Job)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Czy studenci wyższych semestrów częściej pracują?")