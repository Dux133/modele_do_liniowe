# Ładowanie danych i pakietów
library(ggplot2) # Wizualizacja
library(dplyr)   # Przetwarzanie danych
library(broom)   # Podsumowanie modeli
library(tidyverse) # Przetwarzanie danych
par(bg="darkgrey") # Ustawienie globalne koloru tła wykresów
# Dane wbudowane mtcars
data("mtcars")
head(mtcars)
# Modelujemy zależność między zużyciem paliwa (mpg) a masą (wt)

# 1. Model potęgowy --------------------------------------------------------

# y=a*x^b - Przekształcamy równanie w postać liniową poprzez logarytmowanie:
# log(y)=log(a)+b*log(x)
# Tworzymy nową ramkę danych z logarytmami
mtcars_log <- mtcars %>%
  mutate(log_mpg = log(mpg), log_wt = log(wt))
# Dopasowujemy model liniowy dla logarytmów
model_log <- lm (log_mpg ~ log_wt, data = mtcars_log)
# Wyniki modelu
summary(model_log)
# Wizualizacja dopasowania
ggplot(mtcars_log, aes(x = log_wt, y = log_mpg))+
  # Funkcja aes() definiuje, które kolumny z ramki danych mtcars_log będą 
  # reprezentować osie wykresu:
  geom_point()+ # Każdy punkt na wykresie odpowiada jednej obserwacji 
  # (samochodowi) z danych.
  geom_smooth(method = "lm", col = "blue")+
  # Dodaje linię dopasowania modelu liniowego (prostej regresji) do wykresu.
  labs(tilte = "model potęgowy", x = "log masa", y = "log spalanie")
a <- exp(model_log$coefficients[1])
b <- model_log$coefficients[2]
y <- a*mtcars$wt^b # predykcja (mpg)
plot(mtcars$wt, mtcars$mpg, pch = 20)
points(mtcars$wt, y, pch = 20, col = 3)

# 2. Model wykładniczy -----------------------------------------------------

# y=a*e^b*x logarytmujemy otrzymując log(y)=log(a)+b*x
# Dopasowujemy model dla logarytmu zmiennej objaśnianej
model_exp <- lm(log(mpg) ~ wt, data = mtcars)
# Wyniki modelu
summary(model_exp)
# Wizualizacja
ggplot(mtcars, aes(x = wt, y = log(mpg)))+
  geom_point()+
  geom_smooth(method = "lm", color = "red")
# Aby nie nadpisać zmiennych przyjmijmy
a1 <- exp(model_exp$coefficients[1])
b1 <- model_exp$coefficients[2]
y1 <- a1*exp(b1*mtcars$wt)
# Wizualizacja
plot(mtcars$wt, mtcars$mpg, pch = 20, col = "black")
points(mtcars$wt, y1, pch = 20, col = 2)
points(mtcars$wt, y, pch = 20, col = 4)

# 3. Model potęgowo-wykładniczy ("mieszany”) ------------------------------
# y=a*x^b*e^c*x Logarytmujemy obie strony:
# log(y)=log(a)+b⋅log(x)+cx
# Dodajemy logarytm zmiennej niezależnej
mtcars_mieszany <- mtcars %>%
  mutate(log_wt = log(wt))
# Dopasowujemy model mieszany
model_mieszany <- lm(log(mpg) ~ log_wt + wt, data = mtcars_mieszany )
# Podsumowanie modelu
summary(model_mieszany)
a2 <- exp(model_mieszany$coefficients[1])
b2 <- model_mieszany$coefficients[2]

# Wizualizacja dopasowania
ggplot(mtcars, aes(x = wt, y = log(mpg)))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color = "blue")
