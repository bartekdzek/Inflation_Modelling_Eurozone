library(dplyr)
library(readxl)
library(psych)
library(tseries)

#### wczytanie danych ####

dane <- read_xlsx("dane.xlsx")

dane <- data.frame(
  indeks = dane$indeks[3:81],
  stopa_inflacji = dane$stopa_inflacji[3:81],
  ceny_importowe = dane$ceny_importowe[2:80],
  indeks_cen_producentow = dane$indeks_cen_producentow[2:80],
  wzrost_pkb = dane$wzrost_pkb[1:79],
  wzrost_kredytow = dane$wzrost_kredytow[1:79],
  stopa_bezrobocia = dane$stopa_bezrobocia[2:80],
  indeks_zew = dane$indeks_zew[2:80],
  moc_produkcyjna = dane$moc_produkcyjna[1:79],
  ropa_brent = dane$ropa_brent[2:80]
)

#### statystyki ####

dane %>%
  select(-c(indeks)) %>%
  describe() %>%
  select(mean, sd, median, kurtosis, skew) %>%
  mutate(kurtosis = kurtosis + 3,
         cv = sd / mean)

library(corrplot)
corr_matrix <- cor(dane %>% select(-c(indeks)))
corrplot(corr_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

# zmienne do wykluczenia na tym etapie
# znacząco niski współczynnik zmienności

dane <- dane %>% select(-c(moc_produkcyjna))

library(car)
vif(lm(stopa_inflacji ~., dane %>% select(-c(indeks))))

#### stacjonarność ####

adf.test(dane$stopa_inflacji) # nstc !!!
adf.test(dane$ceny_importowe) # nstc !!!
adf.test(dane$indeks_cen_producentow) # nstc !!!
adf.test(dane$wzrost_pkb) # stac
adf.test(dane$wzrost_kredytow) # nstc !!!
adf.test(dane$stopa_bezrobocia) # nstc !!!
adf.test(dane$indeks_zew) # stc
adf.test(dane$ropa_brent) # nstc !!!

kpss.test(dane$stopa_inflacji) # stc
kpss.test(dane$ceny_importowe) # nstc !!!
kpss.test(dane$indeks_cen_producentow) # stc
kpss.test(dane$wzrost_pkb) # stc
kpss.test(dane$wzrost_kredytow) # nstc !!!
kpss.test(dane$stopa_bezrobocia) # nstc !!!
kpss.test(dane$indeks_zew) # stc
kpss.test(dane$ropa_brent) # stc

# ceny importowe
diff_ceny_importowe <- diff(dane$ceny_importowe)
adf.test(diff_ceny_importowe) # stc
kpss.test(diff_ceny_importowe) # stc
dane$ceny_importowe <- c(NA, diff_ceny_importowe)

# wzrost kredytów
diff_wzrost_kredytow <- diff(dane$wzrost_kredytow)
adf.test(diff_wzrost_kredytow) # nstc !!!
kpss.test(diff_wzrost_kredytow) # stc
dane$wzrost_kredytow <- c(NA, diff_wzrost_kredytow)

# stopa bezrobocia
diff_stopa_bezrobocia <- diff(dane$stopa_bezrobocia)
adf.test(diff_stopa_bezrobocia) # stc
kpss.test(diff_stopa_bezrobocia) # stc
dane$stopa_bezrobocia <- c(NA, diff_stopa_bezrobocia)

# ropa Brent
diff_ropa_brent <- diff(dane$ropa_brent)
adf.test(diff_ropa_brent) # stc
kpss.test(diff_ropa_brent) # stc
dane$ropa_brent <- c(NA, diff_ropa_brent)

plot(dane$indeks, dane$stopa_inflacji, type = "l")
plot(dane$indeks, dane$ceny_importowe, type = "l")
plot(dane$indeks, dane$indeks_cen_producentow, type = "l")
plot(dane$indeks, dane$wzrost_pkb, type = "l")
plot(dane$indeks, dane$wzrost_kredytow, type = "l")
plot(dane$indeks, dane$stopa_bezrobocia, type = "l")
plot(dane$indeks, dane$indeks_zew, type = "l")
plot(dane$indeks, dane$ropa_brent, type = "l")

dane <- dane[2:79,]

#### podział zbioru treningowy i testowy ####

dane_treningowe <- dane[1:62,]
dane_testowe <- dane[63:78,]

#### kryterium doboru zmiennych ####

library(mbstats)
Hellwig <- hellwig(dane_treningowe$stopa_inflacji,
                   dane_treningowe %>%
                     select(-c(indeks, stopa_inflacji)))

# metoda krokowa wsteczna z AIC
model0 <- lm(stopa_inflacji ~., dane_treningowe %>% select(-c(indeks)))
step(model0, direction = "backward")

dane_treningowe <- dane_treningowe %>%
  select(indeks,
         stopa_inflacji,
         ceny_importowe,
         indeks_cen_producentow,
         wzrost_kredytow,
         stopa_bezrobocia,
         indeks_zew,
         ropa_brent)

dane_testowe <- dane_testowe %>%
  select(indeks,
         stopa_inflacji,
         ceny_importowe,
         indeks_cen_producentow,
         wzrost_kredytow,
         stopa_bezrobocia,
         indeks_zew,
         ropa_brent)

#### budowa modelu ####

model <- lm(stopa_inflacji ~., dane_treningowe %>% select(-c(indeks)))
summary(model)

### normalność reszt ###

plot(model$residuals, type = "l")
plot(density(model$residuals))
library(nortest)
shapiro.test(model$residuals) 
lillie.test(model$residuals)

### autokorelacja ###

acf(model$residuals)
pacf(model$residuals)
library(lmtest)
library(stats)
dwtest(model)

# usuwanie autokorelacji rzędu pierwszego
rho <- acf(model$residuals, plot = FALSE)$acf[2]
dane_treningowe <- dane_treningowe %>%
  mutate(
    stopa_inflacji = stopa_inflacji - rho * lag(stopa_inflacji),
    ceny_importowe = ceny_importowe - rho * lag(ceny_importowe),
    indeks_cen_producentow = indeks_cen_producentow - rho * lag(indeks_cen_producentow),
    wzrost_kredytow = wzrost_kredytow - rho * lag(wzrost_kredytow),
    stopa_bezrobocia =  stopa_bezrobocia - rho * lag(stopa_bezrobocia),
    indeks_zew = indeks_zew - rho * lag(indeks_zew),
    ropa_brent = ropa_brent - rho * lag(ropa_brent)
  ) %>%
  slice(-1)

# weryfikacja
model2 <- lm(stopa_inflacji ~., dane_treningowe %>% select(-c(indeks)))
summary(model2)
acf(model2$residuals)
plot(density(model2$residuals))
shapiro.test(model2$residuals) 
lillie.test(model2$residuals)

#### heteroskedastyczność ####

plot(model2$residuals)
plot(dane_treningowe$ceny_importowe, model2$residuals)
plot(dane_treningowe$indeks_cen_producentow, model2$residuals)
plot(dane_treningowe$wzrost_kredytow, model2$residuals)
plot(dane_treningowe$stopa_bezrobocia, model2$residuals)
plot(dane_treningowe$indeks_zew, model2$residuals)
plot(dane_treningowe$ropa_brent, model2$residuals)

bptest(model2)
gqtest(model2, alternative = "two.sided", 
       order.by = ~ dane_treningowe$ceny_importowe) # homosked
gqtest(model2, alternative = "two.sided", 
       order.by = ~ dane_treningowe$indeks_cen_producentow) # homosked
gqtest(model2, alternative = "two.sided", 
       order.by = ~ dane_treningowe$wzrost_kredytow) # homosked
gqtest(model2, alternative = "two.sided", 
       order.by = ~ dane_treningowe$stopa_bezrobocia) # homosked, pv = 0.045 !!!
gqtest(model2, alternative = "two.sided", 
       order.by = ~ dane_treningowe$indeks_zew) # homosked
gqtest(model2, alternative = "two.sided", 
       order.by = ~ dane_treningowe$ropa_brent) # homosked

# usuwanie heteroskedastyczności
model_res <- lm(log(model2$residuals ^ 2) ~
                  dane_treningowe$indeks_cen_producentow +
                  dane_treningowe$wzrost_kredytow +
                  dane_treningowe$ropa_brent)
w <- sqrt(exp(model_res$fitted.values))
#w <- sqrt(model2$residuals^2)
model3 <- lm(stopa_inflacji ~., dane_treningowe %>% select(-c(indeks)),
             weights = 1 / w ^ 2)
summary(model3)
model3_res <- (1 / w) * model3$residuals
ncvTest(model3, var.formula = . ~ ceny_importowe + 
          indeks_cen_producentow +
          wzrost_kredytow +
          ropa_brent)

lillie.test(model3_res)
shapiro.test(model3_res)
# pierwsze przekształcenie nie pomogło usunąć heteroskedastyczności
# drugie zaburzyło założenie normalności składnika losowego

#### współliniowość ####

vif(model2)
# występuje bardzo nieznaczna współliniowość
# dla kazdego vif < 1.7

#### stabilność parametrów modelu - test Chowa ####

RSK <- sum(model2$residuals^2)
k <- ncol(dane_treningowe %>% select(-c(indeks, stopa_inflacji)))
n <- nrow(dane_treningowe)
r_1 <- k + 1
r_2 <- n - 2 * (k + 1)
chow_wyniki <- c()
for (i in 10:52) {
  dane_chow_1 <- dane_treningowe[1:i, ]
  dane_chow_2 <- dane_treningowe[(i + 1):n, ]
  
  model_chow_1 <- lm(dane_chow_1$stopa_inflacji ~.,
                     dane_chow_1 %>% select(-c(indeks, stopa_inflacji)))
  model_chow_2 <- lm(dane_chow_2$stopa_inflacji ~.,
                     dane_chow_2 %>% select(-c(indeks, stopa_inflacji)))
  
  RSK_1 <- sum(model_chow_1$residuals^2)
  RSK_2 <- sum(model_chow_2$residuals^2)
  
  F_statistic <- ((RSK - (RSK_1 + RSK_2)) / (RSK_1 + RSK_2)) * 
    ((n - 2 * (k + 1)) / (k + 1))
  p = 1 - pf(F_statistic, r_1, r_2)
  chow_wyniki <- c(chow_wyniki, p)
}

plot(x = 10:52, y = chow_wyniki, type = "l", xlim = c(0, 61)) 
abline(h = 0.05, col = "red", lty = 2)

# parametry modelu są stabilne jedynie w przypadku podziału względem
# 30 obserwacji p_value ok. 0.03

#### postać modelu -- test RESET, test serii ####

resettest(model2)
runs.test(factor(model2$residuals > 0))

# postać modelu jest poprawna

#### badanie efektu katalizy ####
#corr_matrix <- cor(dane_model %>% select(-c(indeks)))
#R0 <- corr_matrix[2:4, 1]
#R <- corr_matrix[2:4, 2:4]

#R0[2] <- -R0[2]
#R[,2] <- -R[,2]
#R[2,] <- -R[2,]

#R0 <- c(R0[2], R0[3], R0[1])
#R <- R[,c(2, 3, 1)]
#R <- R[c(2, 3, 1),]

#katalizator <- c()

#for (i in 1:3) {
#for (j in 1:3) {
#if (i < j) {
#k <- R[i,j] < 0 | R[i,j] > (R0[i] / R0[j])
#k <- paste(i, "-", j, k)
#katalizator <- c(katalizator, k)
#}
#}
#}

# natężenie efektu katalizy

ni <- summary(model2)$r.squared - Hellwig$h[Hellwig$k == "1-2-4-5-6-7"]
ni / summary(model2)$r.squared * 100 

# natężenie 17% występuje silny efekt katalizy

#### badanie koincydencji ####

sign(cor(dane_treningowe$stopa_inflacji, dane_treningowe %>%
           select(-c(indeks, stopa_inflacji)))) & sign(model2$coefficients[2:7])

# model jest koincydencyjny

#############################





### podpunkt 6

#PREZENTACJA MODELU
summary(model2)

#model
# stopa_inflacji = 0,7854 + 0,1161 * ceny_importowe + 0,1807 * indeks_cen_producentow - 0,6199 * wzrost_kredytow - 0,6682 * stopa_bezrobocia - 0,0053 * indeks_zew + 0,0114 * ropa_brent + błąd losowy



#Dopasowanie modelu
#R^2: 0.6409 – około 64% wariancji stopy inflacji jest wyjaśniane przez model.

#Dostosowany R^2: 60.1% – uwzględnia liczbę predyktorów, więc model ma dość dobre dopasowanie. Nie odbiega znacząco od zwykłego R^2.

#statystyka F : 16.06 (p-value = 1.699e-10 < 0.05) – model jest statystycznie istotny jako całość.




### podpunkt 7


# Obliczenia prognoz i błędów
prognoza <- predict(model2, newdata = dane_testowe)
wzg_bledy <- abs((dane_testowe$stopa_inflacji - prognoza) / dane_testowe$stopa_inflacji) * 100
MAPE <- mean(wzg_bledy)

#Wykres

y_min <- min(min(dane_testowe$stopa_inflacji), min(prognoza)) * 1.1
y_max <- max(max(dane_testowe$stopa_inflacji), max(prognoza)) * 1.1 

plot(dane_testowe$indeks, dane_testowe$stopa_inflacji, 
     type = "o", col = "blue", pch = 19, lwd = 2, 
     ylim = c(y_min, y_max), 
     xlab = "Okres", ylab = "Stopa inflacji (%)",
     main = paste("Prognoza vs rzeczywiste wartości\nMAPE =", round(MAPE, 2), "%"))
lines(dane_testowe$indeks, prognoza, type = "o", col = "red", lwd = 2, lty = 2, pch = 17)
grid()


legend("top", 
       legend = c("Rzeczywista", "Prognoza"),
       col = c("blue", "red"), 
       lty = c(1, 2), pch = c(19, 17),
       lwd = 2, bty = "n")

#etykiety procentów w danych kwartałach
text(dane_testowe$indeks, pmin(dane_testowe$stopa_inflacji, prognoza), 
     labels = paste0(round(wzg_bledy, 1), "%"), 
     pos = 3, cex = 0.8, col = "darkgray")

