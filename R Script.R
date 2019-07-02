install.packages("ggplot2")
install.packages("hexbin")
install.packages("survey")
install.packages("plotly")
library(tidyverse)
library(survey)
library(ggplot2)
library(magrittr)
library(dplyr)
library(plotly)

load("pgss.RData")

pgss <- pgss %>%
  mutate(one = 1,
         q87c = factor(pgss$q87c, levels = c(1, 2, 3, 4, 5, 6, 8), labels = c("Bardzo zadowolony", "Zadowolony", "Raczej zadowolony",
                                                                              "Raczej niezadowlony", "Niezadowolony", "Bardzo niezadowolony", "Nie wiem")),
         q107 = factor(pgss$q107, levels = c(1, 2, 3, 4, 8), labels = c("Zdecydowanie się zgadzam.",
                                                                        "Zgadzam się",
                                                                        "Nie zgadzam się",
                                                                        "Zdecydowanie się nie zgadzam",
                                                                        "Nie wiem")),
         
         q8 = factor(q8, levels = c(1, 2), labels = c("Mężczyzna", "Kobieta")),
         q144gx = factor(pgss$q144gx, levels = c(0, 1), labels = c("Brak dostępu do internetu", "Internet w domu")),
         ordered = T)

#Sprawdzmy czy sa NA
any(is.na(pgss[]))
sum(is.na(pgss))

sum(is.na(pgss$q87c))
sum(is.na(pgss$q107))
colSums(is.na(pgss))

pgss_clean <- pgss[ ,colSums(is.na(pgss)) < 20]
pgss_clean <- na.omit(pgss_clean)

any(is.na(pgss_clean[]))
sum(is.na(pgss_clean))



colnames(pgss)
colnames(pgss)[colnames(pgss)=="q87c"] <- "Zadowolenie z zycia w rodzinie"
colnames(pgss)[colnames(pgss)=="q107"] <- "Stosunek do przemocu w rodzinie"
colnames(pgss)[colnames(pgss)=="q8"] <- "Plec"
colnames(pgss)[colnames(pgss)=="q144gx"] <- "Dostęp do internetu"

colnames(pgss)[colnames(pgss)=="Zadowolenie z zycia w rodzinie"] <- "q87c"
colnames(pgss)[colnames(pgss)=="Stosunek do przemocu w rodzinie"] <- "q107"
colnames(pgss)[colnames(pgss)=="Plec"] <- "g8"
colnames(pgss)[colnames(pgss)=="Dostęp do internetu"] <- "q144gx"

schemat <- svydesign(ids = ~recordid, weights = ~weight, data = pgss_clean)

q87c_q107 <- svyby(formula = ~one, by = ~q87c + ~q107, design = schemat, FUN = svytotal)


ggplot(q87c_q107, aes(x = q107, y = q87c, fill = one)) +
  geom_tile() +
  geom_label(aes(label = round(one)), color = "black") +
  scale_fill_gradient2(low = "white", high = "pink", mid = "steelblue", midpoint = 90) +
  xlab("Zgoda na bicie dzieci") + ylab("Zadowolenie z zycia rodzinnego") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")


svychisq(formula = ~q87c + q107, design = schemat, statistic = "Chisq")

# Wnioski ? Odrzucamy H0 na rzecz H1.Pomiedzy zmiennymi jest korelacja.

####CZ 2

p <- ggplot(pgss_clean, aes(x = q144gx, y = q9age, fill = q144gx), na.rm = TRUE) +
  geom_violin(trim = FALSE) +
  scale_fill_brewer(type = "seq", palette = 3, direction = 1,
                    aesthetics = "colour") +
  xlab("Dostęp do intenetu w gospodarstwie domowym") +
  ylab("Srednia wieku respodenta") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_boxplot(width = 0.2) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank())

p

p1 <- plot_ly(pgss_clean, x = pgss_clean$q144gx, y = pgss_clean$q9age, fill = pgss_clean$q144gx, type = "violin", color = ~pgss_clean$q144gx) 
p1

# Test shapiro
shapiro.test(pgss_clean$q9age[pgss_clean$q144gx == "Brak dostępu do internetu"])
shapiro.test(pgss_clean$q9age[pgss_clean$q144gx == "Internet w domu"])
# p value male, odrzucame h0 na rzecz h1.Rozklad nie jest normalny.

svyranktest(formula = q9age~q144gx, design = schemat)
# Odrzucamy h0 na rzecz H1.Sredni wiek rozni sie w grupach dostępu do internetu.