# Autor
Projekt autorstwa Wojciecha Brąszkiewicza.<br/>
wojciechbraszkiewicz96@gmail.com

# Wytyczne
Dla wybranych zmiennych z [Polskiego Generalnego Sondażu Społecznego](http://www.ads.org.pl/opis-szczeg.php?id=91) z 2010 roku przedstawię następujące zagadnienia:

* Oczyszczenie danych
* Wizualizacja dwóch zmiennych jakościowych,
* Test niezależności χ2,
* Wizualizacja zmiennej jakościowej i ilościowej,
* Test t-średnich lub rang,
* Badanie hipotez,
* Test na "normalność" zmiennych.

W zbiorze danych wartości zmiennych jakościowych są zastąpione przez ich kody. W dokumentacji badania w pliku `P0091DOC` dostępne są kwestionariusze, z których można odczytać oryginalne wartości cech.

## Polski Generalny Sondaż Społeczny (PGSS)
Polski Generalny Sondaż Społeczny od 1992 roku jest stałym programem badań statutowych Instytutu Studiów Społecznych Uniwersytetu Warszawskiego, finansowanym przez Ministerstwo Nauki i Szkolnictwa Wyższego. 

Ogólnym celem PGSS jest systematyczny pomiar trendów i skutków zmian społecznych w Polsce. Problematyka PGSS obejmuje badanie indywidualnych postaw, cenionych wartości, orientacji i zachowań społecznych, jak również pomiar zróżnicowania społeczno-demograficznego, zawodowego, edukacyjnego i ekonomicznego reprezentatywnych grup i warstw społecznych w Polsce. Coroczny, a od 1997 roku dwu-trzy letni, cykl powtarzania badań, zachowujących porównywalne standardy metodologiczne i identyczne wskaźniki, umożliwia systematyczną analizę trendów społecznych. Pod tym względem PGSS jest unikalnym programem badania zmian systemowych w Polsce. Dane PGSS z lat 1992-2002 pochodzą z indywidualnych wywiadów z ogólnopolską, reprezentatywną próbą dorosłych członków gospodarstw domowych, zaś dane od 2005 roku z wywiadów uzyskanych od osób wylosowanych z bazy PESEL. Zintegrowany zbiór danych PGSS z lat 1992-2010 obejmuje 17497 respondentów oraz blisko 1680 zmiennych (w tym około 500 jest powtarzanych w kolejnych latach). 

## Wymagania techniczne

Projekt został stworzony w [języku R](https://www.r-project.org/), w środowisku [R Studio](https://www.rstudio.com/). Jeśli będą Państwo chcieli wprowadzić jakieś zmiany w kodzie to należy pobrać oba te elementy.


# Kod

Po otworzeniu R studio należy stworzyć nowy projekt oraz skrypt R (lub R markdown).

Na początku należy zainstalować oraz zaimportować wszystkie wymagane biblioteki, z których będziemy korzystać.

``` r
install.library(tidyverse)
install.library(survey)
install.library(ggplot2)
install.library(magrittr)
install.library(dplyr)
install.library(plotly)

library(tidyverse)
library(survey)
library(ggplot2)
library(magrittr)
library(dplyr)
library(plotly)
```

Nie będę na tym etapie opisywał tych bibliotek. Dokumentację każdej z nich można bez problemu znaleźć w internecie.


Zacznijmy zatem od zaimportowaniu zbioru danych `pgss`:

``` r
load("pgss.RData")
```

W pierwszym kroku należy zastanowić się nad wyborem odpowiednich zmiennych do badania. Przypominam, że plik `pgss` zawiera **okrojoną** wersję badania z 2010 roku. Zawiera on 1263 obserwacje 51. zmiennych.



## Przerobienie danych

W pliku `P0091DOC` znajduje się opis każdej zmiennej. Po przeanalizowaniu tego dokumentu wybrałem cztery przykładowe zmienne przykładowe zmienne:
Do badania dwóch zmiennych jakościowych wybrałem zmienne:
* q87c - Zadowolenie z życia w rodzinie. (Wartości od 1 do 8)
* q107 - Stosunek do bicia dzieci. (Wartości od 1 do 8)

Do badania zmiennej jakościowej i ilościowej wybrałem:
* q144gx - Dostęp do internetu w gospodarstwie domowym (TAK/NIE)
* q9age - Wiek

Dodatkowo wzięto pod uwagę zmienną **q8** oznaczającą płeć respondenta.


W oparciu o dokumentację badania na początku zamienimy wartości zmiennych, by liczby odpowiadały odpowiedniemu parametrowi w następujący sposób:
``` r
pgss <- pgss %>%
  mutate(one = 1,
         q87c = factor(pgss$q87c, levels = c(1, 2, 3, 4, 5, 6, 8), labels = c("Bardzo zadowolony", "Zadowolony", "Raczej zadowolony",
                                                                              "Raczej niezadowolny", "Niezadowolony", "Bardzo niezadowolony", "Nie wiem")),
         q107 = factor(pgss$q107, levels = c(1, 2, 3, 4, 8), labels = c("Zdecydowanie się zgadzam.",
                                                                        "Zgadzam się",
                                                                        "Nie zgadzam się",
                                                                        "Zdecydowanie się nie zgadzam",
                                                                        "Nie wiem")),
         
         q8 = factor(q8, levels = c(1, 2), labels = c("Mężczyzna", "Kobieta")),
         q144gx = factor(pgss$q144gx, levels = c(0, 1), labels = c("Brak dostępu do internetu", "Internet w domu")),
         ordered = T)
```


Na tym etapie mógłbym również zastąpić nazwy kolumn, tak by reprezentowały zmienne w następujący sposób:
``` r
colnames(pgss)[colnames(pgss)=="q87c"] <- "Zadowolenie z życia w rodzinie"
colnames(pgss)[colnames(pgss)=="q107"] <- "Stosunek do przemocy w rodzinie"
colnames(pgss)[colnames(pgss)=="q8"] <- "Plec"
colnames(pgss)[colnames(pgss)=="q144gx"] <- "Dostęp do internetu"
```
Natomiast zrobię to dopiero na samym końcu aby ułatwić pisanie kodu.

### Oczyszczenie danych

Bardzo ważnym elementem każdego badania jest sprawdzenie jakości danych. Ważne jest, żeby usunąć rekordy mogące istotnie wpływać na wyniki badania oraz pozbycie się niepotrzebnych elementów. W tym kroku zajmę się usunięciem ze zbioru elementów gdzie występuje brak danych (N/A).

Zacznę od sprawdzenia czy w ogóle zachodzi potrzeba sprawdzenia jakości danych. Warto zauważyć, że warto spojrzeć własnym okiem i spróbować znaleźć błędy tworzącego dany zbiór (np ktoś mógł w dacie wpisać rok 3020 przez pomyłkę, bądź w danych finansowych dopisać jedno zero co zniekształca dane.)

Zastosuję następujące funkcje:

``` r
any(is.na(pgss[]))
[1] TRUE ### Zbior pgss zawiera braki danych.
sum(is.na(pgss))
[1] 3204 ### W zbiorze pgss znaleziono 3204 braki danych.
```

Sprawdźmy teraz ile braków występuje w każdej z badanych zmiennych jakościowych:

``` r
sum(is.na(pgss$q87c))
[1] 1 ### Kolumna Zadowolenie z życia w rodzinie zawiera 1 brak danych.
sum(is.na(pgss$q107)) 
[1] 2 ### Kolumna Stosunek do przemocy w rodzinie zawiera 2 braki danych.
```
Pojedyncze braki wartości nie będą istotną przeszkodą w badaniu, ale i tak je usuniemy w późniejszym kroku. Póki co warto zobaczyć w takim razie które kolumny zawierają najwięcej braków:

```r
ColSums(is.na(pgss))
recordid   weight  voiev16  region6     size   hompop   adults       q8    q9age     q14a     q16a      q21     q22g 
       0        0        0        0        0        0        3        0        0      115       17      173      414 
     q32      q34      q37     q61e     q61h     q61r     q87a     q87b     q87c     q87d     q87e     q104     q105 
     674      644      604        0        1        2        0        1        1        0        0        1        5 
    q107     q110     q111    q144a    q144b    q144c    q144d    q144e    q144f    q144g   q144gx    q144h    q144i 
       2        2        1        1        1        1        1        1        1        1        1        1        1 
   q144j    q144k    q144l    q144m    q144n    q144o     in1f     in4e     in5e     in7a      one  ordered 
       1        1        1        1        1        1        0      307      220        1        0        0 
```

Jak widać zmienne np. q32, q34, q37, q22g, q21, q14a zawierają bardzo dużo braków. Mogą one stanowić poważny problem w badaniu, wiec wyeliminujemy wszystkie braki:

```r
pgss_clean <- pgss[ ,colSums(is.na(pgss)) < 20]
pgss_clean <- na.omit(pgss_clean)

any(is.na(pgss_clean[]))
[1] FALSE
sum(is.na(pgss_clean))
[1] 0
```

Jak widać, nowy zbiór `pgss_clean` nie zawiera już żadnych braków. Możemy przejść do wizualizacji.

### Wizualizacja dwóch zmiennych jakościowych

Zacznijmy od zbudowania _schematu_ za pomocą funkcji _svydesign_ oraz _svyby_ z pakietu _survey_, które posłużą nam do zdefiniowania modelu:

```r
schemat <- svydesign(ids = ~recordid, weights = ~weight, data = pgss_clean)

q87c_q107 <- svyby(formula = ~one, by = ~q87c + ~q107, design = schemat, FUN = svytotal)
```

W tym kroku możemy już przejść do zbudowania wykresu. W tym projekcie wykorzystam pakiety _ggplot2_ oraz _pyplot_.
Wykorzystam do tego wykres kafelkowy:

```r
ggplot(q87c_q107, aes(x = q107, y = q87c, fill = one)) +
  geom_tile() +
  geom_label(aes(label = round(one)), color = "black") +
  scale_fill_gradient2(low = "white", high = "pink", mid = "steelblue", midpoint = 90) +
  xlab("Zgoda na bicie dzieci") + ylab("Zadowolenie z zycia rodzinnego") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
```

![alt text](https://i.imgur.com/NbPgwlJ.jpg "Logo Title Text 1")

Wykres przedstawia każdą możliwą kombinacje badanych zmiennych oraz ile razy ankietowani ją wybrali.
Z wykresu widać zdecydowane skupienie odpowiedzi, które mówi, że osoby które nie akceptują przemocy w rodzinie są szczęśliwe bądź bardzo szczęśliwe co było do przewidzenia.
Widać również wyraźną korelację zmiennych, choć warto się upewnić stosując odpowiednią formułę na test niezależności χ2:

```r
svychisq(formula = ~q87c + q107, design = schemat, statistic = "Chisq")

##Pearson's X^2: Rao & Scott adjustment
##
##data:  svychisq(formula = ~q87c + q107, design = schemat, statistic = "Chisq")
##X-squared = 41.06, df = 24, p-value = 0.02069
```

Przy poziomie istotności 5% możemy śmiało stwierdzić, że odrzucamy hipotezę zerową na rzecz alternatywnej. Zatem te dwie zmienne faktycznie okazały się **ze sobą skorelowane**.


### Wizualizacja zmiennej jakościowej i ilościowej.

Kolejnym krokiem będzie pokazanie zależności pomiędzy dwoma zmiennymi o innym charakterze. 

Przypomnę, że w tym kroku wykorzystam zmienne:
* q144gx - Dostęp do internetu w gospodarstwie domowym (TAK/NIE)
* q9age - Wiek

Jako, że dane oczyściliśmy już wcześniej to możemy od razu przejść do budowania wykresu. Tym razem wykorzystam wykres skrzypcowy.

```r
p <- ggplot(pgss_clean, aes(x = q144gx, y = q9age, fill = q144gx), na.rm = TRUE) +
  geom_violin(trim = FALSE) +
  scale_fill_brewer(type = "seq", palette = 3, direction = 1,
                    aesthetics = "colour") +
  xlab("Dostęp do intenetu w gospodarstwie domowym") +
  ylab("Srednia wieku respodenta") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  geom_boxplot(width = 0.2) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank())
```

![alt text](https://i.imgur.com/uhuacbG.jpg "Violin plot")

Wykres ten pokazuje dokładnie rozkład zmiennych, wraz z wartością mediany oraz kwantyli. 
Niestety nie pozwala on nam na dokładne odczytanie wartości a jedynie w przybliżeniu (np. Dla pierwszego wykresu widać, że mediana wynosi ok. 60 lat, natomiast nie znamy dokładnej wartości).
Do poznania dokładnych wartości dla każdej zmiennej warto wykorzystać pakiet _plotly_, który pozwala na tworzenie interaktywnych wykresów. Pakiet ten niestety jednak nie jest obsługiwany przez githuba, więc jestem zmuszony odesłać Państwa **[tutaj](http://rpubs.com/WojciechBraszkiewicz/VC1)**, gdzie można zobaczyć efekt końcowy dla tego wykresu. Wynika z niego, że mediana dla 1. Wykresu wynosi równo 59 a dla drugiego 37. Można tam również odczytać dodatkowe statystyki.

### Test na rozkład zmiennych

W tym kroku przedstawię [Test Shapiro-Wilka](https://pl.wikipedia.org/wiki/Test_Shapiro-Wilka), który bada czy rozkład danej zmiennej jest normalny.
Test przeprowadzimy dla zmiennej "Wiek" w połączeniu z wartościami zmiennej "Dostęp do internetu":

```r
shapiro.test(pgss_clean$q9age[pgss_clean$q144gx == "Brak dostępu do internetu"])
##Shapiro-Wilk normality test
##data:  pgss_clean$q9age[pgss_clean$q144gx == "Brak dostępu do internetu"]
##W = 0.94359, p-value = 2.074e-12

shapiro.test(pgss_clean$q9age[pgss_clean$q144gx == "Internet w domu"])
##Shapiro-Wilk normality test
##data:  pgss_clean$q9age[pgss_clean$q144gx == "Internet w domu"]
##W = 0.95942, p-value = 1.207e-13
```

Jak widać, w obu przypadkach **mamy podstawy do odrzucenia hipotezy zerowej**. W związku z tym zakładamy, że oba rozkłady **nie są** normalne.


### Test rang
Jako, że zmienna "Dostęp do internetu" przyjmuje jedynie dwie wartości mamy wskazania do przeprowadzenia testu rang. Skorzystamy zatem z funkcji _svyranktest_:

```r
svyranktest(formula = q9age~q144gx, design = schemat)

##Design-based KruskalWallis test
##data:  q9age ~ q144gx
##t = -18.604, df = 1260, p-value < 2.2e-16
##alternative hypothesis: true difference in mean rank score is not equal to 0
##sample estimates:
##difference in mean rank score 
##                  -0.2848898
```

W związku z powyższym mamy podstawy do odrzucenia hipotezy zerowej na rzecz alternatywnej. Mówimy zatem, że średnie wieku w grupach dostępu do internetu **są statystycznie różne**.



# Podsumowanie

Ten projekt miał za zadanie przedstawić podstawowe zagadnienia związane z programowaniem w pakiecie R oraz statystyką. Na podstawie zbioru danych PGSS można przeprowadzić również wiele innych badań, które mogą doprowadzić do ciekawych wniosków. Jeżeli państwo są zainteresowani kontaktem, proszę o wiadomość na githubie. 

Dziękuję za uwagę.
