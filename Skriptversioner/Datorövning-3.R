# Slumpvariabler
## Repetition från datorövning 2

# install.packages("tidyverse")
library(tidyverse)

# install.packages("gapminder")
library(gapminder)

gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(`Livslängd, medel` = mean(lifeExp),
            `Befolkning, median` = median(pop),
            `Bnp per capita, standardavvikelse` = sd(gdpPercap))

ggplot(gapminder, aes(lifeExp, continent, fill = continent)) +
  geom_boxplot() +
  facet_wrap(~ year)

dat_sum <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(Mean = mean(lifeExp),
            SD = sd(lifeExp))
dat_sum

ggplot(dat_sum, aes(continent, Mean, fill = continent)) +
  geom_col() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.3) +
  facet_wrap(~ year)

## Särskilda diskreta fördelningar: binomialfördelning

dbinom(3, size = 10, prob = 1/2)

pbinom(3, size = 10, prob = 1/2)

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 1/2),
         P = pbinom(x, size = 10, prob = 1/2))

g1 <- ggplot(dat_bin, aes(x, p, label = round(p, 3))) + 
  geom_col() + 
  geom_text(nudge_y = 0.01) +
  labs(title = "Sannolikhetsfunktion, P(X = x)")
g2 <- ggplot(dat_bin, aes(x, P, label = round(P, 3))) + 
  geom_col() + 
  geom_text(nudge_y = 0.01) +
  labs(title = "Fördelningsfunktion, P(X ≤ x)")

# install.packages("patchwork")
library(patchwork)
g1 / g2

##### Övningsuppgift 3.1 #####
# Följande uppgift kan lösas på *klassiskt vis* genom att slå upp sannolikheter i en tabell. För en viss frösort är sannolikheten att gro 60 %. Om man sår 10 slumpmässigt valda frön, hur stor är då chansen att högst 6 frön gror? Att minst 8 frön gror?
##########

##### Övningsuppgift 3.2 #####
# Vad måste läggas till i stycket nedan för att besvara frågan om grobarhet?

pbinom(___, 10, prob = 0.6)
1 - pbinom(7, size = ___, prob = ___)

##########

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 0.6),
         P = pbinom(x, size = 10, prob = 0.6))

g1 <- ggplot(dat_bin, aes(x, p, label = round(p, 3), fill = x <= 6)) + 
  geom_col() + 
  geom_text(nudge_y = 0.01) +
  labs(title = "Sannolikhetsfunktion, P(X = x)")
g2 <- ggplot(dat_bin, aes(x, P, label = round(P, 3), fill = x == 6)) + 
  geom_col() + 
  geom_text(nudge_y = 0.01) +
  labs(title = "Fördelningsfunktion, P(X ≤ x)")

# install.packages("patchwork")
library(patchwork)
g1 / g2 & theme(legend.position = "none")

##### Övningsuppgift 3.3 #####
# Gör lämpliga ändringar i kodstycket ovan för att illustrera sannolikheten att minst 8 frön gror.
##########

##### Övningsuppgift 3.4 #####
# Beräkna populationsmedelvärde och populationsvarians för antalet frön som gror.
##########

## Särskilda diskreta fördelningar: poissonfördelning

##### Övningsuppgift 3.5 #####
# Följande uppgift kan lösas på *klassiskt vis* genom att slå upp sannolikheter i en tabell.
# Sannolikheten att en individ i en viss population skall vara albino är 1/20000. 
# Hur stor är sannolikheten att på 40 000 födslar ingen albino föds? Minst en albino föds?
##########

dpois(2, lambda = 2)

##### Övningsuppgift 3.6 #####
# Använd `dpois()` och `ppois()` för att beräkna sannolikhterna för ingen albino och minst en albino.
##########

dat_pois <- data.frame(x = 0:15) %>% 
  mutate(p = dpois(x, lambda = 4),
         P = ppois(x, lambda = 4))

g1 <- ggplot(dat_pois, aes(x, p, label = round(p, 3), fill = x <= 3)) + 
  geom_col() + 
  geom_text(nudge_y = 0.01) +
  labs(title = "Sannolikhetsfunktion, P(X = x)")
g2 <- ggplot(dat_pois, aes(x, P, label = round(P, 3), fill = x == 3)) + 
  geom_col() + 
  geom_text(nudge_y = 0.01) +
  labs(title = "Fördelningsfunktion, P(X ≤ x)")

g1 / g2 & theme(legend.position = "none")

##### Övningsuppgift 3.7 #####
# Gör ändringar i kodstycket ovan för att visa sannolikheterna från albinofrågan.
##########

dat_preus <- data.frame(Deaths = 0:6,
                        Frequency = c(109, 65, 22, 3, 1, 0, 0))

##### Övningsuppgift 3.8 #####
# Fyll i det saknade lambda-värdet i stycket nedan. Ligger de förväntade värdena nära de faktiska?

dat_preus <- dat_preus %>% 
  mutate(Expected = 200 * dpois(Deaths, lambda = ___))
dat_preus

##########

##### Övningsuppgift 3.9 #####
# Utfallet kan illustreras med en graf med de faktiska värdena som staplar och de förväntade antalen som punkter.

ggplot(dat_preus, aes(x = Deaths)) +
  geom_col(aes(y = Frequency)) +
  geom_point(aes(y = Expected))

# Är poissonfördelningen en passande fördelning för datan?
##########

## Kontinuerliga fördelningar i allmänhet
## Särskilda kontinuerliga fördelningar: normalfördelningen

ggplot() +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 2), color = "red", linewidth = 2) +
  geom_function(fun = dnorm, args = list(mean = 3, sd = 1), color = "blue", linewidth = 2) +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 1), color = "green3", linewidth = 2) +
  xlim(0, 10)

##### Övningsuppgift 3.10 #####
# Gör lämpliga ändringar i stycken ovan för att illustrera två normalfördelningar: en med medelvärde 0 och standardavvikelse 1 och en med medelvärde 1 och standardavvikelse 2. Kan du utifrån kurvorna säga vilken av de två fördelningarna som ger störst sannolikhet att få ett utfall under minus två?
##########

pnorm(1, mean = 2, sd = 3)

x_value <- 1
mu <- 2
sigma <- 3

P_value <- pnorm(x_value, mean = mu, sd = sigma)
P_value

dat_norm <- data.frame(x = seq(from = mu - 4 * sigma, to = mu + 4 * sigma, 0.1)) %>% 
  mutate(p = dnorm(x, mean = mu, sd = sigma),
         P = pnorm(x, mean = mu, sd = sigma))

g1 <- ggplot(dat_norm, aes(x, p)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = p), data = dat_norm %>% filter(x < x_value), fill = "salmon", color = "black") +
  labs(title = "Täthetsfunktion")

g2 <- ggplot(dat_norm, aes(x, P)) +
  geom_line() +
  annotate("segment", x = x_value, y = 0, xend = x_value, yend = P_value) +
  annotate("segment", x = x_value, y = P_value, xend = -Inf, yend = P_value) +
  labs(title = "Fördelningsfunktion")

g1 / g2

##### Övningsuppgift 3.11 #####
# Följande uppgift kan lösas på *klassiskt vis* genom att slå upp sannolikheter i en tabell. 
# Använd standardisering för att avgöra om det är rimligt att få en individ med vikten 150 kg om man slumpmässigt drar ur en population som är normalfördelad med populationsmedelvärdet 100 kg och populationsstandardavvikelsen 30 kg?
##########

##### Övningsuppgift 3.12 #####
# Sannolikheten från viktfrågan kan antingen tas fram efter standardisering eller genom att ange populationsmedelvärde och -standardavvikelse i `pnorm()`. Fyll i de saknade delarna nedan för att få samma sannolikhet som i lösningen för hand.

pnorm(___)
pnorm(150, mean = ___, sd = ___)

##########

##### Övningsuppgift 3.13 #####
# I kodstycket ovan gavs en figur av normalfördelningen. Sätt värdena för `x_value`, `mu` och `sigma` så att grafen illustrerar övningen med vikter.
##########

##### Övningsuppgift 3.14 #####
# Använd en normalfördelningstabell för att ta fram följande sannolikheter. 
# Z är en standardiserad normalfördelning (medelvärde 0 och standardavvikelse 1). Beräkna

# - P(Z ≤ 0.75),
# - P(Z < 0.75),
# - P(Z > 0.75),
# - P(Z > 0),
# - P(0.30 < Z < 2.45).

# Gör alltid en skiss av fördelningen och den sökta arean.
##########

##### Övningsuppgift 3.15 #####
# Beräkna sannolikheterna i uppgiften ovan med hjälp av `pnorm()` i R.
##########

qnorm(0.01)

##### Övningsuppgift 3.16 #####
# Använd en normalfördelningstabell för att ta fram följande värden. 
# Z är en standardiserad normalfördelning (medelvärde 0 och standardavvikelse 1).
# Bestäm värdet på w så att P(-w < Z < w) = 0.99.
##########

##### Övningsuppgift 3.17 #####
# Fyll i stycket nedan för att hitta w.

qnorm(___)

##########

##### Övningsuppgift 3.18 #####
# Slumpvariabeln Y är normalfördelad med medelvärde 2 och varians 9. Beräkna följande för hand.

# - P(Y ≤ 2.75),
# - P(Y > 2.75),
# - P(2.30 < Y < 2.45),
# - P(Y > -0.02).
##########

pnorm(1, mean = 2, sd = 3)

##### Övningsuppgift 3.19 #####
# Beräkna sannolikheterna från uppgiften ovan med `pnorm()`.
##########

qnorm(0.9, mean = 5, sd = 4)

##### Övningsuppgift 3.20 #####
# Y är normalfördelad med medelvärde 2 och varians 9. Bestäm värdet på a som uppfyller F(a) = 0.95. F(a) är kortform för P(Y < a).

# Gör först beräkningen för hand och sedan med `qnorm()`.
##########

## Bonus. Summan av två slumpvariabler med slumptal

dat_pois <- data.frame(x = rpois(10000, lambda = 3)) %>% 
  count(x) %>% 
  mutate(p = n / sum(n),
         theoretical_p = dpois(x, lambda = 3))

ggplot(dat_pois, aes(x, p)) +
  geom_point() +
  geom_line(aes(y = theoretical_p))

##### Övningsuppgift 3.21 #####
# Gör lämpliga ändringar i kodstycket ovan för att göra motsvarande beräkning för en poissonfördelning med lambda lika med 2. Ligger slumptalen nära den teoretiska fördelningen?
##########

dat_pois <- data.frame(x1 = rpois(10000, lambda = 3),
                       x2 = rpois(10000, lambda = 2)) %>% 
  mutate(x = x1 + x2) %>% 
  count(x) %>% 
  mutate(p = n / sum(n),
         theoretical_p = dpois(x, lambda = 5))

ggplot(dat_pois, aes(x, p)) +
  geom_point() +
  geom_line(aes(y = theoretical_p))

dat_dice_sum <- data.frame(x1 = sample(c(1,2,3,4,5,6), size = 10000, replace = T),
                           x2 = sample(c(1,2,3,4,5,6), size = 10000, replace = T)) %>% 
  mutate(x = x1 + x2) %>% 
  count(x) %>% 
  mutate(p = n / sum(n))

ggplot(dat_dice_sum, aes(x, p)) +
  geom_col()

##### Övningsuppgift 3.22 #####
# Gör lämpliga ändringar i stycket ovan för att beräkna summan av två utfall för en åtta-sidig tärning.
##########

##### Övningsuppgift 3.23 #####
# Gör lämpliga tillägg i stycket ovan för att beräkna summan av tre utfall för en sex-sidig tärning.
##########

## Bonus. Cirkelns area från slumptal

# install.packages("ggforce")
library(ggforce)
g <- ggplot() + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) + 
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1), fill = NA, color = "black")
g

dat_random <- data.frame(x = runif(100, min = -1, max = 1),
                         y = runif(100, min = -1, max = 1))

g + geom_point(aes(x, y), data = dat_random)

dat_random <- data.frame(x = runif(100, min = -1, max = 1),
                         y = runif(100, min = -1, max = 1)) %>% 
  mutate(inner = x^2 + y^2 < 1)

4 * mean(dat_random$inner)
pi

##### Övningsuppgift 3.24 #####
# Beräkningens precision ökar med antalet slumptal. Vad i kodstycket ovan ska ändras för att generera fler slumptal? Blir utfallet närmre det väntade värdet om antalet slumptal ökar?
##########

## Bonus. Slump och ordning - Sierpinski-triangeln

# Ta ut tre punkter
x_original <- c(0,1,2)
y_original <- c(0,2,0)

# Välj en punkt inom triangeln
x_new <- 0.4
y_new <- 0.2
dat_tri <- data.frame(x = x_new, y = y_new)

n <- 100

for(i in 1:n){
  new_point <- sample(c(1,2,3), 1)
  x_new <- (x_new + x_original[new_point]) / 2
  y_new <- (y_new + y_original[new_point]) / 2
  
  dat_tri <- bind_rows(dat_tri, data.frame(x = x_new, y = y_new))
}

ggplot(dat_tri, aes(x, y)) +
  geom_point()

##### Övningsuppgift 3.25 #####
# Vad måste ändras i stycket ovan för att generera fler punkter? Tiotusen kan vara ett lämpligt antal för en tydligare illustration. Vad händer om man ändrar värdena i `x_original` och `y_original`?
##########

