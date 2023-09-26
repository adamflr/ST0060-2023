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

## Diskreta fördelningar i allmänhet

dat_dice6 <- data.frame(x = c(1,2,3,4,5,6),
                        p = 1/6)
dat_dice6

ggplot(dat_dice6, aes(x, p)) + geom_col()

##### Övningsuppgift 3.1 #####
# Ta fram en tärning som *inte* har sex sidor. Följ exemplet ovan för att kodifiera din tärning som en slumpvariabel. Ett bra namn på det nya objektet kan vara `dat_diceN` där N anger antalet sidor på tärningen, t.ex. `dat_dice20` för en 20-sidig tärning.
##########

dat_dice6 %>% 
  mutate(x_times_p = x * p) %>% 
  summarise(Expected_value = sum(x_times_p))

##### Övningsuppgift 3.2 #####
# Upprepa beräkningen ovan, denna gång med den slumpvariabel du kodifierade i den tidigare uppgiften.
##########

pop_var <- function(x, p) sum(p * (x - sum(x * p))^2)

dat_dice6 %>% 
  summarise(varians = pop_var(x, p))

##### Övningsuppgift 3.3 #####
# Upprepa beräkningen ovan, denna gång med den slumpvariabel du kodifierade i den tidigare uppgiften.
##########

##### Övningsuppgift 3.4 #####
# Kasta din tärning 20 gånger. Gärna på en mjuk yta. Skriv in utfallen i koden nedan och beräkna medelvärde och varians.

utfall <- c(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
mean(utfall)
var(utfall)
sd(utfall)

# Ligger medelvärde och varians från stickprovet nära de teoretiska beräkningarna?
##########

slumputfall <- sample(x = dat_dice6$x, size = 10000, replace = T)

mean(slumputfall)
var(slumputfall)

##### Övningsuppgift 3.5 #####
# Använd `sample()` för att dra tiotusen observationer från din egen tärning. Beräkna medelvärde och varians från det stickprovet. Är utfallen nära de teoretiska beräkningarna av populationsmedelvärde och -varians?
##########

## Särskilda diskreta fördelningar: binomialfördelning

dbinom(3, size = 10, prob = 1/6)

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 1/6))

ggplot(dat_bin, aes(x, p)) + geom_col()

##### Övningsuppgift 3.6 #####
# Ta tärningen från tidigare uppgift. Om man kastar tärningen tjugo gånger, vad är fördelningen för antalet gånger man får tärningens lägsta utfall? (Till exempel, vad är fördelningen för antalet ettor vid tjugo kast med en sexsidig tärning?) Fyll i stycket nedan för att beräkna sannolikheterna i den fördelningen och illustrera med ett stapeldiagram.

dat_bin <- data.frame(x = 0:20) %>% 
  mutate(p = dbinom(x, size = 20, prob = ___))

ggplot(dat_bin, aes(x, p)) + geom_col()

##########

##### Övningsuppgift 3.7 #####
# I den fördelning du beräknade i uppgiften ovan. Vad är sannolikheten att få exakt tre positiva utfall? (Ledning: för en sexsidig tärning skulle det ges av `dbinom(3, size = 20, prob = 1/6))`.)
##########

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 1/6),
         P = pbinom(x, size = 10, prob = 1/6))
dat_bin

##### Övningsuppgift 3.8 #####
# Använd binomialfördelningen från den tidigare uppgiften till att beräkna sannolikheten att få *tre eller färre* positiva utfall.
##########

pbinom(4, size = 10, prob = 1/6)

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 1/6),
         P = pbinom(x, size = 10, prob = 1/6))

ggplot(dat_bin, aes(x, p, fill = x <= 4)) +
  geom_col()

##### Övningsuppgift 3.9 #####
# Gör lämpliga ändringar i exemplet ovan för att illustrera sannolikheten att få exakt tre positiva utfall i exemplet med tärningskastet. 
##########

##### Övningsuppgift 3.10 #####
# I en tidigare uppgift kastade du tärningen tjugo gånger. Hur många gånger fick du det lägsta möjliga utfallet på tärningen (t.ex. en etta på en vanlig sex-sidig tärning)?
##########

##### Övningsuppgift 3.11 #####
# Beräkna medelvärde och varians för antalet ettor om man kastar en 16-sidig tärning tjugo gånger.
##########

##### Övningsuppgift 3.12 #####
# För en viss frösort är sannolikheten att gro 60%. Om man sår 10 slumpmässigt valda frön, hur stor är då chansen att

# a. högst 6 frön gror?
# b. minst 6 frön gror?
# c. Beräkna populationsmedelvärde och populationsvrians för antalet frön som gror.

# (Denna uppgift finns också i uppgiftsdokumentet och kan lösas för hand.)

##########

## Särskilda diskreta fördelningar: poissonfördelning

##### Övningsuppgift 3.13 #####
# Följande ger sannolikheten att få utfall 2 i en poissonfördelning med lambda satt till 4.

dpois(2, lambda = 4)

# Gör lämpliga ändringar för att beräkna sannolikheten för exakt 5 i en fördelning med lambda satt till 3.
##########

##### Övningsuppgift 3.14 #####
# Följande ger sannolikheten att få mindre än eller lika med 2 i en poissonfördelning med lambda satt till 4.

ppois(2, lambda = 4)

# Gör lämpliga ändringar för att beräkna sannolikheten för *mindre än eller lika med 5* i en fördelning med lambda satt till 3.
# Hur kan man beräkna sannolikheten att få mer än 5 i en fördelning med lambda satt till 3? 
##########

##### Övningsuppgift 3.15 #####
# Sannolikheten att en viss individ i en viss population skall vara albino är 1/20 000. Hur stor är sannolikheten att på 40 000 födslar

# a. ingen albino föds,
# b. minst en albino föds.
# c. Vilka antaganden skall vara uppfyllda för att sannolikheterna i a och b ska gälla?

# (Denna uppgift finns också i uppgiftsdokumentet och kan lösas för hand.)

##########

## Kontinuerliga fördelningar i allmänhet
## Särskilda kontinuerliga fördelningar: normalfördelningen

ggplot() +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 2), color = "red", size = 2) +
  geom_function(fun = dnorm, args = list(mean = 3, sd = 1), color = "blue", size = 2) +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 1), color = "green3", size = 2) +
  xlim(0, 10)

##### Övningsuppgift 3.16 #####
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

# install.packages("patchwork")
library(patchwork)
g1 / g2

##### Övningsuppgift 3.17 #####
# Fyll i kodstycket nedan för att beräkna sannolikheten att få ett värde under minus två i en normalfördelning med medelvärde 0 och standardavvikelse 1, och i en normalfördelning med medelvärde 1 och standardavvikelse 2.

pnorm(-2, mean = ___, sd = ___)
pnorm(-2, mean = ___, sd = ___)

##########

pnorm(1, mean = 0, sd = 1) - pnorm(-1, mean = 0, sd = 1)

x_values <- c(-1,1)
mu <- 0
sigma <- 1

dat_norm <- data.frame(x = seq(from = mu - 4 * sigma, to = mu + 4 * sigma, 0.1)) %>% 
  mutate(p = dnorm(x, mean = mu, sd = sigma))

ggplot(dat_norm, aes(x, p)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = p), data = dat_norm %>% filter(x < max(x_values) & x > min(x_values)), fill = "salmon", color = "black") +
  labs(title = "Täthetsfunktion")

##### Övningsuppgift 3.18 #####
# Fyll i kodstycket nedan för att beräkna sannolikheten att få ett värde mellan *minus två* och tre i en normalfördelning med medelvärde 1 och standardavvikelse 2.

pnorm(___, mean = ___, sd = ___) - pnorm(___, mean = ___, sd = ___)

##########

1 - pnorm(1.96)

##### Övningsuppgift 3.19 #####
# Fyll i kodstycket nedan för att beräkna sannolikheten att få ett värde över sju i en normalfördelning med medelvärde 3 och standardavvikelse 5.

1 - pnorm(___, mean = ___, sd = ___)

##########

pnorm(7, mean = 8, sd = 4)

pnorm(-0.25, mean = 0, sd = 1)

##### Övningsuppgift 3.20 #####
# En slumpvariabel Y följer en normalfördelning med medelvärde 2 och varians 9. Vad är sannolikheten att få

# - P(Y > 2.75),
# - P(Y < 2.75),
# - P(2.30 < Y < 2.45)?

# Beräkna först sannolikheten för hand och sedan med `pnorm()`.
# Notera att denna fråga finns bland instuderingsuppgifterna.
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

