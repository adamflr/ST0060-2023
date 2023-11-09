# Ett stickprov av normalfördelad data
## Repetition av datorövning 3

# install.packages("tidyverse")
library(tidyverse)

dbinom(3, 5, prob = 0.1)

dpois(5, lambda = 10)

1 - pnorm(13, mean = 10, sd = 2)

## Test av medelvärde för normalfördelad data

library(tidyverse)

dat <- data.frame(x = c(49.8, 58.4, 49.4, 57.1, 52.2, 49.1, 44.6, 55.4))
dat

ggplot(dat, aes(x, 0)) + 
  geom_point() +
  geom_vline(xintercept = 50, color = "red")

t.test(dat$x, mu = 50)   # Ett t-test på variabeln x i dataobjektet dat

##### Övningsuppgift 4.1 #####
# Hur hade hypoteserna sett ut om vi ville testa om medelvärdet är *större* än 50?
##########

mean(dat$x)
sd(dat$x)

t_value <- (52 - 50) / (4.680354 / sqrt(8))

##### Övningsuppgift 4.2 #####
# Räkna ut samma sak på miniräknare eller telefon. Vad händer om man missar parenteser runt `4.680354 / sqrt(8)`?
##########

##### Övningsuppgift 4.3 #####
# Vad händer med t-värdet om något av följande händer, givet att övriga delar är desamma? Det observerade medelvärdet (här 52) ökar. Nollhypotesens värde (här 50) minskar. Standardavvikelsen (här 4.680354) minskar. Antalet observationer (här 8) ökar. 

# Testa genom att ändra värdena i kodstycket ovan och beräkna `t_value` på nytt.
##########

dat_t <- data.frame(x = seq(-4, 4, 0.1)) %>% 
  mutate(p = dt(x, df = 7),
         P = pt(x, df = 7))

ggplot(dat_t, aes(x, p)) +
  geom_line()

ggplot(dat_t, aes(x, P)) +
  geom_line()

ggplot(dat_t) +
  geom_line(aes(x, p)) +
  geom_ribbon(aes(x = x, ymin = 0, ymax = p), data = dat_t %>% filter(x > abs(t_value)), fill = "salmon") +
  geom_ribbon(aes(x = x, ymin = 0, ymax = p), data = dat_t %>% filter(x < -abs(t_value)), fill = "salmon")

2 * pt(-abs(t_value), 7) # Tvåsidigt p-värde

##### Övningsuppgift 4.4 #####
# Om man gör ett t-test för hand kan man inte enkelt ta fram ett p-värde, men kan se om p-värdet är större eller mindre än fem procent genom att ställa testvärdet mot ett kritiskt värde. Använd en tabell för t-fördelning för att hitta det kritiska värdet.

# I R kan man ta fram kritiska värden med `qt()`. För fem procent i svansarna har man 0.025 i respektive svans och det kritiska värdet ges av

qt(0.975, 7)

##########

t.test(dat$x, mu = 50) # Tvåsidigt test

##### Övningsuppgift 4.5 #####
# Använd `?t.test` för att ta fram funktionens hjälpsida. Försök att utifrån hjälpsidan beräkna ett *ensidigt* test för att se om medelskörden är *större* än 50.
##########

##### Övningsuppgift 4.6 #####
# Upprepa det tvåsidiga testet från exemplet ovan. Testa denna gång om medelskörden är skild från 48. Dra en tydlig slutsats.
##########

##### Övningsuppgift 4.7 #####
# På canvassidan finns en excelfil med data för kursens uppgifter *Uppgiftsdata.xlsx*. Fliken *Smältdata* innehåller data för en legerings smältpunkt.

# Ladda ner filen till lämplig plats på datorn och importera datan genom att fylla i följande rad.

library(readxl)
dat_smält <- read_excel("___", sheet = "Smältpunkt")

##########

##### Övningsuppgift 4.8 #####
# Illustrera smältpunktsdatan på samma sätt som exempeldatan genom att fylla i följande kod. Vårt mål är att testa om medelvärde är skilt från 1050, vilket här kan noteras med ett vertikalt streck vid 1050.

ggplot(___, aes(x = Smältpunkt, y = 0)) + 
  ___() +
  geom_vline(xintercept = ___, color = "red")

# Kan man utifrån grafen säga om 1050 är ett rimligt medelvärde för populationen, givet det stickprov vi observerar?
##########

##### Övningsuppgift 4.9 #####
# Genomför ett t-test för hand för att se om medelsmältpunkten är skild från 1050. Skriv ut tydliga hypoteser. Medelvärde och standardavvikelse ges av följande.

mean(dat_smält$Smältpunkt)
sd(dat_smält$Smältpunkt)

# Ett kritiskt värde kan tas från en tabell över t-fördelningen eller beräknas i R med

qt(0.975, df = 9)

##########

##### Övningsuppgift 4.10 #####
# Genomför ett t-test med funktionen `t.test()` för att se om medelsmältpunkten är skild från 1050.
##########

## Konfidensintervall för normalfördelad data

t.test(dat$x, mu = 50)
t.test(dat$x, mu = 48)

mean(dat$x)
sd(dat$x)
qt(0.975, 7)

##### Övningsuppgift 4.11 #####
# Ta fram medelvärde, standardavvikelse och kritiskt värde för smältpunktsdata, med hjälp av R (eller en tabell för det kritiska värdet). Beräkna konfidensintervallet för smältpunktsdatan för hand.
##########

##### Övningsuppgift 4.12 #####
# Gör lämplig ändring i koden nedan för att beräkna ett 99-procentigt konfidensintervall, istället för ett 95-procentigt.

t.test(dat$x, conf.level = 0.95)

# Är ett 99-procentigt konfidensintervall bredare eller smalare än ett 95-procentigt?
##########

##### Övningsuppgift 4.13 #####
# I en tidigare uppgift användes argumentet `alternative` för att göra ett ensidigt test med `t.test()`. Vad händer med konfidensintervallet om man anger ett ensidigt test?
##########

##### Övningsuppgift 4.14 #####
# Ta datan över smältpunkter och beräkna ett konfidensintervall med `t.test()`. Tolka intervallet.
##########

interval <- t.test(dat$x)$conf.int
ggplot(dat, aes(x, 0)) +
  geom_point() +
  annotate("errorbar", xmin = interval[1], xmax = interval[2], y = -1, width = 0.1)

##### Övningsuppgift 4.15 #####
# Använd exempelillustrationen för havredata till en liknande illustration av smältpunktsdatan.
##########

## Normalfördelad data och centrala gränsvärdesatsen

ggplot(dat, aes(x)) + geom_histogram(bins = 5)

n <- 10
ggplot() + geom_histogram(aes(x = rnorm(n)), bins = 30)

##### Övningsuppgift 4.16 #####
# Testa koden ovan för lite olika värden på `n`. Det kan vara nyttigt att sätta antalet staplar `bins` för att få ett bättre histogram. Hur stort måste n vara för att ge en karaktäristisk klockform för histogrammet?
##########

ggplot(dat, aes(sample = x)) + geom_qq() + geom_qq_line()

n <- 10
dat_norm <- data.frame(x = rnorm(n))
ggplot(dat_norm, aes(sample = x)) + geom_qq() + geom_qq_line()

##### Övningsuppgift 4.17 #####
# Funktionen `runif()` ger slumpmässiga värden mellan 0 och 1. Testa att ändra i kodstycket ovan så att slumptal genereras med `runif()` istället för `rnorm()`. Hur påverkar det QQ-grafen?
##########

## Centrala gränsvärdesatsen

n <- 2

dat_sim_unif <- expand_grid(Observation = 1:n, Upprepning = 1:10000) %>% 
  mutate(x = runif(n())) %>% 
  group_by(Upprepning) %>% 
  summarise(x = mean(x))

ggplot(dat_sim_unif, aes(x)) + geom_histogram(bins = 50)
ggplot(dat_sim_unif, aes(sample = x)) + geom_qq() + geom_qq_line()

##### Övningsuppgift 4.18 #####
# Vad måste ändras i koden ovan för beräkna medelvärdet av tio observationer? Följer de medelvärdena en ungefärlig normalfördelning? Vad är det lägsta antalet observationer som ger ungefärligen normalfördelade medelvärden?
##########

n <- 10
lambda <- 10
dat_sim_unif <- expand_grid(Observation = 1:n, Upprepning = 1:10000) %>% 
  mutate(x = rpois(n(), lambda = lambda)) %>% 
  group_by(Upprepning) %>% 
  summarise(x = mean(x))

ggplot(dat_sim_unif, aes(x)) + geom_histogram(bins = 30)
ggplot(dat_sim_unif, aes(sample = x)) + geom_qq() + geom_qq_line()

##### Övningsuppgift 4.19 #####
# Testa att minska värdena på n och lambda. Vad är de lägsta värdena som ger ett histogram med en symmetrisk fördelning och punkter nära linjen i QQ-grafen?
##########

library(gapminder)
gapminder_2007 <- gapminder %>% filter(year == 2007)

g1 <- ggplot(gapminder_2007, aes(pop)) + geom_histogram(bins = 30)
g2 <- ggplot(gapminder_2007, aes(sample = pop)) + geom_qq() + geom_qq_line()
g3 <- ggplot(gapminder_2007, aes(sqrt(pop))) + geom_histogram(bins = 30)
g4 <- ggplot(gapminder_2007, aes(sample = sqrt(pop))) + geom_qq() + geom_qq_line()
g5 <- ggplot(gapminder_2007, aes(log10(pop))) + geom_histogram(bins = 30)
g6 <- ggplot(gapminder_2007, aes(sample = log10(pop))) + geom_qq() + geom_qq_line()

library(patchwork)
g1 + g3 + g5 + g2 + g4 + g6

##### Övningsuppgift 4.20 #####
# Använd kodstycket ovan som mall och ta fram grafer för medellivslängd (`lifeExp`) istället för befolkningsstorlek (`pop`). Visar grafen samma mönster som för befolkningsdatan?
##########

## Bonus. Simuleringar för t-test och konfidensintervall

dat_sim <- data.frame(x = rnorm(10, mean = 7, sd = 5))
t.test(dat_sim$x, mu = 7)

##### Övningsuppgift 4.21 #####
# Kör de två raderna i stycket ovan ett tiotal gånger. Du bör se att man ibland förkastar nollhypotesen trots att den ska stämma. Kan du få en känsla för hur stor andel av gångerna man felaktigt förkastar?
##########

dat_sim <- data.frame()
for(i in 1:1000){
  new_data <- data.frame(x = rnorm(10, mean = 7, sd = 5))
  test <- t.test(new_data$x, mu = 7)
  new_results <- data.frame(t_value = test$statistic, p_value = test$p.value,
               ci_lower = test$conf.int[1], ci_upper = test$conf.int[2])
  dat_sim <- bind_rows(dat_sim, new_results)
}

ggplot(dat_sim) +
  geom_histogram(aes(t_value, y = ..density..), bins = 50, fill = "white", color = "black") +
  geom_function(fun = dt, args = list(df = 9), color = "red", size = 1)

mean(dat_sim$p_value < 0.05)

##### Övningsuppgift 4.22 #####
# Hur många av de simulerade konfidensintervallen täcker värdet 7?
##########

##### Övningsuppgift 4.23 #####
# Stycket nedan simulerar data när populationsmedelvärdet är 9 och t-test har nollhypotesen att populationsmedelvärdet är 7. Här vill vi alltså förkasta nollhypotesen.

dat_sim <- data.frame()
for(i in 1:1000){
  new_data <- data.frame(x = rnorm(10, mean = 9, sd = 5))
  test <- t.test(new_data$x, mu = 7)
  new_results <- data.frame(t_value = test$statistic, p_value = test$p.value,
               ci_lower = test$conf.int[1], ci_upper = test$conf.int[2])
  dat_sim <- bind_rows(dat_sim, new_results)
}

# Använd kod från den första simuleringen för att undersöka hur väl histogrammet stämmer med den teoretiska fördelningen och för att se hur stor andel av gångerna man förkastar nollhypotesen på signifikansnivån 5 procent.
##########

##### Övningsuppgift 4.24 #####
# Stycket nedan simulerar data när populationsmedelvärdet är 9 och t-test har nollhypotesen att populationsmedelvärdet är 7. Ändra värdet för n och se hur det påverkar andelen gånger man förkastar nollhypotesen.

n <- 10

dat_sim <- data.frame()
for(i in 1:1000){
  new_data <- data.frame(x = rnorm(n, mean = 9, sd = 5))
  test <- t.test(new_data$x, mu = 7)
  new_results <- data.frame(t_value = test$statistic, p_value = test$p.value,
               ci_lower = test$conf.int[1], ci_upper = test$conf.int[2])
  dat_sim <- bind_rows(dat_sim, new_results)
}

mean(dat_sim$p_value < 0.05)

# Ungefär hur många observationer behövs för att ha femtio procents sannolikhet att förkasta nollhypotesen?
##########

##### Övningsuppgift 4.25 #####
# Ett konfidensintervall blir smalare och smalare ju större stickprovet är. Koden nedan ger medelvärdet för stickprovsbredden i simulerad data med standardavvikelsen 1.

n <- 10

dat_sim <- data.frame()
for(i in 1:1000){
  new_data <- data.frame(x = rnorm(n, mean = 0, sd = 1))
  test <- t.test(new_data$x, mu = 7)
  new_results <- data.frame(t_value = test$statistic, p_value = test$p.value,
               ci_lower = test$conf.int[1], ci_upper = test$conf.int[2])
  dat_sim <- bind_rows(dat_sim, new_results)
}

mean(dat_sim$ci_upper - dat_sim$ci_lower)

# Ungefär hur många observationer behövs för att konfidensintervallets bredd ska bli under 1, under 0.9, under 0.8, och så vidare ned till 0.1?
##########
NA
