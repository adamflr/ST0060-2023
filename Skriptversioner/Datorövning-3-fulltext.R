# # Slumpvariabler
#
# Datorövning 3 handlar om sannolikhetslära i R. Efter övningen ska vi kunna
#
# - Identifiera en passande slumpfördelning för verkliga fenomen,
#
# - Beräkna sannolikheter från en antagen fördelning,
#
# - Simulera fenomen med en antagen fördelning,
#
# - Använda slumptal för att utforska teoretiska resultat från sannolikhetsläran.
#
# ## Repetition från datorövning 2
#
# När man startar en ny R-session bör man ladda de paket man vet kommer behövas med `library()`. Om
# paket inte finns installerade måste man först köra `install.packages()`.
#

# install.packages("tidyverse")
library(tidyverse)

#
# I datorövning 2 tittade vi på hur insamlade variabler kan sammanfattas med lägesmått och
# spridningsmått. Ett enkelt sätt att ta fram dem är att använda `summarise()` och ange de mått och
# variabler man vill använda. Vi hade uppe ett exempel på data från Gapminder som vi importerade från
# en excel-fil. För nu kan vi dock hämta datan från paketet `gapminder`.
#

# install.packages("gapminder")
library(gapminder)

gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(`Livslängd, medel` = mean(lifeExp),
            `Befolkning, median` = median(pop),
            `Bnp per capita, standardavvikelse` = sd(gdpPercap))

#
# Beskrivande mått sammanfattas ofta i någon enkel vetenskaplig graf. Två vanliga val är
# lådagrammet, som illustrerar kvartiler och möjliga extremvärden, och stapeldiagrammet med
# felstaplar. Vi ger först ett exempel på ett lådagram över livslängd per kontinent uppdelat efter
# år.
#

ggplot(gapminder, aes(lifeExp, continent, fill = continent)) +
  geom_boxplot() +
  facet_wrap(~ year)

#
# Därefter ett exempel på ett stapeldiagram med felstaplar för samma data. Felstapeln ges av
# standardavvikelsen.
#

dat_sum <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(Mean = mean(lifeExp),
            SD = sd(lifeExp))
dat_sum

ggplot(dat_sum, aes(continent, Mean, fill = continent)) +
  geom_col() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.3) +
  facet_wrap(~ year)

#
# ## Diskreta fördelningar i allmänhet
#
# En diskret slumpvariabel kan bara anta specifika värden på tallinjen. Det absolut vanligaste
# fallet är när utfallen är heltal. Varje utfall är kopplat till *en* sannolikhet och summan av
# sannolikheterna är ett.
#
# Ett kast med en 6-sidig tärning har sex möjliga utfall och samma sannolikhet för varje utfall.
# Eftersom summan av sannolikheter ska bli ett måste sannolikheten för ett specifik utfall vara en
# sjättedel. Vi kan kodifiera slumpvariabeln i en tabell över utfall och sannolikhet.
#

dat_dice6 <- data.frame(x = c(1,2,3,4,5,6),
                        p = 1/6)
dat_dice6

#
# En diskret slumpvariabel illustreras ofta med ett stapeldiagram.
#

ggplot(dat_dice6, aes(x, p)) + geom_col()

#
# Uppgift 3.1. (En annan tärning)
# Ta fram en tärning som *inte* har sex sidor. Följ exemplet ovan för att kodifiera din tärning som
# en slumpvariabel. Ett bra namn på det nya objektet kan vara `dat_diceN` där N anger antalet sidor
# på tärningen, t.ex. `dat_dice20` för en 20-sidig tärning.
# :::
#
# Under tidigare datorövningar såg vi hur man kan beräkna medelvärde och varians från ett
# stickprov. De måtten kan också beräknas på en teoretisk slumpvariabel. Den här kopplingen mellan
# beräkningar på ett stickprov och teoretiska egenskaper hos en slumpvariabel är väldigt central inom
# statistiken, eftersom vårt mål är att koppla verklig data till teoretiska modeller.
#
# För en diskret slumpvariabel ges medelvärdet (som då också kallas *populationsmedelvärde*, eller
# vanligare *väntevärde*) av summan av utfallen gånger sannolikheterna. För vår tärning kan vi räkna
# ut det genom att multiplicera utfall och sannolikhet i ett `mutate()` steg och sedan summera.
#

dat_dice6 %>% 
  mutate(x_times_p = x * p) %>% 
  summarise(Expected_value = sum(x_times_p))

#
# Notera att medelvärdet inte behöver vara ett möjligt utfall.
#
# Uppgift 3.2. (Medelvärde för din tärning)
# Upprepa beräkningen ovan, denna gång med den slumpvariabel du kodifierade i den tidigare
# uppgiften.
# :::
#
# Beräkningen av en teoretisk varians (som vi kan kalla *populationsvarians*) är lite mer
# komplicerad och vi går inte in på några detaljer här. Koden nedan skapar en funktion som beräknar
# populationsvariansen.
#

pop_var <- function(x, p) sum(p * (x - sum(x * p))^2)

#
# För en 6-sidig tärning kan vi beräkna variansen med följande.
#

dat_dice6 %>% 
  summarise(varians = pop_var(x, p))

#
# Uppgift 3.3. (Varians för din tärning)
# Upprepa beräkningen ovan, denna gång med den slumpvariabel du kodifierade i den tidigare
# uppgiften.
# :::
#
# Vår modell för tärningskastet säger att en serie tärningskast ska ha ett visst medelvärde och
# varians. Låt oss nu testa det genom att kasta tärning.
#
# Uppgift 3.4. (Kasta tärningen)
# Kasta din tärning 20 gånger. Gärna på en mjuk yta. Skriv in utfallen i koden nedan och beräkna
# medelvärde och varians.
#

utfall <- c(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)
mean(utfall)
var(utfall)
sd(utfall)

#
# Ligger medelvärde och varians från stickprovet nära de teoretiska beräkningarna?
# :::
#
# R kommer också med en rad funktioner för att simulera data. Funktionen `sample()` drar ett urval
# ur en serie möjliga utfall. Följande kod drar tiotusen slumputfall från en 6-sidig tärning, och
# beräknar medelvärde och varians.
#

slumputfall <- sample(x = dat_dice6$x, size = 10000, replace = T)

mean(slumputfall)
var(slumputfall)

#
# Uppgift 3.5. (Simulera tärningen)
# Använd `sample()` för att dra tiotusen observationer från din egen tärning. Beräkna medelvärde
# och varians från det stickprovet. Är utfallen nära de teoretiska beräkningarna av
# populationsmedelvärde och -varians?
# :::
#
# ## Särskilda diskreta fördelningar: binomialfördelning
#
# Under föreläsningen såg vi tre familjer av fördelningar som uppstår i vissa specifika
# situationer:
#
# - binomialfördelningen (för antalet positiva utfall vid ett antal försök),
# - poissonfördelningen (för antalet händelser vid en stor mängd försök), och
# - normalfördelningen (för kontinuerliga variabler).
#
# Binomialfördelningen ger sannolikheter för antalet positiva utfall vid ett visst antal försök.
# Varje försök har ett av två utfall (positivt och negativt) och försöken är oberoende av varandra. I
# en binomialfördelning ges antalet försök av en parameter *n* och sannolikheten för ett positivt
# utfall i ett försök av en parameter *p*.
#
# Som exempel kan vi ta ett tärningskast. Om man kastar en 6-sidig tärning tio gånger kommer
# antalet ettor (vårt positiva utfall) följa en binomialfördelning där n ges av tio och p av en
# sjättedel. I R kan sannolikheter från en binomialfördelning tas fram med `dbinom()`. Sannolikheten
# för tre ettor ges till exempel av
#

dbinom(3, size = 10, prob = 1/6)

#
# Om vi vill illustrera en binomalfördelning kan vi ta fram sannolikheterna för varje utfall och
# göra ett stapeldiagram.
#

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 1/6))

ggplot(dat_bin, aes(x, p)) + geom_col()

#
# Uppgift 3.6. (Binomialfördelning för tärningen)
# Ta tärningen från tidigare uppgift. Om man kastar tärningen tjugo gånger, vad är fördelningen för
# antalet gånger man får tärningens lägsta utfall? (Till exempel, vad är fördelningen för antalet
# ettor vid tjugo kast med en sexsidig tärning?) Fyll i stycket nedan för att beräkna sannolikheterna
# i den fördelningen och illustrera med ett stapeldiagram.
#

dat_bin <- data.frame(x = 0:20) %>% 
  mutate(p = dbinom(x, size = 20, prob = ___))

ggplot(dat_bin, aes(x, p)) + geom_col()

# :::
#
# Uppgift 3.7. (Sannolikheter i binomialen)
# I den fördelning du beräknade i uppgiften ovan. Vad är sannolikheten att få exakt tre positiva
# utfall? (Ledning: för en sexsidig tärning skulle det ges av `dbinom(3, size = 20, prob = 1/6))`.)
# :::
#
# Utöver sannolikhetsfunktionen (som ger sannolikheten för varje utfall) används ofta också
# fördelningsfunktionen (som för varje utfall ger sannolikheten att ligga exakt på eller under ett
# specifikt värde). Fördelningsfunktionen används vid sannolikhetsberäkningar, t.ex. är de tabeller
# för fördelningar man ofta ser i slutet av statistikböcker på fördelningsformen.
#
# I R beräknas fördelningsfunktionen med `pbinom()`. För fallet med tio tärningskast kan man ta
#

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 1/6),
         P = pbinom(x, size = 10, prob = 1/6))
dat_bin

#
# Fördelningsfunktionen ges av att summera sannolikhetsfunktionen uppifrån.
#
# Uppgift 3.8. (Fördelningsfunktion i binomialen)
# Använd binomialfördelningen från den tidigare uppgiften till att beräkna sannolikheten att få
# *tre eller färre* positiva utfall.
# :::
#
# Vi kan använda ett logiskt uttryck för att illustrera sannolikheter i stapeldiagrammet. Om vi
# vill beräkna och illustrera sannolikheten för fyra eller färre positiva utfall i exemplet med en
# tärning kan vi använda följande.
#

pbinom(4, size = 10, prob = 1/6)

dat_bin <- data.frame(x = 0:10) %>% 
  mutate(p = dbinom(x, size = 10, prob = 1/6),
         P = pbinom(x, size = 10, prob = 1/6))

ggplot(dat_bin, aes(x, p, fill = x <= 4)) +
  geom_col()

#
# Den blå ytan motsvarar sannolikheten att få fyra eller färre ettor. Den sannolikheten kan
# beräknas till 0.985.
#
# Uppgift 3.9. (Illustration av sannolikheten)
# Gör lämpliga ändringar i exemplet ovan för att illustrera sannolikheten att få exakt tre positiva
# utfall i exemplet med tärningskastet.
# :::
#
# Uppgift 3.10. (Teori och verklighet)
# I en tidigare uppgift kastade du tärningen tjugo gånger. Hur många gånger fick du det lägsta
# möjliga utfallet på tärningen (t.ex. en etta på en vanlig sex-sidig tärning)?
# :::
#
# I det första exemplet med tärningen kunde vi beräkna väntevärde och varians med de allmänna
# formlerna. För parametriserade fördelningar som binomialfördelningen finns ofta enklare formler som
# helt beror på parametrarna. I en binomial ges populationsmedelvärdet av antalet upprepningar gånger
# sannolikheten (n gånger p) och variansen av antalet upprepningar gånger sannolikheten gånger
# ett-minus-sannolikheten (n gånger p gånger (1 - p)). För en sexsidig tärning (n = 10 och p = 1/6)
# ges medelvärdet av 1.667 och variansen av 1.389.
#
# Uppgift 3.11. (Medelvärde och varians i binomialen)
# Beräkna medelvärde och varians för antalet ettor om man kastar en 16-sidig tärning tjugo gånger.
# :::
#
# Uppgift 3.12. (Frösort)
# För en viss frösort är sannolikheten att gro 60%. Om man sår 10 slumpmässigt valda frön, hur stor
# är då chansen att
#
# a. högst 6 frön gror?
# b. minst 6 frön gror?
# c. Beräkna populationsmedelvärde och populationsvrians för antalet frön som gror.
#
# (Denna uppgift finns också i uppgiftsdokumentet och kan lösas för hand.)
#
# :::
#
# ## Särskilda diskreta fördelningar: poissonfördelning
#
# En poissonfördelning är en vanlig sannolikhetsfördelning för antalsdata (alltså data som antar
# positiva heltal som 0, 1, 2, 3 och så vidare). Poissonfördelningen har en nära koppling till
# binomialfördelningen: om antalet försök n är stort och sannolikheten p liten liknar en binomial
# fördelning en poissonfördelning. Det innebär att en poissonfördelning är en lämplig fördelning för
# händelser som har många upprepningar men låg sannolikhet att inträffa i det enskilda försöket.
# Typexempel är olika typer av olyckor eller observationsantal för sällsynta växter och djur.
#
# Poissonfördelningen styrs av en enda parameter, lambda. Till skillnad från en binomialfördelning,
# där det högsta möjliga utfall ges av n, har en poissonfördelning inget maxvärde. Man kan i teorin
# få vilket positivt heltalsutfall som helst. För en poissonfördelning är populationsmedelvärdet och
# populationsvariansen lika med lambda.
#
# I R kan man beräkna sannolikheter för en poissonfördelning med `dpois()` och `ppois()`.
#
# Uppgift 3.13. (Sannolikhetsfunktionen för en poisson)
# Följande ger sannolikheten att få utfall 2 i en poissonfördelning med lambda satt till 4.
#

dpois(2, lambda = 4)

#
# Gör lämpliga ändringar för att beräkna sannolikheten för exakt 5 i en fördelning med lambda satt
# till 3.
# :::
#
# Uppgift 3.14. (Fördelningsfunktionen för en poisson)
# Följande ger sannolikheten att få mindre än eller lika med 2 i en poissonfördelning med lambda
# satt till 4.
#

ppois(2, lambda = 4)

#
# Gör lämpliga ändringar för att beräkna sannolikheten för *mindre än eller lika med 5* i en
# fördelning med lambda satt till 3.
# Hur kan man beräkna sannolikheten att få mer än 5 i en fördelning med lambda satt till 3?
# :::
#
# Uppgift 3.15. (Albinofödsel)
# Sannolikheten att en viss individ i en viss population skall vara albino är 1/20 000. Hur stor är
# sannolikheten att på 40 000 födslar
#
# a. ingen albino föds,
# b. minst en albino föds.
# c. Vilka antaganden skall vara uppfyllda för att sannolikheterna i a och b ska gälla?
#
# (Denna uppgift finns också i uppgiftsdokumentet och kan lösas för hand.)
#
# :::
#
# ## Kontinuerliga fördelningar i allmänhet
#
# En kontinuerlig fördelning kan anta vilka värden som helst på hela tallinjen eller i något
# intervall på tallinjen. Ett enkelt exempel på en kontinuerlig slumpvariabel kan vara att stoppa ett
# stoppur slumpmässigt och titta på decimaldelen. Det kommer ge något värde mellan 0 och 1.
# (Stoppuret kommer naturligtvis avrunda värdet, så man får tänka sig ett magiskt stoppur med
# oändligt antal decimaler.) Till skillnad från en diskret slumpvariabel, som kan beskrivas med
# utfallen och dess sannolikheter, måste en kontinuerlig variabel förklaras med en matematisk
# funktion, en så kallad *täthetsfunktion*. Detta beror på att enskilda utfall alltid har sannolikhet
# noll för en kontinuerlig fördelning: om man har oändligt antal decimaler är det som exempel
# sannolikhet noll att få exakt 0.345000... som decimaler på ett stoppur.
#
# Den absolut vanligaste kontinuerliga fördelningen är en *normalfördelning*.
#
# ## Särskilda kontinuerliga fördelningar: normalfördelningen
#
# En normalfördelning är en kontinuerlig fördelning some kan anta värden över hela tallinjen och
# beror på två parametrar: mu som styr var fördelningens är centrerad, och sigma som styr hur
# utspridd fördelningen är.
# En normalfördelning har en karaktäristisk *klockform*. Vi illustrerar två normalfördelningar med
# hjälp av geomet `geom_function()` i en ggplot.
#

ggplot() +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 2), color = "red", size = 2) +
  geom_function(fun = dnorm, args = list(mean = 3, sd = 1), color = "blue", size = 2) +
  geom_function(fun = dnorm, args = list(mean = 5, sd = 1), color = "green3", size = 2) +
  xlim(0, 10)

#
# Den gröna och röda kurvan har samma medelvärde mu men skilda standardavvikelser sigma. Den blå
# och gröna kurvan har skilda medelvärden mu men samma standardavvikelse sigma.
#
# Uppgift 3.16. (Normalfördelningar)
# Gör lämpliga ändringar i stycken ovan för att illustrera två normalfördelningar: en med
# medelvärde 0 och standardavvikelse 1 och en med medelvärde 1 och standardavvikelse 2. Kan du
# utifrån kurvorna säga vilken av de två fördelningarna som ger störst sannolikhet att få ett utfall
# under minus två?
# :::
#
# Kurvorna illustrerar *täthetsfunktionen*. I en kontinuerlig fördelning har täthetsfunktionen
# ingen tolkning i termer av sannolikheter. För att kunna få fram sannolikheter från en
# normalfördelning behöver vi istället titta på *fördelningsfunktionen*. Kom ihåg från det diskreta
# fallet att fördelningsfunktionen anger sannolikheten för exakt lika eller under ett givet värde.
# Samma sak gäller för en kontinuerlig variabel. Fördelningsfunktionen värde ges av `pnorm()`. Om vi
# vill beräkna sannolikheten att ligga under 1 i en normalfördelning med medelvärde 2 och
# standardavvikelse 3 tar vi
#

pnorm(1, mean = 2, sd = 3)

#
# Vi kan ta fram en illustration med följande kod. Detaljer spelar mindre roll här, men gör gärna
# några ändringar i `x_value`, `mu` och `sigma`för att se hur grafen ändras. Om paketet `patchwork`
# inte är installerat, kör raden som här är utkommenterad med `#`.
#

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

#
# Sannolikheten att ligga under ett värde på x ges av kurvan för fördelningsfunktionen vid det
# x-värdet. Det motsvarar den fyllda ytan under täthetsfunktionen till vänster om x-värdet.
#
# Uppgift 3.17. (Sannolikhet från normalfördelningen)
# Fyll i kodstycket nedan för att beräkna sannolikheten att få ett värde under minus två i en
# normalfördelning med medelvärde 0 och standardavvikelse 1, och i en normalfördelning med medelvärde
# 1 och standardavvikelse 2.
#

pnorm(-2, mean = ___, sd = ___)
pnorm(-2, mean = ___, sd = ___)

#
# :::
#
# Om man vill ta fram en sannolikhet att ligga i ett visst intervall kan man ta skillnaden mellan
# två värden från fördelningsfunktionen. Sannolikheten att ligga mellan 1 och minus 1 i en
# normalfördelning med medelvärde 0 och standardavvikelse 1 ges till exempel av
#

pnorm(1, mean = 0, sd = 1) - pnorm(-1, mean = 0, sd = 1)

#
# Den normalfördelningen (medelvärde 0 och standardavvikelse 1) kallas den *standardiserade*
# normalfördelning. Vi kan illustrerade med följande.
#

x_values <- c(-1,1)
mu <- 0
sigma <- 1

dat_norm <- data.frame(x = seq(from = mu - 4 * sigma, to = mu + 4 * sigma, 0.1)) %>% 
  mutate(p = dnorm(x, mean = mu, sd = sigma))

ggplot(dat_norm, aes(x, p)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = p), data = dat_norm %>% filter(x < max(x_values) & x > min(x_values)), fill = "salmon", color = "black") +
  labs(title = "Täthetsfunktion")

#
# Uppgift 3.18. (Sannolikhet mellan två värden)
# Fyll i kodstycket nedan för att beräkna sannolikheten att få ett värde mellan *minus två* och tre
# i en normalfördelning med medelvärde 1 och standardavvikelse 2.
#

pnorm(___, mean = ___, sd = ___) - pnorm(___, mean = ___, sd = ___)

# :::
#
# Fördelningsfunktionen ger sannolikheten att ligga under ett visst x-värde. Man kan enkelt beräkna
# sannolikheten att ligga över värdet genom att ta ett minus fördelningsfunktionen. Sannolikheten att
# ligga över 1.96 i en standardiserad normalfördelning ges till exempel av ungeför 2.5 procent.
#

1 - pnorm(1.96)

#
# Uppgift 3.19. (Sannolikhet över x)
# Fyll i kodstycket nedan för att beräkna sannolikheten att få ett värde över sju i en
# normalfördelning med medelvärde 3 och standardavvikelse 5.
#

1 - pnorm(___, mean = ___, sd = ___)

# :::
#
# En normalfördelning kan transformeras till en annan normal genom att addera och multiplicera med
# någon konstant. Mer specifik kan vilken normalfördelning som helst återföras till en standardiserad
# normalfördelning genom att dra ifrån medelvärdet och dela med standardavvikelsen. Det här utnyttjas
# när man beräknar sannolikheter för hand. Säg till exempel att vi har en normalfördelning med
# medelvärde 8 och standardavvikelse 4, och vi vill ta fram sannolikheten att ligga under 7. Det kan
# beräknas med
#

pnorm(7, mean = 8, sd = 4)

#
# Alternativt kan man standardiserade genom att att ta 7 minus 8, delat på 4, vilket ger (7 - 8) /
# 4 = -0.25, och sedan göra sannolikhetsberäkningen i den standardiserade normalen.
#

pnorm(-0.25, mean = 0, sd = 1)

#
# Uppgift 3.20. (Standardisering)
# En slumpvariabel Y följer en normalfördelning med medelvärde 2 och varians 9. Vad är
# sannolikheten att få
#
# - P(Y > 2.75),
# - P(Y < 2.75),
# - P(2.30 < Y < 2.45)?
#
# Beräkna först sannolikheten för hand och sedan med `pnorm()`.
# Notera att denna fråga finns bland instuderingsuppgifterna.
# :::
#
# ## Bonus. Summan av två slumpvariabler med slumptal
#
# Utöver funktioner för sannolikhetsfunktion (eller täthetsfunktion) och fördelningsfunktion (som
# `dbinom()`, `dpois()`, `dnorm()`, respektive `pbinom()`, `ppois()`, `pnorm()`) har R funktioner för
# att ta fram slumptal (`rbinom()`, `rpois()`, `rnorm()`). Slumptal kan vara användbara för att
# undersöka egenskaper hos en slumpprocess.
#
# Låt oss som ett första exempel undersöka egenskaper hos poissonfördelningen genom att titta på
# följande fråga: om vi har två poissonfördelningar, med lambda 3 respektive 2, vilken fördelning har
# summan av de två fördelningarna? Vi kan t.ex. tänka oss att antalet blåsippor i en försöksruta är
# poissonfördelat med medelvärde 3 och antalet vitsippor är poissonfördelat med medelvärde 2, och att
# vi vill veta fördelningen för antalet blåsippor plus antalet vitsippor.
#
# Vi börjar med att dra slumptal för en poissonfördelning med lambda lika med 3 och jämför
# slumptalen med en teoretisk fördelning.
#

dat_pois <- data.frame(x = rpois(10000, lambda = 3)) %>% 
  count(x) %>% 
  mutate(p = n / sum(n),
         theoretical_p = dpois(x, lambda = 3))

ggplot(dat_pois, aes(x, p)) +
  geom_point() +
  geom_line(aes(y = theoretical_p))

#
# Linjen ger den teoretiska poissonfördelningen och punkterna ger fördelningen för slumptalen.
# Punkterna ligger nära linjen, vilket är helt efter förväntan. Notera dock att det kommer finnas
# vissa naturliga avvikelser när man arbetar med slumptal.
#
# Uppgift 3.21. (Poisson från slumptal)
# Gör lämpliga ändringar i kodstycket ovan för att göra motsvarande beräkning för en
# poissonfördelning med lambda lika med 2. Ligger slumptalen nära den teoretiska fördelningen?
# :::
#
# Vi kan nu ta från två serier av slumptal, beräkna dess summa och jämföra med en teoretisk
# fördelning. Som teoretisk fördelning tar vi en poissonfördelning med lambda 5, eftersom 5 är summan
# av våra två lambdavärden.
#

dat_pois <- data.frame(x1 = rpois(10000, lambda = 3),
                       x2 = rpois(10000, lambda = 2)) %>% 
  mutate(x = x1 + x2) %>% 
  count(x) %>% 
  mutate(p = n / sum(n),
         theoretical_p = dpois(x, lambda = 5))

ggplot(dat_pois, aes(x, p)) +
  geom_point() +
  geom_line(aes(y = theoretical_p))

#
# Summans värden ligger när den teoretiska fördelningen. Det stödjer tanken att summan av två
# poissonfördelningar är poissonfördelad.
#
# Vi såg tidigare hur utfallet av en tärning kan ses som en slumpvariabel (n stycken utfall där
# samtliga är lika sannolika) och att funktionen `sample()` kan användas för att simulera
# tärningskast. Vi kan utnyttja det för att undersöka fördelningen för summan av två tärningskast.
# Exemplet nedan tittar på en sex-sidig tärning.
#

dat_dice_sum <- data.frame(x1 = sample(c(1,2,3,4,5,6), size = 10000, replace = T),
                           x2 = sample(c(1,2,3,4,5,6), size = 10000, replace = T)) %>% 
  mutate(x = x1 + x2) %>% 
  count(x) %>% 
  mutate(p = n / sum(n))

ggplot(dat_dice_sum, aes(x, p)) +
  geom_col()

#
# Summan av två tärningskast har en triangelformad fördelning där 7 är det vanligaste utfallet och
# 2 och 12 är de ovanligaste.
#
# Uppgift 3.22. (Summan av tärningskast med annan tärning)
# Gör lämpliga ändringar i stycket ovan för att beräkna summan av två utfall för en åtta-sidig
# tärning.
# :::
#
# Uppgift 3.23. (Summan av tre tärningskast)
# Gör lämpliga tillägg i stycket ovan för att beräkna summan av tre utfall för en sex-sidig
# tärning.
# :::
#
# ## Bonus. Cirkelns area från slumptal
#
# En intressant aspekt av slumptal är att de kan användas till att utforska icke-slumpmässiga
# problem. Som exempel tar vi cirkelns area. I figuren nedan är en cirkel inskriven i en kvadrat.
# Kvadraten har sidor med längden två, så dess totala yta är fyra (två i kvadrat). Om man vet hur
# stor andel av ytan upptas av cirkeln kan man beräkna cirkelns area genom att ta den andelen gånger
# fyra. Från rent matematiska resultat vet vi att arean ska vara lika med pi, alltså runt 3.14.
#

# install.packages("ggforce")
library(ggforce)
g <- ggplot() + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1)) + 
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1), fill = NA, color = "black")
g

#
# Funktionen `runif()` ger ett slumpmässigt värde mellan två gränsvärden (som sätts med argumenten
# `min` och `max`). Slumpmässiga punkter inom kvadraten kan dras genom att dra en x- och en
# y-koordinat. En fördelning där alla värden är lika sannolika kallas *uniform* (eller *likformig*).
#

dat_random <- data.frame(x = runif(100, min = -1, max = 1),
                         y = runif(100, min = -1, max = 1))

g + geom_point(aes(x, y), data = dat_random)

#
# Pytagoras sats kan användas för att beräkna avståndet mellan en slumpmässig punkt och origo
# (nollpunkten). Punkter med ett avstånd under ett ligger inom cirkeln. Cirkelns area ska ges av
# andelen punkter i kvadraten som också ligger i cirkeln, gånger fyra.
#

dat_random <- data.frame(x = runif(100, min = -1, max = 1),
                         y = runif(100, min = -1, max = 1)) %>% 
  mutate(inner = x^2 + y^2 < 1)

4 * mean(dat_random$inner)
pi

#
# Den beräknade arean ligger inte så långt från konstanten pi.
#
# Uppgift 3.24. (Fler punkter ger bättre skattning)
# Beräkningens precision ökar med antalet slumptal. Vad i kodstycket ovan ska ändras för att
# generera fler slumptal? Blir utfallet närmre det väntade värdet om antalet slumptal ökar?
# :::
#
# ## Bonus. Slump och ordning - Sierpinski-triangeln
#
# Slumpmässiga processer kan ge intressanta mönster. Ett exempel är följande procedur.
#
# 1. Ta tre *rampunkter* i planet så att de bildar en triangel.
# 2. Ta en *startpunkt* inom triangeln.
# 3. Välj slumpmässigt en av de tre rampunkterna och beräkna punkten mellan startpunkten och den
# valda rampunkten. Detta ger en ny punkt.
# 4. Välj slumpmässigt en av de tre rampunkterna och beräkna punkten mellan den nya punkten från
# föregående steg och rampunkten. Detta ger en ny punkt.
# 5. Upprepa steg 4 ett godtyckligt antal gånger.
#
# I R kan proceduren programmeras med en `for`-loop. Här ges ett exempel där 100 punkter genereras.
# Koden är viss överkurs och innehåller funktioner vi inte behöver till den övriga kursen.
#

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

#
# Uppgift 3.25. (Sierpinski-triangeln)
# Vad måste ändras i stycket ovan för att generera fler punkter? Tiotusen kan vara ett lämpligt
# antal för en tydligare illustration. Vad händer om man ändrar värdena i `x_original` och
# `y_original`?
# :::
#
