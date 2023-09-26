# Beskrivande statistik
## Repetition från datorövning 1

# install.packages("tidyverse")        # Installera tidyverse (behövs ej om redan gjort)
library(tidyverse)                     # Ger ett felmeddelande om paketet inte installerats

dat <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Spotify_data.csv")
dat                                              # Skriv ut objektet dat

mean(dat$tempo, na.rm = T)                       # Beräkna medelvärdet av kolumnen tempo i datan dat

dat %>%                                                    # Ta datan, och sen
  filter(artist_name == "Tame Impala", tempo > 170) %>%    # ta ut rader där artisten är Tame Impala och tempot är större än 170, och sen
  select(artist_name, track_name, tempo)                   # ta ut kolumnerna artist_name, track_name och tempo

dat_small <- dat %>% filter(artist_name == "The Weeknd")                            # Skapa en mindre datamängd genom att filtrera på en artist

ggplot(dat_small, aes(tempo, danceability, size = valence, color = mode_name)) +    # Koppla grafegenskaper och variabler
  geom_point()                                                                      # Illustrera med punkter

## Import av data från en Excelfil

##### Övningsuppgift 2.1 #####
# Hitta excelfilen *Gapminder.xlsx* på Canvas och ladda ner den. Hitta mappen som filen laddats ned till.
##########

library(readxl)                                                           # Ladda readxl

gapminder <- read_excel("C:/Users/User_name/Downloads/Gapminder.xlsx")    # Läs in från en lokal excelfil
gapminder                                                                 # Skriv ut objektet gapminder

##### Övningsuppgift 2.2 #####
# Var ligger den nedladdade filen *Gapminder.xlsx*? Gör lämplig ändring i koden ovan för att läsa in data från den filen. Notera att R använder högerlutande snedstreck `/`, så om en kopierad sökväg har vänster-snedstreck måste de ändras. Kontrollera att datan blivit korrekt inläst genom att köra objektnamnet `gapminder`.
##########

getwd()                                # Ange working directory

gapminder <- read_excel("Data/Gapminder.xlsx")             # Läs in från en lokal excelfil (relativt wd)
gapminder                                                  # Skriv ut objektet gapminder

##### Övningsuppgift 2.3 #####
# Identifiera *working directory* för din nuvarande Rs-session genom att köra `getwd()`.
##########

# install.packages("gapminder")        # Installera paketet gapminder (behövs ej om redan gjort)
library(gapminder)                     # Ladda gapminder
gapminder                              # Skriv ut objektet gapminder

## Ändra och skapa nya kolumner med `mutate`

gapminder <- gapminder %>%             # Ta datan, och sen
  mutate(gdp = gdpPercap * pop)        # Beräkna en ny kolumn som bnp per capita (bnpPercap) gånger befolkningen (pop)

gapminder %>% select(gdpPercap, pop, gdp)        # Ta datan och sen välj tre kolumner

gapminder %>% 
  mutate(`National GDP` = gdpPercap * pop)

## Sammanfattande lägesmått

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +    # Koppla grafens egenskaper till kolumner
  geom_point() +                                                               # Illustrera med punkter
  facet_wrap(~ year)                                                           # Skapa småfönster efter år

g <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, text = country)) +
  geom_point() +
  facet_wrap(~ year)

# install.packages("plotly")
library(plotly)                        # Ladda paketet plotly
ggplotly(g)                            # Ta fram en interaktiv version av grafen g

gdpPercap <- gapminder$gdpPercap       # Skapa en vektor gdpPercap genom att ta ut kolumnen från gapminder
mean(gdpPercap)                        # Beräkna medelvärdet av gdpPercap
median(gdpPercap)                      # Beräkna medianen av gdpPercap

gapminder %>%                                    # Ta datan, och sen
  summarise(Mean = mean(gdpPercap),              # summera med medelvärdet av gdpPercap och
            Median = median(gdpPercap))          # med medianen av gdpPercap

##### Övningsuppgift 2.4 #####
# Gör lämpliga ändringar i exemplet ovan för att beräkna lägesmått för medellivslängd (`lifeExp`).
##########

gapminder %>%                                    # Ta datan, och sen
  group_by(year) %>%                             # gruppera efter år, och sen
  summarise(Mean = mean(gdpPercap),              # summera med medelvärdet av gdpPercap och
            Median = median(gdpPercap))          # med medianen av gdpPercap

##### Övningsuppgift 2.5 #####
# Gör lämpliga ändringar i exemplet ovan för att beräkna lägesmått per kontinent. Vad måste läggas till för att också beräkna maximum och minimum per kontinent (funktionerna `max()` och `min()`)?
##########

##### Övningsuppgift 2.6 #####
# Finns det några problem med att beräkna medelvärde per kontinent på den här datan? (Jag kan se minst två.)
##########

dat_gdp_2007 <- gapminder %>%                    # Ta datan, och sen
  filter(year == 2007) %>%                       # filtrera för 2007, och sen
  group_by(continent) %>%                        # gruppera efter kontinent, och sen
  summarise(Mean = mean(gdpPercap))              # summera med medelvärdet av gdpPercap

ggplot(dat_gdp_2007, aes(continent, Mean)) +     # Skapa en ggplot med continent på x-axeln och Mean på y-axeln
  geom_col()                                     # Illustrera med kolumner (columns)

ggplot(dat_gdp_2007, aes(continent, Mean)) +
  geom_col() +
  geom_point(aes(continent, gdpPercap), data = gapminder %>% filter(year == 2007))

##### Övningsuppgift 2.7 #####
# Gör om stapeldiagrammet. Denna gång med livslängd (`lifeExp`) istället för bnp per capita (`gdpPercap`).
##########

## Sammanfattande spridningsmått

gdpPercap <- gapminder$gdpPercap                 # Skapa en vektor gdpPercap genom att ta ut kolumnen från gapminder

var(gdpPercap)                                   # Beräkna variansen av gdpPercap
sd(gdpPercap)                                    # Beräkna standardavvikelsen av gdpPercap
IQR(gdpPercap)                                   # Beräkna kvartilavståndet av gdpPercap

gapminder %>%                                    # Ta datan, och sen
  summarise(Varians = var(gdpPercap),            # summera med varians,
            Standardavvikelse = sd(gdpPercap),   # standardavvikelse,
            Kvartilavstånd = IQR(gdpPercap))     # och kvartilavstånd

gapminder %>%                                    # Ta datan, och sen
  group_by(year) %>%                             # gruppera efter år, och sen
  summarise(Varians = var(gdpPercap),            # summera med varians,
            Standardavvikelse = sd(gdpPercap),   # standardavvikelse,
            Kvartilavstånd = IQR(gdpPercap))     # och kvartilavstånd

##### Övningsuppgift 2.8 #####
# Gör lämpliga ändringar i det sista exempel för att istället beräkna spridningsmått för livslängd.
##########

dat_sum <- gapminder %>%                         # Ta datan, och sen
  group_by(year, continent) %>%                  # grupper efter år och kontinent, och sen
  summarise(Mean = mean(gdpPercap),              # summera med medelvärde
            SE = sd(gdpPercap) / sqrt(n()))      # och medelfel (standardavvikelsen delat på roten ur n)
dat_sum

ggplot(dat_sum, aes(year, Mean, color = continent)) +      # Skapa en ggplot från datan dat_sum med year som x-axel, Mean som y-axel och färg efter kontinent
  geom_line() +                                            # Illustrera med linjer
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE))   # Illustrera med felstaplar

##### Övningsuppgift 2.9 #####
# Felstaplarna från `geom_errorbar()` har väldigt breda ändar. Använd hjälpsidan för geomet `?geom_errorbar`, i synnerhet exemplen längst ned, och se om det går att ändra bredden.
##########

dat_sum <- gapminder %>%                         # Ta datan, och sen
  filter(year == 2007) %>%                       # filtrera på år, och sen
  group_by(continent) %>%                        # gruppera efter kontinent, och sen
  summarise(Mean = mean(lifeExp),                # summera med medelvärde,
            SD = sd(lifeExp))                    # och standardavvikelse
dat_sum

ggplot(dat_sum, aes(continent, Mean, fill = continent)) +          # Skapa en ggplot med continent och Mean på axlarna och ifylld färg given av kontinent
  geom_col()+                                                      # Illustrera med kolumner
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD)) +         # Illustrera med felstaplar
  labs(title = "Average life expectancy by continent, 2007",       # Ange titel och förklarande text
       caption = "Errorbars given by mean +/- standard deviation.
       Source: Gapminder")

##### Övningsuppgift 2.10 #####
# Gör lämpliga ändringar i exempel ovan för att konstruera ett stapeldiagram med felstaplar för året 1982 och variabeln gdpPercap.
##########

ggplot(gapminder, aes(year, lifeExp, fill = continent, group = year)) +        # Skapa en ggplot med år och lifeExp på axlarna, kontinent som ifylld färg, och gruppera efter år
  geom_boxplot() +                                                             # Illustrera med lådagram
  facet_wrap(~ continent)                                                      # Småfönster efter kontinent

##### Övningsuppgift 2.11 #####
# I lådagrammet används argumentet `group`. Vad gör det? Vad händer om man tar bort det?
##########

gapminder_2007 <- gapminder %>% filter(year == 2007)
gapminder_agg <- gapminder_2007 %>% 
  group_by(continent) %>% 
  summarise(lifeExp = mean(lifeExp))
gapminder_agg

ggplot() +
  geom_boxplot(aes(lifeExp, continent), data = gapminder_2007) +
  geom_point(aes(lifeExp, continent), data = gapminder_agg, color = "red", shape = "X", size = 6)

gapminder_eu <- gapminder %>% filter(continent == "Europe")

ggplot(gapminder_eu, aes(gdpPercap, lifeExp, group = country, label = country)) +
  geom_text(data = gapminder_eu %>% filter(year == 2007)) +
  geom_path(color = "blue", size = 3, data = gapminder_eu %>% filter(country == "Sweden"), alpha = 0.3) +
  geom_path(color = "red", size = 3, data = gapminder_eu %>% filter(country == "Poland"), alpha = 0.3)

## Ordna upp beskrivande statistik och exportera

dat_sum <- gapminder %>%                         # Ta datan, och sen
  filter(year == 2007) %>%                       # filtrera på år, och sen
  group_by(continent) %>%                        # gruppera efter kontinent, och sen
  summarise(Mean = mean(lifeExp),                # summera med medelvärde,
            SD = sd(lifeExp))                    # och standardavvikelse
dat_sum

dat_sum %>% 
  mutate(mean_plus_minus_sd = paste(Mean, "±", SD))        # Skapa en ny kolumn genom att slå ihop Mean och SD

dat_sum <- dat_sum %>% 
  mutate(mean_plus_minus_sd = paste(round(Mean, 1), "±", round(SD, 1)))        # Skapa en ny kolumn med avrundade värden
dat_sum

##### Övningsuppgift 2.12 #####
# Utfallet ovan är nära men inte heller riktigt vad som behövs. I de fall där funktionen avrundat till en nolla har decimal tappats. Hur kan man visa en avslutande nolla? Följande tråd på StackOverflow besvarar samma fråga.

# https://stackoverflow.com/questions/42105336/how-to-round-a-number-and-make-it-show-zeros

# Försök använda kod därifrån för att lägga till en avslutande nolla.
##########

getwd()                                                    # Se nuvarande working directory
write_csv(dat_sum, "Exporterad data från R.csv")           # Skriv datan till en csv-fil.

## Kumulativt medelvärde

dat_dice <- data.frame(Utfall = c(6,3,2,3,5)) %>%               # Skapa data med kolumnen Utfall, och sen
  mutate(Kast = 1:n())                                          # skapa en kolumn med antal kast från 1 löpande
dat_dice

dat_dice <- dat_dice %>%                                        # Ta datan, och sen
  mutate(`Kumulativ summa` = cumsum(Utfall),                    # skapa en kolumn som ger den kumulativa summan och
         `Kumulativt medelvärde` = `Kumulativ summa` / Kast)    # dela den kumulativa summan med antalet kast
dat_dice

##### Övningsuppgift 2.13 #####
# Vad ska läggas till för att stycket nedan ska ge en linjegraf över medelvärdet?

ggplot(dat_dice, aes(x = Kast, y = ___)) +
  ___()

##########

##### Övningsuppgift 2.14 #####
# Kasta din tärning ytterligare några gånger, gärna på en mjuk yta. Fyll i dina utfall och gör grafen från föregående uppgift. Kan man se en tendens för medelvärdet att minska i varians vid fler kast?

dat_dice <- data.frame(Utfall = c(___)) %>% 
  mutate(Kast = 1:n(),                                          # Lägg till värden från 1 till n
         `Kumulativ summa` = cumsum(Utfall),                    # Beräkna kumulativ summa av utfall
         `Kumulativt medelvärde` = `Kumulativ summa` / Kast)    # Dela summan på antal för relativt medel
dat_dice

##########

##### Övningsuppgift 2.15 #####
# Om man vill titta på andelen gånger ett visst utfall inträffat talar man om *kumulativ frekvens* snarare än *kumulativt medelvärde*. Använd stycket nedan för att titta på andelen gånger utfallet varit en etta (ett *positivt* utfall, i begreppets kliniska mening). Om ett inte är ett möjligt utfall på din tärning, ändra ettan till något mer lämpligt.

dat_dice <- data.frame(Utfall = c(___)) %>% 
  mutate(Kast = 1:n(),
         `Positivt utfall` = Utfall == 1,                       # Skapa en variabel som anger om utfall var ett eller ej
         `Kumulativt antal` = cumsum(`Positivt utfall`),
         `Kumulativ frekvens` = `Kumulativt antal` / Kast)
dat_dice

ggplot(dat_dice, aes(x = Kast, y = `Kumulativ frekvens`)) +
  geom_line()

##########

## Darwin-exempel

##### Övningsuppgift 2.16 #####
# Ladda ner filen med uppgiftsdata till din lokala hårddisk.
##########

##### Övningsuppgift 2.17 #####
# Gör lämpliga ändringar i koden nedan för att läsa in fliken *Darwin*.

dat_darwin <- read_excel("Data/Uppgiftsdata.xlsx", sheet = "___")         # Läs in data från fliken Darwin
dat_darwin %>% print(n = 30)                                              # Skriv ut datan (högst 30 rader)

##########

##### Övningsuppgift 2.18 #####
# Fyll i koden nedan för att beräkna medelvärde, standardavvikelse, antal observationer och medelfel. Gör beräkningen per grupp (`Metod`)

dat_sum <- dat_darwin %>%                                                 # Ta datan, och sen
  group_by(___) %>%                                                       # gruppera efter Metod, och sen
  summarise(Medelvärde = mean(___),                                       # beräkna medelvärde,
            Standardavvikelse = ___(Utfall),                              # beräkna standardavvikelse,
            `Antal observationer` = n(),                                  # beräkna antal observationer,
            Medelfel = Standardavvikelse / sqrt(`Antal observationer`))   # beräkna medelfel.
dat_sum

##########

##### Övningsuppgift 2.19 #####
# Fyll i koden nedan för att skapa ett stapeldiagram med felstaplar av de sammanfattande måtten i objektet som skapades i uppgiften ovan. Felstaplarna styrs med argumenten `ymin` och `ymax`. Dess ska sättas till medelvärdet minus ett medelfel respektive medelvärdet plus ett medelfel. Välj lämpliga värden för staplarnas bredd. Välj en lämplig färg för staplarnas kant och inre del. Se `colors()` för en lista över färger, eller använd hex-koder som `"#ff00ff"`.

ggplot(dat_sum, aes(Metod, Medelvärde)) +
  geom_col(color = ___, fill = ___, width = ___) +
  geom_errorbar(aes(ymin = ___ - ___, ymax = ___ + ___),
                width = ___) +
  labs(caption = "Felstapel anger medelvärde ± ett medelfel")

##########

##### Övningsuppgift 2.20 #####
# Fyll i koden nedan för att skapa ett lådagram för de två metoderna. Låt x-axeln ange planthöjden (`Utfall`) och y-axeln metoden (`Metod`). Även här kan man styra färger och bredd med `color`, `fill` och `width`.

ggplot(dat_darwin, aes(x = ___, y = ___)) +
  geom_boxplot(color = "purple", fill = "hotpink", width = 0.2)

# Funktionen `quantile()` kan användas för att beräkna kvartiler. Notera att man första måste dela per grupp.

dat_darwin_kors <- dat_darwin %>% filter(Metod == "Korsbefruktade")
quantile(dat_darwin_kors$Utfall)

dat_darwin_själv <- dat_darwin %>% filter(Metod == "Självbefruktade")
quantile(dat_darwin_själv$Utfall)

# Vad är kopplingen mellan kvantilerna och lådagrammet?
##########

## Bonus. Tredimensionella grafer med `plotly`

library(plotly)

dat_ex <- data.frame(Var1 = c(1,2,3), Var2 = c(3,1,2), Var3 = c(2,3,1), Type = c("A", "B", "C"))
dat_ex

plot_ly(dat_ex, x = ~Var1, y = ~Var2, z = ~Var3, color = ~Type) %>% 
  add_markers()

##### Övningsuppgift 2.21 #####
# Vad måste läggas till i funktionen nedan för att filtrera för data där året är 2007?

dat_2007 <- gapminder %>% 
  ___(year == ___)

##########

##### Övningsuppgift 2.22 #####
# Vad måste läggas till i funktionen nedan för en 3d-graf med befolkningsmängd (`pop`)  på x-axeln, livslängd (`lifeExp`) på y-axeln, bnp per capita (`gdpPercap`) på z-axeln, och färg efter kontinent (`continent`)? För att kunna identifiera specifika länder kan man också sätta argumentet `text`.

plot_ly(___, x = ~___, y = ~___, z = ~___, color = ~___, text = ~country) %>% 
  add_markers()

##########

##### Övningsuppgift 2.23 #####
# Inom statistiken är det vanligt att transformera variabler för att ta bort extremeffekter och visa på specifika dataegenskaper. En vanlig transform är att *logaritmera* ett värde, vilket innebär att man istället för att använda det ursprungliga värdet använder exponenten i någon bas (ofta basen tio). Ta till exempel värdet 10000, dess tio-logaritm är 4, eftersom 10 upphöjt i 4 är 10000. Logaritmer är vanliga vid data med extremvärden.

# Grafen i uppgiften ovan präglas mycket av skillnader i bnp och befolkningsstorlek. Testa att tio-logaritmera variablerna och se om det blir en mer eller mindre överskådlig graf. Logaritmen kan göras genom att byta den ursprungliga variabeln mot en variabel transformerad med `log10()`. Fyll i stycket nedan.

plot_ly(___, x = ~log10(___), y = ~log10(___), z = ~___, color = ~___, text = ~country) %>% 
  add_markers()

##########

##### Övningsuppgift 2.24 #####
# Likt en ggplot kan man lägga till graf-element. Här använder man dock en pipe för lägga till ett nytt element. Fyll i kodstycket nedan. Vad, om något, har lagts till i grafen?

plot_ly(___, x = ~log10(___), y = ~log10(___), z = ~___, color = ~___, text = ~country) %>% 
  add_markers() %>% 
  add_lines(data = gapminder %>% filter(country == "Costa Rica"))

##########

##### Övningsuppgift 2.25 #####
# Som avslutning återvänder vi till spotify-datan från datorövning 1. Fyll i stycket nedan för att skapa en graf med tempo, dansbarhet och valens (`tempo`, `danceability`, `valence`) på axlarna, storlek efter energi (`energy`) och text efter spårnamn (`track_name`). Filtrera på valfri artist. Använd `unique(dat_spot$artist_name)` för att se tillgängliga artister.

dat_spot <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Spotify_data.csv")
dat_small <- dat_spot %>% filter(artist_name == "Weyes Blood")

plot_ly(dat_small, x = ~___, y = ~___, z = ~___, size = ~___, text = ~___) %>% 
  add_markers()

##########
NA
