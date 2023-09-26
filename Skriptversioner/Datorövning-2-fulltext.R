# # Beskrivande statistik
#
# Datorövning 2 handlar om beräkning av beskrivande statistik i R. Efter övningen ska vi kunna
#
# - Importera data från en excelfil,
#
# - Beräkna lämpliga lägesmått för en variabel,
#
# - Beräkna lämpliga spridningsmått för en variabel,
#
# - Konstruera grafer som jämför två eller flera gruppers läge och spridning.
#
# ## Repetition från datorövning 1
#
# När man arbetar i R är det klokt att använda ett avancerat gränssnitt som RStudio och att skriva
# sin kod i ett separat skript. I RStudio kan man starta ett nytt skript genom *Ctrl + Shift + N*.
#
# Mycket funktionalitet i R ligger i tilläggspaket (*Packages*). Paket måste installeras första
# gången de används och laddas varje session de används, t.ex.
#

# install.packages("tidyverse") # Installera tidyverse (behövs ej om redan gjort)
library(tidyverse)                     # Ger ett felmeddelande om paketet inte installerats

#
# Data läses in med import-funktioner där valet av funktion beror på typen av fil. Importerad data
# sparas som ett objekt i R genom *assign*-pilen `<-`.
#

dat <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Spotify_data.csv")
dat                                              # Skriv ut objektet dat

#
# Funktioner agerar på objekt och ger något utfall. Här beräknas medeltempot med funktionen
# `mean()`. Dollartecknet används för att ange en specifik kolumn i dataobjektet. Funktioner styrs av
# möjliga argument - här används `na.rm` för att ange att saknade värden inte ska tas med i
# beräkningen
#

mean(dat$tempo, na.rm = T)                       # Beräkna medelvärdet av kolumnen tempo i datan dat

#
# Funktionerna `filter()` och `select()` kan användas för att välja kolumner och rader. Funktioner
# kan länkas samman med en pipe `%>%` för att skapa sekvenser av funktioner. Man kan tänka på pipen
# som *och sen*.
#

dat %>%                                                    # Ta datan, och sen
  filter(artist_name == "Tame Impala", tempo > 170) %>%    # ta ut rader där artisten är Tame Impala och tempot är större än 170, och sen
  select(artist_name, track_name, tempo)                   # ta ut kolumnerna artist_name, track_name och tempo

#
# Slutligen tittade vi på grafer med ggplot2-paketet. En ggplot byggs upp med tre grundelar: data,
# geometrier (grafens objekt och former), och *aesthetics* (utseende och placering av geometrierna).
# I ett enkelt spridningsdiagram är data två numeriska variabler, geometrierna är punkter, och
# punkternas placering ges av en x-koordinat och en y-koordinat. Ytterligare *aesthetics* kan vara
# punkternas färger (`color`) och storlek (`size`).
#

dat_small <- dat %>% filter(artist_name == "The Weeknd")                            # Skapa en mindre datamängd genom att filtrera på en artist

ggplot(dat_small, aes(tempo, danceability, size = valence, color = mode_name)) +    # Koppla grafegenskaper och variabler
  geom_point()                                                                      # Illustrera med punkter

#
# ## Import av data från en Excelfil
#
# Inom vetenskapen är Excel det vanligaste filformatet för mindre datamängder. Till den här delen
# ska vi arbeta med data från *Gapminder*, en stiftelse som sprider information om socio-ekonomisk
# utveckling och global hälsa.
#
# Uppgift 2.1. (Excelfil från Canvas)
# Hitta excelfilen *Gapminder.xlsx* på Canvas och ladda ner den. Hitta mappen som filen laddats ned
# till.
# :::
#
# I R kan man läsa in data från en Excel-fil med funktionen `read_excel()` från paketet `readxl`.
# Som argument till funktionen sätts filens sökväg - dess placering på hårddisken. Stycket nedan
# importerar från en excelfil som ligger på hårddisken *C:* i mappen *Downloads*, under *User_name*,
# under *Users*.
#

library(readxl)                                                           # Ladda readxl

#
#

gapminder <- read_excel("C:/Users/User_name/Downloads/Gapminder.xlsx")    # Läs in från en lokal excelfil
gapminder                                                                 # Skriv ut objektet gapminder

#
# Uppgift 2.2. (Importera från excelfil)
# Var ligger den nedladdade filen *Gapminder.xlsx*? Gör lämplig ändring i koden ovan för att läsa
# in data från den filen. Notera att R använder högerlutande snedstreck `/`, så om en kopierad sökväg
# har vänster-snedstreck måste de ändras. Kontrollera att datan blivit korrekt inläst genom att köra
# objektnamnet `gapminder`.
# :::
#
# En R-session har alltid en grundmapp, ett *Working directory*. Man kan se vilken mapp det är
# genom att köra
#

getwd()                                # Ange working directory

#
# En filsökväg kan anges antingen som en fullständig sökväg, som ovan, eller relativt *working
# directory*. Om man till exempel har en fil *Gapminder.xlsx* som ligger i en mapp *Data* som i sin
# tur ligger i *working directory*, kan man importera data från filen med
#

gapminder <- read_excel("Data/Gapminder.xlsx")             # Läs in från en lokal excelfil (relativt wd)
gapminder                                                  # Skriv ut objektet gapminder

#
# Uppgift 2.3. (Working directory)
# Identifiera *working directory* för din nuvarande Rs-session genom att köra `getwd()`.
# :::
#
# RStudio har också en inbyggd funktionalitet för att importera data. Man kan hitta den genom att
# gå till *Environment*-fliken och sedan *Import Dataset*. Det kan vara en bra hjälp, i synnerhet om
# man vill sätta datatyp för någon specifik kolumn.
#
# Om du inte har tillgång till Canvas kan Gapminder-datan alternativt hämtas från paketet
# Gapminder.
#

# install.packages("gapminder") # Installera paketet gapminder (behövs ej om redan gjort)
library(gapminder)                     # Ladda gapminder
gapminder                              # Skriv ut objektet gapminder

#
# ## Ändra och skapa nya kolumner med `mutate`
#
# Variabler kan omräknas och nya variabler kan skapas med `mutate`-funktionen. I gapminder-datan
# finns bnp per capita. Om man vill ha nationell BNP kan man skapa en ny kolumn och beräkna den som
# bnp per capita gånger populationen.
#

gapminder <- gapminder %>%             # Ta datan, och sen
  mutate(gdp = gdpPercap * pop)        # Beräkna en ny kolumn som bnp per capita (bnpPercap) gånger befolkningen (pop)

#
# Den inledande delen med `gapminder <-` gör så att utfallet av beräkningen sparas i objektet
# `gapminder`.
# Vi kan skriva ut objektet och se resultatet av beräkningen:
#

gapminder %>% select(gdpPercap, pop, gdp)        # Ta datan och sen välj tre kolumner

#
# Om man vill skapa en kolumn med mellanrum i namnet måste man skriva namnet inom *backticks* `\``
# för att ange att namnet ska tolkas som en enhet. Jag rekommenderar att undvika mellanrum i
# kolumnnamn och istället använda stora bokstäver eller understreck för ett nytt ord (`NationalGDP`
# eller `National_GDP`).
#

gapminder %>% 
  mutate(`National GDP` = gdpPercap * pop)

#
# ## Sammanfattande lägesmått
#
# Den importerade datan ger medellivslängd, populationsstorlek och bnp per capita per land och år.
# Vi kan börja med att producera en bubbelgraf över datan - en av de presentationer Gapminder ofta
# använder. En bubbelgraf är ett spridningsdiagram där punktens storlek beror på en tredje variabel.
#

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +    # Koppla grafens egenskaper till kolumner
  geom_point() +                                                               # Illustrera med punkter
  facet_wrap(~ year)                                                           # Skapa småfönster efter år

#
# En interaktiv version kan vara bra om man vill identifiera någon specifik punkt.
#

g <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, text = country)) +
  geom_point() +
  facet_wrap(~ year)

# install.packages("plotly")
library(plotly)                        # Ladda paketet plotly
ggplotly(g)                            # Ta fram en interaktiv version av grafen g

#
# Under föreläsningen såg vi exempel på två lägesmått: medelvärdet (egentligen det *aritmetiska*
# medelvärdet) och medianen. De har bägge enkla funktioner i R: `mean()` respektive `median()`. Vi
# plockar ut en variabel ur datan och beräknar bägge.
#

gdpPercap <- gapminder$gdpPercap       # Skapa en vektor gdpPercap genom att ta ut kolumnen från gapminder
mean(gdpPercap)                        # Beräkna medelvärdet av gdpPercap
median(gdpPercap)                      # Beräkna medianen av gdpPercap

#
# Samma sak kan göras med en pipe `%>%` och `summarise()`.
#

gapminder %>%                                    # Ta datan, och sen
  summarise(Mean = mean(gdpPercap),              # summera med medelvärdet av gdpPercap och
            Median = median(gdpPercap))          # med medianen av gdpPercap

#
# Uppgift 2.4. (Lägesmått av livslängd)
# Gör lämpliga ändringar i exemplet ovan för att beräkna lägesmått för medellivslängd (`lifeExp`).
# :::
#
# Den andra lösningen, med en pipe och `summarise()`, kan enkelt utvecklas med ett
# `group_by()`-steg för att beräkna medel och median per någon grupp, t.ex. per år.
#

gapminder %>%                                    # Ta datan, och sen
  group_by(year) %>%                             # gruppera efter år, och sen
  summarise(Mean = mean(gdpPercap),              # summera med medelvärdet av gdpPercap och
            Median = median(gdpPercap))          # med medianen av gdpPercap

#
# Uppgift 2.5. (Lägesmått per kontinent)
# Gör lämpliga ändringar i exemplet ovan för att beräkna lägesmått per kontinent. Vad måste läggas
# till för att också beräkna maximum och minimum per kontinent (funktionerna `max()` och `min()`)?
# :::
#
# Uppgift 2.6. (Upprepade mätningar)
# Finns det några problem med att beräkna medelvärde per kontinent på den här datan? (Jag kan se
# minst två.)
# :::
#
# I vetenskapliga publikationer redovisas medelvärden ofta med ett stapeldiagram. Som exempel ges
# staplar för medelvärdet av bnp per kontinent för 2007.
#

dat_gdp_2007 <- gapminder %>%                    # Ta datan, och sen
  filter(year == 2007) %>%                       # filtrera för 2007, och sen
  group_by(continent) %>%                        # gruppera efter kontinent, och sen
  summarise(Mean = mean(gdpPercap))              # summera med medelvärdet av gdpPercap

ggplot(dat_gdp_2007, aes(continent, Mean)) +     # Skapa en ggplot med continent på x-axeln och Mean på y-axeln
  geom_col()                                     # Illustrera med kolumner (columns)

#
# För att få lite mer information kan man lägga till de individuella punkterna.
#

ggplot(dat_gdp_2007, aes(continent, Mean)) +
  geom_col() +
  geom_point(aes(continent, gdpPercap), data = gapminder %>% filter(year == 2007))

#
# Eftersom `geom_point()` här bygger på annan data än kolumnerna från `geom_col()` anger vi en ny
# `aes()`-funktionen och sätter argumentet `data`.
#
# Uppgift 2.7. (Graf för livslängd)
# Gör om stapeldiagrammet. Denna gång med livslängd (`lifeExp`) istället för bnp per capita
# (`gdpPercap`).
# :::
#
# ## Sammanfattande spridningsmått
#
# Under föreläsningarna såg vi några mått på spridning: varians, standardavvikelse och
# kvartilavstånd (IQR, *inter-quartile range*). De har alla motsvarande funktioner i R (`var()`,
# `sd()`, och `IQR()`) som kan användas på samma sätt som funktionerna för lägesmått.
#

gdpPercap <- gapminder$gdpPercap                 # Skapa en vektor gdpPercap genom att ta ut kolumnen från gapminder

var(gdpPercap)                                   # Beräkna variansen av gdpPercap
sd(gdpPercap)                                    # Beräkna standardavvikelsen av gdpPercap
IQR(gdpPercap)                                   # Beräkna kvartilavståndet av gdpPercap

#
# Alternativt med en pipe och `summarise()`.
#

gapminder %>%                                    # Ta datan, och sen
  summarise(Varians = var(gdpPercap),            # summera med varians,
            Standardavvikelse = sd(gdpPercap),   # standardavvikelse,
            Kvartilavstånd = IQR(gdpPercap))     # och kvartilavstånd

#
# Lösningen med pipe och `summarise()` kan som tidigare utvecklas med `group_by()`.
#

gapminder %>%                                    # Ta datan, och sen
  group_by(year) %>%                             # gruppera efter år, och sen
  summarise(Varians = var(gdpPercap),            # summera med varians,
            Standardavvikelse = sd(gdpPercap),   # standardavvikelse,
            Kvartilavstånd = IQR(gdpPercap))     # och kvartilavstånd

#
# Uppgift 2.8. (Graf för livslängd)
# Gör lämpliga ändringar i det sista exempel för att istället beräkna spridningsmått för livslängd.
# :::
#
# Vi avslutar med tre vanliga illustrationer av vetenskaplig data - ett linjediagram med
# felstaplar, ett stapeldiagram med felstaplar, och ett lådagram. För linjediagrammet beräknar vi
# medelvärdet och spridningsmått för bnp över år och kontinent. Som spridningsmått använder vi
# *medelfelet*, vilket beräknas som standardavvikelse delat på roten ur antalet observationer.
#

dat_sum <- gapminder %>%                         # Ta datan, och sen
  group_by(year, continent) %>%                  # grupper efter år och kontinent, och sen
  summarise(Mean = mean(gdpPercap),              # summera med medelvärde
            SE = sd(gdpPercap) / sqrt(n()))      # och medelfel (standardavvikelsen delat på roten ur n)
dat_sum

#
# Med `ggplot2` kan vi bygga ett linjediagram med `geom_line()` och lägga till felstaplar med
# `geom_errorbar()`. Den senare behöver `aes()`-argument för `ymin` och `ymax` - nedre och övre del
# av felstapeln. Vi sätter dem till medelvärdet minus respektive plus ett medelfel.
#

ggplot(dat_sum, aes(year, Mean, color = continent)) +      # Skapa en ggplot från datan dat_sum med year som x-axel, Mean som y-axel och färg efter kontinent
  geom_line() +                                            # Illustrera med linjer
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE))   # Illustrera med felstaplar

#
# Uppgift 2.9. (Bredd)
# Felstaplarna från `geom_errorbar()` har väldigt breda ändar. Använd hjälpsidan för geomet
# `?geom_errorbar`, i synnerhet exemplen längst ned, och se om det går att ändra bredden.
# :::
#
# En graf med staplar och felstaplar kan konstrueras på ett likande sätt. Följande exempel visar
# staplar över livslängd per kontinent. Felstapeln ges av standardavvikelsen.
#

dat_sum <- gapminder %>%                         # Ta datan, och sen
  filter(year == 2007) %>%                       # filtrera på år, och sen
  group_by(continent) %>%                        # gruppera efter kontinent, och sen
  summarise(Mean = mean(lifeExp),                # summera med medelvärde,
            SD = sd(lifeExp))                    # och standardavvikelse
dat_sum

#
# Vi bygger en ggplot med `geom_col()` och `geom_errorbar()`. Felstapels konstruktion kan anges i
# en notis med funktionen `labs()`.
#

ggplot(dat_sum, aes(continent, Mean, fill = continent)) +          # Skapa en ggplot med continent och Mean på axlarna och ifylld färg given av kontinent
  geom_col()+                                                      # Illustrera med kolumner
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD)) +         # Illustrera med felstaplar
  labs(title = "Average life expectancy by continent, 2007",       # Ange titel och förklarande text
       caption = "Errorbars given by mean +/- standard deviation.
       Source: Gapminder")

#
# Uppgift 2.10. (Staplar för 1982)
# Gör lämpliga ändringar i exempel ovan för att konstruera ett stapeldiagram med felstaplar för
# året 1982 och variabeln gdpPercap.
# :::
#
# Ett lådagram anger fördelningen av en variabel genom att illustrera minimum, maximum och
# kvartiler. Kvartiler är mått som delar en datamängd i fyra lika stora delar (så att en fjärdedel
# ligger under första kvartilen, en fjärdedel mellan första och andra kvartil, och så vidare). Med
# `ggplot2` kan vi bygga ett lådagram med `geom_boxplot()`. Exempel ger en låda per år och kontinent.
#

ggplot(gapminder, aes(year, lifeExp, fill = continent, group = year)) +        # Skapa en ggplot med år och lifeExp på axlarna, kontinent som ifylld färg, och gruppera efter år
  geom_boxplot() +                                                             # Illustrera med lådagram
  facet_wrap(~ continent)                                                      # Småfönster efter kontinent

#
# Uppgift 2.11. (Group-argumentet)
# I lådagrammet används argumentet `group`. Vad gör det? Vad händer om man tar bort det?
# :::
#
# Om man vill ha med både data på observationsnivån och summerad data i en graf kan man ange olika
# data till olika geom. Vi kan t.ex. lägga till en markör för medelvärdet i en boxplot.
#

gapminder_2007 <- gapminder %>% filter(year == 2007)
gapminder_agg <- gapminder_2007 %>% 
  group_by(continent) %>% 
  summarise(lifeExp = mean(lifeExp))
gapminder_agg

ggplot() +
  geom_boxplot(aes(lifeExp, continent), data = gapminder_2007) +
  geom_point(aes(lifeExp, continent), data = gapminder_agg, color = "red", shape = "X", size = 6)

#
# Här byggs boxarna med datan gapminder och punkterna med den aggregerade datan.
#
# Möjligheten att sätta data inom en `geom_()`-funktion kan blandas med `filter()` för att visa
# olika data i olika geom. Här ges ett spridningsdiagram över bnp och medellivslängd per land, och
# linjeserier (`geom_path()`) över tid för två specifika länder. Geom `geom_point()` är alltså på
# flera länder vid en tid, medan `geom_path()` är över tid för ett land.
#

gapminder_eu <- gapminder %>% filter(continent == "Europe")

ggplot(gapminder_eu, aes(gdpPercap, lifeExp, group = country, label = country)) +
  geom_text(data = gapminder_eu %>% filter(year == 2007)) +
  geom_path(color = "blue", size = 3, data = gapminder_eu %>% filter(country == "Sweden"), alpha = 0.3) +
  geom_path(color = "red", size = 3, data = gapminder_eu %>% filter(country == "Poland"), alpha = 0.3)

#
# ## Ordna upp beskrivande statistik och exportera
#
# Efter att ha beräknat någon beskrivande statistik kan det vara bra att titta på hur resultaten
# kan snyggas upp och exporteras i något lämpligt format. Ta den tabell med medelvärden vi
# producerade i ett tidigare exempel.
#

dat_sum <- gapminder %>%                         # Ta datan, och sen
  filter(year == 2007) %>%                       # filtrera på år, och sen
  group_by(continent) %>%                        # gruppera efter kontinent, och sen
  summarise(Mean = mean(lifeExp),                # summera med medelvärde,
            SD = sd(lifeExp))                    # och standardavvikelse
dat_sum

#
# Ett vanligt skrivsätt för medelvärde och standardavvikelse är som *m ± sd*. Vi kan använda
# funktionen `paste()` för att slå ihop kolumner till en sammanhängande text.
#

dat_sum %>% 
  mutate(mean_plus_minus_sd = paste(Mean, "±", SD))        # Skapa en ny kolumn genom att slå ihop Mean och SD

#
# Vi måste avrunda medelvärdet och standardavvikelsen först. Det kan man göra med `round()`, vars
# argument är den variabel man vill avrunda och antalet decimaler man vill avrunda till.
#

dat_sum <- dat_sum %>% 
  mutate(mean_plus_minus_sd = paste(round(Mean, 1), "±", round(SD, 1)))        # Skapa en ny kolumn med avrundade värden
dat_sum

#
# Uppgift 2.12. (Tappade nollor)
# Utfallet ovan är nära men inte heller riktigt vad som behövs. I de fall där funktionen avrundat
# till en nolla har decimal tappats. Hur kan man visa en avslutande nolla? Följande tråd på
# StackOverflow besvarar samma fråga.
#
# https://stackoverflow.com/questions/42105336/how-to-round-a-number-and-make-it-show-zeros
#
# Försök använda kod därifrån för att lägga till en avslutande nolla.
# :::
#
# Objekt kan exporteras från R på liknande som det importeras - med särskilda exportfunktioner
# (*write*-funktioner) beroende på filtyp. För att exportera till en csv-fil man man anvnäda
# `write_csv()`. Ingående argument är det objekt man vill exportera och den fil man vill exportera
# till. R ger ingen varning om man skriver över en existerande fil, så var lite försiktiga här.
#
# Precis som vid import använda R *working directory* om inget annat anges. Följande exporterar
# objektet `dat_sum` till en csv-fil *Exporterad data från R.csv* i *working directory*.
#

getwd()                                                    # Se nuvarande working directory
write_csv(dat_sum, "Exporterad data från R.csv")           # Skriv datan till en csv-fil.

#
# Därifrån skulle man kunna öppna filen i något kalkylprogram, snygga till layouten, och sedan
# klippa in i ett textdokument.
#
# ## Kumulativt medelvärde
#
# Om man har data som av någon anledning samlas in i sekvens kan det vara intressant att beräkna
# och illustrera den med ett *kumulativt medelvärde*. En serie med kumulativa medelvärden beräknas
# genom att för varje nytt värde ta medelvärden av de värden man hittills samlat in - vid tio värden
# tar man medelvärdet av de tio, vid elva värden medelvärdet av de elva, och så vidare.
#
# Med de tärningsvärden vi hade innan kan vi beräkna ett kumulativt medelvärde genom att först
# beräkna summan med `cumsum()` och sedan dela på antalet kast. För att förenkla beräkningen av
# antalen tar vi fram sekvensen med antal kast i ett `mutate()`-steg.
#

dat_dice <- data.frame(Utfall = c(6,3,2,3,5)) %>%               # Skapa data med kolumnen Utfall, och sen
  mutate(Kast = 1:n())                                          # skapa en kolumn med antal kast från 1 löpande
dat_dice

dat_dice <- dat_dice %>%                                        # Ta datan, och sen
  mutate(`Kumulativ summa` = cumsum(Utfall),                    # skapa en kolumn som ger den kumulativa summan och
         `Kumulativt medelvärde` = `Kumulativ summa` / Kast)    # dela den kumulativa summan med antalet kast
dat_dice

#
# Om beräkning inte är uppenbar här, ta någon minut för att förstå den.
#
# Uppgift 2.13. (Kumulativt medelvärde)
# Vad ska läggas till för att stycket nedan ska ge en linjegraf över medelvärdet?
#

ggplot(dat_dice, aes(x = Kast, y = ___)) +
  ___()

# :::
#
# Uppgift 2.14. (Fler tärningskast)
# Kasta din tärning ytterligare några gånger, gärna på en mjuk yta. Fyll i dina utfall och gör
# grafen från föregående uppgift. Kan man se en tendens för medelvärdet att minska i varians vid fler
# kast?
#

dat_dice <- data.frame(Utfall = c(___)) %>% 
  mutate(Kast = 1:n(),                                          # Lägg till värden från 1 till n
         `Kumulativ summa` = cumsum(Utfall),                    # Beräkna kumulativ summa av utfall
         `Kumulativt medelvärde` = `Kumulativ summa` / Kast)    # Dela summan på antal för relativt medel
dat_dice

# :::
#
# Uppgift 2.15. (Kumulativ frekvens)
# Om man vill titta på andelen gånger ett visst utfall inträffat talar man om *kumulativ frekvens*
# snarare än *kumulativt medelvärde*. Använd stycket nedan för att titta på andelen gånger utfallet
# varit en etta (ett *positivt* utfall, i begreppets kliniska mening). Om ett inte är ett möjligt
# utfall på din tärning, ändra ettan till något mer lämpligt.
#

dat_dice <- data.frame(Utfall = c(___)) %>% 
  mutate(Kast = 1:n(),
         `Positivt utfall` = Utfall == 1,                       # Skapa en variabel som anger om utfall var ett eller ej
         `Kumulativt antal` = cumsum(`Positivt utfall`),
         `Kumulativ frekvens` = `Kumulativt antal` / Kast)
dat_dice

ggplot(dat_dice, aes(x = Kast, y = `Kumulativ frekvens`)) +
  geom_line()

# :::
#
# ## Darwin-exempel
#
# På kursen Canvas-sida finns en excelfil *Uppgiftdata.xlsx* som innehåller data för de flesta
# räkneupgifter på kursen. I den här delen ska vi titta på datan i fliken *Darwin* som innehåller en
# jämförelse i planthöjd mellan kors- och självbefruktade plantor.
#
# Uppgift 2.16. (Ladda ner uppgiftsdata)
# Ladda ner filen med uppgiftsdata till din lokala hårddisk.
# :::
#
# Vi såg tidigare hur en excelfil kan läsas in med `read_excel()`. Ett argument den funktionen kan
# ta är `sheet`, som styr vilken flik som ska läsas in. Som tidigare måste man ange var på datorn
# excel-filen ligger.
#
# Uppgift 2.17. (Läs in Darwin-datan)
# Gör lämpliga ändringar i koden nedan för att läsa in fliken *Darwin*.
#

dat_darwin <- read_excel("Data/Uppgiftsdata.xlsx", sheet = "___")         # Läs in data från fliken Darwin
dat_darwin %>% print(n = 30)                                              # Skriv ut datan (högst 30 rader)

# :::
#
# När data är inläst kan man sammanfatta den med medelvärde, standardavvikelse och medelfel (där
# medelfelet ges av standardavvikelsen delad på roten ur antalet observationer).
#
# Uppgift 2.18. (Sammanfatta Darwin-datan)
# Fyll i koden nedan för att beräkna medelvärde, standardavvikelse, antal observationer och
# medelfel. Gör beräkningen per grupp (`Metod`)
#

dat_sum <- dat_darwin %>%                                                 # Ta datan, och sen
  group_by(___) %>%                                                       # gruppera efter Metod, och sen
  summarise(Medelvärde = mean(___),                                       # beräkna medelvärde,
            Standardavvikelse = ___(Utfall),                              # beräkna standardavvikelse,
            `Antal observationer` = n(),                                  # beräkna antal observationer,
            Medelfel = Standardavvikelse / sqrt(`Antal observationer`))   # beräkna medelfel.
dat_sum

# :::
#
# Slutligen kan vi presentera de sammanfattande måtten med en lämplig graf. Ett vanligt val är ett
# stapeldiagram med felstaplar för medelfelen.
#
# Uppgift 2.19. (Illustrera Darwin-datan)
# Fyll i koden nedan för att skapa ett stapeldiagram med felstaplar av de sammanfattande måtten i
# objektet som skapades i uppgiften ovan. Felstaplarna styrs med argumenten `ymin` och `ymax`. Dess
# ska sättas till medelvärdet minus ett medelfel respektive medelvärdet plus ett medelfel. Välj
# lämpliga värden för staplarnas bredd. Välj en lämplig färg för staplarnas kant och inre del. Se
# `colors()` för en lista över färger, eller använd hex-koder som `"#ff00ff"`.
#

ggplot(dat_sum, aes(Metod, Medelvärde)) +
  geom_col(color = ___, fill = ___, width = ___) +
  geom_errorbar(aes(ymin = ___ - ___, ymax = ___ + ___),
                width = ___) +
  labs(caption = "Felstapel anger medelvärde ± ett medelfel")

# :::
#
# Ett annat alternativ för en graf ett lådagram per grupp. Här används den ursprungliga datan,
# snarare än beräknad beskrivande statistik.
#
# Uppgift 2.20. (Illustrera Darwin-datan)
# Fyll i koden nedan för att skapa ett lådagram för de två metoderna. Låt x-axeln ange planthöjden
# (`Utfall`) och y-axeln metoden (`Metod`). Även här kan man styra färger och bredd med `color`,
# `fill` och `width`.
#

ggplot(dat_darwin, aes(x = ___, y = ___)) +
  geom_boxplot(color = "purple", fill = "hotpink", width = 0.2)

#
# Funktionen `quantile()` kan användas för att beräkna kvartiler. Notera att man första måste dela
# per grupp.
#

dat_darwin_kors <- dat_darwin %>% filter(Metod == "Korsbefruktade")
quantile(dat_darwin_kors$Utfall)

dat_darwin_själv <- dat_darwin %>% filter(Metod == "Självbefruktade")
quantile(dat_darwin_själv$Utfall)

#
# Vad är kopplingen mellan kvantilerna och lådagrammet?
# :::
#
# ## Bonus. Tredimensionella grafer med `plotly`
#
# Förra gången använde vi paketet `plotly` för att göra en interaktiv graf. Paketet har också
# funktioner för 3d-grafer. Börja med att ladda paketet.
#

library(plotly)

#
# Vi börjar med ett enkelt exempel på en 3d-graf med lite skapad data.
#

dat_ex <- data.frame(Var1 = c(1,2,3), Var2 = c(3,1,2), Var3 = c(2,3,1), Type = c("A", "B", "C"))
dat_ex

plot_ly(dat_ex, x = ~Var1, y = ~Var2, z = ~Var3, color = ~Type) %>% 
  add_markers()

#
# Om grafen inte kommer upp direkt kan det fungera att trycka på den lilla ikonen med ett fönster
# och en pil i *Viewer*-fliken. Grafen ska då öppnas i en webbläsare.
#
# Syntaxen till `plot_ly()` är inte helt olik `ggplot()`. Först anges datan, därefter argument för
# x- y-, och z-koordinater. Notera tilde-tecknet `~` före variabelnamnen. Eventuell färg sätts med
# `color`. Efter det lägger man till punkter (här *markers*) med en pipe in i `add_markers()`. Vi
# vill göra en liknande graf med gapminder-datan, men får börja med att filtrera på ett visst år.
#
# Uppgift 2.21. (Filtrera för år)
# Vad måste läggas till i funktionen nedan för att filtrera för data där året är 2007?
#

dat_2007 <- gapminder %>% 
  ___(year == ___)

# :::
#
# Vi kan nu konstruera en 3d-graf med datan.
#
# Uppgift 2.22. (Gapminder i 3d)
# Vad måste läggas till i funktionen nedan för en 3d-graf med befolkningsmängd (`pop`) på x-axeln,
# livslängd (`lifeExp`) på y-axeln, bnp per capita (`gdpPercap`) på z-axeln, och färg efter kontinent
# (`continent`)? För att kunna identifiera specifika länder kan man också sätta argumentet `text`.
#

plot_ly(___, x = ~___, y = ~___, z = ~___, color = ~___, text = ~country) %>% 
  add_markers()

# :::
#
# Uppgift 2.23. (Log-transformationer)
# Inom statistiken är det vanligt att transformera variabler för att ta bort extremeffekter och
# visa på specifika dataegenskaper. En vanlig transform är att *logaritmera* ett värde, vilket
# innebär att man istället för att använda det ursprungliga värdet använder exponenten i någon bas
# (ofta basen tio). Ta till exempel värdet 10000, dess tio-logaritm är 4, eftersom 10 upphöjt i 4 är
# 10000. Logaritmer är vanliga vid data med extremvärden.
#
# Grafen i uppgiften ovan präglas mycket av skillnader i bnp och befolkningsstorlek. Testa att
# tio-logaritmera variablerna och se om det blir en mer eller mindre överskådlig graf. Logaritmen kan
# göras genom att byta den ursprungliga variabeln mot en variabel transformerad med `log10()`. Fyll i
# stycket nedan.
#

plot_ly(___, x = ~log10(___), y = ~log10(___), z = ~___, color = ~___, text = ~country) %>% 
  add_markers()

# :::
#
# Uppgift 2.24. (Följa ett land)
# Likt en ggplot kan man lägga till graf-element. Här använder man dock en pipe för lägga till ett
# nytt element. Fyll i kodstycket nedan. Vad, om något, har lagts till i grafen?
#

plot_ly(___, x = ~log10(___), y = ~log10(___), z = ~___, color = ~___, text = ~country) %>% 
  add_markers() %>% 
  add_lines(data = gapminder %>% filter(country == "Costa Rica"))

# :::
#
# Uppgift 2.25. (Spotify 3d)
# Som avslutning återvänder vi till spotify-datan från datorövning 1. Fyll i stycket nedan för att
# skapa en graf med tempo, dansbarhet och valens (`tempo`, `danceability`, `valence`) på axlarna,
# storlek efter energi (`energy`) och text efter spårnamn (`track_name`). Filtrera på valfri artist.
# Använd `unique(dat_spot$artist_name)` för att se tillgängliga artister.
#

dat_spot <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Spotify_data.csv")
dat_small <- dat_spot %>% filter(artist_name == "Weyes Blood")

plot_ly(dat_small, x = ~___, y = ~___, z = ~___, size = ~___, text = ~___) %>% 
  add_markers()

# :::
