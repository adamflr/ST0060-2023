# Datahantering och grafer
## Uppstart och orientering

##### Övningsuppgift 1.1 #####
# Hitta fliken *Help*, klicka på husikonen under fliken. Finns det en länk med *RStudio Cheat Sheets*? Följ den länken för att hitta guider till R som kan bli nyttiga längre fram. För nu, gå tillbaka till RStudio.
##########

##### Övningsuppgift 1.2 #####
# Öppna ett nytt skript genom File-menyn eller genom *Ctrl + Shift + N*.
# Skriv

a <- 5

# i skriptet och tryck *Ctrl + Enter*. Titta i flikarna *Console* och *Environment*. Har något hänt? Du bör se att koden i skriptet körts i konsollen och att ett nytt objekt `a` ligger i *Environment*.
##########

## *Packages* från CRAN

install.packages("tidyverse")

##### Övningsuppgift 1.3 #####
# Kör raden ovan för att installera `tidyverse`. Du kan antingen köra raden genom att skriva den i *Console* eller genom att skriva i ett skript och köra därifrån genom *Ctrl + Enter*.
##########

##### Övningsuppgift 1.4 #####
# Paketet `gapminder` innehåller lite intressant data vi kommer använda senare. Installera paketet `gapminder` genom att fylla i den saknade biten och köra raden nedan.

install.packages("___")

##########

library(tidyverse)

##### Övningsuppgift 1.5 #####
# Ladda paketet `gapminder` genom att fylla i och köra raden nedan.

library(___)

##########

##### Övningsuppgift 1.6 #####
# Vad händer om man försöker installera ett paket som inte finns på *CRAN*? Testa till exempel

install.packages("ThisIsNotTheNameOfAnyPackage")

# och

library(ThisIsNotTheNameOfAnyPackage)

##########

## Objekt och funktioner

a <- 5

b <- c(3, 1, 4, 1, 5, 9)

b[3]               # Det tredje värdet i vektorn b
b[c(3,5)]          # Det tredje och femte värdet i b

##### Övningsuppgift 1.7 #####
# Skapa ett objekt med namnet `new_vector` som innehåller värden 5, 7 och 10 genom att fylla i följande rad.

new_vector <- c(_, _, _)

##########

##### Övningsuppgift 1.8 #####
# Använd hakparenteser för att plocka ut det andra värdet ur vektorn `new_vector`.
##########

sum(b)

plot(b)

sqrt(b)

##### Övningsuppgift 1.9 #####
# Fyll i och kör följande rad för att beräkna summan av vektorn `new_vector`

sum(___)

##########

b <- c(3, 1, 4, 1, 5, 9, NA)           # Lägger till ett saknat värde
sum(b)                                 # na.rm = FALSE är grundinställning
sum(b, na.rm = TRUE)                   # na.rm sätts till TRUE

## Sekvenser av funktioner

c <- c(-4, -2, -1, 1, 2, 4)            # Skapa en vektor av värden
c_absolute <- abs(c)                   # Ta absolutvärden. Spara som c_absolut
sum(c_absolute)                        # Summera värden i c_absolut

##### Övningsuppgift 1.10 #####
# Fyll i och kör följande rader för att ta varje värde i `new_vector` i kvadrat, *sedan* summera, och sedan ta roten ur.

new_vector_squared <- new_vector^2     # Ta kvadraten av varje värde
new_vector_squared_sum <- sum(___)     # Summera vektorn med kvadrater
sqrt(___)                              # Ta kvadratroten ur summan

##########

sum(abs(c(-4, -2, -1, 1, 2, 4)))       # Ta summan av absolutvärden av vektorn

sqrt(sum(new_vector^2))                # Ta roten ur summan av vektorn i kvadrat

library(tidyverse)                     # Ladda tidyverse, ej nödvändigt om redan gjort

c(-4, -2, -1, 1, 2, 4) %>%             # Skapa en datamängd och sen
  abs() %>%                            # ta absolutvärden, och sen
  sum()                                # beräkna summan.

##### Övningsuppgift 1.11 #####
# Fyll i de saknade funktionerna och kör följande rader för att ta varje värde i `new_vector` i kvadrat, *sedan* summera, och sedan ta roten ur, denna gång genom att länka funktionerna med en pipe `%>%`.

new_vector^2 %>%                       # Ta kvadraterna av new_vector, och sen
  ___() %>%                            # beräkna summan, och sen
  ____()                               # Ta kvadratroten med sqrt()

##########

## Datainskrivning och dataimport från web
### Inskrivning av data

dat <- data.frame(Vecka = c(7,7,7,7,7,7,11,11,11,11,11,11),
                  Behandling = c("A","A","A","B","B","B","A","A","A","B","B","B"),
                  Vikt = c(232,161,148,368,218,257,1633,2213,972,2560,2430,855),
                  N = c(2.63,2.90,2.99,3.54,3.30,2.85,1.53,1.90,NA,2.58,NA,NA))
dat

##### Övningsuppgift 1.12 #####
# Kasta din tärning tio gånger och skriv in resultatet i en datatabell i R med hjälp av grundkoden nedan. Om du saknar en tärning, fråga lämplig person om du kan få en. Behåll tärningen, den behövs till nästa datorövning (och närhelst man står inför ett avgörande livsbeslut).

dat_dice <- data.frame(Kast = c(1,2,3,4,5,6,7,8,9,10),
                       Utfall = c(_,_,_,_,_,_,_,_,_,_))
dat_dice

##########

dat$Vikt

dat[2,3]          # Andra raden och tredje kolumnen
dat[2, ]          # Tomt värde ger alla värden. Här alla värden i rad 2
dat[ ,3]          # Alla värden i kolumn 3

##### Övningsuppgift 1.13 #####
# I den tidigare övningen skapade du ett objekt `dat_dice`. Använd dollartecken för att plocka ut kolumnen *Utfall* från det objektet.

dat_dice$___

##########

mean(dat$Vikt)
sd(dat$Vikt)

plot(dat$Vecka, dat$Vikt)

##### Övningsuppgift 1.14 #####
# Använd datan i objektet `dat_dice` och skapa ett diagram med kolumnen kast på x-axeln och kolumnen Utfall på y-axeln.

plot(dat_dice$___, dat_dice$___)

##########

### Import från en extern fil

dat <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/master/Data/Spotify_data.csv") 
                                       # Läs in en csv-fil från Github
dat                                    # Skriv ut objektet dat

unique(dat$artist_name)                # Skriv ut unika värden i kolumnen artist_name i dat

## Urval ur en tabell med `select` och `filter`

dat %>%                                # Ta spotify-datan och sen
  filter(artist_name == "Robyn")       # filtrera för en specifik artist

##### Övningsuppgift 1.15 #####
# Vad måste ändras i koden för att istället plocka ut rader där artisten är Kate Bush? Hur många rader har det urvalet i datan?

dat %>%                                # Ta spotify-datan och sen
  filter(artist_name == "Robyn")       # filtrera för en specifik artist

##########

dat %>%                                               # Ta datan, och sen
  filter(artist_name %in% c("Robyn", "Kate Bush"))    # filtrera för specifika artister

dat %>%                                             # Ta datan, och sen
  filter(artist_name %in% c("Robyn", "Kate Bush"),  # filtrera för specifika artister
         key_name == "D#")                          # och för tonart

dat %>%                                                     # Ta datan, och sen
  filter(artist_name %in% c("Robyn", "Kate Bush")) %>%      # filtrera för artister, och sen
  filter(key_name == "D#")  %>%                             # filtrera för tonart, och sen
  print(n = 5)                                              # skriv ut de fem första raderna

dat %>%                                # Ta datan, och sen
  select(artist_name, album_name)      # välj kolumnerna artist_name och album_name

dat %>%                                                    # Ta datan och sen
  filter(album_release_year == 2015,                       # filtrera för rader där år är 2015, och 
         tempo > 180) %>%                                  # tempot över 160, och sen
  select(artist_name, album_release_year, track_name)      # selektera på artist, år och spår

##### Övningsuppgift 1.16 #####
# Funktionen `arrange()` sorterar data efter en angiven kolumn. Följande stycke ger oss Björks snabbaste spår. 

dat %>%                                                    # Ta datan, och sen
  filter(artist_name == "Björk") %>%                       # filtrera för rader där artist är Björk, och sen
  select(artist_name, album_name, track_name, tempo) %>%   # välj kolumner med artist, album, spår och tempo, och sen
  arrange(-tempo)                                          # ordna efter tempo (minus för fallande ordning)

# Gör lämpliga ändringar för att hitta Carly Rae Jepsens snabbaste spår.
##########

## Grafer med `ggplot2`

dat_small <- dat %>%                                      # Ta data, och sen
  filter(artist_name == "Robyn", album_type == "album")   # filtrera på artist och albumtyp

##### Övningsuppgift 1.17 #####
# Om du vill titta på data för någon annan artist, gör lämplig ändring i stycket ovan. Kom ihåg att man kan skriva ut artister i datan med `unique(dat$artist_name)`.
##########

plot(dat_small$tempo, dat_small$danceability)   # Plotta en graf med tempo och dansbarhet på axlarna

ggplot(dat_small, aes(x = tempo, y = danceability)) +  # Ta datan, koppla tempo och dansbarhet till x och y
  geom_point()                                         # och illustrera varje observation med punkt

ggplot(dat_small, aes(x = tempo, y = danceability, color = mode_name)) +   # Ta datan, koppla tempo, dansbarhet och tonart till axlarna respektive färg
  geom_point() +                                                           # Illustrera med punkter
  facet_wrap(~ album_name)                                                 # Skapa småfönster efter album

##### Övningsuppgift 1.18 #####
# Vad ska ändras i stycket nedan för att skapa en graf med dur/moll (`mode_name`) på x-axeln, valens (`valence`) på y-axeln och skilda småfönster för olika år (`album_release_year`)?

ggplot(dat_small, aes(x = ____, y = ____, color = album_name)) + # Ta datan och koppla variabler till egenskaper
  geom_point() +                                                 # Illustrera med punkter
  facet_wrap(~ album_release_year)                               # Skapa småfönster efter år

# Har spår i dur (*major*) högre valens?
##########

dat_small <- dat %>% 
  filter(artist_name == "Phoebe Bridgers", album_type == "album")

ggplot(dat_small, aes(danceability, track_name, fill = album_name)) +
  geom_col(color = "black")

dat_small <- dat %>%                                            # Ta datan, och sen
  filter(artist_name == "Beach House", album_type == "album")   # filtrera på artist och albumtyp

ggplot(dat_small, aes(x = danceability, y = album_name)) +      # Ta data och koppla dansbarhet och album till axlarna
  geom_boxplot()                                                # Illustrera med lådagram

ggplot(dat_small, aes(danceability, album_name)) +
  geom_boxplot(fill = "lightblue") +                                         # Fyll lådagrammen med en färg
  theme(panel.background = element_rect(fill = "red3"),                      # Sätt grafenfönstrets bakgrund
        text = element_text(size = 15, color = "white", family = "serif"),   # Sätt textens storlek och snitt
        axis.text = element_text(color = "white"),                           # Sätt axel-textens färg
        plot.background = element_rect(fill = "grey30", color = "black"),    # Sätt grafens bakgrund
        panel.grid.major.y = element_blank())                                # Sätt rutnätet till blankt

##### Övningsuppgift 1.19 #####
# Ändra färgvalen i grafen ovan för att skapa snyggast möjliga graf. Funktionen `colors()` ger de färger som finns tillängliga i R. Man kan också använda hex-koden för färger, t.ex. `fill = "#ffdd00"`.
##########

##### Övningsuppgift 1.20 #####
# Ändra färgvalen i grafen ovan för att skapa fulast möjliga graf. Visa de två graferna för någon annan och se om de kan säga vilken som är vilken.
##########

## Bonus. Warming stripes

# Läs in data från fil
dat_temp <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Temperatur%2C%20Stockholm.csv")

ggplot(dat_temp, aes(x = Year, y = 1, fill = Value)) +
  geom_col() 

##### Övningsuppgift 1.21 #####
# Ett första problem är att staplarna inte fyller ytan. Man kan styra staplars bredd med argumentet `width`, t.ex.

ggplot(dat_temp, aes(x = Year, y = 1, fill = Value)) +
  geom_col(width = 0.1)

# Hitta ett värde för `width` som ger staplar utan mellanrum.
##########

##### Övningsuppgift 1.22 #####
# Ett andra problem är att ggplots grundval för färger är från svart till blått. För klassiska warming stripes vill vi ha en skala från blått till rött. Färgerna i en skala ändras med särskilda `scale_()`-funktioner. En färgskala för ifylld färg kan sättas med `scale_fill_gradientn()`, till exempel

ggplot(dat_temp, aes(x = Year, y = 1, fill = Value)) +
  geom_col(width = 0.1) +
  scale_fill_gradientn(colours = c("darkgreen", "blue", "white", "yellow", "purple"))

# Välj färger som ger en naturlig skala från blått till rött. Funktionen `colors()` ger valbara färger i R. Några möjliga val kan vara `darkblue`, `blue`, `white`, `red`, `salmon`, `darkred`, `steelblue` och `skyblue`.
##########

##### Övningsuppgift 1.23 #####
# Slutligen brukar warming stripes presenteras med så lite kringinformation som möjligt. I ggplot kan grafelement tas bort med `theme()`. Här är som exempel en graf utan y-axel, tickmärken och legend.

ggplot(dat_temp, aes(x = Year, y = Value)) +
  geom_line() +
  theme(axis.title = element_blank(),            # Sätt titel till blank
        legend.position = "none",                # Ta bort legenden
        plot.background = element_blank(),       # Sätt bakgrunden till blank
        panel.background = element_blank(),      # Sätt graffältets bakgrund till blank
        axis.text.y = element_blank(),           # Sätt axeltext till blank
        axis.ticks = element_blank())            # Sätt axelticks till blank

# Använd temat från exemplet för att skapa en enklare version av grafen från föregående uppgift. Ett liknande resultat kan fås med temat `theme_void()`.
##########

## Bonus. Interaktiva grafer med `plotly`

# install.packages("plotly")                     # Installera plotly (ej nödvändigt om redan installerat)
library(plotly)                                  # Ladda plotly

dat_small <- dat %>%                                                 # Ta datan, och sen
  filter(artist_name == "David Bowie", album_type == "album") %>%    # filtrera på artist och albumtyp, och sen
  mutate(Decade = floor(album_release_year / 10) * 10)               # skapa en variable för årtionde.

g <- ggplot(dat_small, aes(danceability, valence, color = album_name, text = track_name)) +
  geom_point() +
  facet_wrap(~ Decade) +            # Skapa småfönster per årtionde
  theme(legend.position = "none")   # Ta bort legenden (kopplingen mellan färg och album)
g

ggplotly(g)

##### Övningsuppgift 1.24 #####
# Gör lämpliga ändringar i stycket nedan för att skapa en interaktiv graf med en annan artist och med tempo på x-axeln och dansbarhet på y-axeln. Kom ihåg att du kan se tillgängliga artister med raden `unique(dat$artist_name)`.

dat_small <- dat %>% 
  filter(artist_name == "David Bowie", album_type == "album") %>% 
  mutate(Decade = floor(album_release_year / 10) * 10)

g <- ggplot(dat_small, aes(danceability, valence, color = album_name, text = track_name)) +
  geom_point() +                                 # Skapa punkter
  facet_wrap(~ Decade) +                         # Skapa småfönster per årtionde
  theme(legend.position = "none")                # Ta bort legenden (kopplingen mellan färg och album)
g

ggplotly(g)

##########

##### Övningsuppgift 1.25 #####
# Vi fortsätter med ett nytt exempel, nu med ett spridningsdiagram med album på y-axeln och valens på x-axeln. Funktionen `reorder()` ordnar en kolumn efter en annan. Här ordnas album efter release-år.

dat_small <- dat %>% 
  filter(artist_name == "Rihanna", album_type == "album") %>% 
  mutate(Decade = floor(album_release_year / 10) * 10,
         Decade = as.character(Decade),
         album_name = reorder(album_name, album_release_year))

g <- ggplot(dat_small, aes(valence, album_name, color = Decade, text = track_name)) +
  geom_point()
g

# Ändra gärna artist på lämpligt ställe.
# Vad måste läggas till för en interaktiv version av samma graf?
##########

