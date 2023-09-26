# Test för två stickprov
## Repetition av datorövning 5

# install.packages("tidyverse")
library(tidyverse)

prop.test(x = 16, n = 20, p = 0.5, correct = F)

library(binom)
binom.asymp(16, 20)

chisq.test(c(102, 53, 75, 12), p = c(0.5, 0.15, 0.25, 0.1))

## Två stickprov och normalfördelad data
### t-test för två matchade stickprov

dat_apple <- tibble(Tree = 1:4, 
              Before = c(48, 43, 30, 47), 
              After = c(51, 44, 42, 54))
dat_apple

dat_long <- dat_apple %>% pivot_longer(-Tree, names_to = "Time", values_to = "Height")
dat_long

##### Övningsuppgift 6.1 #####
# Fyll i kodstycket nedan för en graf av äppeldatan. Axlarna ges av `Time` och `Height`. Två observationer kan kopplas genom att sätta `Tree` som grupp.

ggplot(dat_long, aes(___, ___, group = ___)) +
  geom_point() +
  geom_line()

##########

t.test(dat_apple$Before - dat_apple$After)

t.test(dat_apple$Before, dat_apple$After, paired = T)

##### Övningsuppgift 6.2 #####
# Gör ett tillägg till ett av kodstyckena med `t.test()` för att beräkna ett ensidigt test med mothypotesen att träden ökar i höjd efter behandling. Hjälpsidan för `t.test()` kan tas fram genom att köra `?t.test()`.
##########

##### Övningsuppgift 6.3 #####
# Åtta monoglukosidmätningar på lök samlas in från fyra konventionella och fyra ekologiska ordlare. Resultatet finns i fliken *Lökfärg* i excelfilen *Uppgiftsdata.xlsx* på canvassidan. Ladda ner filen och importera datan genom att fylla i raden nedan.

library(readxl)
dat_onion <- read_excel("____", sheet = "Lökfärg")
# dat_onion <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Uppgiftsdata/Uppgift_L%C3%B6kf%C3%A4rg.csv") # Alternativ lösning

##########

##### Övningsuppgift 6.4 #####
# Fyll i stycket nedan för en graf av lökdatan från föregående uppgift.

dat_long <- dat_onion %>% 
  pivot_longer(-Odlare, names_to = "Odlingstyp", values_to = "Utfall")
dat_long

ggplot(dat_long, aes(___, ___, group = Odlare)) +
  geom_point() +
  geom_line()

# Tyder grafen på någon skillnad mellan odlingstyper?
##########

##### Övningsuppgift 6.5 #####
# Använd lökdatan i föregående uppgift för att testa om det finns en signifikant skillnad mellan konventionell och ekologisk.
# Formulera hypoteser och genomför testet med `t.test()`. Lös gärna uppgiften med miniräknare först.
##########

### t-test för två oberoende stickprov

dat_berry <- data.frame(Behandling = c("A", "A", "A", "A", "B", "B", "B", "B"),
              Vikt = c(40, 48.2, 39.2, 47.9, 57.5, 61.5, 58, 66.5))
dat_berry

ggplot(dat_berry, aes(Behandling, Vikt)) +
  geom_point()

# Formelskrivning
t.test(Vikt ~ Behandling, data = dat_berry, var.equal = T)

# Två separata vektorer
## Filtrera ut data där behandling är A
Vikt_A <- dat_berry$Vikt[dat_berry$Behandling == "A"]

## Filtrera ut data där behandling är B
Vikt_B <- dat_berry$Vikt[dat_berry$Behandling == "B"]

t.test(Vikt_A, Vikt_B, var.equal = T)

##### Övningsuppgift 6.6 #####
# Vilka resultatvärden ändras i utskriften om man sätter `var.equal = F`?
##########

##### Övningsuppgift 6.7 #####
# Gör lämpliga tillägg till kodstycket nedan för att göra ett ensidigt test (om B ger högre vikt än A).

t.test(Vikt ~ Behandling, data = dat_berry, var.equal = T, alternative = "two.sided")

##########

pairwise.t.test(dat_berry$Vikt, dat_berry$Behandling, p.adjust.method = "none", pool.sd = F)

##### Övningsuppgift 6.8 #####
# I en undersökning av hur den europeiska ekorren (Sciurus vulgaris) förändras i vikt under övervintring mäts 7 slumpmässigt valda ekorrar före och 5 slumpmässigt valda ekorrar efter övervintring. Datan finns tillgänglig i excelfilen *Uppgiftsdata.xlsx* på canvassidan, i fliken *Ekorrar*. Ladda ner filen och fyll i stycket nedan för att importera datan.

dat_sq <- read_excel("___", sheet = "Ekorrar")
dat_sq

# dat_sq <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Uppgiftsdata/Uppgift_Ekorrar.csv") # Alternativ lösning

##########

##### Övningsuppgift 6.9 #####
# Fyll i följande stycke för en lämplig graf för att jämföra mätningarna före och mätningarna efter.

ggplot(dat_sq, aes(x = ___, y = ___)) +
  ___()

# Finns det någon synlig viktskillnad?
##########

##### Övningsuppgift 6.10 #####
# Genomför ett t-test för två oberoende stickprov på ekorrdatan genom att fylla i kodstycket nedan. Formulera tydliga hypoteser och dra en klar slutsats.

t.test(___ ~ ___, data = dat_sq, var.equal = ___)

##########

##### Övningsuppgift 6.11 #####
# Ett problem med att mäta skilda individer före och efter övervintring är att det kan finnas en stor skillnad i vikt mellan individuella ekorrar. Kan man lägga upp försöket på ett sätt som reducerar det problemet?
##########

## z-test och konfidensintervall för två proportioner

prop.test(c(17, 26), c(50, 60), correct = F)

##### Övningsuppgift 6.12 #####
# Z-test bygger på en normalapproximation. Som tumregel för när approximationen är rimlig används ofta att n * p * (1 - p) ska vara större än 10 för bägge stickproven. Gör beräkningen för datan i exemplet (17 av 50 respektive 26 av 60).
##########

##### Övningsuppgift 6.13 #####
# Det finns en förvånansvärt stor mängd studier på kopplingen mellan innehav av burfågel och lungcancer. En sådan studie (Kohlmeier et al 1992) ger följande antal för burfågelägande och lungcancer.

dat_bird <- data.frame(Burfågel = c("Burfågel", "Ej_burfågel"),
              Lungcancer = c(98, 141),
              Ej_lungcancer = c(101, 328))
dat_bird

# Datan tyder på att människor med burfågel har en förhöjd risk att drabbas av lungcancer. Genomför ett z-test för att se om andelen burfågelägare än densamma i de två patientgrupperna.

prop.test(x = c(___, ___), n = c(___, ___), correct = F)

# Genomför ett z-test för att se om andelen cancerdrabbade är densamma i de två burfågelsgrupperna. Hur förhåller sig p-värdena i de bägge testerna till varandra?

prop.test(x = c(___, ___), n = c(___, ___), correct = F)

# Finns det någon industri som kan ha ett intresse av att finansiera forskning som söker alternativa riskfaktorer för lungcancer?
##########

## Chi-två-test för korstabeller

dat_titanic <- Titanic %>% data.frame() %>% filter(Sex == "Male", Age == "Adult")
dat_titanic

dat_wide <- dat_titanic %>% 
  pivot_wider(names_from = Survived, values_from = Freq)
dat_wide

ggplot(dat_titanic, aes(Class, Freq, fill = Survived)) +
  geom_col(position = position_fill(), color = "black") +
  scale_fill_manual(values = c("red4", "white"))

dat_wide[, 4:5] # De två numeriska kolumnerna

chisq.test(dat_wide[, 4:5])

test_result <- chisq.test(dat_wide[, 4:5])
test_result$expected # Samtliga förväntade värden över 5

##### Övningsuppgift 6.14 #####
# Ta följande lilla korstabell och kör `chisq.test()` för att få ett felmeddelande.

dat <- matrix(c(4,2,5,1), 2)
dat

##########

##### Övningsuppgift 6.15 #####
# En svensk studie på koppling mellan burfågel och lungcancer (Modigh et al, 1996) ger följande antal (för män).

dat_bird_swe <- data.frame(Burfågel = c("Burfågel", "Ej_burfågel"),
              Lungcancer = c(108, 144),
              Ej_lungcancer = c(171, 256))
dat_bird_swe

# Genomför ett chi-två-test för att se om andelen cancerdrabbade än densamma i de två burfågelsgrupperna. Formulera tydliga hypoteser. För att få utfall som stämmer med en handräkning kan man sätta `correct = F`.

dat_bird_swe[, c(2,3)]
chisq.test(___, correct = F)

##########

##### Övningsuppgift 6.16 #####
# I en undersökning på potatis används fyra behandlingar (a1b1, a1b2, a2b1 och a2b2). 125 potatisar från varje behandling sorteras in i fyra olika färggrupper (A, B, C och D). Datan finns i fliken *Po-ta-toes* i excelfilen *Uppgiftsdata.xlsx* på canvassidan. Ladda ned filen och läs in datan genom att fylla i stycket nedan.

dat_pot <- read_excel("___", sheet = "Po-ta-toes")
dat_pot

# dat_pot <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Uppgiftsdata/Uppgift_Po-ta-toes.csv")

##########

##### Övningsuppgift 6.17 #####
# För att göra en graf kan man pivotera datan till lång form.

dat_long <- dat_pot %>% pivot_longer(-Färg, values_to = "Antal", names_to = "Behandling")
dat_long

# Skapa ett stapeldiagram med uppdelade staplar genom att fylla i kodstycket nedan. Behandling ska vara på x-axeln och ifylld färg ska ges av `Färg`.

ggplot(dat_long, aes(x = ___, y = ___, fill = ___)) +
  geom_col(col = "black", width = 0.6) +
  scale_fill_brewer(palette = "Reds")

# Finns det några synbara skillnader mellan behandlingar?
##########

##### Övningsuppgift 6.18 #####
# Beräkna ett chi-två-test på potatisdatan för att se om det finns färgskillnader mellan behandlingarna. Formulera tydliga hypoteser och ge ett tydligt svar.

dat_pot[,-1]
chisq.test(___)

##########

##### Övningsuppgift 6.19 #####
# Vi vill undersöka om andelen hemmasegrar i herrallsvenskan förändrats över tid. Vi importerar data över matchresultat sedan 1920-talet.

dat_alls <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Allsvenskan%2C%20herrar%2C%201924-2020.csv")
dat_alls

# Följande kod skapar en variabel för årtionde, en variabel för hemmaseger, och räknar ut antalen hemmasegrar per årtionde. Detaljer är oviktiga här.

library(lubridate)
dat_hemma <- dat_alls %>% 
  mutate(År = year(Datum),
         Årtionde = floor(År / 10) * 10,
         Hemmaseger = ifelse(Hemmamål > Bortamål, "Hemmaseger", "Ej_hemmaseger")) %>% 
  count(Årtionde, Hemmaseger) %>% 
  pivot_wider(values_from = n, names_from = Hemmaseger) %>% 
  mutate(Total = Hemmaseger + Ej_hemmaseger,
         Proportion = Hemmaseger / (Hemmaseger + Ej_hemmaseger))

# Fyll i koden nedan för att skapa en tidsserie (en linjegraf med tid på x-axeln) för andelen `Proportion`.

ggplot(dat_hemma, aes(x = ___, y = ___)) +
  ___()

##########

##### Övningsuppgift 6.20 #####
# Använd ett z-test för att se om proportionen hemmasegrar under 1920-talet (371 av 738) är skild från 1960-talet (590 av 1320).

prop.test(c(___, ___), n = c(___, ___), correct = F)

##########

## Bonus. Bilder i R

# install.packages("magick")
library(magick)

url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Nils_Dardel_D%C3%B6ende_dandyn.jpg/1920px-Nils_Dardel_D%C3%B6ende_dandyn.jpg"
img <- image_read(url)
img

##### Övningsuppgift 6.21 #####
# Hitta någon annan bild online, vad som helst. Gör lämplig ändring i stycken ovan för att läsa in bilden med `image_read()`.
##########

img <- img %>% 
  image_resize("500")
img

##### Övningsuppgift 6.22 #####
# Vad kan vara koden för att sätta en bild till halva storleken, alltså 50% av den ursprungliga bilden?
##########

img %>% 
  image_modulate(saturation = 50) %>% 
  image_modulate(hue = 50)

img %>% image_quantize(max = 10)

##### Övningsuppgift 6.23 #####
# Med 50 enskilda färger blir *Den döende dandyn* något mattare, men karaktärernas klädsel har klara färger. Hur få måste det totala antalet färger bli innan *du* ser en klar försämring av bilden?
##########

img <- img %>% image_quantize(max = 10)
info <- img %>% image_info()
pixel_values <- img %>% image_data() %>% as.vector()

dat_pix <- expand_grid(y = info$height:1, x = 1:info$width, color = c("R", "G", "B")) %>% 
  mutate(value = pixel_values) %>% 
  pivot_wider(values_from = value, names_from = color) %>% 
  mutate(hex = paste0("#", R, G, B))

dat_pix

ggplot(dat_pix, aes(x, y)) +
  geom_raster(fill = dat_pix$hex)

##### Övningsuppgift 6.24 #####
# Vad händer om man sätter `fill = hex` inom `aes()`-funktionen istället?

ggplot(dat_pix, aes(x, y, fill = ___)) +
  geom_raster()

# Funktionen `scale_fill_manual()` kan styra färgvalet i det fallet.

ggplot(dat_pix, aes(x, y, fill = ___)) +
  geom_raster() +
  scale_fill_manual(values = c('white', 'aliceblue', 
                               'antiquewhite', 'antiquewhite1', 
                               'antiquewhite2', 'antiquewhite3', 
                               'antiquewhite4', 'aquamarine', 
                               'aquamarine1', 'aquamarine2')) +
  theme_void()

# Tillgängliga färger kan tas fram med `colors()`.
##########

dat_pix_count <- dat_pix %>% 
  count(hex) %>% 
  mutate(hex = reorder(hex, n))

ggplot(dat_pix_count, (aes(n, hex))) +
  geom_col(fill = dat_pix_count$hex)

##### Övningsuppgift 6.25 #####
# Låt oss ta ett mindre stickprov från bilden. Funktionen `set.seed()` sätter ett startvärde för slumtalsgeneratorn, vilket är bra om man vill reproducera ett visst utfall.

set.seed(1573)
dat_sample <- dat_pix %>% slice_sample(n = 100)
dat_sample %>% count(hex)

ggplot(dat_sample, aes(x, y)) +
  geom_point(color = dat_sample$hex, size = 8)

# I stickprovet är 62 av 100 pixlar en mörkblå färg. Genomför ett test med `prop.test()` för att se om andelen i populationen (som i detta fall är hela tavlan) är skild från 0.7. Jämför med proportionen i den större datamängden `dat_pix`.
##########
NA
