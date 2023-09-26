# # Test för två stickprov
#
# Datorövning 6 handlar om hypotestest och konfidensintervall för jämförelse av två stickprov.
# Efter övningen ska vi kunna genomföra och tolka
#
# - t-test för jämförelse av två medelvärden,
#
# - z-test för jämförelse av två proportioner,
#
# - chi-två-test för jämförelse fördelningar mellan två eller flera grupper,
#
# - konfidensintervall för skillnaden mellan två medelvärden eller två proportioner.
#
# ## Repetition av datorövning 5
#
# När man startar en ny R-session bör man ladda de paket man vet kommer behövas med `library()`. Om
# paket inte finns installerade måste man först köra `install.packages()`.
#

# install.packages("tidyverse")
library(tidyverse)

#
# I datorövning 5 tittade vi på tester som inte bygger på normalfördelning: dels tester för
# proportioner, där varje observation är något av två möjliga utfall; dels tester för kategoridata,
# där varje observation har ett ufall i någon av flera kategorier.
#
# Proportioner kan testas med ett z-test. Säg att man testar om en doft har en effekt på en insekt,
# man skickar 20 insekter i ett y-rör och 16 går mot doften. Hypoteserna ges av
#
# - H0: proportionen i populationen är 0.5,
# - H1: proportionen i populationen är inte 0.5.
#
# I R genomförs ett z-test med `prop.test()`. Funktionen gör en korrektion som ger ett något bättre
# test än det man ofta beräknar för hand. Korrektionen kan sättas av med argumentet `correct`.
#

prop.test(x = 16, n = 20, p = 0.5, correct = F)

#
# Ett lågt p-värde tyder på en signifikant skillnad från 0.5.
#
# För att få samma konfidensintervall som det man beräknar för hand kan man använda `binom.asymp()`
# från paketet `binom`.
#

library(binom)
binom.asymp(16, 20)

#
# Populationens proportion ligger med 95 procents konfidens mellan 0.62 och 0.98.
#
# Om utfallen är mer än två kategorier kan en hypotes om datans fördelning testas med ett
# goodness-of-fit-test, vilket är ett slags chi-två-test. Testet bygger på att observerade antal (O)
# ställs mot förväntade antal (E). I R genomförs ett chi-två-test med `chisq.test()`. Säg som exempel
# att man studerar fågelpopulationer genom en observationsstudie. Observerade antal av fyra
# fågelarter är 102, 53, 75 och 12 och de förväntade andelarna av arterna är 50, 15, 25 och 10
# procent. Testets hypoteser ges av
#
# - H0: proportionerna följer den förväntade fördelningen,
# - H1: proportionerna följer inte den förväntade fördelningen,
#
# och testet ges av
#

chisq.test(c(102, 53, 75, 12), p = c(0.5, 0.15, 0.25, 0.1))

#
# Ett lågt p-värde tyder på att de antagna proportionerna inte stämmer. Antalet frihetsgrader ges i
# ett chi-två-test av antalet klasser minus antalet skattade parametrar minus ett. I det här fallet
# finns fyra klasser och ingen skattad parameter.
#
# ## Två stickprov och normalfördelad data
#
# Vid normalfördelad data från två stickprov eller grupper vill vi nästan alltid testa om
# populationerna har samma medelvärde. Det kan också ses som att vi testar om differensen mellan
# medelvärdena är noll. Vi skiljer mellan två fall: *matchade stickprov* - där varje observation i
# den ena gruppen är *kopplad* till en observation i den andra gruppen; och *oberoende stickprov* -
# där det inte finns någon sådan koppling mellan stickproven. Typiska exempel på matchade stickprov
# är när man mäter samma individ för och efter en behandling och syskonstudier där ett syskon får en
# behandling och den andra en annan behandling.
#
# ### t-test för två matchade stickprov
#
# Vid matchade stickprov kan varje observation i en behandlingsgrupp paras med en observation i den
# andra gruppen. Själva testet är ett t-test för *ett* stickprov på differensserien beräknat från
# varje par. I R kan man antingen beräkna den differensserien eller använda `t.test()` med två
# dataserier och argumentet för parvisa observationer satt till sant, `paired = T`.
# Som exempel ges följande data från en studie på äpple, där trädhöjd mätts före och efter en
# näringsbehandling.
#

dat_apple <- tibble(Tree = 1:4, 
              Before = c(48, 43, 30, 47), 
              After = c(51, 44, 42, 54))
dat_apple

#
# Datan kan illustreras med ett punktdiagram där en linje binder samman paret. För att enkelt skapa
# grafen i `ggplot2` kan man först omstrukturera datan till lång form genom `pivot_longer`.
#

dat_long <- dat_apple %>% pivot_longer(-Tree, names_to = "Time", values_to = "Height")
dat_long

#
# Uppgift 6.1. (Äppelgraf)
# Fyll i kodstycket nedan för en graf av äppeldatan. Axlarna ges av `Time` och `Height`. Två
# observationer kan kopplas genom att sätta `Tree` som grupp.

ggplot(dat_long, aes(___, ___, group = ___)) +
  geom_point() +
  geom_line()

# :::
#
# För att testa för skillnad före och efter behandling sätter vi upp hypoteser
#
# - H0: mu före behandling är lika med mu efter behandling
# - H1: mu före behandling är skild från mu efter behandling
#
# Testet kan antingen utföras som ett enkelt t-test på differensserien
#

t.test(dat_apple$Before - dat_apple$After)

#
# eller som ett t-test för två stickprov där man särskilt anger att datan är parad
#

t.test(dat_apple$Before, dat_apple$After, paired = T)

#
# För bägge alternativen måste datan vara ordnad så att de två vektorerna matchar varandra parvis.
# Ett p-värde på $0.0987$ ger att man inte förkastar vid en signifikansnivå på fem procent. Vi drar
# därmed slutsatsen att det inte finns någon signifikant skillnad före och efter behandling.
#
# Uppgift 6.2. (Ensidigt test)
# Gör ett tillägg till ett av kodstyckena med `t.test()` för att beräkna ett ensidigt test med
# mothypotesen att träden ökar i höjd efter behandling. Hjälpsidan för `t.test()` kan tas fram genom
# att köra `?t.test()`.
# :::
#
# Konfidensintervallet beräknas från differenserna på samma sätt som vid ett stickprov med
# normalfördelad data. Tolkningen liknar den för ett stickprov: med 95 procents konfidens ligger den
# sanna skillnaden i medelvärden i intervallet.
#
# Uppgift 6.3. (Lökimport)
# Åtta monoglukosidmätningar på lök samlas in från fyra konventionella och fyra ekologiska ordlare.
# Resultatet finns i fliken *Lökfärg* i excelfilen *Uppgiftsdata.xlsx* på canvassidan. Ladda ner
# filen och importera datan genom att fylla i raden nedan.
#

library(readxl)
dat_onion <- read_excel("____", sheet = "Lökfärg")
# dat_onion <-
# read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Uppgiftsdata/Uppgift_L%C3%B6kf%C3%A4rg.csv")
# Alternativ lösning

# :::
#
# Uppgift 6.4. (Lökgraf)
# Fyll i stycket nedan för en graf av lökdatan från föregående uppgift.
#

dat_long <- dat_onion %>% 
  pivot_longer(-Odlare, names_to = "Odlingstyp", values_to = "Utfall")
dat_long

ggplot(dat_long, aes(___, ___, group = Odlare)) +
  geom_point() +
  geom_line()

#
# Tyder grafen på någon skillnad mellan odlingstyper?
# :::
#
# Uppgift 6.5. (Löktest)
# Använd lökdatan i föregående uppgift för att testa om det finns en signifikant skillnad mellan
# konventionell och ekologisk.
# Formulera hypoteser och genomför testet med `t.test()`. Lös gärna uppgiften med miniräknare
# först.
# :::
#
# ### t-test för två oberoende stickprov
#
# Ett t-test för två oberoende stickprov testar om två populationsmedelvärden är lika. Ta som
# exempel följande data på jordgubbsskörd vid två olika näringsbehandlingar (A och B). Här är
# stickproven inte matchade - det finns **ingen** direkt koppling mellan en observation i den ena
# behandlingsgruppen till någon observation i den andra.
#

dat_berry <- data.frame(Behandling = c("A", "A", "A", "A", "B", "B", "B", "B"),
              Vikt = c(40, 48.2, 39.2, 47.9, 57.5, 61.5, 58, 66.5))
dat_berry

#
# Datan kan illustreras med ett enkelt punktdiagram.
#

ggplot(dat_berry, aes(Behandling, Vikt)) +
  geom_point()

#
# Ett t-test för två oberoende stickprov har nollhypotesen att grupperna har samma
# populationsmedelvärde och alternativhypotesen att populationsmedelvärdena är skilda (för det
# tvåsidiga fallet):
#
# - H0: mu_A är lika med mu_B
# - H1: mu_A är ej lika med mu_B
#
# Testet kan utföras i R genom funktionen `t.test()`. Data kan antingen anges som en formel med
# dess data `Vikt ~ Behandling, data = dat_berry` (vilket man kan läsa som *vikt uppdelat efter
# behandling*) eller som två skilda vektorer. Det förra alternativet är oftast enklare om man har
# datan på lång form - med en kolumn som anger grupp (i exemplet *Behandling*) och en kolumn som
# anger utfallsvärdet (i exemplet *Vikt*).
#
# För formen med formel ger det

# Formelskrivning
t.test(Vikt ~ Behandling, data = dat_berry, var.equal = T)

#
# och för formen med vektorer
#

# Två separata vektorer
## Filtrera ut data där behandling är A
Vikt_A <- dat_berry$Vikt[dat_berry$Behandling == "A"]

## Filtrera ut data där behandling är B
Vikt_B <- dat_berry$Vikt[dat_berry$Behandling == "B"]

t.test(Vikt_A, Vikt_B, var.equal = T)

#
# Argumentet `var.equal = T` används för att beräkna testet där gruppernas varianser antas vara
# lika. Grundinställningen är testet där varianser inte antas vara lika, så `t.test(Vikt ~
# Behandling, data = dat)` ger ett lite annat resultat.
#
# Uppgift 6.6. (Ej lika varianser)
# Vilka resultatvärden ändras i utskriften om man sätter `var.equal = F`?
# :::
#
# Testet ger ett p-värde på $0.0018$, vilket leder till att nollhypotesen förkastas på
# enprocentsnivån. Detta tyder på att det finns en viktskillnad mellan behandlingarna. Utskriften ger
# också ett 95-procentigt konfidensintervall på $(-24.898, -9.202)$. Tolkningen är att skillnaden
# mellan populationsmedelvärden ligger i intervallet med 95 procents konfidens. Notera att värdet
# noll inte ligger i intervallet.
#
# Uppgift 6.7. (Ensidigt test)
# Gör lämpliga tillägg till kodstycket nedan för att göra ett ensidigt test (om B ger högre vikt än
# A).
#

t.test(Vikt ~ Behandling, data = dat_berry, var.equal = T, alternative = "two.sided")

# :::
#
# Om man har fler än två grupper kan man vilja göra parvisa t-test - alltså ett t-test för varje
# par av grupper.
# Ett exempel på funktionen `pairwise.t.test()` ges nedan. Funktionen bygger på att datan är i
# *lång* form, med en kolumn som anger det numeriska utfallet och en kolumn som anger behandlingen.
#

pairwise.t.test(dat_berry$Vikt, dat_berry$Behandling, p.adjust.method = "none", pool.sd = F)

#
# *Matchade* observationer kan också kallas *parade* (eng. paired) så se upp med terminologin.
# Funktionen `pairwise.t.test()` för *parvisa jämförelse* mellan behandlingar, men testerna är t-test
# för oberoende stickprov.
#
# Uppgift 6.8. (Ekorrdata)
# I en undersökning av hur den europeiska ekorren (Sciurus vulgaris) förändras i vikt under
# övervintring mäts 7 slumpmässigt valda ekorrar före och 5 slumpmässigt valda ekorrar efter
# övervintring. Datan finns tillgänglig i excelfilen *Uppgiftsdata.xlsx* på canvassidan, i fliken
# *Ekorrar*. Ladda ner filen och fyll i stycket nedan för att importera datan.
#

dat_sq <- read_excel("___", sheet = "Ekorrar")
dat_sq

# dat_sq <-
# read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Uppgiftsdata/Uppgift_Ekorrar.csv")
# Alternativ lösning

# :::
#
# Uppgift 6.9. (Ekorrgraf)
# Fyll i följande stycke för en lämplig graf för att jämföra mätningarna före och mätningarna
# efter.
#

ggplot(dat_sq, aes(x = ___, y = ___)) +
  ___()

#
# Finns det någon synlig viktskillnad?
# :::
#
# Uppgift 6.10. (Ekorrtest)
# Genomför ett t-test för två oberoende stickprov på ekorrdatan genom att fylla i kodstycket nedan.
# Formulera tydliga hypoteser och dra en klar slutsats.
#

t.test(___ ~ ___, data = dat_sq, var.equal = ___)

# :::
#
# Uppgift 6.11. (Ekorrdesign)
# Ett problem med att mäta skilda individer före och efter övervintring är att det kan finnas en
# stor skillnad i vikt mellan individuella ekorrar. Kan man lägga upp försöket på ett sätt som
# reducerar det problemet?
# :::
#
# ## z-test och konfidensintervall för två proportioner
#
# Om man vill jämföra två proportioner kan man använda z-testet för två stickprov. Säg till exempel
# att man har två sorter av någon planta och vill se hur stor proportion som är infekterad av
# bladmögel. I den ena gruppen (sort A) är 17 av 50 infektera och i den andra (sort B) är 26 av 60
# infekterade. Testets hypoteser är i det tvåsidiga fallet
#
# - H0: proportion A är lika med proportion B
# - H1: proportion A är skild från proportion B
#
# I R kan testet genomföras med `prop.test`-funktionen. Funktionens första argument är antalen
# infekterade, som en vektor med två värden, och dess andra argument är totalerna. Likt testet med
# ett stickprov finns en möjlighet att göra en kontinuitetskorrektion med `correct`-argumentet. För
# att få samma resultat som räkning för hand anger vi att korrektion inte ska göras med `correct =
# F`.
#

prop.test(c(17, 26), c(50, 60), correct = F)

#
# Notera att funktionen inte ger ett z-värde utan ett $\chi^2$-värde (utskrivet `X-squared`). Det
# beror på att funktionen beräknar z-testet som ett likvärdigt $\chi^2$-test. Det z-värde man får om
# man genomför testet som ett z-test är detsamma som roten ur utskriftens $\chi^2$-värde. Testet ger
# ett högt p-värde på 0.32 vilket innebär att nollhypotesen inte förkastas: det finns ingen
# signifikant skillnad i infektionsproportion.
#
# Funktionen `prop.test()` ger också en utskrift av konfidensintervallet. Tolkning är att
# skillnaden i proportioner mellan populationerna ligger i intervallet med 95 procents konfidens.
# Notera att nollan ingår i intervallet.
#
# Uppgift 6.12. (Lämplig approximation?)
# Z-test bygger på en normalapproximation. Som tumregel för när approximationen är rimlig används
# ofta att n * p * (1 - p) ska vara större än 10 för bägge stickproven. Gör beräkningen för datan i
# exemplet (17 av 50 respektive 26 av 60).
# :::
#
# Uppgift 6.13. (Burfågel)
# Det finns en förvånansvärt stor mängd studier på kopplingen mellan innehav av burfågel och
# lungcancer. En sådan studie (Kohlmeier et al 1992) ger följande antal för burfågelägande och
# lungcancer.
#

dat_bird <- data.frame(Burfågel = c("Burfågel", "Ej_burfågel"),
              Lungcancer = c(98, 141),
              Ej_lungcancer = c(101, 328))
dat_bird

#
# Datan tyder på att människor med burfågel har en förhöjd risk att drabbas av lungcancer. Genomför
# ett z-test för att se om andelen burfågelägare än densamma i de två patientgrupperna.
#

prop.test(x = c(___, ___), n = c(___, ___), correct = F)

#
# Genomför ett z-test för att se om andelen cancerdrabbade är densamma i de två burfågelsgrupperna.
# Hur förhåller sig p-värdena i de bägge testerna till varandra?
#

prop.test(x = c(___, ___), n = c(___, ___), correct = F)

#
# Finns det någon industri som kan ha ett intresse av att finansiera forskning som söker
# alternativa riskfaktorer för lungcancer?
# :::
#
# ## Chi-två-test för korstabeller
#
# Data med två kategoriska variabler kan presenteras med en korstabell. Ta som (ett något deppigt)
# exempel överlevnadsdata från Titanic. Datan finns tillgänglig i R som `Titanic`. I detta fall ges
# överlevnad filtrerad på vuxna män, uppdelat efter klass.
#

dat_titanic <- Titanic %>% data.frame() %>% filter(Sex == "Male", Age == "Adult")
dat_titanic

#
# En korstabell kan konstrueras med `pivot_wider`.
#

dat_wide <- dat_titanic %>% 
  pivot_wider(names_from = Survived, values_from = Freq)
dat_wide

#
# Datan tyder på att överlevnad är beroende av klass. Datan kan illustreras med uppdelade staplar
#

ggplot(dat_titanic, aes(Class, Freq, fill = Survived)) +
  geom_col(position = position_fill(), color = "black") +
  scale_fill_manual(values = c("red4", "white"))

#
# Argumentet `position` i `geom_bar` används för att skapa proportionella staplar.
#
# Ett chi-två-test på en korstabell har nollhypotesen att det inte finns något samband mellan
# variabeln för rader och variabeln för kolumner. Antal frihetsgrader ges av antal rader minus ett
# gånger antal kolumner minus ett. Testet kan enkelt göras med `chisq.test()`. Som ingångsvärde kan
# man plocka ut kolumnerna med numeriska värden genom hakparenteser.
#

dat_wide[, 4:5] # De två numeriska kolumnerna

chisq.test(dat_wide[, 4:5])

#
# Utskriften ger teststorheten, antal frihetsgrader, och p-värdet. I det här fallet är p-värdet
# mycket litet och slutsatsen blir att nollhypotesen förkastas - det finns ett samband mellan klass
# och överlevnad. Antalet frihetsgrader ges av antalet rader minus ett gånger antalet kolumner minus
# ett (här (4-1) * (2-1) = 3).
#
# Chi-två-testet är ett asymptotiskt test - dess egenskaper är beroende av *stora* stickprov. Som
# gräns för storleken används ofta att samtliga förväntade antal ska vara större än 5. Funktionen ger
# en varning om förväntade värden är små. En möjlig lösning i sådana fall är att slå ihop klasser.
#

test_result <- chisq.test(dat_wide[, 4:5])
test_result$expected # Samtliga förväntade värden över 5

#
# Om detta krav inte är uppfyllt skriver funktionen ut en varning.
#
# Uppgift 6.14. (Ogiltig approximation)
# Ta följande lilla korstabell och kör `chisq.test()` för att få ett felmeddelande.

dat <- matrix(c(4,2,5,1), 2)
dat

# :::
#
# Uppgift 6.15. (Burfågeln återvänder)
# En svensk studie på koppling mellan burfågel och lungcancer (Modigh et al, 1996) ger följande
# antal (för män).
#

dat_bird_swe <- data.frame(Burfågel = c("Burfågel", "Ej_burfågel"),
              Lungcancer = c(108, 144),
              Ej_lungcancer = c(171, 256))
dat_bird_swe

#
# Genomför ett chi-två-test för att se om andelen cancerdrabbade än densamma i de två
# burfågelsgrupperna. Formulera tydliga hypoteser. För att få utfall som stämmer med en handräkning
# kan man sätta `correct = F`.
#

dat_bird_swe[, c(2,3)]
chisq.test(___, correct = F)

# :::
#
# Chi-två-testet kan tillämpas på korstabeller med godtyckligt antal rader och kolumner.
#
# Uppgift 6.16. (Po-ta-toes-import)
# I en undersökning på potatis används fyra behandlingar (a1b1, a1b2, a2b1 och a2b2). 125 potatisar
# från varje behandling sorteras in i fyra olika färggrupper (A, B, C och D). Datan finns i fliken
# *Po-ta-toes* i excelfilen *Uppgiftsdata.xlsx* på canvassidan. Ladda ned filen och läs in datan
# genom att fylla i stycket nedan.
#

dat_pot <- read_excel("___", sheet = "Po-ta-toes")
dat_pot

# dat_pot <-
# read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Uppgiftsdata/Uppgift_Po-ta-toes.csv")

# :::
#
# Uppgift 6.17. (Po-ta-toes-graf)
# För att göra en graf kan man pivotera datan till lång form.
#

dat_long <- dat_pot %>% pivot_longer(-Färg, values_to = "Antal", names_to = "Behandling")
dat_long

#
# Skapa ett stapeldiagram med uppdelade staplar genom att fylla i kodstycket nedan. Behandling ska
# vara på x-axeln och ifylld färg ska ges av `Färg`.
#

ggplot(dat_long, aes(x = ___, y = ___, fill = ___)) +
  geom_col(col = "black", width = 0.6) +
  scale_fill_brewer(palette = "Reds")

#
# Finns det några synbara skillnader mellan behandlingar?
# :::
#
# Uppgift 6.18. (Po-ta-toes-test)
# Beräkna ett chi-två-test på potatisdatan för att se om det finns färgskillnader mellan
# behandlingarna. Formulera tydliga hypoteser och ge ett tydligt svar.
#

dat_pot[,-1]
chisq.test(___)

# :::
#
# Uppgift 6.19. (Hemmasegrar över årtionden)
# Vi vill undersöka om andelen hemmasegrar i herrallsvenskan förändrats över tid. Vi importerar
# data över matchresultat sedan 1920-talet.
#

dat_alls <- read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2023/main/Data/Allsvenskan%2C%20herrar%2C%201924-2020.csv")
dat_alls

#
# Följande kod skapar en variabel för årtionde, en variabel för hemmaseger, och räknar ut antalen
# hemmasegrar per årtionde. Detaljer är oviktiga här.
#

library(lubridate)
dat_hemma <- dat_alls %>% 
  mutate(År = year(Datum),
         Årtionde = floor(År / 10) * 10,
         Hemmaseger = ifelse(Hemmamål > Bortamål, "Hemmaseger", "Ej_hemmaseger")) %>% 
  count(Årtionde, Hemmaseger) %>% 
  pivot_wider(values_from = n, names_from = Hemmaseger) %>% 
  mutate(Total = Hemmaseger + Ej_hemmaseger,
         Proportion = Hemmaseger / (Hemmaseger + Ej_hemmaseger))

#
# Fyll i koden nedan för att skapa en tidsserie (en linjegraf med tid på x-axeln) för andelen
# `Proportion`.
#

ggplot(dat_hemma, aes(x = ___, y = ___)) +
  ___()

# :::
#
# Uppgift 6.20. (1920-talet mot 1960-talet)
# Använd ett z-test för att se om proportionen hemmasegrar under 1920-talet (371 av 738) är skild
# från 1960-talet (590 av 1320).

prop.test(c(___, ___), n = c(___, ___), correct = F)

# :::
#
# ## Bonus. Bilder i R
#
# Det finns en stor mängd paket som kan hantera bilder. Låt oss ta en titt på ett av dem - `magick`
# - vilket bygger på en koppling till ImageMagick (https://imagemagick.org/).
#

# install.packages("magick")
library(magick)

#
# Med funktionen `image_read()` kan man läsa in en bild, antingen från lokal hårddisk eller från en
# internetaddress. Här hämtar vi en bild av Nils Dardels *Den döende dandyn* (1918) från Wikipedia.
#

url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Nils_Dardel_D%C3%B6ende_dandyn.jpg/1920px-Nils_Dardel_D%C3%B6ende_dandyn.jpg"
img <- image_read(url)
img

#
# Uppgift 6.21. (Någon annan bild)
# Hitta någon annan bild online, vad som helst. Gör lämplig ändring i stycken ovan för att läsa in
# bilden med `image_read()`.
# :::
#
# Låt oss börja med att ändra storleken med `image_resize()`. Följande ger en bild där den kortaste
# av höjd och bredd är 500 pixlar.
#

img <- img %>% 
  image_resize("500")
img

#
# Uppgift 6.22. (Storlek)
# Vad kan vara koden för att sätta en bild till halva storleken, alltså 50% av den ursprungliga
# bilden?
# :::
#
# Man kan också manipulera egenskaper som kontrast, mättnad och färgton.
#

img %>% 
  image_modulate(saturation = 50) %>% 
  image_modulate(hue = 50)

#
# För mer information av tillgängliga funktioner, titta på paketets hjälpsida med `?magick` och
# introduktionen på https://docs.ropensci.org/magick/articles/intro.html.
#
# En enkel vetenskaplig tillämpning av bildanalys kan baseras på de relativa andelarna av olika
# färger. Det kan till exempel användas för att beräkna skadegrad på löv (som efter färgning kan ha
# specifika färger för skadade delar) eller storlek på trädkronor. Funktionen `image_quantize()` kan
# minska antalet färger i en bild till ett mer hanterbart antal.
#

img %>% image_quantize(max = 10)

#
# Uppgift 6.23. (Antal färger)
# Med 50 enskilda färger blir *Den döende dandyn* något mattare, men karaktärernas klädsel har
# klara färger. Hur få måste det totala antalet färger bli innan *du* ser en klar försämring av
# bilden?
# :::
#
# Funktionen `image_data()` kan användas för att ta ut färgvärdet för varje pixel. Därefter kan man
# enkelt beräkna andelen för olika färger. Följande stycke förenklar bilden till tio färger,
# extraherar datan och beräknar antalet pixlar med respektive färg. Den exakta koden är inte så
# viktig här och kan läsas kursivt.
#

img <- img %>% image_quantize(max = 10)
info <- img %>% image_info()
pixel_values <- img %>% image_data() %>% as.vector()

dat_pix <- expand_grid(y = info$height:1, x = 1:info$width, color = c("R", "G", "B")) %>% 
  mutate(value = pixel_values) %>% 
  pivot_wider(values_from = value, names_from = color) %>% 
  mutate(hex = paste0("#", R, G, B))

dat_pix

#
# Den konstruerade datan innehåller koordinater med x och y samt färgvärden i tre färgband och en
# hexkod som anger färgen. Härifrån kan vi göra en grafversion av bilden med `geom_raster()`.
#

ggplot(dat_pix, aes(x, y)) +
  geom_raster(fill = dat_pix$hex)

#
# Notera att vi sätter `fill` i geom-funktionen, eftersom målet är att sätta färgen till den som
# anges i kolumnen hex.
#
# Uppgift 6.24. (Färg som aesthetic)
# Vad händer om man sätter `fill = hex` inom `aes()`-funktionen istället?
#

ggplot(dat_pix, aes(x, y, fill = ___)) +
  geom_raster()

#
# Funktionen `scale_fill_manual()` kan styra färgvalet i det fallet.
#

ggplot(dat_pix, aes(x, y, fill = ___)) +
  geom_raster() +
  scale_fill_manual(values = c('white', 'aliceblue', 
                               'antiquewhite', 'antiquewhite1', 
                               'antiquewhite2', 'antiquewhite3', 
                               'antiquewhite4', 'aquamarine', 
                               'aquamarine1', 'aquamarine2')) +
  theme_void()

#
# Tillgängliga färger kan tas fram med `colors()`.
# :::
#
# Slutligen kan vi nu göra en enkel bildanalys genom att räkna antal eller andel pixlar med en viss
# färg.
#

dat_pix_count <- dat_pix %>% 
  count(hex) %>% 
  mutate(hex = reorder(hex, n))

ggplot(dat_pix_count, (aes(n, hex))) +
  geom_col(fill = dat_pix_count$hex)

#
# Uppgift 6.25. (Avslutande proportionstest)
# Låt oss ta ett mindre stickprov från bilden. Funktionen `set.seed()` sätter ett startvärde för
# slumtalsgeneratorn, vilket är bra om man vill reproducera ett visst utfall.
#

set.seed(1573)
dat_sample <- dat_pix %>% slice_sample(n = 100)
dat_sample %>% count(hex)

ggplot(dat_sample, aes(x, y)) +
  geom_point(color = dat_sample$hex, size = 8)

#
# I stickprovet är 62 av 100 pixlar en mörkblå färg. Genomför ett test med `prop.test()` för att se
# om andelen i populationen (som i detta fall är hela tavlan) är skild från 0.7. Jämför med
# proportionen i den större datamängden `dat_pix`.
# :::
