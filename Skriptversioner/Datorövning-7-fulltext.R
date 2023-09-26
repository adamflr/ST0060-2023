# # Variansanalys
#
# Datorövning 7 handlar om variansanalys. Efter övningen ska vi kunna
#
# - beräkna en anova-modell i R,
#
# - ta fram och tolka en anova-tabell,
#
# - göra lämpliga tester av modellantaganden,
#
# - göra parvisa jämförelser mellan behandlingar.
#
#
# ## Repetition av datorövning 6
#
# När man startar en ny R-session bör man ladda de paket man vet kommer behövas med `library()`. Om
# paket inte finns installerade måste man först köra `install.packages()`.
#

# install.packages("tidyverse")
library(tidyverse)

#
# I datorövning 6 tittade vi på tester för två stickprov. För normalfördelad data kan man då
# använda ett t-test för två stickprov (för två matchade stickprov eller för två oberoende stickprov
# beroende på situation) för data med utfall i två eller flera kategorier kan man använda ett z-test
# för två stickprov eller ett chi-två-test för en korstabell.
#
# Ett t-test för matchade stickprov används när de två grupper man jämför är matchade så att en
# observation i den ena gruppen är kopplad till en observation i den andra gruppen. Ett t-test för
# oberoende stickprov används om man inte har matchade stickprov, det vill säga då det inte finns
# någon koppling mellan behandlinggrupperna.
#
# Ta som exempel följande fiskefångster för sex båtar från två regioner och två fiskearter.
#

dat_fish <- data.frame(Vessel = c("A", "B", "C", "D", "E", "F"),
                       Region = c("N", "N", "N", "S", "S", "S"),
                       Species1 = c(115.7, 98.5, 82.1, 89.2, 95.7, 99.4),
                       Species2 = c(122.8, 105.3, 99.8, 106.8, 114, 102.7))

#
# Vi vill här testa om det finns en skillnad mellan arter och om det finns skillnad mellan
# regioner.
#

dat_long <- dat_fish %>% 
  pivot_longer(-c(Vessel, Region), names_to = "Species", values_to = "Catch")
ggplot(dat_long, aes(Species, Catch, group = Vessel)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Fångster av två arter", subtitle = "Linje sammanbinder observationer från samma fartyg")

#

ggplot(dat_fish, aes(Species1, Region)) + 
  geom_point() +
  labs(title = "Fångster i två regioner")

#
# För arterna har vi matchad data - varje observation av den ena arten är kopplad till en
# observation från den andra arten eftersom den kommer från samma båt - och vi kan testa om
# medelfångsterna av de två arterna är lika med ett t-test. Hypoteserna ges av
#
# - H0: populationsmedelvärdet av fångster för art 1 är lika med det för art 2,
# - H1: populationsmedelvärdena är ej lika.
#
# I R kan ett test för matchad data genomföras med `t.test()` och argumentet `paired`, eller genom
# att beräkna differensen per båt och göra ett t-test för ett stickprov.
#

t.test(dat_fish$Species1, dat_fish$Species2, paired = T)

# Alternativt
# t.test(dat_fish$Species1 - dat_fish$Species2)

#
# Det beräknade p-värdet ställs mot en signifikansnivå, vanligen fem procent, och om p-värdet är
# under signifikansnivån förkastar vi nollhypotesen. I det här exemplet tyder på värdet på att
# nollhypotesen inte stämmer - en art är vanligare än den andra.
#
# För att jämföra regioner kan vi göra ett t-test för två oberoende stickprov. Hypoteser ges av
#
# - H0: populationsmedelvärdet av fångster är lika mellan regioner,
# - H1: populationsmedelvärdet av fångster är ej lika mellan regioner.
#
# Testet kan genomföras med `t.test()` och kan antingen göras med ett antagande om lika varianser
# (vilket motsvarar det som görs för hand under kursen) eller utan det antagandet. Variabler kan
# anges med en formel som `Species1 ~ Group`, vilket vi kan tänka på som värden för art 1 uppdelat
# efter grupp.
#

t.test(Species1 ~ Region, data = dat_fish, var.equal = T)

#
# Ett högt p-värde tyder på att det inte finns någon skillnad i fångst mellan regioner.
#
# För data där utfallen är två eller flera kategorier kan ett chi-två-test testa om det finns något
# samband mellan två variabler. Följande data anger vilka partier ett urval väljare från tre kommuner
# planerar rösta på i nästa riksdagsval.
#

dat_parti <- data.frame(Kommun = c("Malmö", "Lund", "Kävlinge"),
                        S = c(54, 102, 40),
                        M = c(30, 98, 53),
                        MP = c(7, 50, 5))
dat_parti

#
# För att testa om det finns något samband mellan kommun och parti sätter vi upp hypoteserna
#
# - H0: det finns inget samband mellan parti och kommun (ingen skillnad mellan kommuner),
# - H1: det finns något samband mellan parti och kommun.
#
# Detta kan testas med ett chi-två-test med funktionen `chisq.test()`. Som argument ges den
# numeriska delen av korstabellen - vi tar alltså bort den första kolumnen för kommun.
#

chisq.test(dat_parti[, -1])

#
# Det låga p-värdet på 0.000037 ger att vi förkastar nollhypotesen och drar slutsatsen att det
# finns ett samband mellan kommun och parti.
#
# ## Allmänt
#
# Variansanalys (eller *anova-modellen*) är en statistisk modell där medelvärdet varierar beroende
# på en behandling och ett normalfördelat slumpfel. Från en anova-modell kan man beräkna ett F-test,
# som testar om det finns någon övergripande gruppskillnad, och post-hoc-test, som jämför specifika
# grupper med varandra.
#
# Den specifika modellen beror på försöksupplägget. Här ges exempel på variansanalys med en faktor,
# en faktor med block, och två faktorer.
#
# ## Variansanalys. En faktor
#
# Vid variansanalys med en faktor har man observationer av en kontinuerlig utfallsvariabel från två
# eller flera behandlingsgrupper. Som exempel används en datamängd på ett odlingsförsök med tre
# behandlingar (varav en kontroll). Exemplet finns tillgängligt i R som `PlantGrowth`.
#

PlantGrowth

#
# Datan har 30 observationer av vikt `weight` och varje observation tillhör någon specifik
# behandling `group`. Datan kan illustreras med ett spridningsdiagram.
#

ggplot(PlantGrowth, aes(group, weight)) +
  geom_point()

#
# Behandling 1 verkar vara något lägre än kontrollen medan behandling 2 verkar vara något högre.
#
# En anova-modell kan i R skattas med funktionen `lm()` (för *linjär modell*). Från modellobjektet
# kan man sedan plocka fram en anova-tabell (som bland annat anger utfallet av F-testet) och
# genomföra parvisa jämförelser genom `emmeans`.
#

mod <- lm(weight ~ group, data = PlantGrowth)

#
# Modellen anges som en formel `weight ~ group`, vilket kan utläsas *vikt beroende på
# behandlingsgrupp*. Därefter anges data med argumentet `data`.
#
# För anova-tabellen finns flera alternativ. Här används funktionen `Anova()` från paketet `car`.
#

library(car)
Anova(mod)

#
# Anova-tabellen ger kvadratsummor (`Sum Sq`), frihetsgrader (`Df`) och utfallet av ett F-test.
# Testets hypoteser ges av
#
# H0: alla behandlingsgrupper har samma medelvärde
# H1: alla behandlingsgrupper har inte samma medelvärde
#
# Det låga p-värdet tyder på att nollhypotesen bör förkastas, vilket alltså pekar på att det finns
# någon eller några skillnader i medelvärde.
#
# Uppgift 7.1. (Anova för hand)
# Anovatabell från `Anova()` ger kvadratsummor och frihetsgrader. Använd den informationen för att,
# för hand, beräkna medelkvadratsummor och F-värdet.
# :::
#
# Uppgift 7.2. (Tabellvärde för F-fördelningen)
# Anova-tabellen ger ett p-värde från vilket vi kan dra en direkt slutsats. Om man istället löser
# uppgiften för hand ställer man det beräknade F-värdet mot ett kritiskt värde från en tabell över
# F-fördelningen. Se efter om man kan hitta ett lämpligt tabellvärde för det aktuella testet (med 2
# och 27 frihetsgrader). Det är möjligt att det inte finns en rad för 27 i en vanlig
# F-fördelningstabell, använd isåfall värdet på närmast övre rad (t.ex. 26 eller 25). I R kan
# kvantiler för F-fördelningen tas fram med `qf()`, t.ex.
#

qf(0.95, 2, 27)

# :::
#
# En naturlig följdfråga är vilka behandlingsgrupper som skiljer sig åt. För att besvara det krävs
# *parvisa jämförelser* där behandlingarna jämförs två och två. Parvisa jämförelse kan göras med
# paketet `emmeans` och funktionen med samma namn. Funktionen tar modellobjektet som första argument
# och en formel för jämförelsetyp som andra argument (här `pairwise ~ group`, en parvis jämförelse
# mellan nivåer i `group`).
#

# install.packages("emmeans")
library(emmeans)
emmeans(mod, pairwise ~ group)

#
# I den nedre tabellen med jämförelser ges alla parvisa jämförelser. Nollhypotesen är att de två
# grupper som jämförs har samma medelvärde - ett lågt p-värde tyder alltså på att de två grupperna är
# signifikant skilda. Notera också att p-värden justeras med tukey-metoden, även känt som Tukeys HSD.
#
# Om man istället vill använda Fishers LSD kan man styra justeringen med argumentet `adjust`.
#

emmeans(mod, pairwise ~ group, adjust = "none")

#
# Parvisa jämförelser presenteras ofta med signifikansbokstäver (en. *compact letter display,
# cld*). Dessa kan plockas fram med `multcomp`-paketet och funktionen `cld()`.
#

em <- emmeans(mod, pairwise ~ group)

library(multcomp)
cld(em, Letters = letters)

#
# Tolkning av grupperingen till höger är att grupper som delar en bokstav inte är signifikant
# skilda. I det här fallet är den lägsta nivån skild från de två högsta. I övrigt finns inga
# signifikanta skillnader. Jämför gärna med p-värdena från tabellen med parvisa jämförelser. Man bör
# se att parvisa jämförelser med ett p-värde under fem procent motsvaras av att de behandlingarna
# inte delar någon bokstav i bokstavstabellen.
#
# Uppgift 7.3. (Anova med två behandlingar)
# Följande kod skapar en datamängd med två behandlingar.
#

dat_two <- PlantGrowth %>% filter(group %in% c("trt1", "trt2"))

#
# Använd den datan för att göra ett t-test för två oberoende stickprov med lika varians, ett t-test
# för två oberoende stickprov utan antagande om lika varians, och ett F-test (ofullständig exempelkod
# nedan). Vad kan sägas om p-värdena från de tre testen?
#

t.test(___ ~ group, data = dat_two, var.equal = T)
t.test(weight ~ ___, data = dat_two, var.equal = F)

mod <- lm(weight ~ group, data = ___)
Anova(mod)

# :::
#
# Uppgift 7.4. (Mass-signifikans)
# Anledning till att vi justerar p-värden är att man vid varje test har en sannolikhet att
# förkasta. Om man gör ett stort antal tester är man nästan garanterad att få något (falskt)
# signifikant resultat. Justering höjer p-värdena för att minska den risken. Följande kod simulerar
# data med 5 grupper och producerar de parvisa jämförelserna.
#

n_groups <- 5
dat_sim <- expand_grid(obs = 1:10, group = letters[1:n_groups]) %>% mutate(y = rnorm(n()))
mod <- lm(y ~ group, dat_sim)
emmeans(mod, pairwise ~ group, adjust = "none")

#
# Kör koden tio gånger. Hur många gånger av de tio ger de parvisa jämförelserna *någon* signifikant
# skillnad (det vill säga något p-värde under 0.05)?
#
# En passande xkcd-serie: https://xkcd.com/882/
# :::
#
# Uppgift 7.5. (Äppelinfektionsimport)
# En studie har givit ett mått på infektion hos äppelträd. Fyra sorter jämförs med tre replikat per
# sort. Data finns i fliken *Äppelangrepp* i excelfilen *Uppgiftsdata.xslx* på canvassidan. Fyll i
# kodstycket nedan för att importera datan.
#

library(readxl)
dat_apple <- read_excel("___", sheet = "Äppelangrepp")
dat_apple

# dat_apple <-
# read_csv("https://raw.githubusercontent.com/adamflr/ST0060-2022/main/Data/Uppgiftsdata/Uppgift_%C3%84ppelangrepp.csv")
# Alternativ lösning

# :::
#
# Uppgift 7.6. (Äppelinfektionsgraf)
# Fyll i kodstycket nedan för att skapa en graf av äppeldatan.
#

ggplot(___, aes(x = ___, y = ___)) +
  geom_point()

# :::
#
# Uppgift 7.7. (Äppelinfektionsmodell)
# Fyll i kodstycket nedan för att skatta en anovamodell och ta fram anovatabellen. Vad är F-testets
# noll- och alternativhypotes? Vilken slutsats kan man dra från testet?
#

mod <- lm(___ ~ ___, data = dat_apple)
Anova(mod)

# :::
#
# ## Variansanalys. En faktor med block
#
# I en blockdesign delas försöksobjekten (de enheter man ger en behandling och sedan mäter, t.ex.
# en försöksruta eller en planta) in i grupper av lika objekt (ett *block*). Sedan ger man enheterna
# inom blocket varsin behandling. Blockförsök är ofta balanserade, så att varje behandling förekommer
# en gång i varje block.
#
# Som exempel på ett blockförsök kan vi titta på datan `oats` från paketet `MASS`. Datan kommer
# från ett agrikulturellt försök och blockdesignen sker genom att man delar in ett fält i flera delar
# (blocken) och sätter varje behandling i varje block. Datan har två faktorer (kväve `N` och sort
# `V`), men låt oss i den här första delen titta på en specifik sort.
#

library(MASS)
oats_marvel <- oats %>% filter(V == "Marvellous")
oats_marvel

#
# En vanlig illustration av ett blockförsök är ett punktdiagram kombinerat med ett linjediagram.
#

ggplot(oats_marvel, aes(N, Y, color = B, group = B)) +
  geom_point(size = 4) +
  geom_line()

#
# Färg och linje sammanbinder observationer från samma block. Det finns tecken på en blockeffekt:
# block I är nästan alltid högst och block V är nästan alltid lägst. Det finns också en tydlig
# behandlingseffekt i att högre kväve ger högre skörd.
#
# Blockeffekten kan enkelt föras in i modellen genom att lägga till variabeln `B` i
# `lm`-funktionen. Anova-tabellen och parvisa jämförelser kan göras på samma sätt som tidigare.
# Resultaten påverkas av att modellen har en blockfaktor; man behöver vanligen inte ange det
# explicit.
#

mod_bl <- lm(Y ~ N + B, data = oats_marvel)
Anova(mod_bl)

#
# P-värdet från F-testet på variabeln N är nu klart mindre än tidigare. Detta beror på att en stor
# del av variationen kan förklaras med blockeffekten, vilket är tydligt i att blockeffekten också har
# ett litet p-värde i F-testet.
#
# Det kan vara intressant att jämföra med modellen utan block.
#

mod_wo_block <- lm(Y ~ N, data = oats_marvel)
Anova(mod_wo_block)

#
# Det som är residualens kvadratsumma i modellen utan block är i blockmodellen uppdelat i en
# blockeffekt och en residualterm. Eftersom F-testet bygger på en jämförelse mellan
# behandlingseffekten och residualtermen leder blockdesignen till starkare signifikans i
# blockmodellen. Å andra sidan kostar blockfaktorn frihetsgrader vilket ger oss ett svagare test.
# Effekten av att ta med ett block beror alltså på om det finns en verklig skillnad mellan blocken
# eller ej.
#
# Vi kan gå vidare med att titta på parvisa jämförelser mellan kvävenivåer. Funktionen `emmeans()`
# och `cld()` fungerar som tidigare.
#

cld(emmeans(mod_bl, ~ N), Letters = letters)

#
# Signifikansbokstäver anger att den lägsta nivån är skild från övriga och att den näst lägsta är
# skild från den högsta. Även här kan det vara intressant att jämföra med modellen utan block.
#

cld(emmeans(mod_wo_block, ~ N), Letters = letters)

#
# Modellen utan block ger samma medelvärden `emmean` men större medelfel `SE` och färre
# signifikanta skillnader.
#
# Uppgift 7.8. (Block med två behandlingar. Graf)
# Det minsta möjliga blocket är det med två behandlingar. Vi filtrerar havredatan för att den
# situationen.
#

dat_small_block <- oats %>% filter(V == "Marvellous", N %in% c("0.6cwt", "0.0cwt"))
dat_small_block

#
# Fyll i stycket nedan för att skapa en graf med `N` på x-axeln, `Y` på y-axeln och en gruppering
# som länkar observationer från samma block.
#

ggplot(dat_small_block, aes(x = ___, y = ___, group = ___)) +
  geom_point() +
  geom_line()

# :::
#
# Uppgift 7.9. (Block med två behandlingar. Test)
# Eftersom det är ett försök med en förklarande faktor och block kan man modellera det med den
# tidigare blockmodellen. Men eftersom man bara har två observationer per block kan man också se det
# som matchade stickprov, vilket kan lösas med ett t-test. Fyll i stycket nedan för att göra de två
# testen - utfallsvariabeln är skörd `Y` och den förklarande faktorn är kvävenivån `N`. Jämför
# resultaten.
#

mod <- lm(___ ~ ___ + B, data = dat_small_block)
Anova(mod)

t.test(___ ~ ___, data = dat_small_block, paired = ___)

# :::
#
# Uppgift 7.10. (Majshybridimport)
# I fliken *Majshybrider* i excelfilen *Uppgiftsdata.xlsx* finns data på fyra majssorter, vardera
# sorterad på fem platser (som agerar som block). Importera datan med funktionen `read_excel()` genom
# att fylla i kodstycket nedan.
#

dat_corn <- read_excel("", sheet = ___)

#
# :::
#
# Uppgift 7.11. (Majshybridgraf)
# Skapa en lämplig graf av datan på majshybrider. Grafen ska illustrera både jämförelsen mellan
# hybrider och jämförelsen mellan platser. Se exemplet ovan som guide.
# :::
#
# Uppgift 7.12. (Majshybridmodell)
# Fyll i koden nedan för att skatta en anova-modell med block för datan på majshybrider. Ta fram
# anovatabellen med `Anova()`. Vilka slutsatser kan man dra från anovatabellen?
#

mod <- lm(___ ~ ___ + Plats, data = dat_corn)
Anova(mod)

# :::
#
# Uppgift 7.13. (Majshybridjämförelser)
# Gör lämplig ändring i koden nedan för att jämföra hybrider, istället för platser.
#

emmeans(mod, pairwise ~ Plats)

# :::
#
# ## Variansanalys. Två faktorer med block
#
# Exempeldata på havre tar med två förklarande faktorer och ett block. Datan kan illustreras med
# ett punktdiagram där `facet_wrap` delar grafen efter sort.
#

ggplot(oats, aes(N, Y, color = B)) +
  geom_point(size = 4) +
  facet_wrap(~ V)

#
# Grafen visar samma kvävesamband som tidigare. Det finns inga tydliga skillnader mellan sorter,
# möjligen har sorten Victory givit något lägre skörd än övriga. Det finns fortfarande en tydlig
# blockeffekt, till exempel har block I höga värden och block V låga värden.
#
# Modellen skattas genom att lägga till variabeln för sort (V för variety) i `lm`-formeln. En
# modell med två faktorer kan antingen vara med eller utan en *interaktion*. Interaktionstermen
# fångar påverkan mellan faktorerna. Ett exempel hade varit om någon sort svarat starkare på ökad
# kväve än någon annan. Standardmodellen är att ta med interaktionen, vilket vi anger genom att sätta
# `N * V` istället för `N + V`. Blocket tas fortfarande med som en adderad faktor
#

mod_two_fact <- lm(Y ~ N * V + B, data = oats)

#
# Anovatabellen kan plockas fram på samma sätt som tidigare.
#

Anova(mod_two_fact)

#
# Raden `N:V` gäller interaktionseffekten mellan kväve och sort. I det här fallet är det ingen
# signifikant interaktion - vilket tyder på att sorterna svarar på kvävebehandling på liknande sätt.
# Samtliga huvudeffekter (raderna för N, V och B) är signifikanta. Kvadratsummorna och p-värdena
# tyder på att kväve förklarar mer av variationen än sort, vilket också är i linje med grafen ovan.
#
# Vid flerfaktoriella försök kan man presentera parvisa jämförelser på flera olika sätt. Man kan
# ange huvudeffekter för en faktor utan att ange den andra faktorn, man kan ange medelvärden för
# samtliga kombinationer av två faktorer, och man kan ange medelvärden uppdelat efter nivåer i en
# annan faktor.
#

emmeans(mod_two_fact, ~ N)
emmeans(mod_two_fact, ~ N + V)
emmeans(mod_two_fact, ~ N | V)

#
# Även här kan man göra jämförelser mellan nivåer genom att sätta `pairwise ~ N + V` eller beräkna
# signifikansbokstäver med `cld`. Följande kod jämför kvävenivåer *inom* sort.
#

cld(emmeans(mod_two_fact, ~ N | V), Letters = letters)

#
# Uppgift 7.14. (Sort uppdelat efter kvävenivå)
# Gör lämplig ändring i koden ovan för att jämföra sorter *inom* kvävenivå. Finns det några
# signifikanta skillnader?
# :::
#
# Uppgift 7.15. (Interaktion med ett block)
# I modellen ovan är block en *additiv* faktor - den ingår inte i någon interaktionseffekt. Vad
# händer med testerna om man skattar modellen där samtliga interaktioner tas med? Varför?
#

mod_two_fact <- lm(Y ~ N * V * B, data = oats)

# :::
#
# ## Modellantaganden och residualer
#
# Samtliga anovamodeller har samma grundläggande antaganden: feltermerna (den kvarvarande
# slumpmässigheten) är normalfördelade, sinsemellan oberoende, och variansen är samma för samtliga
# behandlingsgrupper. Antagandena testas oftast genom att titta på modellens *residualer* -
# skillnaden mellan det faktiska värdet och det skattade värdet. För en skattad modell kan man ta upp
# residualerna med `residuals()` och de skattade värdena med `fitted()`. Vi kan lägga till residualer
# och skattningar till datan med ett `mutate()`-steg.
#

oats <- oats %>% 
  mutate(Residualer = residuals(mod_two_fact),
         Skattade = fitted(mod_two_fact))

#
# Normalfördelning kan undersökas grafiskt med ett histogram eller en QQ-graf.
#

g_hist <- ggplot(oats, aes(Residualer)) + geom_histogram(bins = 20)
g_qq <- ggplot(oats, aes(sample = Residualer)) + geom_qq() + geom_qq_line()

library(patchwork)
g_hist + g_qq

#
# Punkterna avviker något från normalfördelningen i svansarna, men det är förstås alltid en
# bedömningsfråga.
#
# Lika varians undersöks ofta med ett spridningsdiagram med de skattade värdena på x-axeln och
# residualerna på y-axeln.
#

ggplot(oats, aes(x = Skattade, y = Residualer)) +
  geom_point() +
  geom_hline(yintercept = 0, alpha = 0.3)

#
# Om datan är i linje med antaganden ska diagrammet se ut som slumpmässigt placerade punkter med
# ungefär lika stor spridning kring noll-linjen för samtliga nivåer på x-axeln. För det här exemplet
# ser det okej ut.
#
# Uppgift 7.16. (Bakterieimport)
# Fliken *Bakterier* i filen *Uppgiftsdata.xlsx* innehåller data om tillväxt hos gräs efter
# inokulering av bakterier. Ladda ner filen och importera datan genom att fylla i koden nedan.
#

dat_bact <- read_excel("___", sheet = "Bakterier")

# :::
#
# Uppgift 7.17. (Bakterieimport)
# Illustrera datan med en lämplig graf, till exempel ett spridningsdiagram med `Inoculation` på
# x-axeln, `Dry weight` på y-axeln, småfönster efter `Cultivar` och färg efter `Block`.
#

ggplot(dat_bact, aes(x = ___, y = `___`, color = Block)) +
  geom_point(size = 6) +
  facet_wrap(~ ___)

#
# Hur blev färgerna för blocket? Om de inte blev distinkta färger kan variabeln `Block` ha blivit
# inläst som numerisk. Transformera variabeln med `as.character()` och gör om grafen. Ändras
# färgerna?
#

dat_bact <- dat_bact %>% 
  mutate(Block = as.character(Block))

# :::
#
# Uppgift 7.18. (Bakteriemodell)
# Bakteriedatan har två faktorer och en blockfaktor. Skatta en anova-modell med interaktion och
# block genom att fylla i stycket nedan. Ta fram anovatabell och dra en slutsats från F-testen.
# Ligger slutsatsen i linje med grafen?
#

mod <- lm(`___` ~ ___ * ___ + ___, data = dat_bact)
Anova(mod)

# :::
#
# Uppgift 7.19. (Bakteriejämförelser)
# Använd `emmeans()` för parvisa jämförelser mellan inokuleringsmetoder. Vilka par är signifikant
# åtskilda?
#

emmeans(mod, pairwise ~ ___)

# :::
#
# Uppgift 7.20. (Bakterieresidualer)
# Vi använder den skattade modellen för att ta fram skattade värden och residualer.
#

dat_bact <- dat_bact %>% 
  mutate(Residualer = residuals(mod),
         Skattade = fitted(mod))

#
# Använd exemplet på residualtester ovan för att undersöka antagandet om normalfördelade
# residualer.
# :::
#
# ## Bonus. Statistik för ekologi
#
# Här tittar vi på några statistiska metoder som är vanliga inom ekologin, men går bortom
# materialet på en statistisk grundkurs. Vi börjar med datastruktur och visualisering för
# populationsdata, för att sedan titta på diversitetsmått, principalkomponentanalys (PCA) och
# hierarkisk klustering. Det vanligaste paketet för ekologi är `vegan`, så vi kan börja med att
# installera och ladda det. Vi kommer också använda `factoextra` för en graf.
#

# install.packages("vegan")
library(vegan)

# install.packages("factoextra")
library(factoextra)

#
# Data för ekologiska populationer för flera platser eller tillfällen ordnas oftast i en tabell med
# plats som rad och arter som kolumner. Värdena i tabellen anger antingen antalet observerade
# individer eller ett binärt utfall (1 för förekomst, 0 för ingen förekomst). Exempeldatan `dune`,
# som kan laddas med funktionen `data()`, ger ett exempel. För att illustrera en typisk
# jämförelsestudie skapar vi en kolumn för platstyp och lägger till ett plats-id.
#

data(dune)
dune <- dune %>% 
  mutate(Site = 1:n(), 
         Type = rep(c("A", "B"), each = 10))

#
# Vi kan illustrera data genom att pivotera till långt format och göra en graf med `ggplot()`. En
# heatmap eller ett spridningsdiagram med storlek för antal observationer kan vara lämpliga grafer.
# Tolkning kräver förstås kod artkännedom och beror på den vetenskapliga frågan.
#

dune_long <- dune %>%
  pivot_longer(-c(Site, Type), names_to = "Species", values_to = "Abundance")

ggplot(dune_long, aes(Site, Species, fill = Abundance)) +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()

ggplot(dune_long %>% filter(Abundance > 0), aes(Site, Species, size = Abundance, color = Type)) +
  geom_point()

#
# Ytterligare alternativ kan vara upprepade lådagram eller staplar med småfönster per art.
#
# Uppgift 7.21. (Populationsgrafer)
# Vad måste läggas till i stycket nedan för göra ett lådagram (med art på y-axeln och abundans på
# x-axeln) och ett stapeldiagram (med platstyp på x-axeln och abundans på y-axeln)?
#

dune_long
ggplot(dune_long, aes(x = ___, y = ___, fill = Type)) +
  geom_boxplot()

ggplot(dune_long, aes(x = ___, y = ___, fill = Type)) +
  geom_col() +
  facet_wrap(~ ___, nrow = 2)

# :::
#
# Ekologiska populationer kan analyseras genom *heirarkisk klustring* - metoder där platser (rader)
# eller arter (kolumner) sorteras efter hur lika de är. Först beräknas ett avstånd mellan samtliga
# enheter (platser eller arter) och därefter sker klustringen genom att slå ihop enheter som ligger
# *nära* varandra. Resultatet illustreras med ett träddiagram.
#

dune_data <- dune %>% select(-Site, -Type)
d <- dist(dune_data, method = "euclidean")
hc <- hclust(d)
plot(hc, hang = -1, labels = dune$Type, 
     axes = F, xlab = "", ylab = "", ann = F)

#
# Uppgift 7.22. (Avståndsmått)
# Ta upp hjälpsidan till distansfunktionen med `?dist`. Under `method` finns flera möjliga
# avståndsmått. Vad måste ändras i kodstycket ovan för att ange ett Manhattan-avstånd? Har avståndet
# någon betydande effekt på träddiagrammet?
# :::
#
# För att göra en klustring av arter kan man *transponera* data så att rader och kolumner byter
# plats med varandra. Här kommer artnamn automatisk med eftersom raderna i datan har namn. Det är
# inte alltid fallet, så det kan vara nödvändigt att sätta etiketter med argumentet `labels` i
# `plot()`.
#

dune_data <- t(dune_data)
d <- dist(dune_data, method = "euclidean")
hc <- hclust(d)
plot(hc, hang = -1,
     axes = F, xlab = "", ylab = "", ann = F)

#
# Träddiagrammet tolkas så att enheter vars koppling ligger lågt är mer lika varandra - arterna
# förekommer ofta på samma plats.
#
# En annan vanlig metod för *multivariat* data, vilket populationsdata är ett exempel på, är
# *principalkomponentsanalys* (PCA, Principal Component Analysis). En PCA är ett försök att
# sammanfatta den ursprungliga datans 30 variabler (en per art) med ett mindre antal variabler. De
# nya variablerna - *komponenterna* - skapas genom att väga och addera de ursprungliga variablerna på
# ett sätt som förklarar så mycket som möjligt av variationen med minsta möjliga antal variabler.
# Resultatet illustreras vanligen med en *biplot* - ett spridningsdiagram som placerar ut både
# platser och arter.
#
# I R kan en PCA göras med `prcomp()` och en biplot kan göras med `fviz_pca_biplot()` från
# `factoextra`.
#

dune_data <- dune %>% select(-Site, -Type)
pca <- prcomp(dune_data, scale. = F)
fviz_pca_biplot(pca, geom.ind = "point", habillage = dune$Type, labelsize = 3)

#
# Platserna illustreras med punkter och arterna med pilar. Pilar i samma riktning motsvarar arter
# som är lika (de finns på samma platser), närliggande punkter motsvarar lika platser (de har samma
# arter), och punkter i samma riktning som en pil har höga värden för den arten.
#
# Uppgift 7.23. (Skalning i en PCA)
# En PCA kan göras med och utan att skala variablerna. Om variablerna skalas får en variabel som
# varierar mycket samma vikt som en variabel som varierar lite. Det kan vara bra om man har variabler
# som är mätta på olika sätt, till exempel om en variabel är i meter och en är i centimeter. Gör
# lämplig ändring i kodstycket ovan för att skala variablerna i `prcomp()`. Har det någon effekt på
# grafen?
# :::
#
# Den sista ansatsen vi ska titta på är att sammanfatta en population i ett enskilt tal - ett
# diversitetsindex. Genom att beräkna ett index kan man reducera datan till en observation på plats.
# Man kan därifrån tillämpa de metoder vi sett i övriga delar av kursen (t-test och variansanalys).
# Det finns en stor mängd olika index. Det vanligaste än Shannon-Weaver indexet (eller entropi),
# vilket beräknas genom att ta andelen per art, multiplicera med logaritmen av andelen, summera över
# arter, och multiplicera med minus ett. Om man har tre arter med andelarna 0.3, 0.5 och 0.2 ges
# Shannon-Weaver alltså av
#

-(0.3 * log(0.3) + 0.5 * log(0.5) + 0.2 * log(0.2))

#
# Indexet ökar om det finns många arter och om andelen per art är samma. En population med *en*
# dominant art kommer alltså ha ett lågt index.
#
# För en tabell med data kan index beräknas med `diversity()`.
#

diver <- diversity(dune_data, index = "shannon")
dune <- dune %>% mutate(Diversity = diver)

#
# Diversitetsindexen kan sedan illustreras och analyseras som vilken numerisk variabel som helst.
#

ggplot(dune, aes(Diversity, Type)) + geom_point()

mod <- lm(Diversity ~ Type, data = dune)
Anova(mod)
emmeans(mod, ~ Type)

#
# Här finns en signifikant skillnad med platstyper.
#
# Uppgift 7.24. (Diversitetsindex)
# Ta upp hjälpsidan till funktionen `diversity()`. Hur anger man att funktionen ska ge Simpsons
# index?
# :::
#
# Uppgift 7.25. (Test på nytt index)
# Gör om analysen på diversitet (anovamodellen och F-testet) med Simpsons index istället för
# Shannon-Weaver. Påverkar valet av diversitetsindex utfallet av testet?
# :::
