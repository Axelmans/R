# ComputerZitting R0
# Alle gegeven commands zijn gebruikt, alsook extra commands voor
# plots op te slaan als afbeeldingen (in folder "Res0").
# dit kan door de commands "png(filename.png)"
# gevolgd door de plot
# en dan "dev.off()"

# Lezen van CSV en TXT bestanden
# file = locatie van bestand om in te lezen (in dit geval de Data folder)
# header = geeft aan of de namen van de variabelen in rij 1 staan
# dec = symbool gebruikt als komma voor decimale getallen
# sep = symbool om observaties van de variabelen te scheiden
gegevens = read.csv(file="Data/resultaten.csv", header=TRUE, dec=",", sep=";")
gegevens = read.table(file="Data/resultaten.txt", header=TRUE)

# Eigen dataset intoetsen
kleur = c('D', 'D', 'L', 'L', 'R', 'R')
geslacht = c('M', 'V', 'M', 'V', 'M', 'V')
aantal = c(9, 14, 12, 11, 3, 7)
gegevens = data.frame(kleur,geslacht,aantal)
gegevens

# Dataset opslaan
save(kleur, gegevens, file="Saved Datasets/kleur.Rdata")

# Probleem 1: verband tussen 2 continue variabelen nagaan
# Middelbaar en bachelor
gegevens = read.table(file="Data/resultaten.txt", header=TRUE)
# Idee = Scatterplot van 2 variabelen nemen:
# 2 variabelen nodig, uit gegevens halen met '$'
# type = soort plot (p = punten)
# x/ylab = label voor assen
# main = titel grafiek
png("Res0/ScatterplotR0.png")
plot(gegevens$middelbaar, gegevens$bachelor, type="p",
     xlab="Score Middelbaar", ylab="Score 1ste Bachelor",
     main="Score 1ste Bachelor = f(Score Middelbaar)")
dev.off()

# Probleem 2: Centrumkenmerken
# Test: Zijn jongens echt groter dan meisjes?
lengteM = gegevens$lengte[gegevens$geslacht=="M"]
lengteV = gegevens$lengte[gegevens$geslacht=="V"]
# Berekenen van gemiddelden en medianen
mu_M = mean(lengteM)
mu_M
med_M = median(lengteM)
med_M
mu_V = mean(lengteV)
mu_V
med_V = median(lengteV)
med_V
# Plotten van histograms
par(mfcol=c(1,2))
png("Res0/HistMale.png")
h_M = hist(lengteM)
dev.off()
png("Res0/HistFemale.png")
h_V = hist(lengteV)
dev.off()

# Probleem 3: Bepalen van spreidingskenmerken van lengte
lengte = gegevens$lengte
lengte
# Variantie
var_l = var(lengte)
var_l
# Minimum en Maximum (= Range)
min_max_l=range(lengte)
min_max_l
range_l=min_max_l [2] - min_max_l [1]
range_l
# Interkwartielafstand
iqr_l = IQR(lengte)
iqr_l
# Standaardafwijking (= sqrt(variantie))
sd_l=sd(lengte)
sd_l

# Maken van een Boxplot
# Lengte, onafhankelijk van gender
png("Res0/BoxplotLengte.png")
boxplot(lengte, ylab="Lengte (in cm)", main="Boxplot van de variabele lengte")
dev.off()
# Lengte, voor beide genders apart
png("Res0/BoxplotLengtePerGender.png")
boxplot(list(lengteM, lengteV),ylab="lengte (in cm)",
        names = c("jongens", "meisjes"),
        main="Boxplot van de lengte van de jongens en de meisjes apart")
dev.off()

# Probleem 4: Vergelijken van 2 of meerdere groepen (middelbaar en bachelor)
verschil = gegevens$middelbaar - gegevens$bachelor
mu_verschil = mean(verschil)
mu_verschil
med_verschil = median(verschil)
med_verschil
png("Res0/BoxplotVerschilBachelorMiddelbaar.png")
boxplot(verschil, ylab="Verschil tussen middelbare en universitaire scores",
        main="Boxplot van het verschil tussen middelbare en universitaire scores")
dev.off()

# Vergelijken van bachelor en studierichting (ongepaard)
mu_bachelor = mean(gegevens$bachelor)
med_bachelor = median(gegevens$bachelor)
png("Res0/BoxplotBachelorPerStudieRichting.png")
boxplot(gegevens$bachelor ~ gegevens$studierichting,
        xlab="Studierichting", ylab="Bachelor-Scores",
        main="Boxplot van Bachelor-scores en Studierichting")
dev.off()






