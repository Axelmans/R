# Oefening 1 + 2 + 3: Weerbericht
weer = read.csv(file="Data/datajuni.csv", header=TRUE, dec=",", sep=";")

temp = weer$TEMPERATUUR
# We moeten nagaan of de gegevens normaal verdeeld zijn! Boxplot, QQ, Histogram
# Is de boxplot symmetrisch?
png("Res2/BoxplotTemperatuur.png")
boxplot(temp, ylab="Temperatuur (in °C)", main="Boxplot van de temperatuur")
dev.off()
# Volgt de QQ-plot een rechte?
png("Res2/QQtemperatuur.png")
qqnorm(weer$TEMPERATUUR)
dev.off()
# Neemt het histogram de vorm van een klokcurve aan?
png("Res2/HistogramTemperatuur.png")
hist(weer$TEMPERATUUR)
dev.off()
# H0: µ = 20
# HA: µ ≠ 20
# Informatie over t-test; Help moet in de print!!
# print(help("t.test"))
# Test op significantieniveau 5%
# Genereert hypothese-object met o.a. betrouwbaarheidsinterval en p-waarde
# Default op niveau 5%
t1 = t.test(weer$TEMPERATUUR,mu=20, alternative="two.sided")
t1
# Test op significantieniveau 1%
t2 = t.test(weer$TEMPERATUUR,mu=20, alternative="two.sided", conf.level=0.99)
t2

# Oefening 4: Normaliteitstest: Shapiro-Wilk
shapiro_res = shapiro.test(weer$TEMPERATUUR)
shapiro_res

# Test over mediaan
med_test = SIGN.test(weer$TEMPERATUUR, md=20, alternative="two.sided")
med_test

# Oefening 5: Test over proportie p (oefening 6 bestaat niet)
roker_data <- c(1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1,
                0, 0, 0, 1, 1, 0, 0, 0, 1)
# print(help("binom.test"))
binom_res = binom.test(sum(roker_data), 30, 0.5, alternative="greater")

# Oefening 7 + 8: Cholesterol
cholesterol = read.table(file="Data/cholesterol.txt", header=TRUE)
# We verwachten dat het gehalte na 2 en 4 dagen gepaarde gegevens zijn
cholest2 = cholesterol$cholest2
cholest4 = cholesterol$cholest4
t_cholest_tz = t.test(cholest2, cholest4, paired=TRUE)
t_cholest_le = t.test(cholest2, cholest4, paired=TRUE, alternative="less")
t_cholest_re = t.test(cholest2, cholest4, paired=TRUE, alternative="greater")

# Oefening 9 + 10 + 11: Evenwicht
evenwicht = read.csv(file="Data/evenwicht.csv", header=TRUE, dec=",", sep=";")
zwaai = evenwicht$zwaai
leeftijd = evenwicht$leeftijd
# Test of de variantie van de 2 groepen gelijk is, we besluiten van wel.
var_test = var.test(zwaai[leeftijd==1], zwaai[leeftijd==2])
t_evenwicht = t.test(zwaai[leeftijd==1], zwaai[leeftijd==2], var.equal=TRUE)
t_evenwicht2 = t.test(zwaai[leeftijd==1], zwaai[leeftijd==2], var.equal=TRUE, conf.level=0.99)

# Oefening 12: Rivieren
rivieren = read.csv(file="Data/rivieren.csv", header=TRUE, dec=",", sep=";")
lengte = rivieren$lengte
zee = rivieren$zee
rivieren_var_test = var.test(lengte[zee==1], lengte[zee==2])
rivieren_t_test1 = t.test(lengte[zee==1], lengte[zee==2], var.equal=FALSE)
rivieren_t_test2 = t.test(lengte[zee==1], lengte[zee==2], var.equal=FALSE, alternative="greater")