# ComputerZitting R1
# Alle gegeven commands zijn gebruikt

# Kansberekening van normaal, student, chi-kwadraat en F-verdeling
# Syntax werd eerst gegeven, dan voorbeeldoefening met gegeven variabelen
# P(X < 8), N(5, 4)
# lower.tail, berekent kans voor X < x indien TRUE, anders voor X > x
# van toepassing bij alle continue verdelingen
p1 = pnorm(8, mean=5, sd=4, lower.tail=TRUE)
p1
# P(X <= 3.45) = P(X < 3.45), t(6), x^2(5)
p2 = pt(3.45, 6)
p2
p3 = pchisq(3.45, 5)
p3
# P(X > 2.5), F(m, n) = F(25, 2), opgelet: R verwacht eerst n, dan m!
p4 = pf(2.5, 2, 25, lower.tail=FALSE)
p4
# P(2 < X < 5) = P(X < 5) - P(X < 2), N(3, 4)
p5 = pnorm(5, mean=3, sd=4) - pnorm(2, mean=3, sd=4)
p5

# Kwantielen
# Percentiel = "Voor welke x is P(X < x) = p%?", p is gegeven
q1 = qnorm(0.1, mean=7, sd=1, lower.tail=TRUE)
q1
q2 = qnorm(0.91, mean=25, sd=5, lower.tail=TRUE)
q2
q3 = qnorm(0.06, mean=25, sd=5, lower.tail=TRUE)
q3
q4 = qt(0.25, 67)
q4
q5 = qchisq(0.25, 16)
q5

# Genereren van Steekproeven
r1 = rnorm(25, mean=0, sd=1)
r1
mean_r1 = mean(r1)
mean_r1
med_r1 = median(r1)
med_r1
var_r1 = var(r1)
var_r1
iqr_r = IQR(r1)
iqr_r
r2 = rf(83, 15, 17)
r2

# QQplots voor bepaling en transformatie van normale verdelingen
statdata1 = read.csv(file="Data/statdata1.csv", dec=",", sep=" ")
png("Res1/QQstatdata1.png")
qqnorm(statdata1$data1)
dev.off()
png("Res1/QQlogstatdata1.png")
qqnorm(log(statdata1$data1))
dev.off()
statdata2 = read.csv(file="Data/statdata2.csv", header=TRUE, dec=",", sep=" ")
statdata3 = read.csv(file="Data/statdata3.csv", header=TRUE, dec=",", sep=" ")
png("Res1/QQstatdata3.png")
qqnorm(statdata3$data3)
dev.off()

# Nakijken van verdeling van gegevens
# Lengte (onafhankelijk van gender)
resultaten = read.table(file="Data/resultaten.txt", header=TRUE)
png("Res1/BoxplotLengte.png")
boxplot(resultaten$lengte)
dev.off()
png("Res1/HistogramLengte.png")
hist(resultaten$lengte)
dev.off()
png("Res1/QQplotLengte.png")
qqnorm(resultaten$lengte)
dev.off()

# Lengte (Vrouwen)
png("Res1/BoxplotLengteVrouwen.png")
boxplot(resultaten$lengte[resultaten$geslacht=="V"])
dev.off()
png("Res1/HistogramLengteVrouwen.png")
hist(resultaten$lengte[resultaten$geslacht=="V"])
dev.off()
png("Res1/QQplotLengteVrouwen.png")
qqnorm(resultaten$lengte[resultaten$geslacht=="V"])
dev.off()

# Lengte (Niet-Vrouw)
png("Res1/BoxplotLengteNietVrouwen.png")
boxplot(resultaten$lengte[resultaten$geslacht=="M"])
dev.off()
png("Res1/HistogramLengteNietVrouwen.png")
hist(resultaten$lengte[resultaten$geslacht=="M"])
dev.off()
png("Res1/QQplotLengteNietvrouwen.png")
qqnorm(resultaten$lengte[resultaten$geslacht=="M"])
dev.off()

# Extra oefeningen
ep1 = pnorm(120, mean=110, sd=20, lower.tail=TRUE)
eq1 = qnorm(0.9, mean=110, sd=20, lower.tail=TRUE)
ep2 = pnorm(6.15, mean=6, sd=0.1, lower.tail=TRUE) - 
  pnorm(5.90, mean=6, sd=0.1, lower.tail=TRUE)
ep3 = pnorm(6.10, mean=6, sd=0.1, lower.tail=FALSE)
ep4 = pnorm(5.95, mean=6, sd=0.1, lower.tail=TRUE)
eq2 = qnorm(0.95, mean=6, sd=0.1, lower.tail=TRUE)



