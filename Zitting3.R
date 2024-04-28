# Inleiding
# Testen omtrent discrete verdelingen
NrJongens = c(34,33,7)
NrMeisjes = c(22,22,4)
Ctable = data.frame(NrJongens,NrMeisjes)
ChiSq = chisq.test(Ctable)
ChiSq
ChiSq$observed
ChiSq$expected

# Rijen en kolommen omwisselen zou zelfde resultaat moeten opbrengen
NrAtletiek = c(34, 22)
NrPaardrijden = c(33, 22)
NrSchaatsen = c(7, 4)
Ctable_2 = data.frame(NrAtletiek, NrPaardrijden, NrSchaatsen)
ChiSq_2 = chisq.test(Ctable_2)
ChiSq_2$observed
ChiSq_2$expected

# Oefening 1
gegevens = read.csv(file="Data/resultaten.csv", header=TRUE, dec=",", sep=";")
# data.frame werkt enkel bij getalwaarden, anders: gebruik ftable!!
gender_kleur_ctable = ftable(gegevens$geslacht, gegevens$kleur)
gender_kleur_test = chisq.test(gender_kleur_ctable)
gender_studie_ctable = ftable(gegevens$geslacht, gegevens$bachelor)
gender_studie_test = chisq.test(gender_studie_ctable)

# Oefening 2
roker = read.csv(file="Data/roken.csv", header=TRUE, dec=",", sep=";")
rokers = roker$rokers
mortaliteit = roker$mortaliteit
rokersmort = data.frame(rokers, mortaliteit)
# De plot toont een uitschieter, die verwijderen we.
plot(rokersmort, type="p", xlab="# Rokers", ylab = "Mortaliteit", 
     main="Mortaliteit = f(# Rokers)")
# Toont plot, klik op punt dat vermoedelijk de uitschieter is!
# index_uitschieters<-identify(rokersmort, n = 1)
# Verwijdert aangeduid punt.
# rokersmort_zonder_uitschieter = rokersmort[-index_uitschieters,]
png("Res3/ScatterplotRokersMortaliteitZonderUitschieters.png")
plot(rokersmort_zonder_uitschieter, type="p", xlab="# Rokers", ylab = "Mortaliteit", 
     main="Mortaliteit = f(# Rokers)")
dev.off()
# Zijn de gegevens normaal verdeeld?
normaliteit_rokers = shapiro.test(rokersmort_zonder_uitschieter$rokers)
normaliteit_mortaliteit = shapiro.test(rokersmort_zonder_uitschieter$mortaliteit)
# Berekening correlatiecoëfficiënt.
lineair_verband_test = cor.test(rokersmort_zonder_uitschieter$rokers, 
         rokersmort_zonder_uitschieter$mortaliteit, method="pearson")

# Oefening 3
gegevens = read.csv(file="Data/azijnzuur.csv", header=TRUE, dec=",", sep=";")
# Een plot is zinvol voor uitvoering van de test
#png("Res3/Azijnzuur.png")
#plot(gegevens, type="p", xlab="gewicht", ylab = "dichtheid", 
#     main="Mortaliteit = f(gewicht)")
#dev.off()
# Geen zichtbare uitschieters
azijnzuur_test = cor.test(gegevens$gewicht, gegevens$dichtheid, method="pearson")
normaliteit_gewicht = shapiro.test(gegevens$gewicht)
normaliteit_dichtheid = shapiro.test(gegevens$dichtheid)

# Oefening 4
gegevens = read.csv(file="Data/vis.csv", header=TRUE, dec=",", sep=";")
prijzen = data.frame(gegevens$Prijs70, gegevens$Prijs80)
png("Res3/Vis.png")
plot(prijzen, type="p", xlab="Prijs70", ylab = "Prijs80", 
     main="Prijs80 = f(Prijs70)")
dev.off()
normaliteit_prijs70 = shapiro.test(gegevens$Prijs70)
normaliteit_prijs80 = shapiro.test(gegevens$Prijs80)
# De gegevens zijn niet normaal verdeeld dus werken we met de Spearman
# correlatiecoëfficiënt
vis_test = cor.test(gegevens$Prijs70, gegevens$Prijs80, method="spearman")

# Niet aangeduid als oefening, maar er waren wat vragen voor het opstellen 
# van een lineair regressiemodel voor de roker data
model = lm(rokersmort_zonder_uitschieter$mortaliteit ~ rokersmort_zonder_uitschieter$rokers)
# Print het resultaat van het model
# print(summary(model))
# De residuen zijn ook interessant
rawres = residuals(model)
png("Res3/ResidualsQQ.png")
qqnorm(rawres, xlab="Standard Normal Quantiles", ylab="Raw Residuals", 
       main= "Raw Residual-Normal QQ-plot")
dev.off()
# Nog enkele interessante plots
# Scatterplot tussen 2 variabelen
png("Res3/RokersMortaliteitQQ.png")
plot(rokersmort_zonder_uitschieter$rokers, rokersmort_zonder_uitschieter$mortaliteit, 
     xlab="Rokers", ylab="Mortaliteit", main= "Mortaliteit = f(Rokers)")
abline(model, col="red", lty=1)
dev.off()
# Gestandaardiseerde residuen en Verklarende Variabele "Roker"
standres = rstandard(model)
png("Res3/RokersStandaardresiduen.png")
plot(rokersmort_zonder_uitschieter$rokers, standres, 
     xlab="Rokers", ylab="Standardized Residuals", main= "Standardized Residuals")
abline(h=0, col="blue", lty=1)
abline(h=-2.5, col="red", lty=1)
abline(h=-2.5, col="red", lty=1)
dev.off()
# Plot tussen voorspelde en werkelijke waarden
predictmort = predict.lm(model)
png("Res3/MortaliteitVoorspeldWaargenomen.png")
plot(rokersmort_zonder_uitschieter$mortaliteit, predictmort, xlab="Geobserveerde Mortaliteit",
     ylab="Voorspelde Mortaliteit", main="Voorspelde Mortaliteit = f(Geobserveerde Mortaliteit)")
abline(a=0, b=1, col="red", lty=1)
dev.off()

# Oefening 5 (extra)
zuurstof = read.csv("Data/zuurstof.csv", header=TRUE, dec=",", sep=";")
# Scatterplot van 2 variabelen
png("Res3/ScatterplotZuurstof.png")
plot(zuurstof$zuiverheid, zuurstof$koolwaterstof, xlab="Zuiverheid", 
     ylab="Koolwaterstof", main="Koolwaterstof = f(Zuiverheid)")
dev.off()
# Zijn de gegevens normaal verdeeld?
zuiverheid_normaal_test = shapiro.test(zuurstof$zuiverheid)
koolwaterstof_normaal_test = shapiro.test(zuurstof$koolwaterstof)
zuurstof_corr_test = cor.test(zuurstof$zuiverheid, zuurstof$koolwaterstof, method="pearson")
# Lineair regressiemodel
zuurstof_model = lm(zuurstof$koolwaterstof ~ zuurstof$zuiverheid)
# print(summary(zuurstof_model))
samenvatting_zuurstofmodel = summary(zuurstof_model)
# Check de residuen
zuurstof_rawres = residuals(zuurstof_model)
png("Res3/ZuurstofResidualsQQ.png")
qqnorm(rawres, xlab="Standard Normal Quantiles", ylab="Raw Residuals", 
       main= "Raw Residual-Normal QQ-plot")
dev.off()
# Nog enkele plots voor het besluit
# Scatterplot van 2 variabelen met rechte van model
png("Res3/ScatterplotZuurstofMetRechte.png")
plot(zuurstof$zuiverheid, zuurstof$koolwaterstof, xlab="Zuiverheid", 
     ylab="Koolwaterstof", main="Koolwaterstof = f(Zuiverheid)")
abline(zuurstof_model, col="red", lty=1)
dev.off()
# Gestandaardiseerde residuen en Verklarende Variabele "Zuiverheid"
zuurstof_standres = rstandard(zuurstof_model)
png("Res3/ZuiverheidStandaardresiduen.png")
plot(zuurstof$zuiverheid, zuurstof_standres, 
     xlab="Zuiverheid", ylab="Standardized Residuals", main= "Standardized Residuals")
abline(h=0, col="blue", lty=1)
abline(h=-2.5, col="red", lty=1)
abline(h=-2.5, col="red", lty=1)
dev.off()
# Plot tussen voorspelde en werkelijke waarden
predictkoolwaterstof = predict.lm(zuurstof_model)
png("Res3/KoolwaterstofVoorspeldWaargenomen.png")
plot(zuurstof$koolwaterstof, predictkoolwaterstof, xlab="Geobserveerde Koolwaterstof",
     ylab="Voorspelde Koolwaterstof", main="Voorspelde Koolwaterstof = f(Geobserveerde Koolwaterstof)")
abline(a=0, b=1, col="red", lty=1)
dev.off()
# Bereken de waarde voor zuiverheid = 91
# Zo krijg je de intercept en de rico
intercept = summary(zuurstof_model)$coefficients[1, 1]
rico = summary(zuurstof_model)$coefficients[2, 1]
# Welke waarde is verwacht voor zuiverheid = 91?
print(91*rico + intercept)

# Oefening 6 (extra)
politie = read.csv(file="Data/politie.csv", header=TRUE, dec=",", sep=";")
agent_normaliteit = shapiro.test(politie$agent)
diefstal_normaliteit = shapiro.test(politie$diefstal)
# P-waarde geeft geen montoon verband aan tussen 2 variabelen
agent_diefstal_corr = cor.test(politie$agent, politie$diefstal, method="spearman")
agent_diefstal = data.frame(politie$agent, politie$diefstal)
# Scatterplot agent en diefstal
png("Res3/ScatterplotAgentDiefstal.png")
plot(agent_diefstal, type="p", xlab="Agent", ylab="Diefstal", main="Scatterplot Agent en Diefstal")
dev.off()
# Volgend command werkte niet, de uitschieter wordt dus manueel verwijderd
#politie_uitschieters<-identify(agent_diefstal, n=1)
agent_diefstal_zonder_uitschieter = agent_diefstal[-14,]
agent_zu = agent_diefstal_zonder_uitschieter$politie.agent
diefstal_zu = agent_diefstal_zonder_uitschieter$politie.diefstal
agent_normaliteit_zu = shapiro.test(agent_zu)
diefstal_normaliteit_zu = shapiro.test(diefstal_zu)
agent_diefstal_corr_zu = cor.test(agent_zu, diefstal_zu, method="pearson")
png("Res3/ScatterplotAgentDiefstalZonderUitschieter.png")
plot(agent_diefstal, type="p", xlab="Agent", ylab="Diefstal", main="Scatterplot Agent en Diefstal")
dev.off()