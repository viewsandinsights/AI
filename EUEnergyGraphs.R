# R Skript zum Beitrag (PDF File): 
# Uwe Engel: Energiewende im europäischen Green Deal. Bremen, Oktober 2022
# Herunterladbar von https://www.viewsandinsights.com/lehrvideos 


rm(list=ls())
load("Energy.Rdata")
tr <- subset(energy, select = c(Concern, KKS, ppltrst, pplfair, pplhlp, 
                               Povertyrisk, Deprivation, hincfel34, RHEthnicity, RHFEthnicity,
                                parties, justice, administration,
                               government, parliament, public,
                               EU2050, erneuerbare, einsparen, biodivers, kreislauf, plastik,
                               null, solar, wind, atom, ki, bio, vakzin, weltall, nano,
                               EPclimate, EPeconomy, problem   ))
library(psych)
corr.test(tr, use ="pairwise", method = "pearson")


summary(energy$KKS, na.rm = TRUE)
sd(energy$KKS, na.rm = TRUE)

summary(energy$ppltrst, na.rm = TRUE)
sd(energy$ppltrst, na.rm = TRUE)

summary(energy$EU2050, na.rm = TRUE)
sd(energy$EU2050, na.rm = TRUE)

summary(energy$EPclimate, na.rm = TRUE)
sd(energy$EPclimate, na.rm = TRUE)

summary(energy$public, na.rm = TRUE)
sd(energy$public, na.rm = TRUE)

summary(energy$problem, na.rm = TRUE)
sd(energy$problem, na.rm = TRUE)

summary(energy$erneuerbare, na.rm = TRUE)
sd(energy$erneuerbare, na.rm = TRUE)

summary(energy$einsparen, na.rm = TRUE)
sd(energy$einsparen, na.rm = TRUE)

summary(energy$kreislauf, na.rm = TRUE)
sd(energy$kreislauf, na.rm = TRUE)

summary(energy$solar, na.rm = TRUE)
sd(energy$solar, na.rm = TRUE)

summary(energy$wind, na.rm = TRUE)
sd(energy$wind, na.rm = TRUE)

summary(energy$atom, na.rm = TRUE)
sd(energy$atom, na.rm = TRUE)

summary(energy$ki, na.rm = TRUE)
sd(energy$ki, na.rm = TRUE)

summary(energy$biodivers, na.rm = TRUE)
sd(energy$biodivers, na.rm = TRUE)

summary(energy$plastik, na.rm = TRUE)
sd(energy$plastik, na.rm = TRUE)

summary(energy$null, na.rm = TRUE)
sd(energy$null, na.rm = TRUE)

summary(energy$bio, na.rm = TRUE)
sd(energy$bio, na.rm = TRUE)

summary(energy$vakzin, na.rm = TRUE)
sd(energy$vakzin, na.rm = TRUE)

summary(energy$weltall, na.rm = TRUE)
sd(energy$weltall, na.rm = TRUE)

summary(energy$nano, na.rm = TRUE)
sd(energy$nano, na.rm = TRUE)

summary(energy$EPeconomy, na.rm = TRUE)
sd(energy$EPeconomy, na.rm = TRUE)

summary(energy$parties, na.rm = TRUE)
sd(energy$parties, na.rm = TRUE)

summary(energy$justice, na.rm = TRUE)
sd(energy$justice, na.rm = TRUE)

summary(energy$administration, na.rm = TRUE)
sd(energy$administration, na.rm = TRUE)

summary(energy$government, na.rm = TRUE)
sd(energy$government, na.rm = TRUE)

summary(energy$parliament, na.rm = TRUE)
sd(energy$parliament, na.rm = TRUE)

summary(energy$pplfair, na.rm = TRUE)
sd(energy$pplfair, na.rm = TRUE)

summary(energy$pplhlp, na.rm = TRUE)
sd(energy$pplhlp, na.rm = TRUE)

summary(energy$Povertyrisk, na.rm = TRUE)
sd(energy$Povertyrisk, na.rm = TRUE)

summary(energy$Deprivation, na.rm = TRUE)
sd(energy$Deprivation, na.rm = TRUE)

summary(energy$hincfel34, na.rm = TRUE)
sd(energy$hincfel34, na.rm = TRUE)

summary(energy$Concern, na.rm = TRUE)
sd(energy$Concern, na.rm = TRUE)

# ==============================================================

rm(list=ls())
load("Energy.Rdata")
tr <- subset(energy, select = c(EU2050, erneuerbare, einsparen, biodivers, kreislauf, plastik,
                                null ))
library(psych)

corr.test(tr, use ="pairwise", method = "pearson")


EU50    <- c( 1.00,  0.12,  -0.03,  -0.05,  0.06,  0.03,  0.14)
neu     <- c( 0.12,  1.00,   0.08,   0.35,  0.12,  0.17, -0.01)
spar    <- c(-0.03,  0.08,   1.00,  -0.45, -0.51,  0.13,  0.50)
divers  <- c(-0.05,  0.35,  -0.45,   1.00,  0.25, -0.18, -0.29)
kreisl  <- c( 0.06,  0.12,  -0.51,   0.25,  1.00, -0.04, -0.48)
plast   <- c( 0.03,  0.17,   0.13,  -0.18, -0.04,  1.00,  0.17)
null    <- c( 0.14, -0.01,   0.50,  -0.29, -0.48,  0.17,  1.00)

as.matrix(greendeal <- cbind(EU50, neu, spar, divers, kreisl,  
                              plast, null ))
greendeal
is.matrix(greendeal)
library(GGally)
jpeg("greendeal300.jpg", units="in", width=9, height = 3.5, res=300 )

ggcorr(data=NULL, method="everything", cor_matrix = greendeal,
       label = TRUE, label_round = 2, limits = FALSE, 
       midpoint = NULL, name = "Korrelation") +
  labs(title = "Im Green Deal sollte oberste Priorität haben")
dev.off()

# ===============================================================

rm(list=ls())
load("Energy.Rdata")
tr <- subset(energy, select = c(ppltrst, pplfair, pplhlp, 
                                parties, justice, administration,
                                government, parliament))
library(psych)
corr.test(tr, use ="pairwise", method = "pearson")

trst <- c( 1.00, 0.90, 0.88, 0.59, 0.90, 0.80, 0.69, 0.75)
fair <- c( 0.90, 1.00, 0.89, 0.56, 0.90, 0.80, 0.68, 0.73)
hlp  <- c( 0.88, 0.89, 1.00, 0.53, 0.82, 0.75, 0.62, 0.66)
part <- c( 0.59, 0.56, 0.53, 1.00, 0.69, 0.73, 0.83, 0.88)
just <- c( 0.90, 0.90, 0.82, 0.69, 1.00, 0.84, 0.82, 0.87)
admi <- c( 0.80, 0.80, 0.75, 0.73, 0.84, 1.00, 0.87, 0.83)
gove <- c( 0.69, 0.68, 0.62, 0.83, 0.82, 0.87, 1.00, 0.91)
parl <- c( 0.75, 0.73, 0.66, 0.88, 0.87, 0.83, 0.91, 1.00)

as.matrix(trust <- cbind(trst, fair, hlp, part, just, admi, gove, parl ))  
trust
is.matrix(trust)
library(GGally)
jpeg("trust300(1).jpg", units="in", width=9, height = 3.5, res=300 )

ggcorr(data=NULL, method="everything", cor_matrix = trust,
       label = TRUE, label_round = 2, limits = FALSE, 
       midpoint = NULL, name = "Korrelation") +
  labs(title = "Vertrauen in Mitmenschen und Institutionen")
dev.off()

# ===============================================================
 
rm(list=ls())
load("Energy.Rdata")
tr <- subset(energy, select = c(KKS, Povertyrisk, Deprivation, hincfel34,
                                problem, EU2050, EPclimate, EPeconomy, public))
library(psych)

corr.test(tr, use ="pairwise", method = "pearson")

KKS  <- c( 1.00, -0.34, -0.50, -0.59,  0.61,  0.39,  0.69, -0.47,  0.52)
Pov  <- c(-0.34,  1.00,  0.82,  0.64, -0.46, -0.25, -0.54,  0.69, -0.34)
Dep  <- c(-0.50,  0.82,  1.00,  0.80, -0.55, -0.32, -0.65,  0.67, -0.39)
Hinc <- c(-0.59,  0.64,  0.80,  1.00, -0.64, -0.46, -0.69,  0.54, -0.53)
Prob <- c( 0.61, -0.46, -0.55, -0.64,  1.00,  0.59,  0.95, -0.60,  0.77)
EU50 <- c( 0.39, -0.25, -0.32, -0.46,  0.59,  1.00,  0.64, -0.49,  0.36)
EPcl <- c( 0.69, -0.54, -0.65, -0.69,  0.95,  0.64,  1.00, -0.69,  0.71)
EPec <- c(-0.47,  0.69,  0.67,  0.54, -0.60, -0.49, -0.69,  1.00, -0.41)
Pub  <- c( 0.52, -0.34, -0.39, -0.53,  0.77,  0.36,  0.71, -0.41,  1.00)

as.matrix(oeko <- cbind(KKS, Pov, Dep, Hinc, Prob, EU50, EPcl, EPec, Pub ))  
oeko
is.matrix(oeko)
library(GGally)
jpeg("oeko300.jpg", units="in", width=11, height = 5, res=300 )

ggcorr(data=NULL, method="everything", cor_matrix = oeko,
       label = TRUE, label_round = 2, limits = FALSE, 
       midpoint = NULL, name = "Korrelation") +
  labs(title = "Wohlstand, ökonomische Deprivation,\nPräferenz für politische Intervention")
dev.off()

# ==================================================================

rm(list=ls())
load("Energy.Rdata")
tr <- subset(energy, select = c(solar, wind, atom, ki, bio, vakzin, weltall, nano ))
library(psych)

corr.test(tr, use ="pairwise", method = "pearson")

# -------------------------------

rm(list=ls())
#Correlation matrix 
solar <-  c( 1.00, 0.82, -0.14, 0.60, 0.45, 0.67, 0.27, 0.59)
wind  <-  c( 0.82, 1.00,  0.04, 0.59, 0.48, 0.52, 0.41, 0.50)
atom  <-  c(-0.14, 0.04,  1.00, 0.35, 0.34, 0.10, 0.49, 0.33)
ki    <-  c( 0.60, 0.59,  0.35, 1.00, 0.66, 0.63, 0.49, 0.72)
bio   <-  c( 0.45, 0.48,  0.34, 0.66, 1.00, 0.55, 0.56, 0.75)
vak   <-  c( 0.67, 0.52,  0.10, 0.63, 0.55, 1.00, 0.43, 0.76)
all   <-  c( 0.27, 0.41,  0.49, 0.49, 0.56, 0.43, 1.00, 0.66)
nano  <-  c( 0.59, 0.50,  0.33, 0.72, 0.75, 0.76, 0.66, 1.00)

as.matrix(techmatrix <- cbind(solar, wind, atom, ki, bio, vak, 
                            all, nano ))
techmatrix
is.matrix(techmatrix)
library(GGally)
jpeg("tech300-2.jpg", units="in", width=9, height = 3.5, res=300 )

ggcorr(data=NULL, method="everything", cor_matrix = techmatrix,
       label = TRUE, label_round = 2, limits = FALSE, 
       midpoint = NULL, name = "Korrelation") +
labs(title = "Technologiewahrnehmung")
dev.off()

# ===========================================================

rm(list=ls())
library(ggplot2)
load("Energy.Rdata")
tr <- subset(energy, select = c(Country.x, solar))
jpeg("solar300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(solar, reorder(Country.x, solar))) + geom_point() +
  ylab("EU Land") +
  xlab("Mittelwerte auf Skala: sehr negative (-2), eher negative (-1),\nkeine (0), eher positive (+1), sehr positive (+2)") +
  labs(title = "Solarenergie:\nErwartete Auswirkungen darauf, wie wir künftig leben") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) + 
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()  

# ------- next

rm(list=ls())
library(ggplot2)
load("Energy.Rdata")
tr <- subset(energy, select = c(Country.x, wind))
jpeg("wind300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(wind, reorder(Country.x, wind))) + geom_point() +
  ylab("EU Land") +
  xlab("Mittelwerte auf Skala: sehr negative (-2), eher negative (-1),\nkeine (0), eher positive (+1), sehr positive (+2)") +
  labs(title = "Windenergie:\nErwartete Auswirkungen darauf, wie wir künftig leben") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) + 
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()  

# ------- next

rm(list=ls())
library(ggplot2)
load("Energy.Rdata")
tr <- subset(energy, select = c(Country.x, atom))
jpeg("atom300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(atom, reorder(Country.x, atom))) + geom_point() +
  ylab("EU Land") +
  xlab("Mittelwerte auf Skala: sehr negative (-2), eher negative (-1),\nkeine (0), eher positive (+1), sehr positive (+2)") +
  labs(title = "Atomenergie zur Energieproduktion:\nErwartete Auswirkungen darauf, wie wir künftig leben") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) + 
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()  

# ------- next

rm(list=ls())
library(ggplot2)
load("Energy.Rdata")
tr <- subset(energy, select = c(Country.x, ki))
jpeg("ki300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(ki, reorder(Country.x, ki))) + geom_point() +
  ylab("EU Land") +
  xlab("Mittelwerte auf Skala: sehr negative (-2), eher negative (-1),\nkeine (0), eher positive (+1), sehr positive (+2)") +
  labs(title = "Künstliche Intelligenz:\nErwartete Auswirkungen darauf, wie wir künftig leben") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) + 
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()  

# ===========================================================

rm(list=ls())
load("Energy.Rdata")
library(ggplot2)
tr <- subset(energy, select = c(Country.x, KKS))
jpeg("kks300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(KKS, reorder(Country.x, KKS))) + geom_point() +
ylab("EU Land") +
  xlab("Kaufkraftstandard") +
  labs(title = "Wohlstand") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()

#  ----- next 

rm(list=ls())
load("Energy.Rdata")
library(ggplot2)
tr <- subset(energy, select = c(Country.x, EPclimate))
tr <- na.omit(tr)
jpeg("EPclimate300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(EPclimate, reorder(Country.x, EPclimate))) + geom_point() +
  ylab("EU Land") +
  xlab("... Aktion gegen Klimawandel behandeln") +
  labs(title = "Prozentsatz: EU-Parlament soll vorrangig ...") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()

# ------ next

rm(list=ls())
load("Energy.Rdata")
library(ggplot2)
tr <- subset(energy, select = c(Country.x, EU2050))
tr <- na.omit(tr)
jpeg("EU2050-300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(EU2050, reorder(Country.x, EU2050))) + geom_point() +
  ylab("EU Land") +
  xlab("Prozentsatz: EU bis 2050 klimaneutral machen") +
  labs(title = "Präferenz für europäischen Green Deal") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()

# ------ next

rm(list=ls())
load("Energy.Rdata")
library(ggplot2)
tr <- subset(energy, select = c(Country.x, public))
tr <- na.omit(tr)
jpeg("public300.jpg", units="in", width=6, height = 9, res=300 )
ggplot (tr, aes(public, reorder(Country.x, public))) + geom_point() +
  ylab("EU Land") +
  xlab("... bedarf es des Eingreifens der Politik") +
  labs(title = "Prozentsatz: Damit KI-Anwendungen\nin ethischer Art und Weise entwickelt werden, ...") +
  theme(legend.title = element_text())+
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11)) 
dev.off()

# ===================================================================
rm(list=ls())
load("Energy.Rdata")

# KKS recode 51-78 =1)(81-104 = 2)(111-261=3)
energy$KKS3 <- ifelse(energy$KKS <= 78, "[1] unteren Drittel",
                      ifelse(energy$KKS <= 104, "[2] mittleren Drittel",
                             "[3] oberen Drittel"))
addmargins(table(energy$KKS3))

library(ggplot2)
tr <- subset(energy, select = c(Country.x, KKS3, public, EPclimate))
tr <- na.omit(tr)
jpeg("intervention600.jpg", units="in", width=9, height = 6, res=600 )
ggplot (data = tr) +
  geom_point(mapping = aes(x=EPclimate, y=public, color = KKS3), 
             size = 3.5) +
  ylab("Prozentsatz: KI bedarf des Eingreifens der Politik") +
  xlab("Prozentsatz: EU-Parlament soll vorrangig\nAktion gegen Klimawandel behandeln") +
  labs(caption = "Berechnungen auf Basis von Eurostat- und Eurobarometer-Daten", 
       color = "Kaufkraftstandard\ndes EU-Landes liegt im",
       title = "Wohlstand und Präferenz für staatliche Intervention bei KI und Energiewende")+
  theme(legend.title = element_text(size= 16)) +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.title = element_text(size = 16)) +
  theme(plot.caption = element_text(size=14)) + 
  theme(plot.title = element_text(size=16)) 
dev.off()

# ==================== next

rm(list=ls())
load("Energy.Rdata")

# KKS recode 51-78 =1)(81-104 = 2)(111-261=3)
energy$KKS3 <- ifelse(energy$KKS <= 78, "[1] unteren Drittel",
                      ifelse(energy$KKS <= 104, "[2] mittleren Drittel",
                             "[3] oberen Drittel"))
addmargins(table(energy$KKS3))

library(ggplot2)
tr <- subset(energy, select = c(Country.x, KKS3, ppltrst, EPclimate))
tr <- na.omit(tr)
jpeg("confidence600-2.jpg", units="in", width=9, height = 6, res=600 )
ggplot (data = tr) +
  geom_point(mapping = aes(x=ppltrst, y=EPclimate, color = KKS3), 
             size = 3.5) +
  xlab("Mittleres Vertrauen in Mitmenschen") +
  ylab("Prozentsatz: EU-Parlament soll vorrangig\nAktion gegen Klimawandel behandeln") +
  labs(caption = "Berechnungen auf Basis von Eurostat-, ESS- und Eurobarometer-Daten", 
       color = "Kaufkraftstandard\ndes EU-Landes liegt im",
       title = "Wohlstand, Vertrauen und Präferenz für EU-Intervention bei Energiewende")+
  theme(legend.title = element_text(size= 16)) +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.title = element_text(size = 16)) +
  theme(plot.caption = element_text(size=14)) + 
  theme(plot.title = element_text(size=16)) 
dev.off()

# ==================== next

rm(list=ls())
load("Energy.Rdata")

# KKS recode 51-78 =1)(81-104 = 2)(111-261=3)
energy$KKS3 <- ifelse(energy$KKS <= 78, "[1] unteren Drittel",
                      ifelse(energy$KKS <= 104, "[2] mittleren Drittel",
                             "[3] oberen Drittel"))
addmargins(table(energy$KKS3))

library(ggplot2)
tr <- subset(energy, select = c(Country.x, KKS3, ppltrst, EU2050))
tr <- na.omit(tr)
jpeg("greendeal600-2.jpg", units="in", width=9, height = 6, res=600 )
ggplot (data = tr) +
  geom_point(mapping = aes(x=ppltrst, y=EU2050, color = KKS3), 
             size = 3.5) +
  xlab("Mittleres Vertrauen in Mitmenschen") +
  ylab("Prozentsatz: EU bis 2050 klimaneutral machen") +
  labs(caption = "Berechnungen auf Basis von Eurostat-, ESS- und Eurobarometer-Daten", 
       color = "Kaufkraftstandard\ndes EU-Landes liegt im",
       title = "Wohlstand, Vertrauen und Präferenz für europäischen Green Deal")+
  theme(legend.title = element_text(size= 16)) +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.title = element_text(size = 16)) +
  theme(plot.caption = element_text(size=14)) + 
  theme(plot.title = element_text(size=16)) 
dev.off()

# ============================================================================

rm(list=ls())
load("Energy.Rdata")
tr <- subset(energy, select = c(KKS, ppltrst, EPclimate, solar))
tr <- na.omit(tr)

library(lavaan) 
energy.model <- '
EPclimate ~ ppltrst + KKS + solar

' 
# Input raw data
energy.fit <- sem(energy.model, data=tr,                   
                 meanstructure = TRUE)   
summary(energy.fit, header = TRUE, fit.measures = TRUE,
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)

library(lavaanPlot)
library(DiagrammeR)
labeling <- list(KKS = "Wohlstand des\nEU-Landes\n\nIndexwert:\nKaufkraftstandard",
                 ppltrst = "Soziale Integration des\nEU-Landes\n\nMittleres Vertrauen\nin Mitmenschen", 
                 solar = "Technologieentwicklung: Solarenergie\n\nÜbergewicht positiver gegenüber negativer\nEinschätzungen der Auswirkungen dieser\nTechnologie auf die künftige Lebensweise",
                 EPclimate = "Politische Intervention\n\nProzentsatz für\nEuropäisches Parlament\nsollte vorrangig behandeln:\nAktion gegen Klimawandel")
plot1 <- lavaanPlot::lavaanPlot(name = "P1",
                                 model = energy.fit,
                                 coefs = TRUE, stand=TRUE,
                                labels = labeling, covs = TRUE,
                                stars = "regress",
                                node_options = list(shape= "box"),
                                edge_options = list(color="black"))
print(plot1)

 
rm(list=ls())
load("Energy.Rdata")
tr <- subset(energy, select = c(KKS, ppltrst, EU2050, solar))
tr <- na.omit(tr)

library(lavaan) 
energy.model <- '
EU2050 ~ ppltrst + KKS + solar

' 
# Input raw data
energy.fit <- sem(energy.model, data=tr,                   
                  meanstructure = TRUE)   
summary(energy.fit, header = TRUE, fit.measures = TRUE,
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)

library(lavaanPlot)
library(DiagrammeR)
labeling <- list(KKS = "Wohlstand des\nEU-Landes\n\nIndexwert:\nKaufkraftstandard",
                 ppltrst = "Soziale Integration des\nEU-Landes\n\nMittleres Vertrauen\nin Mitmenschen", 
                 solar = "Technologieentwicklung: Solarenergie\n\nÜbergewicht positiver gegenüber negativer\nEinschätzungen der Auswirkungen dieser\nTechnologie auf die künftige Lebensweise",
                 EU2050 = "Europäischer Green Deal\n\nProzentsatz für\nEU bis 2050 klimaneutral\nmachen")
plot2 <- lavaanPlot::lavaanPlot(name = "P1",
                                model = energy.fit,
                                coefs = TRUE, stand=TRUE,
                                labels = labeling, covs = TRUE,
                                stars = "regress",
                                node_options = list(shape= "box"),
                                edge_options = list(color="black"))
print(plot2)


rm(list=ls())
load("Energy.Rdata")
#table(energy$EU2050)
tr <- subset(energy, select = c(KKS, ppltrst, EPclimate, wind))
tr <- na.omit(tr)

library(lavaan) 
energy.model <- '
EPclimate ~ ppltrst + KKS + wind

' 
# Input raw data
energy.fit <- sem(energy.model, data=tr,                   
                  meanstructure = TRUE)   
summary(energy.fit, header = TRUE, fit.measures = TRUE,
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)

library(lavaanPlot)
library(DiagrammeR)
labeling <- list(KKS = "Wohlstand des\nEU-Landes\n\nIndexwert:\nKaufkraftstandard",
                 ppltrst = "Soziale Integration des\nEU-Landes\n\nMittleres Vertrauen\nin Mitmenschen", 
                 wind = "Technologieentwicklung: Windenergie\n\nÜbergewicht positiver gegenüber negativer\nEinschätzungen der Auswirkungen dieser\nTechnologie auf die künftige Lebensweise",
                 EPclimate = "Politische Intervention\n\nProzentsatz für\nEuropäisches Parlament\nsollte vorrangig behandeln:\nAktion gegen Klimawandel")
plot3 <- lavaanPlot::lavaanPlot(name = "P1",
                                model = energy.fit,
                                coefs = TRUE, stand=TRUE,
                                labels = labeling, covs = TRUE,
                                stars = "regress",
                                node_options = list(shape= "box"),
                                edge_options = list(color="black"))
print(plot3)


rm(list=ls())
load("Energy.Rdata")
#table(energy$EU2050)
tr <- subset(energy, select = c(KKS, ppltrst, EU2050, wind))
tr <- na.omit(tr)

library(lavaan) 
energy.model <- '
EU2050 ~ ppltrst + KKS + wind

' 
# Input raw data
energy.fit <- sem(energy.model, data=tr,                   
                  meanstructure = TRUE)   
summary(energy.fit, header = TRUE, fit.measures = TRUE,
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)

library(lavaanPlot)
library(DiagrammeR)
labeling <- list(KKS = "Wohlstand des\nEU-Landes\n\nIndexwert:\nKaufkraftstandard",
                 ppltrst = "Soziale Integration des\nEU-Landes\n\nMittleres Vertrauen\nin Mitmenschen", 
                 wind = "Technologieentwicklung: Windenergie\n\nÜbergewicht positiver gegenüber negativer\nEinschätzungen der Auswirkungen dieser\nTechnologie auf die künftige Lebensweise",
                 EU2050 = "Europäischer Green Deal\n\nProzentsatz für\nEU bis 2050 klimaneutral\nmachen")
plot4 <- lavaanPlot::lavaanPlot(name = "P1",
                                model = energy.fit,
                                coefs = TRUE, stand=TRUE,
                                labels = labeling, covs = TRUE,
                                stars = "regress",
                                node_options = list(shape= "box"),
                                edge_options = list(color="black"))
print(plot4)
