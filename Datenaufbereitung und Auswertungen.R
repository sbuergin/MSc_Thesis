###++++++++++++++++++++++++++++++++++++###
###                                    ###
###                                    ###
###             MSC THESIS             ###
###            Steven Bürgin           ###
###                                    ###
###         Uni Basel / FS 2019        ###
###++++++++++++++++++++++++++++++++++++###



#### R vorbereiten ####
if (!require(foreign)) install.packages('foreign')
library(foreign)
if (!require(psych)) install.packages('psych')
library(psych)
if (!require(Hmisc)) install.packages('Hmisc')
library(Hmisc)
if (!require(data.table)) install.packages('data.table')
library(data.table)
if (!require(corrplot)) install.packages('corrplot')
library(corrplot)
if (!require(gvlma)) install.packages('gvlma')
library(gvlma)
if (!require(apa)) install.packages('apa')
library(apa)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(stringr)) install.packages('stringr')
library(stringr)



#### Daten einlesen ####
data_raw = read.spss("181001_msc thesis_rohdaten.sav", 
                     to.data.frame=TRUE, stringsAsFactors=FALSE, use.value.labels = FALSE)
names(data_raw)



#### unnötige Variablen löschen ####
data_raw <- data_raw[, -c(65:94)] # Spalten 67 - 94
data_raw$lfdn <- NULL
data_raw$external_lfdn <- NULL
data_raw$tester <- NULL
data_raw$dispcode <- NULL
data_raw$lastpage <- NULL
data_raw$quality <- NULL
data_raw$duration <- NULL
data_raw$browser <- NULL



#### neue Variablen Textblöcke gesamt (Org A und B) berechnen ####
# Effektivität
data_raw$eff_gesamt <- ((data_raw$eff_a_out - data_raw$eff_a) + (data_raw$eff_b_out - data_raw$eff_b))

# Allgemein
data_raw$allg_gesamt <- ((data_raw$allg_a_out - data_raw$allg_a) + (data_raw$allg_b_out - data_raw$allg_b))

# Aktivitäten
data_raw$akt_gesamt <- ((data_raw$akt_a_out - data_raw$akt_a) + (data_raw$akt_b_out - data_raw$akt_b))



#### neue Variablen Textblöcke Organisationen einzeln (A und B separat) berechnen ####
## A
# Allgemein
data_raw$allg_a_gesamt <- (data_raw$allg_a_out - data_raw$allg_a)
# Aktivitäten
data_raw$akt_a_gesamt <- (data_raw$akt_a_out - data_raw$akt_a)
# Effektivität
data_raw$eff_a_gesamt <- (data_raw$eff_a_out - data_raw$eff_a)

## B
# Allgemein
data_raw$allg_b_gesamt <- (data_raw$allg_b_out - data_raw$allg_b)
# Aktivitäten
data_raw$akt_b_gesamt <- (data_raw$akt_b_out - data_raw$akt_b)
# Effektivität
data_raw$eff_b_gesamt <- (data_raw$eff_b_out - data_raw$eff_b)



#### Neue Variablen Skalen-Scores (mean) ####
data_raw$sas_score_mean <- rowMeans(data_raw[,8:27], na.rm=TRUE) # ACHTUNG Nummern checken bei erneutem Durchgang!
data_raw$iri_score_mean <- rowMeans(data_raw[,28:31], na.rm=TRUE)
data_raw$ous_score_mean <- rowMeans(data_raw[,32:36], na.rm=TRUE)



#### Items benennen wie in Bericht ####
# ("v_" loswerden, "SPF" in "iri", "OUSIB" in ous)
data_raw <- rename_at(data_raw, vars(starts_with("v_")), funs(str_remove(., "v_")))
data_raw <- rename_all(data_raw, funs(str_replace(.,"spf", "iri")))
data_raw <- rename_all(data_raw, funs(str_replace(.,"ousib", "ous")))



#### verschiedene Variablen bearbeiten für Auswertungen ####
# c_zufallsgruppe = 2 = moral nudge !!! (1=Kontrollgruppe)
is.factor(data_raw$c_zufallsgruppe) # is.factor(data_raw$v_spendeorg) contrasts(data_raw$v_spendeorg)

data_raw$c_zufallsgruppe <- as.factor(data_raw$c_zufallsgruppe)
data_raw$spendeorg <- as.factor(data_raw$spendeorg)

contrasts(data_raw$c_zufallsgruppe) # Kontrollgruppe = Referenzgruppe

data$age <- as.numeric(data$age)
data$spendeorg <- as.numeric(data$spendeorg)

data$spendeorg2 <- data$spendeorg
data$spendeorg2[data$spendeorg2 == 1] <- 0
data$spendeorg2[data$spendeorg2 == 2] <- 1
data$spendeorg2[data$spendeorg2 == 3] <- NA



#### Dataframe kopieren für Analysen ####
data <- data_raw
View(data)



############################ ENDE VORBEREITUNG ############################
# Variablen auflisten
# c_zufallsgruppe = 2 = moral nudge !!! (1 = Kontrollgruppe) 
# siehe auch Codebuch für Kodierungen aller Variablen
names(data)
# Struktur des Dataframe
str(data) 



#### Exp. oder Kontrollgruppe? ####
summary(data$c_zufallsgruppe) # 1 =Kontrollgruppe / 2=Versuchsgruppe
table(data$c_zufallsgruppe)



#### Stichprobe beschreiben ####
data$age <- as.numeric(data$age)
summary(data[c("sex","age")])
table(data$sex)

sd(data$age)

table(data$beruf_student)



#### Textblöcke Spendenaufgabe beschreiben ####
summary(data[c("eff_gesamt", "allg_gesamt", "akt_gesamt", "allg_a_gesamt", "akt_a_gesamt",
               "eff_a_gesamt", "allg_b_gesamt", "akt_b_gesamt", "eff_b_gesamt")])



#### WELCHES ALTRUISMUS MASS SOLL VERWENDET WERDEN? ####
#### Korrelation Mean Scores Altruismus Skalen ####
# normalverteilte var?
#  SAS -> 0.003 -> nicht normalverteilt, also pearson eig. nicht erlaubt
shapiro.test(data$iri_score_mean)
shapiro.test(data$ous_score_mean) 
shapiro.test(data$sas_score_mean) 

# Korrelationstests 
#// oder wäre hier KS-Test nötig gewesen?
cor_sasiri <- cor.test(data$sas_score_mean,data$iri_score_mean,method="spearman")
cor_sasous <- cor.test(data$sas_score_mean,data$ous_score_mean,method="spearman")
cor_irious <- cor.test(data$iri_score_mean,data$ous_score_mean,method="pearson")

apa(cor_sasiri)
apa(cor_sasous)
apa(cor_irious)

# Histogramme
# sas
multi.hist(data[,8:27])
# iri
multi.hist(data[,28:31])
# ous
multi.hist(data[,32:36])



#### Neues Altruismus Mass (impartial beneficence(ous) + empathic concern(iri)) ####
data$altruismus_score <- data$iri_score_mean + data$ous_score_mean



#### Korrelationsmatrix zw. einzelnen Items ALLER Altruismus Skalen ####
cor_obj_alle <-cor(data[,8:36], use = "complete.obs")
round(cor_obj_alle, 2)

cor_obj1 <- rcorr(as.matrix(data[,8:36]))
# Extract the correlation coefficients
cor_obj1$r
# Extract p-values
cor_obj1$P



#### Korrelogram ALLER Altruismus Skalen ####
corrplot(cor_obj_alle, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
## ACHTUNG : 
## im Falle eines Bugs ggfls. nötig vor erneuter Durchführung von corrplot(): plot.new() oder dev.off() 
# Insignifikante Korrelationendurchgekreuzt
corrplot(cor_obj1$r, type="upper", order="original", 
         p.mat = cor_obj1$P, sig.level = 0.05, insig = "pch", tl.col = "black") ## insig="n" für ALLE Korrelationen (auch nicht signifikant)



# nur IRI und OUS
#### Korrelationsmatrix zw. einzelnen Items Altruismus Skalen / NUR IRI UND OUS ####
cor_obj_ousiri <-cor(data[,28:36], use = "complete.obs")
round(cor_obj_ousiri, 2)

cor_obj2 <- rcorr(as.matrix(data[,28:36]))
# Extract the correlation coefficients
cor_obj2$r
# Extract p-values
cor_obj2$P



#### Korrelogram Altruismus Skalen / NUR IRI UND OUS ####
corrplot(cor_obj_ousiri, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
## ACHTUNG : 
## im Falle eines Bugs ggfls. nötig vor erneuter Durchführung von corrplot(): plot.new() oder dev.off() 
# Insignifikante Korrelationen durchgekreuzt
corrplot(cor_obj2$r, type="upper", order="original", 
         p.mat = cor_obj2$P, sig.level = 0.05, insig = "p-value", tl.col = "black")



#### KorrelationsMATRIX aller Altruismus Skalen ####
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(cor_obj1$r, cor_obj1$p)



#### Wieviele haben überhaupt gespendet? ####
summary(data$spendeorg)



#### Spenden vs. erarbeiteter Betrag ####
summary(data$spendebetrag)
sd(data$spendebetrag)
summary(data$spendebetrag[data$spendeorg!=3])
sd(data$spendebetrag[data$spendeorg!=3])
summary(data$realeffort)
sd(data$realeffort)

sum(data$realeffort)
sum(data$spendebetrag)

1 - (sum(data$realeffort[data$spendeorg!=3]) - sum(data$spendebetrag)) / sum(data$realeffort[data$spendeorg!=3])

1 - (sum(data$realeffort) - sum(data$spendebetrag)) / sum(data$realeffort) # 95.6% wurden gespendet



#### Hypothese 1 ####
# Hypothese 1, wonach Probanden beider Gruppen Informationen zur Effektivität länger betrachten je altruistischer sie sind
# und Hypothese 2 wonach Probanden beider Gruppen eher an die effektivere NGO spenden je altruistischer sie sind werden
# jeweils mit einer Korrelation und einer einfachen Re-gression getestet. 
# Korrelation
hyp1_cor <- cor.test(data$altruismus_score,data$eff_gesamt,method="pearson") ## 1 NA bei Textblock Mass
cor_apa(hyp1_cor)

# Einfache Regression
plot(data$altruismus_score,data$eff_gesamt, pch = 19)
reg1 <- lm(data$eff_gesamt ~ data$altruismus_score) ## 1 NA bei Textblock Mass
abline(reg1, col = "red")
summary(reg1)
apa(reg1)



#### Hypothese 2 ####
# Korrelation
hyp2_cor <- cor.test(data$altruismus_score[data$spendeorg != 3],data$spendeorg[data$spendeorg != 3],method="pearson") # 1 = effektivere NGO, 2 = weniger effektive NGO
cor_apa(hyp2_cor)

# Logistische Regression
#plot(data$altruismus_score,data$spendeorg, pch = 19)
#reg2 <- glm(spendeorg2 ~ altruismus_score,family=binomial(link='logit'),data=data)
#abline(reg2, col = "red")
#summary(reg2)



#### Hypothese 3 ####
# Hypothese 3, wonach Probanden der Experimentalgruppe Informationen zur Effektivität länger betrachten als die 
# Probanden der Kontrollgruppe und Hypothese 4 wonach Probanden der Experimentalgruppe häufiger an die effektivere NGO
# spenden als die Probanden der Kontrollgruppe werden jeweils mit einem t-Test getestet. 
shapiro.test(data$eff_gesamt[data$c_zufallsgruppe==1]) # 1 =Kontrollgruppe / 2=Versuchsgruppe
shapiro.test(data$eff_gesamt[data$c_zufallsgruppe==2]) ## 1 NA bei Textblock Mass
# sind normalverteilt
hyp3 <- t.test(x = data$eff_gesamt[data$c_zufallsgruppe == 2], y = data$eff_gesamt[data$c_zufallsgruppe == 1], alternative = "greater", var.equal = TRUE)
t_apa(hyp3)



#### Hypothese 4 ####
hyp4 <- t.test(x = data$spendeorg2[data$c_zufallsgruppe == 2], y = data$spendeorg2[data$c_zufallsgruppe == 1], alternative = "greater", var.equal = TRUE)
t_apa(hyp4)



#### Hypothese 5 ####
# Hypothese 5 wonach Probanden in der Experimentalgruppe mit niedriger altruistischer Orientierung (warm-glowers) einen
# tieferen Affekt im Vergleich zu den Probanden der Kontrollgruppe mit niedriger altruistischer Orientierung (warm-glowers)
# aufweisen wird mit einer multiplen Regression getestet.
hyp5 <- lm(affekt ~ altruismus_score + c_zufallsgruppe, data = data)
summary(hyp5)



#### Weitere Analysen (ohne hinführende Hypothesen) ####
#### Wurden Textblöcke unterschiedlich lange von Kontroll- und Expgruppe angeschaut? ####
#
summary(data$akt_gesamt[data$c_zufallsgruppe == 1]) # 1 = Kontrollgruppe / 2 = Versuchsgruppe

summary(data$akt_gesamt[data$c_zufallsgruppe == 2])
t.test(x = data$akt_gesamt[data$c_zufallsgruppe == 1], y = data$akt_gesamt[data$c_zufallsgruppe == 2])


#
summary(data$allg_gesamt[data$c_zufallsgruppe == 1])

summary(data$allg_gesamt[data$c_zufallsgruppe == 2])
t.test(x = data$allg_gesamt[data$c_zufallsgruppe == 1], y = data$allg_gesamt[data$c_zufallsgruppe == 2])


#
summary(data$eff_gesamt[data$c_zufallsgruppe == 1])

summary(data$eff_gesamt[data$c_zufallsgruppe == 2])
t.test(x = data$eff_gesamt[data$c_zufallsgruppe == 1], y = data$eff_gesamt[data$c_zufallsgruppe == 2])



#### Weitere Korrelationen / Spendenmenge als Proxy ####
# 
cor_apa(cor.test(data$spendenmenge_letztesjahr,data$altruismus_score,method="pearson"))
# 
cor_apa(cor.test(data$spendenanzahl_letztesjahr,data$altruismus_score,method="pearson"))
# 
cor_apa(cor.test(data$income,data$spendenmenge_letztesjahr,method="pearson"))



#### Wurden die einzelnen Textblöcke insgesamt verschieden lang angeschaut? ####
# Signifikanztests nicht sinnvoll, da die Blöcke verschieden viele Zeichen/Wörter beinhalteten.
summary(data$akt_gesamt) # 849 / 840 Zeichen
summary(data$allg_gesamt) # 375 / 368 Zeichen
summary(data$eff_gesamt) # 675 / 669 Zeichen




