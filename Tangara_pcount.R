library(sp)
library(rgdal)
library(raster)
library(reshape)
library(lattice)
library(Rcpp)
library(unmarked)




##########################

### MODELO DE DENSIDAD ###

##Tangara##

#  Set de datos
dftangara = read.csv("Tangara2017.csv", header = T)
head(dftangara)
str(dftangara)
View(dftangara)
summary(dftangara)

# definir datos para modelo

y = dftangara[,c(5:8)]
y
summary(y)

var.sitio = dftangara[,c("Habitat", "DAP", "Altura","Riqueza","Abundancia","Diversidad", "CordiaS", "CordiaN", "MiconiaS", "MiconiaN","FicusS", "FicusN", "PalicoureaS", "PalicoureaN")]



#### MODELOS DE CONTEO COMBINANDO VARIABLES AMBIENTALES  ###

de <- unmarkedFramePCount(y=y, siteCovs = var.sitio)

de

mod2 = pcount(~ 1 ~ 1, de, K=50, mixture = "P")
mod2

state_mod2 = backTransform(mod2, 'state')
state_mod2

confint(state_mod2, level=0.95)

det_mod2 = backTransform(mod2,'det')
det_mod2

confint(det_mod2, level=0.90)

#####····

modNB = pcount(~ 1 ~ 1, de, K=50, mixture = "NB")
modNB

state_modNB = backTransform(modNB, 'state')
state_modNB

confint(state_modNB, level=0.90)

det_modNB = backTransform(modNB,'det')
det_modNB

confint(det_modNB, level=0.90)


modZP = pcount(~ 1 ~ 1, de, K=50, mixture = "ZIP")
modZP

state_modZP = backTransform(modZP, 'state')
state_modZP

det_modZP = backTransform(modZP,'det')
det_modZP

confint(state_modZP, level=0.95)

fmList <- fitList(modP=mod2,mpdNB=modNB, modZP=modZP) 

fmList


modSel(fmList)


######### Con k=50############ Recomendado##########

modnull = pcount(~ 1 ~ 1, de, K=50, mixture = "P")
modnull

state_modnull = backTransform(modnull, 'state')
state_modnull


modnull_habit = pcount(~ 1 ~ Habitat, de, K=50, mixture = "P")
modnull_habit


fmList <- fitList(Null=modnull,Null_hab=modnull_habit) 

fmList

predict(fmList, type="state")

modSel(fmList, nullmod="Null")

modhab_null = pcount(~ Habitat ~ 1, de, K=50, mixture = "P")
modhab_habit = pcount(~ Habitat ~ Habitat, de, K=50, mixture = "P")

fmList <- fitList(Null=modnull,Null_hab=modnull_habit, Habit_null=modhab_null, Habi_Habit=modhab_habit) 

fmList

predict(fmList, type="state")

modSel(fmList, nullmod="Null")

Habit = data.frame(Habitat = c("Ripario", "BS", "BM", "BordeB"))
Habit

confint(modnull_habit, type="state")

exp(confint(modnull_habit, type="state", level = 0.90))



############esto para el articulo!!!!


modnull_hab = pcount(~ 1 ~ Habitat, de, K=50, mixture = "P")
modhab_null = pcount(~ Habitat ~ 1, de, K=50, mixture = "P")
modnull_null = pcount(~ 1 ~ 1, de, K=50, mixture = "P")
modhab_hab = pcount(~ Habitat ~ Habitat, de, K=50, mixture = "P")

fl <- fitList(Null=modnull_null, null_hab=modnull_hab, hab_null=modhab_null, hab_hab=modhab_hab) 

fl

ms <- modSel(fl, nullmod="Null")

ms


predict(modnull_hab, type="state")

modSel(fl, nullmod="Null")


state_modhab_hab = backTransform(modhab_hab, 'state')
state_modhab_hab

confint(state_mod2, level=0.95)

det_mod2 = backTransform(mod2,'det')
det_mod2

confint(det_mod2, level=0.90)







