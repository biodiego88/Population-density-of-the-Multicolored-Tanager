# TUTORIAL

vignette("distsamp")

install.packages("unmarked")

install.packages("AICcmodavg")

library(unmarked)

library(AICcmodavg)

# setiamos la carpeta donde se encuenhtran nuestros archivos
setwd("C:/RAnalysis/Unmarked/distsamp/tangara")

# le damos el nombre de "dists" al archivo que contiene nuestros datos
dists <- read.csv("tangara_correcto.csv", header = TRUE, sep=",")

# en algunos tutoriales piden darle el formato de factor a la columna que contiene las etiquetas de nuestras unidades de muestreo 
dists$point <- as.factor(dists$point)


levels(dists$point) <- c(levels(dists$point))
levels(dists$point)

# se convierten los datos de frecuencias de distancias a rangos de distancias, en este caso cada 10 mestros

cp = c(0, 10, 20, 30, 40)
cp

# organizamos el formato de nuestros datos con la funcion formatDistData

yData = formatDistData(dists, "dist", "point", cp)

yData

# importamos los datos de las covariables, de manera que coincidan con la organización de los datos de los conteos en cada unidad de muestreo por cada rango de distancia 

covs <- read.csv("tangaracovs.csv", header = TRUE, sep=",")

# con la funcion unmarkedFrameDS organizamos nuestros datos para correrlos con distamp

umf <- unmarkedFrameDS(y=as.matrix(yData), siteCovs=covs, survey="point", 
                       dist.breaks=c(0, 10, 20, 30, 40), unitsIn="m")

summary(umf)

hist(umf, xlab="distance (m)", main="", cex.lab=0.8, cex.axis=0.8)

# procedemos a ajustar mnuestros datos a los modelos, iniciando con un modelo nulo con las diferentes funciones de distribución halfnormal, hazard, exp y uniforme

hn_Null <- distsamp(~1~1, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_Null <- distsamp(~1~1, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_Null <- distsamp(~1~1, umf, keyfun="exp", output="density", unitsOut="ha")

unf_Null <- distsamp(~1~1, umf, keyfun="uniform", output="density", unitsOut="ha")

# a continuación probamos los modelos en los que la probabilidad de detección sin covariables y la densidad explicada por habitat con cada función de distribución

hn_Nullhab <- distsamp(~1~habitat, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_Nullhab <- distsamp(~1~habitat, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_Nullhab <- distsamp(~1~habitat, umf, keyfun="exp", output="density", unitsOut="ha")

unf_Nullhab <- distsamp(~1~habitat, umf, keyfun="uniform", output="density", unitsOut="ha")

# ajustamos los modelos en los que la probabilidad de detección este explicada por tipo de habitat y la densidad sin covariables (modelo nulo) con cada función de distribución

hn_habNull <- distsamp(~habitat~1, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_habNull <- distsamp(~habitat~1, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_habNull <- distsamp(~habitat~1, umf, keyfun="exp", output="density", unitsOut="ha")

unf_habNull <- distsamp(~habitat~1, umf, keyfun="uniform", output="density", unitsOut="ha")

# ajustamos los modelos en los que la probabilidad de detección y la desnidad esten explicadas por tipo de habitat con cada función de distribución

hn_habhab <- distsamp(~habitat~habitat, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_habhab <- distsamp(~habitat~habitat, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_habhab <- distsamp(~habitat~habitat, umf, keyfun="exp", output="density", unitsOut="ha")

unf_habhab <- distsamp(~habitat~habitat, umf, keyfun="uniform", output="density", unitsOut="ha")

# ajuste y selección del modelo

fmList <- fitList(hn_Null=hn_Null, hz_Null=hz_Null, exp_Null=exp_Null, unf_Null=unf_Null, 
                  hn_Nullhab=hn_Nullhab, hz_Nullhab=hz_Nullhab, exp_Nullhab=exp_Nullhab, 
                  unf_Nullhab=unf_Nullhab, hn_habNull=hn_habNull, hz_habNull=hz_habNull, 
                  exp_habNull=exp_habNull, unf_habNull=unf_habNull, hn_habhab=hn_habhab, 
                  hz_habhab=hz_habhab, exp_habhab=exp_habhab, unf_habhab=unf_habhab)

fmList <- fitList(hn_Null=hn_Null, hz_Null=hz_Null, exp_Null=exp_Null, unf_Null=unf_Null, 
                  hn_Nullhab=hn_Nullhab, hz_Nullhab=hz_Nullhab, exp_Nullhab=exp_Nullhab, 
                  unf_Nullhab=unf_Nullhab)

modSel(fmList, nullmod="hn_Null")


Cand.models <- list(hn_Null, hz_Null, exp_Null, unf_Null, 
                    hn_Nullhab, hz_Nullhab, exp_Nullhab, 
                    unf_Nullhab, hn_habNull, hz_habNull, 
                    exp_habNull, unf_habNull, hn_habhab, 
                    hz_habhab, exp_habhab, unf_habhab)

#nombrar los modelos
Modnames <- c("hn_Null", "hz_Null", "exp_Null", "unf_Null", 
              "hn_Nullhab", "hz_Nullhab", "exp_Nullhab", 
              "unf_Nullhab", "hn_habNull", "hz_habNull", 
              "exp_habNull", "unf_habNull", "hn_habhab", 
              "hz_habhab", "exp_habhab", "unf_habhab")



aictab(cand.set = Cand.models, modnames = Modnames,
       second.ord = TRUE, nobs = NULL, sort = TRUE)

summary(hz_habhab)

# al elegir el mejor modelo, manipulamos los resultados a través de la función Backtransformation para aplicar el antilogaritmo a las estimaciones, debido a que son modelos lineales generalizados de la familia Poisson en escala logaritmica

backTransform(hn_Null, type="state")

backTransform(hn_Null, type="det")

backTransform(hz_Null, type="scale")

# cuando el modelo incluye covariables, se realiza un backtransform a través de la función linearComb. Sin embargo, en este caso no fue posible

backTransform(linearComb(hz_Nullhab['state'], c('BM', 'BS', 'CB', 'R')))

# por lo anterior realizamos la predicción que hace la transformación, con la función predict. Para esto creamos un formato de datos de las variables "nuevo" y luego se calcula con un nivel de 0.90 (IC 95%)

              
nuevo = data.frame(habitat = c("R", "BS", "BM", "CB"))

phz_Nullhab= predict(hz_Nullhab, type = "state", newdata = nuevo, appendData = T, level = 0.90)

# Otra forma de calcular los intervalos de confianza es a través de la función confint. Cuando se realiza sobre el codigo que representa las estimaciones de predicción se hace directo. Si se hace para el modelo sin transformar, se puede realizar un exponencial del confint

confint(phz_Nullhab, type='state', method='normal')

exp(confint(hz_Null, type='state', method='normal', level=0.90))
