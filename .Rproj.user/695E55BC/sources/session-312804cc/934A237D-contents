library(terra)
library(biomod2)
library(openxlsx)

#Tengo datos de ocurrencia y variables explicatorias de clima.
#Ojo Stephi tiene parámetros modificables al inicio, considerar.

nombre_especie <- "zorro_culpeo"
nombre_corrida <- "EJEMPLO_ZORRITO_CULPEO"

registros <- read.csv("data/Culpeo.csv")
registros$presence <- 1L # Asigna un 1 para que sean tratados como datos de 
                         #presencia, si hubiesen ausencias deben quedar con valor 0

amb_vars <- rast("data/raster/capas_clima_1980-2010.tif")

# Selecciona datos de presencia
data_sp <- registros
presencia_val <- as.numeric(data_sp[,"presence"])  #ojo, esto no es un SpatVector, 
                                                   #sólo un vector numérico

spatvector_culpeo <- vect(registros, geom = c("Longitud", "Latitud"), crs = "EPSG:4326") # Convertir en SpatVector
print(spatvector_culpeo) #hay unos warning extraños

# Selecciona coordenadas
presencia_xy <- data_sp[,c("Longitud","Latitud")]  #si presencia_val es un vector numérico
                                                   #esto sirve para resp.xy

#Prepara formato BIOMOD
data_biomod <- BIOMOD_FormatingData(resp.name = nombre_especie, 
                                    resp.var = presencia_val,   
                                    resp.xy = presencia_xy,     
                                    expl.var = amb_vars,        
                                    PA.nb.rep = 1,
                                    PA.nb.absences = 30,
                                    PA.strategy = 'random',
                                    filter.raster = TRUE)

modelo1 <- BIOMOD_Modeling(bm.format = data_biomod,
                           modeling.id = nombre_corrida,
                           models = c("MAXENT", "RF", "GLM"),
                           CV.strategy = 'random',
                           CV.nb.rep = 2,
                           CV.perc = 0.6,                 #DUDA, TENGO POQUITOS DATA
                           OPT.strategy = 'bigboss',      #FALTA UNO?
                           metric.eval = c('KAPPA','TSS','ROC'),
                           var.import = 2,                #será muy poco?
                           seed.val = 42,                 #¿por qué 42?
                           nb.cpu = 4)                    #¿por qué 4???

## Proyecta modelo a las variables

                           