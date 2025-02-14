library(terra)
library(biomod2)
library(openxlsx)

# Parámetros modificables -------------------------------------------------

nombre_especie <- "pudu"
nombre_corrida <- "100m_pp+temp_zoltan"
formato_salida <- "tif" # también se acepta "ascii"

# Carga los registros de la especie ---------------------------------------

registros <- read.xlsx("data/presencias_pudu_no_urbano.xlsx")

## Asigna un 1 para que sean tratados como datos de presencia
## Si hubiesen ausencias deben quedar con valor 0

registros$presence <- 1L


# Carga set de variables ambientales --------------------------------------

amb_vars <- rast("data/raster/stack_variables_100m_pp+temp.tif")


# Modelación --------------------------------------------------------------

## Selecciona datos de presencia
data_sp <- registros
presencia_val <- as.numeric(data_sp[,"presence"])

## Selecciona coordenadas
presencia_xy <- data_sp[,c("longitud","latitud")]

suppressWarnings({
  
  ## Prepara formato biomod
  data_biomod <- BIOMOD_FormatingData(resp.var = presencia_val,
                                      resp.xy = presencia_xy, 
                                      resp.name = nombre_especie,
                                      expl.var = amb_vars,
                                      PA.nb.rep = 1,
                                      PA.nb.absences = 500,
                                      PA.strategy = 'random',
                                      filter.raster = T)
  
  
  ## Configuración de opciones por defecto
  myBiomodOption <- biomod2::OptionsBigboss
    
  
  modelo1 <- BIOMOD_Modeling(bm.format = data_biomod,
                                      modeling.id = nombre_corrida,
                                      models = c("MAXENT", "RF", "GLM"),
                                      CV.strategy = 'random',
                                      CV.nb.rep = 2,
                                      CV.perc = 0.8,
                                      OPT.strategy = 'bigboss',
                                      metric.eval = c('KAPPA','TSS','ROC'),
                                      var.import = 2,
                                      seed.val = 42,
                                      nb.cpu = 4)
  
  ## Proyecta modelo a las variables
  modelo_proj_presente <- BIOMOD_Projection(bm.mod = modelo1,
                                            new.env = amb_vars,
                                            proj.name = 'presente',
                                            compress = 'gzip',
                                            clamping.mask = T,
                                            output.format = '.grd',
                                            do.stack = T
  )
  
  ## Ensambla modelos con TSS > 0.7
  ensamble_presente <- BIOMOD_EnsembleModeling(bm.mod = modelo1,
                                                   em.by = 'all',
                                                   metric.select = c('TSS'),
                                                   metric.select.thresh = 0.7,
                                                   metric.eval = c('TSS','ROC','KAPPA'),
                                                   em.algo = c('EMmedian'))
  ## Proyecta ensamble
  ensamble_proj_presente <- BIOMOD_EnsembleForecasting(ensamble_presente,
                                                      bm.proj = modelo_proj_presente,
                                                      models.chosen = 'all',
                                                      compress = 'gzip')
  
  ## Extrae predicciones finales
  result_presente <- get_predictions(ensamble_proj_presente)
  result_presente <- result_presente / minmax(result_presente)[2]
  result_presenteq <- terra::classify(result_presente, seq(0, 1 ,0.25)) # convierte en cuartil
  
  ## Guarda
  writeRaster(result_presente, filename = paste0("resultados/modelacion/", nombre_especie, "_", nombre_corrida, "_", Sys.Date(), ".", formato_salida),  overwrite = T)
  writeRaster(result_presenteq, filename = paste0("resultados/modelacion/", nombre_especie,"_cuartil_", nombre_corrida,"_", Sys.Date() ,".", formato_salida), overwrite = T)
  
})

message("¡FIN DE LA MODELACIÓN!")
