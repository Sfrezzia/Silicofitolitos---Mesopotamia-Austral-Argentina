# ===================================================================
# MS3 | Interpolacion IDW por fisionomia — Bulliform flabellate
# Alcance: Entrada: Datos_R_mapa; tres fisionomias.
# Nombre normalizado para el repositorio; analisis sin cambios.
# ===================================================================

############################################################
# MAPAS DE CALOR / RASTERS IDW DE BULLIFORM FLABELLATE
# POR FISIONOMIA - ENTRE RIOS
#
# Base esperada:
#   Base_clara_R_heatmaps_fisionomias_BULLIFORMES.xlsx
# Hoja usada:
#   Datos_R_mapa
#
# Fisionomias:
#   - pastizal_seco
#   - pastizal_humedo
#   - monte_selva
#
# Salidas:
#   1) GeoTIFF IDW recortados a toda Entre Rios
#   2) GeoTIFF IDW recortados al soporte espacial de cada fisionomia
#      (envolvente convexa de los puntos; buffer configurable)
#   3) PNG + SVG de control
#   4) GeoPackage con puntos, limite y soportes
#   5) Excel de resumen y control
#
# IMPORTANTE:
# - La interpolacion IDW es exploratoria, especialmente para
#   pastizal_humedo, que tiene menos puntos de muestreo.
# - Para interpretacion ecologica se recomienda priorizar los
#   rasters de la carpeta RASTER_SOPORTE, porque reducen la
#   extrapolacion fuera del dominio muestreado.
############################################################


#============================================================
# 1. PAQUETES
#============================================================

paquetes <- c(
  "readxl",
  "dplyr",
  "sf",
  "gstat",
  "terra",
  "ggplot2",
  "scales",
  "viridis",
  "writexl",
  "rnaturalearth"
)

instalar <- paquetes[
  !(paquetes %in% rownames(installed.packages()))
]

if(length(instalar) > 0){
  install.packages(
    instalar,
    repos = "https://cloud.r-project.org"
  )
}

library(readxl)
library(dplyr)
library(sf)
library(gstat)
library(terra)
library(ggplot2)
library(scales)
library(viridis)
library(writexl)
library(rnaturalearth)


#============================================================
# 2. PARAMETROS EDITABLES
#============================================================

# Potencia del IDW
idp <- 2

# Resolucion espacial en metros
# 2000 = aproximadamente 2 km
resolucion <- 2000

# Buffer adicional alrededor de la envolvente convexa
# de los puntos de cada fisionomia.
# 0 = sin buffer.
# Si luego queres ampliar el dominio, por ejemplo 20 km:
# buffer_soporte_m <- 20000
buffer_soporte_m <- 0

# Generar tambien Bambusoideae como mapa exploratorio
incluir_bambusoideae <- TRUE

# Exportar raster interpolado a TODA la provincia
# Ademas del raster restringido al soporte espacial.
exportar_raster_provincia <- TRUE

# Si tenes un limite provincial local (SHP/GPKG), podes
# colocar aqui la ruta completa. Si queda NA, se intenta
# descargar el limite desde Natural Earth.
archivo_limite_local <- NA_character_


#============================================================
# 3. SELECCIONAR EXCEL
#============================================================

cat("\n")
cat("============================================\n")
cat("SELECCION DE BASE\n")
cat("============================================\n")
cat("\nSelecciona: Base_clara_R_heatmaps_fisionomias_BULLIFORMES.xlsx\n\n")

archivo <- file.choose()

cat("Archivo seleccionado:\n")
cat(normalizePath(archivo), "\n")

hojas <- excel_sheets(archivo)

if(!"Datos_R_mapa" %in% hojas){
  stop(
    paste0(
      "No se encontro la hoja 'Datos_R_mapa'.\n",
      "Hojas disponibles: ",
      paste(hojas, collapse = " | ")
    )
  )
}


#============================================================
# 4. IMPORTAR DATOS
#============================================================

datos <- read_excel(
  archivo,
  sheet = "Datos_R_mapa"
)

datos <- as.data.frame(
  datos,
  check.names = FALSE
)

cat("\nFilas importadas:", nrow(datos), "\n")
cat("Columnas importadas:", ncol(datos), "\n")


#============================================================
# 5. COLUMNAS Y CONTROLES
#============================================================

columnas_base <- c(
  "cuenca",
  "estacion",
  "fisionomia",
  "fisionomia_codigo",
  "denominacion",
  "latitud",
  "longitud",
  "n_perfiles",
  "n_muestras",
  "incluye_transicion"
)

afinidades <- c(
  "bulliform_flabellate"
)

fisionomias <- c(
  "pastizal_seco",
  "pastizal_humedo",
  "monte_selva"
)

faltantes <- setdiff(
  c(columnas_base, afinidades),
  names(datos)
)

if(length(faltantes) > 0){
  stop(
    paste0(
      "Faltan columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

# Convertir a numerico por seguridad
for(v in c("latitud", "longitud", afinidades)){
  datos[[v]] <- suppressWarnings(
    as.numeric(datos[[v]])
  )
}

if(any(is.na(datos$latitud)) | any(is.na(datos$longitud))){
  stop("Hay filas sin coordenadas numericas.")
}

if(any(is.na(datos[, afinidades]))){
  stop("Hay NA en las columnas de afinidades. Revisar la base.")
}

if(any(datos[, afinidades] < 0)){
  stop("Se detectaron frecuencias negativas. Revisar la base.")
}

cat("\n")
cat("============================================\n")
cat("CONTROL DE FISIONOMIAS\n")
cat("============================================\n")

print(
  datos %>%
    count(fisionomia_codigo, name = "n_puntos")
)

for(fis in fisionomias){
  n_fis <- sum(datos$fisionomia_codigo == fis)
  if(n_fis < 3){
    stop(
      paste0(
        "La fisionomia '", fis,
        "' tiene menos de 3 puntos; no se puede interpolar de forma razonable."
      )
    )
  }
  if(n_fis < 8){
    warning(
      paste0(
        "La fisionomia '", fis,
        "' tiene solo ", n_fis,
        " puntos. Interpretar el IDW como exploratorio."
      )
    )
  }
}


#============================================================
# 6. CARPETAS DE SALIDA
#============================================================

marca_tiempo <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

carpeta_salida <- file.path(
  dirname(archivo),
  paste0(
    "RASTERS_BULLIFORMES_PROVINCIA_COMPLETA_",
    marca_tiempo
  )
)

carpeta_raster_prov <- file.path(
  carpeta_salida,
  "RASTER_PROVINCIA"
)

carpeta_raster_soporte <- file.path(
  carpeta_salida,
  "RASTER_SOPORTE"
)

carpeta_png <- file.path(
  carpeta_salida,
  "FIGURAS_PNG"
)

carpeta_svg <- file.path(
  carpeta_salida,
  "FIGURAS_SVG"
)

carpeta_vector <- file.path(
  carpeta_salida,
  "VECTOR_QGIS"
)

carpeta_tablas <- file.path(
  carpeta_salida,
  "TABLAS"
)

carpetas <- c(
  carpeta_salida,
  carpeta_raster_prov,
  carpeta_raster_soporte,
  carpeta_png,
  carpeta_svg,
  carpeta_vector,
  carpeta_tablas
)

for(x in carpetas){
  if(!dir.exists(x)){
    dir.create(
      x,
      recursive = TRUE
    )
  }
}

# Subcarpetas por fisionomia
for(fis in fisionomias){
  dir.create(
    file.path(carpeta_raster_prov, fis),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(carpeta_raster_soporte, fis),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(carpeta_png, fis),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(carpeta_svg, fis),
    recursive = TRUE,
    showWarnings = FALSE
  )
}


#============================================================
# 7. OBTENER LIMITE DE ENTRE RIOS
#============================================================

normalizar_texto <- function(x){
  x <- iconv(
    as.character(x),
    from = "",
    to = "ASCII//TRANSLIT"
  )
  toupper(trimws(x))
}

if(
  !is.na(archivo_limite_local) &&
  file.exists(archivo_limite_local)
){
  
  cat("\nUsando limite provincial local.\n")
  
  entre_rios <- st_read(
    archivo_limite_local,
    quiet = TRUE
  )
  
} else {
  
  cat("\nDescargando limites subnacionales desde Natural Earth...\n")
  
  estados <- tryCatch(
    {
      rnaturalearth::ne_download(
        scale = 10,
        type = "states",
        category = "cultural",
        destdir = file.path(carpeta_salida, "CARTOGRAFIA_NE"),
        load = TRUE,
        returnclass = "sf"
      )
    },
    error = function(e){
      message(conditionMessage(e))
      return(NULL)
    }
  )
  
  if(is.null(estados)){
    stop(
      paste0(
        "No fue posible descargar el limite de Entre Rios. ",
        "Podes descargar/cargar un SHP o GPKG local y colocar su ruta ",
        "en 'archivo_limite_local'."
      )
    )
  }
  
  # Identificar Argentina
  nombres <- names(estados)
  
  candidatos_pais <- intersect(
    c("admin", "ADMIN", "adm0_name", "name_0", "geonunit"),
    nombres
  )
  
  if(length(candidatos_pais) > 0){
    campo_pais <- candidatos_pais[1]
    estados_arg <- estados[
      normalizar_texto(estados[[campo_pais]]) == "ARGENTINA",
    ]
  } else {
    candidatos_iso <- intersect(
      c("iso_a2", "ISO_A2", "adm0_a3", "ADM0_A3"),
      nombres
    )
    
    if(length(candidatos_iso) == 0){
      stop("No se pudo identificar Argentina en Natural Earth.")
    }
    
    campo_iso <- candidatos_iso[1]
    estados_arg <- estados[
      estados[[campo_iso]] %in% c("AR", "ARG"),
    ]
  }
  
  if(nrow(estados_arg) == 0){
    stop("No se encontraron provincias de Argentina.")
  }
  
  campos_nombre <- intersect(
    c(
      "name", "NAME",
      "name_en", "name_es",
      "name_local", "name_alt",
      "gn_name"
    ),
    names(estados_arg)
  )
  
  if(length(campos_nombre) == 0){
    stop("No se encontro un campo de nombre provincial.")
  }
  
  coincide <- rep(FALSE, nrow(estados_arg))
  
  for(campo in campos_nombre){
    coincide <- coincide |
      grepl(
        "ENTRE RIOS",
        normalizar_texto(estados_arg[[campo]]),
        fixed = TRUE
      )
  }
  
  entre_rios <- estados_arg[coincide, ]
  
  if(nrow(entre_rios) == 0){
    stop("No se pudo identificar Entre Rios.")
  }
}

entre_rios <- st_make_valid(entre_rios)
entre_rios <- st_union(entre_rios)
entre_rios <- st_as_sf(entre_rios)
st_crs(entre_rios) <- 4326

# Proyeccion adecuada para Entre Rios
entre_rios_utm <- st_transform(
  entre_rios,
  32721
)

cat("\nLimite de Entre Rios listo.\n")


#============================================================
# 8. CREAR PUNTOS ESPACIALES
#============================================================

puntos_sf <- st_as_sf(
  datos,
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

puntos_utm <- st_transform(
  puntos_sf,
  32721
)

# Exportar todos los puntos
st_write(
  puntos_sf,
  file.path(
    carpeta_vector,
    "Puntos_fisionomias.gpkg"
  ),
  layer = "Puntos",
  delete_dsn = TRUE,
  quiet = TRUE
)

# Exportar limite
st_write(
  entre_rios_utm,
  file.path(
    carpeta_vector,
    "Entre_Rios.gpkg"
  ),
  layer = "Entre_Rios",
  delete_dsn = TRUE,
  quiet = TRUE
)


#============================================================
# 9. GRILLA PROVINCIAL PARA IDW
#============================================================

cat("\nCreando grilla de interpolacion...\n")

bbox <- st_bbox(entre_rios_utm)

# Se genera una grilla regular de centros y luego se conserva
# solo lo que cae dentro del limite provincial.
grilla <- st_make_grid(
  entre_rios_utm,
  cellsize = resolucion,
  what = "centers",
  square = TRUE
)

grilla <- st_as_sf(grilla)

# conservar puntos dentro de provincia
dentro <- lengths(
  st_within(
    grilla,
    entre_rios_utm
  )
) > 0

grilla <- grilla[dentro, ]

if(nrow(grilla) == 0){
  stop("La grilla de interpolacion quedo vacia.")
}

cat("Celdas/puntos de grilla:", nrow(grilla), "\n")


#============================================================
# 10. SOPORTE ESPACIAL POR FISIONOMIA
#============================================================

soportes <- list()

for(fis in fisionomias){
  
  pts <- puntos_utm %>%
    filter(fisionomia_codigo == fis)
  
  soporte <- st_convex_hull(
    st_union(pts)
  )
  
  if(buffer_soporte_m > 0){
    soporte <- st_buffer(
      soporte,
      dist = buffer_soporte_m
    )
  }
  
  soporte <- st_intersection(
    st_make_valid(soporte),
    st_make_valid(st_union(entre_rios_utm))
  )
  
  soporte <- st_as_sf(soporte)
  soporte$fisionomia_codigo <- fis
  
  soportes[[fis]] <- soporte
}

soportes_sf <- bind_rows(soportes)

st_write(
  soportes_sf,
  file.path(
    carpeta_vector,
    "Soporte_fisionomias.gpkg"
  ),
  layer = "Soportes",
  delete_dsn = TRUE,
  quiet = TRUE
)


#============================================================
# 11. FUNCION PARA CREAR RASTER DESDE SF DE PUNTOS
#============================================================

sf_a_raster <- function(pred_sf, columna = "Frecuencia"){
  
  coords <- st_coordinates(pred_sf)
  
  xyz <- data.frame(
    X = coords[,1],
    Y = coords[,2],
    Z = pred_sf[[columna]]
  )
  
  r <- terra::rast(
    xyz,
    type = "xyz",
    crs = "EPSG:32721"
  )
  
  r
}


#============================================================
# 12. INTERPOLACION IDW
#============================================================

cat("\n")
cat("============================================\n")
cat("INTERPOLACION IDW\n")
cat("============================================\n")

resultados <- list()
control <- list()
contador <- 1

for(fis in fisionomias){
  
  cat("\n--------------------------------------------\n")
  cat("FISIONOMIA:", fis, "\n")
  cat("--------------------------------------------\n")
  
  puntos_fis <- puntos_utm %>%
    filter(fisionomia_codigo == fis)
  
  soporte_fis <- soportes[[fis]]
  
  # grilla restringida al soporte
  dentro_soporte <- lengths(
    st_within(
      grilla,
      soporte_fis
    )
  ) > 0
  
  grilla_soporte <- grilla[dentro_soporte, ]
  
  if(nrow(grilla_soporte) == 0){
    stop(
      paste0(
        "La grilla de soporte quedo vacia para: ",
        fis
      )
    )
  }
  
  for(afinidad in afinidades){
    
    cat("Calculando:", afinidad, "\n")
    
    valores_obs <- puntos_fis[[afinidad]]
    
    n_puntos <- length(valores_obs)
    minimo <- min(valores_obs, na.rm = TRUE)
    maximo <- max(valores_obs, na.rm = TRUE)
    promedio <- mean(valores_obs, na.rm = TRUE)
    sd_obs <- sd(valores_obs, na.rm = TRUE)
    
    # Si todos los valores son iguales, no tiene sentido ejecutar
    # IDW: se crea directamente una superficie constante.
    sin_variacion <- isTRUE(
      all.equal(
        min(valores_obs, na.rm = TRUE),
        max(valores_obs, na.rm = TRUE)
      )
    )
    
    if(sin_variacion){
      
      pred_prov <- grilla
      pred_prov$Frecuencia <- valores_obs[1]
      
      pred_sop <- grilla_soporte
      pred_sop$Frecuencia <- valores_obs[1]
      
      estado <- "superficie_constante"
      
    } else {
      
      puntos_idw <- puntos_fis %>%
        select(all_of(afinidad))
      
      names(puntos_idw)[1] <- "Valor"
      
      # Provincia completa
      idw_prov <- gstat::idw(
        Valor ~ 1,
        locations = puntos_idw,
        newdata = grilla,
        idp = idp
      )
      
      pred_prov <- st_as_sf(idw_prov)
      pred_prov$Frecuencia <- pred_prov$var1.pred
      
      # Solo soporte espacial
      idw_sop <- gstat::idw(
        Valor ~ 1,
        locations = puntos_idw,
        newdata = grilla_soporte,
        idp = idp
      )
      
      pred_sop <- st_as_sf(idw_sop)
      pred_sop$Frecuencia <- pred_sop$var1.pred
      
      estado <- "idw_ok"
    }
    
    pred_prov$afinidad <- afinidad
    pred_prov$fisionomia_codigo <- fis
    
    pred_sop$afinidad <- afinidad
    pred_sop$fisionomia_codigo <- fis
    
    # Guardar para figuras posteriores
    clave <- paste(fis, afinidad, sep = "__")
    
    resultados[[clave]] <- list(
      provincia = pred_prov,
      soporte = pred_sop
    )
    
    #----------------------------------------------------------
    # RASTER PROVINCIA
    #----------------------------------------------------------
    
    ruta_prov <- NA_character_
    
    if(exportar_raster_provincia){
      
      r_prov <- sf_a_raster(
        pred_prov,
        "Frecuencia"
      )
      
      r_prov <- terra::mask(
        r_prov,
        terra::vect(entre_rios_utm)
      )
      
      names(r_prov) <- afinidad
      
      ruta_prov <- file.path(
        carpeta_raster_prov,
        fis,
        paste0(
          fis,
          "__",
          afinidad,
          "__IDW_provincia.tif"
        )
      )
      
      terra::writeRaster(
        r_prov,
        ruta_prov,
        overwrite = TRUE,
        wopt = list(
          gdal = "COMPRESS=LZW",
          datatype = "FLT4S"
        )
      )
    }
    
    #----------------------------------------------------------
    # RASTER SOPORTE
    #----------------------------------------------------------
    
    r_sop <- sf_a_raster(
      pred_sop,
      "Frecuencia"
    )
    
    r_sop <- terra::mask(
      r_sop,
      terra::vect(soporte_fis)
    )
    
    names(r_sop) <- afinidad
    
    ruta_sop <- file.path(
      carpeta_raster_soporte,
      fis,
      paste0(
        fis,
        "__",
        afinidad,
        "__IDW_soporte.tif"
      )
    )
    
    terra::writeRaster(
      r_sop,
      ruta_sop,
      overwrite = TRUE,
      wopt = list(
        gdal = "COMPRESS=LZW",
        datatype = "FLT4S"
      )
    )
    
    #----------------------------------------------------------
    # TABLA DE CONTROL
    #----------------------------------------------------------
    
    control[[contador]] <- data.frame(
      fisionomia = fis,
      afinidad = afinidad,
      n_puntos = n_puntos,
      minimo_observado = minimo,
      maximo_observado = maximo,
      promedio_observado = promedio,
      sd_observado = sd_obs,
      estado = estado,
      raster_provincia = ruta_prov,
      raster_soporte = ruta_sop,
      stringsAsFactors = FALSE
    )
    
    contador <- contador + 1
  }
}

control_df <- bind_rows(control)


#============================================================
# 13. FIGURAS DE CONTROL
# MISMA ESCALA PARA UNA MISMA AFINIDAD ENTRE FISIONOMIAS
#============================================================

cat("\nGenerando PNG y SVG EXTRAPOLADOS A TODA LA PROVINCIA...\n")

etiquetas_fisionomia <- c(
  pastizal_seco = "Pastizal seco",
  pastizal_humedo = "Pastizal humedo",
  monte_selva = "Monte / selva"
)

etiquetas_afinidad <- c(
  bulliform_flabellate = "Bulliform flabellate"
)

for(afinidad in afinidades){
  
  # Maximo comun entre las tres fisionomias
  max_comun <- 0
  
  for(fis in fisionomias){
    clave <- paste(fis, afinidad, sep = "__")
    pred <- resultados[[clave]]$provincia
    max_comun <- max(
      max_comun,
      max(pred$Frecuencia, na.rm = TRUE)
    )
  }
  
  if(!is.finite(max_comun) || max_comun <= 0){
    max_comun <- 0.01
  }
  
  for(fis in fisionomias){
    
    clave <- paste(fis, afinidad, sep = "__")
    pred <- resultados[[clave]]$provincia
    
    xy <- st_coordinates(pred)
    
    df_plot <- data.frame(
      X = xy[,1],
      Y = xy[,2],
      Frecuencia = pred$Frecuencia
    )
    
    pts_plot <- puntos_utm %>%
      filter(fisionomia_codigo == fis)
    
    p <- ggplot() +
      geom_raster(
        data = df_plot,
        aes(
          x = X,
          y = Y,
          fill = Frecuencia
        )
      ) +
      geom_sf(
        data = entre_rios_utm,
        fill = NA,
        color = "black",
        linewidth = 0.5
      ) +
      geom_sf(
        data = pts_plot,
        shape = 21,
        fill = "white",
        color = "black",
        size = 2.2,
        stroke = 0.45
      ) +
      scale_fill_viridis_c(
        option = "C",
        limits = c(0, max_comun),
        oob = scales::squish,
        labels = scales::percent_format(
          accuracy = 1
        ),
        name = "Frecuencia\nrelativa"
      ) +
      coord_sf(
        expand = FALSE
      ) +
      theme_classic() +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      ) +
      labs(
        title = paste0(
          etiquetas_afinidad[[afinidad]],
          " - ",
          etiquetas_fisionomia[[fis]]
        ),
        subtitle = paste0(
          "IDW (idp = ", idp,
          "); resolucion = ", resolucion/1000,
          " km; dominio = toda la provincia de Entre Rios"
        )
      )
    
    archivo_png <- file.path(
      carpeta_png,
      fis,
      paste0(
        fis,
        "__",
        afinidad,
        ".png"
      )
    )
    
    archivo_svg <- file.path(
      carpeta_svg,
      fis,
      paste0(
        fis,
        "__",
        afinidad,
        ".svg"
      )
    )
    
    ggsave(
      archivo_png,
      p,
      width = 7,
      height = 8,
      dpi = 300
    )
    
    ggsave(
      archivo_svg,
      p,
      width = 7,
      height = 8,
      device = "svg"
    )
  }
}


#============================================================
# 14. EXPORTAR TABLAS DE CONTROL
#============================================================

puntos_control <- datos %>%
  select(
    cuenca,
    estacion,
    fisionomia,
    fisionomia_codigo,
    denominacion,
    latitud,
    longitud,
    n_perfiles,
    n_muestras,
    incluye_transicion,
    all_of(afinidades)
  )

resumen_fisionomia <- datos %>%
  group_by(fisionomia_codigo) %>%
  summarise(
    n_estaciones = n(),
    n_perfiles = sum(n_perfiles),
    n_muestras = sum(n_muestras),
    .groups = "drop"
  )

parametros <- data.frame(
  parametro = c(
    "idp",
    "resolucion_m",
    "buffer_soporte_m",
    "CRS_trabajo",
    "raster_provincia_exportado"
  ),
  valor = c(
    idp,
    resolucion,
    buffer_soporte_m,
    "EPSG:32721 - WGS84 / UTM 21S",
    exportar_raster_provincia
  ),
  stringsAsFactors = FALSE
)

archivo_excel_salida <- file.path(
  carpeta_tablas,
  "Resumen_IDW_bulliformes_fisionomias.xlsx"
)

writexl::write_xlsx(
  list(
    Parametros = parametros,
    Resumen_fisionomias = resumen_fisionomia,
    Puntos_usados = puntos_control,
    Control_rasters = control_df
  ),
  archivo_excel_salida
)


#============================================================
# 15. CONTROL FINAL
#============================================================

cat("\n")
cat("============================================\n")
cat("PROCESAMIENTO FINALIZADO\n")
cat("============================================\n")

cat("\nFisionomias procesadas:", length(fisionomias))
cat("\nAfinidades procesadas:", length(afinidades))
cat("\nRasters de soporte generados:", nrow(control_df))

if(exportar_raster_provincia){
  cat("\nRasters provinciales generados:", nrow(control_df))
}

cat("\n\nCARPETA PRINCIPAL:\n")
cat(carpeta_salida, "\n")

cat("\nRASTERS EXTRAPOLADOS A TODA LA PROVINCIA:\n")
cat(carpeta_raster_prov, "\n")

cat("\nRASTERS RESTRINGIDOS AL SOPORTE (solo como control):\n")
cat(carpeta_raster_soporte, "\n")

if(exportar_raster_provincia){
  cat("\nRASTERS DE TODA LA PROVINCIA:\n")
  cat(carpeta_raster_prov, "\n")
}

cat("\nFIGURAS PNG:\n")
cat(carpeta_png, "\n")

cat("\nFIGURAS SVG:\n")
cat(carpeta_svg, "\n")

cat("\nCAPAS QGIS:\n")
cat(carpeta_vector, "\n")

cat("\nEXCEL DE CONTROL:\n")
cat(archivo_excel_salida, "\n")

cat("\n============================================\n")


