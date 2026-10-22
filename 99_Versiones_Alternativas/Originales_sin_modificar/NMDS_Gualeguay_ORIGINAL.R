############################################################
# ==========================================================
# NMDS - CUENCA DEL GUALEGUAY
# SCRIPT INTEGRADO FINAL
# ==========================================================
#
# Archivo:
#   Hoja "Gualeguay" = base depurada y fuente para reconstruir perfiles
#
# REPRESENTACIÓN:
#   Punto   = muestra
#   Color   = estación
#   Símbolo = perfil
#
# FIGURAS:
#   1. NMDS + vectores de silicofitolitos
#   2. NMDS + TMA y PMA
#   3. NMDS + pH, MO, sílice y textura
#
# NMDS:
#   Distancia Bray-Curtis
#   k = 2
#   abundancias relativas por muestra
#
############################################################


############################################################
# 0. PAQUETES
############################################################

paquetes <- c(
  "readxl",
  "vegan",
  "ggplot2",
  "ggrepel",
  "writexl",
  "svglite"
)

faltantes <- paquetes[
  !(paquetes %in% rownames(installed.packages()))
]

if(length(faltantes) > 0){
  
  install.packages(
    faltantes,
    repos = "https://cloud.r-project.org"
  )
}

library(readxl)
library(vegan)
library(ggplot2)
library(ggrepel)
library(writexl)
library(svglite)


############################################################
# 1. PARÁMETROS
############################################################

set.seed(1234)

N_PERM <- 999

ALPHA <- 0.05

# Además de los significativos y morfotipos clave,
# se muestran los 12 de mayor r2
TOP_R2 <- 12


############################################################
# 2. SELECCIONAR ARCHIVO
############################################################

archivo <- file.choose()

hojas <- excel_sheets(archivo)

cat("\nHojas encontradas:\n")
print(hojas)


if(!"Gualeguay" %in% hojas){
  
  stop(
    "No se encontró la hoja 'Gualeguay'."
  )
}

############################################################
# 3. CARPETA DE RESULTADOS
############################################################

sello <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

carpeta_resultados <- file.path(
  dirname(archivo),
  paste0(
    "NMDS_Gualeguay_",
    sello
  )
)

dir.create(
  carpeta_resultados,
  showWarnings = FALSE
)

cat(
  "\nLos resultados se guardarán en:\n",
  carpeta_resultados,
  "\n"
)


############################################################
# 4. IMPORTAR BASE PARA RECONSTRUIR PERFILES
############################################################

original <- read_excel(
  archivo,
  sheet = "Gualeguay",
  na = c(
    "",
    "NA",
    "N/A",
    "NaN",
    "Inf",
    "-Inf"
  )
)

original <- as.data.frame(
  original,
  check.names = FALSE
)


############################################################
# 5. IMPORTAR BASE DEPURADA (MISMA HOJA GUALEGUAY)
############################################################

datos <- read_excel(
  archivo,
  sheet = "Gualeguay",
  na = c(
    "",
    "NA",
    "N/A",
    "NaN",
    "Inf",
    "-Inf"
  )
)

datos <- as.data.frame(
  datos,
  check.names = FALSE
)

cat(
  "\nMuestras de la hoja Gualeguay:",
  nrow(datos),
  "\n"
)


############################################################
# 6. FUNCIÓN NUMÉRICA SEGURA
############################################################

a_numerico <- function(x){
  
  if(is.numeric(x)){
    
    return(
      as.numeric(x)
    )
  }
  
  x <- trimws(
    as.character(x)
  )
  
  x[
    x %in% c(
      "",
      "NA",
      "N/A",
      "NaN",
      "Inf",
      "-Inf"
    )
  ] <- NA
  
  x <- gsub(
    ",",
    ".",
    x,
    fixed = TRUE
  )
  
  suppressWarnings(
    as.numeric(x)
  )
}


############################################################
# 7. FUNCIÓN PARA NORMALIZAR ID
############################################################

normalizar_id <- function(x){
  
  x2 <- trimws(
    as.character(x)
  )
  
  num <- suppressWarnings(
    as.numeric(x2)
  )
  
  es_num <- !is.na(num) &
    grepl(
      "^[0-9]+([\\.,][0-9]+)?$",
      x2
    )
  
  if(any(es_num)){
    
    x2[es_num] <- format(
      num[es_num],
      scientific = FALSE,
      trim = TRUE
    )
    
    x2[es_num] <- sub(
      "\\.0+$",
      "",
      x2[es_num]
    )
  }
  
  return(x2)
}


############################################################
# 8. NORMALIZAR ESTACIÓN E ID
############################################################

original$Estación <- trimws(
  as.character(
    original$Estación
  )
)

datos$Estación <- trimws(
  as.character(
    datos$Estación
  )
)

original$ID_norm <- normalizar_id(
  original$ID
)

datos$ID_norm <- normalizar_id(
  datos$ID
)


############################################################
# 9. RECONSTRUIR PERFILES DESDE LA HOJA ORIGINAL
############################################################

original$Perfil <- NA_character_

perfil_actual <- NA_character_
estacion_anterior <- NA_character_


for(i in seq_len(nrow(original))){
  
  estacion_i <- original$Estación[i]
  id_i <- original$ID_norm[i]
  
  
  # Cuando cambia la estación reiniciar el perfil
  
  if(
    i == 1 ||
    is.na(estacion_anterior) ||
    is.na(estacion_i) ||
    estacion_i != estacion_anterior
  ){
    
    perfil_actual <- NA_character_
  }
  
  
  # Detectar H.O.
  
  es_HO <- grepl(
    "^\\s*H\\.O",
    id_i,
    ignore.case = TRUE
  )
  
  
  if(es_HO){
    
    if(
      grepl(
        "[0-9]+\\s*$",
        id_i
      )
    ){
      
      perfil_actual <- sub(
        ".*?([0-9]+)\\s*$",
        "\\1",
        id_i,
        perl = TRUE
      )
    }
  }
  
  
  original$Perfil[i] <- perfil_actual
  
  estacion_anterior <- estacion_i
}


############################################################
# 10. ASIGNAR PERFILES A LA BASE DEPURADA
############################################################

clave_original <- paste(
  toupper(original$Estación),
  original$ID_norm,
  sep = "___"
)

clave_filtrada <- paste(
  toupper(datos$Estación),
  datos$ID_norm,
  sep = "___"
)


if(anyDuplicated(clave_original)){
  
  stop(
    paste(
      "Existen combinaciones Estación + ID duplicadas",
      "en la hoja Gualeguay."
    )
  )
}


datos$Perfil <- original$Perfil[
  match(
    clave_filtrada,
    clave_original
  )
]


############################################################
# 11. CONTROL DE PERFILES
############################################################

if(any(is.na(datos$Perfil))){
  
  cat(
    "\nMuestras que no pudieron asignarse a un perfil:\n"
  )
  
  print(
    datos[
      is.na(datos$Perfil),
      c(
        "Estación",
        "ID"
      )
    ]
  )
  
  stop(
    "Existen muestras sin perfil asignado."
  )
}


niveles_perfil <- sort(
  unique(
    as.numeric(
      datos$Perfil
    )
  )
)

niveles_perfil <- as.character(
  niveles_perfil
)

datos$Perfil <- factor(
  datos$Perfil,
  levels = niveles_perfil
)


cat(
  "\nMuestras por estación y perfil:\n"
)

print(
  table(
    datos$Estación,
    datos$Perfil
  )
)


############################################################
# 12. SÍMBOLOS PARA LOS PERFILES
############################################################

simbolos_disponibles <- c(
  16,  # círculo
  17,  # triángulo
  15,  # cuadrado
  18,  # rombo
  8,
  3,
  7,
  0,
  1,
  2
)


if(
  length(niveles_perfil) >
  length(simbolos_disponibles)
){
  
  stop(
    "Hay más perfiles que símbolos disponibles."
  )
}


simbolos_perfil <- setNames(
  simbolos_disponibles[
    seq_along(
      niveles_perfil
    )
  ],
  niveles_perfil
)


cat(
  "\nSímbolos de perfiles:\n"
)

print(
  simbolos_perfil
)


############################################################
# 13. IDENTIFICAR MORFOTIPOS
############################################################

inicio <- match(
  "Acute bulbosus",
  names(datos)
)

fin <- match(
  "Crenate",
  names(datos)
)


if(
  is.na(inicio) ||
  is.na(fin) ||
  inicio > fin
){
  
  stop(
    "No se pudo localizar el rango Acute bulbosus:Crenate."
  )
}


vars_fito <- names(datos)[
  inicio:fin
]


cat(
  "\nMorfotipos encontrados:",
  length(vars_fito),
  "\n"
)


############################################################
# 14. MATRIZ DE SILICOFITOLITOS
############################################################

fito <- datos[
  ,
  vars_fito,
  drop = FALSE
]


for(j in seq_along(fito)){
  
  fito[[j]] <- a_numerico(
    fito[[j]]
  )
}


cantidad_NA_fito <- sum(
  is.na(
    as.matrix(fito)
  )
)

cat(
  "\nNA fitolíticos convertidos a cero:",
  cantidad_NA_fito,
  "\n"
)


# En la matriz fitolítica se interpretan
# las celdas vacías como ausencia.

fito[
  is.na(fito)
] <- 0


mat_fito <- as.matrix(
  fito
)

storage.mode(
  mat_fito
) <- "double"


############################################################
# 15. CONTROLES DE LA MATRIZ
############################################################

if(any(!is.finite(mat_fito))){
  
  stop(
    "Persisten NA/NaN/Inf en la matriz fitolítica."
  )
}


if(any(mat_fito < 0)){
  
  stop(
    "Existen valores negativos en la matriz fitolítica."
  )
}


############################################################
# 16. ELIMINAR MUESTRAS VACÍAS
############################################################

suma_filas <- rowSums(
  mat_fito
)

usar <- is.finite(suma_filas) &
  suma_filas > 0


if(any(!usar)){
  
  cat(
    "\nMuestras eliminadas porque suman cero:",
    sum(!usar),
    "\n"
  )
  
  print(
    datos[
      !usar,
      c(
        "Estación",
        "Perfil",
        "ID"
      )
    ]
  )
}


datos_nmds <- datos[
  usar,
  ,
  drop = FALSE
]

mat_fito <- mat_fito[
  usar,
  ,
  drop = FALSE
]


############################################################
# 17. ESTANDARIZAR ABUNDANCIAS
############################################################
#
# Algunas filas están expresadas como 0-1
# y otras como porcentajes 0-100.
#
# Se lleva cada muestra a suma = 1.
#
############################################################

fito_rel <- decostand(
  mat_fito,
  method = "total"
)


############################################################
# 18. ELIMINAR MORFOTIPOS SIN VARIACIÓN
############################################################

tiene_variacion <- apply(
  fito_rel,
  2,
  function(x){
    
    x <- x[
      is.finite(x)
    ]
    
    if(length(x) < 2){
      
      return(FALSE)
    }
    
    return(
      length(unique(x)) > 1 &&
        sd(x) > sqrt(.Machine$double.eps)
    )
  }
)


morfotipos_sin_variacion <- colnames(
  fito_rel
)[
  !tiene_variacion
]


fito_nmds <- fito_rel[
  ,
  tiene_variacion,
  drop = FALSE
]


cat(
  "\nMorfotipos utilizados:",
  ncol(fito_nmds),
  "\n"
)

cat(
  "Morfotipos sin variación:",
  length(morfotipos_sin_variacion),
  "\n"
)


############################################################
# 19. NMDS
############################################################

set.seed(1234)

nmds <- metaMDS(
  fito_nmds,
  distance = "bray",
  k = 2,
  trymax = 500,
  autotransform = FALSE,
  wascores = FALSE,
  zerodist = "add",
  trace = FALSE
)


############################################################
# 20. STRESS
############################################################

stress <- nmds$stress

cat(
  "\n=======================================\n"
)

cat(
  "STRESS NMDS =",
  round(stress, 4),
  "\n"
)

cat(
  "=======================================\n"
)


############################################################
# 21. COORDENADAS DEL NMDS
############################################################

coord <- as.matrix(
  scores(
    nmds,
    display = "sites"
  )
)

colnames(coord) <- c(
  "NMDS1",
  "NMDS2"
)


if(any(!is.finite(coord))){
  
  stop(
    "Las coordenadas NMDS contienen NA/NaN/Inf."
  )
}


############################################################
# 22. TABLA DE PUNTOS
############################################################

puntos <- data.frame(
  NMDS1 = coord[, 1],
  NMDS2 = coord[, 2],
  
  Estación = factor(
    datos_nmds$Estación
  ),
  
  Perfil = factor(
    datos_nmds$Perfil,
    levels = niveles_perfil
  ),
  
  ID = as.character(
    datos_nmds$ID
  ),
  
  stringsAsFactors = FALSE
)


############################################################
# 23. FUNCIÓN ROBUSTA PARA AJUSTAR UN VECTOR
############################################################
#
# Envfit se ejecuta variable por variable.
# Así un NA o una variable problemática no rompe
# el análisis completo.
#
############################################################

ajustar_vector <- function(
    coordenadas,
    variable,
    nombre,
    permutations = 999,
    min_n = 4
){
  
  x <- a_numerico(
    variable
  )
  
  ok <- is.finite(x) &
    is.finite(coordenadas[, 1]) &
    is.finite(coordenadas[, 2])
  
  
  x2 <- x[
    ok
  ]
  
  X2 <- coordenadas[
    ok,
    ,
    drop = FALSE
  ]
  
  
  if(length(x2) < min_n){
    
    return(
      data.frame(
        Variable = nombre,
        NMDS1 = NA_real_,
        NMDS2 = NA_real_,
        r2 = NA_real_,
        p = NA_real_,
        N = length(x2),
        Estado = "Omitida: pocos datos",
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  if(
    length(unique(x2)) < 2 ||
    !is.finite(sd(x2)) ||
    sd(x2) <= sqrt(.Machine$double.eps)
  ){
    
    return(
      data.frame(
        Variable = nombre,
        NMDS1 = NA_real_,
        NMDS2 = NA_real_,
        r2 = NA_real_,
        p = NA_real_,
        N = length(x2),
        Estado = "Omitida: sin variación",
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  variable_df <- data.frame(
    valor = x2
  )
  
  
  ajuste <- tryCatch(
    
    vegan::envfit(
      X2,
      variable_df,
      permutations = permutations
    ),
    
    error = function(e){
      
      return(NULL)
    }
  )
  
  
  if(is.null(ajuste)){
    
    return(
      data.frame(
        Variable = nombre,
        NMDS1 = NA_real_,
        NMDS2 = NA_real_,
        r2 = NA_real_,
        p = NA_real_,
        N = length(x2),
        Estado = "Omitida: error envfit",
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  flecha <- tryCatch(
    
    as.matrix(
      scores(
        ajuste,
        display = "vectors"
      )
    ),
    
    error = function(e){
      
      NULL
    }
  )
  
  
  if(
    is.null(flecha) ||
    nrow(flecha) < 1
  ){
    
    return(
      data.frame(
        Variable = nombre,
        NMDS1 = NA_real_,
        NMDS2 = NA_real_,
        r2 = NA_real_,
        p = NA_real_,
        N = length(x2),
        Estado = "Omitida: sin vector",
        stringsAsFactors = FALSE
      )
    )
  }
  
  
  data.frame(
    Variable = nombre,
    
    NMDS1 = as.numeric(
      flecha[1, 1]
    ),
    
    NMDS2 = as.numeric(
      flecha[1, 2]
    ),
    
    r2 = as.numeric(
      ajuste$vectors$r[1]
    ),
    
    p = as.numeric(
      ajuste$vectors$pvals[1]
    ),
    
    N = length(x2),
    
    Estado = "OK",
    
    stringsAsFactors = FALSE
  )
}


############################################################
# 24. AJUSTAR TODOS LOS MORFOTIPOS
############################################################

lista_fito <- vector(
  "list",
  ncol(fito_nmds)
)


for(j in seq_len(ncol(fito_nmds))){
  
  nombre <- colnames(
    fito_nmds
  )[j]
  
  cat(
    "Ajustando morfotipo:",
    nombre,
    "\n"
  )
  
  
  set.seed(
    1000 + j
  )
  
  
  lista_fito[[j]] <- ajustar_vector(
    coordenadas = coord,
    variable = fito_nmds[, j],
    nombre = nombre,
    permutations = N_PERM
  )
}


vectores_fito <- do.call(
  rbind,
  lista_fito
)


vectores_fito_ok <- vectores_fito[
  vectores_fito$Estado == "OK" &
    is.finite(vectores_fito$r2),
  ,
  drop = FALSE
]


############################################################
# 25. MORFOTIPOS QUE QUEREMOS CONSERVAR EN LA FIGURA
############################################################

morfotipos_clave <- c(
  "Bilobate",
  "Bilobate Pan",
  "Bilobate Stip",
  "Bilobate Chl",
  "Rondel",
  "Crenate",
  "Saddle Chl",
  "Saddle Bamb",
  "Saddle",
  "Cross",
  "Cross Pan",
  "Spheroid ornate",
  "Spheroid echinate",
  "Spheroid psilate",
  "Bulliform flabellate",
  "Acute bulbosus",
  "Elongate",
  "Polylobate"
)


############################################################
# 26. SELECCIONAR VECTORES FITOLÍTICOS
############################################################

vectores_fito_ok$Significativo <-
  vectores_fito_ok$p <= ALPHA


vectores_fito_ok$Clave <-
  vectores_fito_ok$Variable %in%
  morfotipos_clave


orden_r2 <- order(
  vectores_fito_ok$r2,
  decreasing = TRUE
)


top_r2 <- head(
  vectores_fito_ok$Variable[
    orden_r2
  ],
  TOP_R2
)


vectores_fito_ok$Top_r2 <-
  vectores_fito_ok$Variable %in%
  top_r2


vectores_fito_ok$Mostrar <-
  vectores_fito_ok$Significativo |
  vectores_fito_ok$Clave |
  vectores_fito_ok$Top_r2


vectores_fito_plot <- vectores_fito_ok[
  vectores_fito_ok$Mostrar,
  ,
  drop = FALSE
]


vectores_fito_plot$Etiqueta <- ifelse(
  vectores_fito_plot$Significativo,
  paste0(
    vectores_fito_plot$Variable,
    " *"
  ),
  vectores_fito_plot$Variable
)


############################################################
# 27. FUNCIÓN PARA ESCALAR FLECHAS
############################################################

escalar_flechas <- function(
    vectores,
    puntos,
    fraccion = 0.78
){
  
  if(nrow(vectores) == 0){
    
    return(vectores)
  }
  
  
  rango_x <- max(
    abs(puntos$NMDS1),
    na.rm = TRUE
  )
  
  rango_y <- max(
    abs(puntos$NMDS2),
    na.rm = TRUE
  )
  
  
  max_x <- max(
    abs(vectores$NMDS1),
    na.rm = TRUE
  )
  
  max_y <- max(
    abs(vectores$NMDS2),
    na.rm = TRUE
  )
  
  
  factores <- numeric(0)
  
  
  if(
    is.finite(max_x) &&
    max_x > 0
  ){
    
    factores <- c(
      factores,
      rango_x / max_x
    )
  }
  
  
  if(
    is.finite(max_y) &&
    max_y > 0
  ){
    
    factores <- c(
      factores,
      rango_y / max_y
    )
  }
  
  
  if(length(factores) == 0){
    
    vectores$NMDS1_plot <-
      vectores$NMDS1
    
    vectores$NMDS2_plot <-
      vectores$NMDS2
    
    return(vectores)
  }
  
  
  multiplicador <- fraccion *
    min(factores)
  
  
  vectores$NMDS1_plot <-
    vectores$NMDS1 *
    multiplicador
  
  vectores$NMDS2_plot <-
    vectores$NMDS2 *
    multiplicador
  
  
  return(vectores)
}


############################################################
# 28. ESCALAR VECTORES FITOLÍTICOS
############################################################

vectores_fito_plot <- escalar_flechas(
  vectores_fito_plot,
  puntos,
  fraccion = 0.80
)


############################################################
# 29. FUNCIÓN DEL GRÁFICO BASE
############################################################

grafico_base <- function(){
  
  ggplot(
    puntos,
    aes(
      x = NMDS1,
      y = NMDS2,
      color = Estación,
      shape = Perfil
    )
  ) +
    
    geom_hline(
      yintercept = 0,
      linewidth = 0.25,
      linetype = 3
    ) +
    
    geom_vline(
      xintercept = 0,
      linewidth = 0.25,
      linetype = 3
    ) +
    
    geom_point(
      size = 3.2,
      alpha = 0.85
    ) +
    
    scale_shape_manual(
      values = simbolos_perfil
    ) +
    
    coord_equal() +
    
    theme_classic() +
    
    theme(
      legend.position = "right",
      
      plot.title = element_text(
        face = "bold"
      )
    )
}


############################################################
# 30. GRÁFICO 1
# NMDS + VECTORES DE SILICOFITOLITOS
############################################################

g_fito <- grafico_base() +
  
  geom_segment(
    data = vectores_fito_plot,
    
    aes(
      x = 0,
      y = 0,
      xend = NMDS1_plot,
      yend = NMDS2_plot
    ),
    
    inherit.aes = FALSE,
    
    arrow = arrow(
      length = grid::unit(
        0.18,
        "cm"
      )
    ),
    
    linewidth = 0.5
  ) +
  
  geom_text_repel(
    data = vectores_fito_plot,
    
    aes(
      x = NMDS1_plot,
      y = NMDS2_plot,
      label = Etiqueta
    ),
    
    inherit.aes = FALSE,
    
    size = 3.0,
    
    max.overlaps = Inf,
    
    box.padding = 0.45,
    
    point.padding = 0.25,
    
    min.segment.length = 0,
    
    segment.size = 0.25
  ) +
  
  labs(
    title =
      "Cuenca del Gualeguay - NMDS de asociaciones silicofitolíticas",
    
    subtitle = paste0(
      "Bray-Curtis | Stress = ",
      round(stress, 3),
      " | * p ≤ 0,05"
    ),
    
    x = "NMDS1",
    
    y = "NMDS2",
    
    color = "Estación",
    
    shape = "Perfil"
  )


############################################################
# ==========================================================
# 31. VARIABLES CLIMÁTICAS
# ==========================================================
#
# TMA y PMA corresponden a la ESTACIÓN.
#
# Por eso NO se usan las 94 muestras como si fueran
# observaciones climáticas independientes.
#
# Se calcula el centroide NMDS de cada estación y se
# ajustan los vectores climáticos sobre esos centroides.
#
############################################################


############################################################
# 32. EXTRAER TMA Y PMA
############################################################

TMA <- a_numerico(
  datos_nmds[["TMA (º C)"]]
)

PMA <- a_numerico(
  datos_nmds[["PMA (mm)"]]
)


############################################################
# 33. TABLA CLIMÁTICA AUXILIAR
############################################################

datos_clima_aux <- data.frame(
  Estación = as.character(
    datos_nmds$Estación
  ),
  
  NMDS1 = coord[, 1],
  
  NMDS2 = coord[, 2],
  
  TMA = TMA,
  
  PMA = PMA,
  
  stringsAsFactors = FALSE
)


############################################################
# 34. CENTROIDES NMDS POR ESTACIÓN
############################################################

centroides_estacion <- aggregate(
  cbind(
    NMDS1,
    NMDS2
  ) ~ Estación,
  
  data = datos_clima_aux,
  
  FUN = mean,
  na.rm = TRUE
)


############################################################
# 35. FUNCIÓN PARA PROMEDIAR VALORES ÚNICOS
############################################################

media_unicos <- function(x){
  
  x <- unique(
    x[
      is.finite(x)
    ]
  )
  
  if(length(x) == 0){
    
    return(
      NA_real_
    )
  }
  
  return(
    mean(x)
  )
}


############################################################
# 36. TMA Y PMA POR ESTACIÓN
############################################################

clima_estacion <- aggregate(
  cbind(
    TMA,
    PMA
  ) ~ Estación,
  
  data = datos_clima_aux,
  
  FUN = media_unicos
)


############################################################
# 37. UNIR CLIMA + CENTROIDES
############################################################

clima_fit <- merge(
  centroides_estacion,
  clima_estacion,
  by = "Estación"
)


cat(
  "\n=======================================\n"
)

cat(
  "DATOS CLIMÁTICOS POR ESTACIÓN\n"
)

cat(
  "=======================================\n"
)

print(
  clima_fit
)


############################################################
# 38. AJUSTAR TMA
############################################################

set.seed(2001)

vector_TMA <- ajustar_vector(
  coordenadas = as.matrix(
    clima_fit[
      ,
      c(
        "NMDS1",
        "NMDS2"
      )
    ]
  ),
  
  variable = clima_fit$TMA,
  
  nombre = "TMA (°C)",
  
  permutations = N_PERM,
  
  min_n = 4
)


############################################################
# 39. AJUSTAR PMA
############################################################

set.seed(2002)

vector_PMA <- ajustar_vector(
  coordenadas = as.matrix(
    clima_fit[
      ,
      c(
        "NMDS1",
        "NMDS2"
      )
    ]
  ),
  
  variable = clima_fit$PMA,
  
  nombre = "PMA (mm)",
  
  permutations = N_PERM,
  
  min_n = 4
)


############################################################
# 40. UNIR VECTORES CLIMÁTICOS
############################################################

vectores_clima <- rbind(
  vector_TMA,
  vector_PMA
)


cat(
  "\nVectores climáticos obtenidos:\n"
)

print(
  vectores_clima
)


vectores_clima_ok <- vectores_clima[
  vectores_clima$Estado == "OK",
  ,
  drop = FALSE
]


if(nrow(vectores_clima_ok) == 0){
  
  stop(
    paste(
      "No pudo ajustarse ningún vector climático.",
      "Revisar TMA/PMA."
    )
  )
}


############################################################
# 41. ETIQUETAS CLIMÁTICAS
############################################################

vectores_clima_ok$Significativo <-
  vectores_clima_ok$p <= ALPHA


vectores_clima_ok$Etiqueta <- ifelse(
  vectores_clima_ok$Significativo,
  
  paste0(
    vectores_clima_ok$Variable,
    " *"
  ),
  
  vectores_clima_ok$Variable
)


############################################################
# 42. ESCALAR VECTORES CLIMÁTICOS
############################################################

vectores_clima_plot <- escalar_flechas(
  vectores_clima_ok,
  puntos,
  fraccion = 0.72
)


############################################################
# 43. GRÁFICO 2
# NMDS + CLIMA
############################################################

g_clima <- grafico_base() +
  
  geom_segment(
    data = vectores_clima_plot,
    
    aes(
      x = 0,
      y = 0,
      xend = NMDS1_plot,
      yend = NMDS2_plot
    ),
    
    inherit.aes = FALSE,
    
    arrow = arrow(
      length = grid::unit(
        0.20,
        "cm"
      )
    ),
    
    linewidth = 0.8
  ) +
  
  geom_text_repel(
    data = vectores_clima_plot,
    
    aes(
      x = NMDS1_plot,
      y = NMDS2_plot,
      label = Etiqueta
    ),
    
    inherit.aes = FALSE,
    
    size = 4,
    
    fontface = "bold",
    
    max.overlaps = Inf,
    
    box.padding = 0.5,
    
    point.padding = 0.25,
    
    min.segment.length = 0
  ) +
  
  labs(
    title =
      "Cuenca del Gualeguay - NMDS y variables climáticas",
    
    subtitle = paste0(
      "Bray-Curtis | Stress = ",
      round(stress, 3),
      " | clima ajustado sobre centroides de estación | * p ≤ 0,05"
    ),
    
    x = "NMDS1",
    
    y = "NMDS2",
    
    color = "Estación",
    
    shape = "Perfil"
  )


############################################################
# ==========================================================
# 44. VARIABLES EDÁFICAS
# ==========================================================
############################################################


############################################################
# 45. EXTRAER pH, MO Y SÍLICE
############################################################

pH <- a_numerico(
  datos_nmds[["pH (%)"]]
)

MO <- a_numerico(
  datos_nmds[["MO"]]
)

silice <- a_numerico(
  datos_nmds[["Sílice/gramos"]]
)


############################################################
# 46. TRANSFORMAR SÍLICE
############################################################

silice_log <- rep(
  NA_real_,
  length(silice)
)


ok_silice <- is.finite(silice) &
  silice >= 0


silice_log[
  ok_silice
] <- log10(
  silice[
    ok_silice
  ] + 1
)


############################################################
# 47. AJUSTAR pH
############################################################

set.seed(3001)

vector_pH <- ajustar_vector(
  coordenadas = coord,
  variable = pH,
  nombre = "pH",
  permutations = N_PERM
)


############################################################
# 48. AJUSTAR MATERIA ORGÁNICA
############################################################

set.seed(3002)

vector_MO <- ajustar_vector(
  coordenadas = coord,
  variable = MO,
  nombre = "M.O.",
  permutations = N_PERM
)


############################################################
# 49. AJUSTAR SÍLICE
############################################################

set.seed(3003)

vector_silice <- ajustar_vector(
  coordenadas = coord,
  variable = silice_log,
  nombre = "log10(Sílice/g + 1)",
  permutations = N_PERM
)


############################################################
# 50. UNIR VECTORES DE SUELO
############################################################

vectores_suelo <- rbind(
  vector_pH,
  vector_MO,
  vector_silice
)


cat(
  "\nVectores edáficos:\n"
)

print(
  vectores_suelo
)


vectores_suelo_ok <- vectores_suelo[
  vectores_suelo$Estado == "OK",
  ,
  drop = FALSE
]


if(nrow(vectores_suelo_ok) > 0){
  
  vectores_suelo_ok$Significativo <-
    vectores_suelo_ok$p <= ALPHA
  
  
  vectores_suelo_ok$Etiqueta <- ifelse(
    vectores_suelo_ok$Significativo,
    
    paste0(
      vectores_suelo_ok$Variable,
      " *"
    ),
    
    vectores_suelo_ok$Variable
  )
  
  
  vectores_suelo_plot <- escalar_flechas(
    vectores_suelo_ok,
    puntos,
    fraccion = 0.72
  )
  
} else {
  
  vectores_suelo_plot <-
    vectores_suelo_ok
}


############################################################
# 51. TEXTURA COMO FACTOR
############################################################

textura <- trimws(
  as.character(
    datos_nmds$Textura
  )
)


textura[
  textura %in% c(
    "",
    "NA",
    "N/A"
  )
] <- NA


ok_textura <-
  !is.na(textura) &
  is.finite(coord[, 1]) &
  is.finite(coord[, 2])


############################################################
# 52. OBJETOS PARA TEXTURA
############################################################

centroides_textura <- data.frame(
  Textura = character(),
  NMDS1 = numeric(),
  NMDS2 = numeric(),
  r2 = numeric(),
  p = numeric(),
  N = integer(),
  stringsAsFactors = FALSE
)


resultado_textura <- data.frame(
  Variable = character(),
  r2 = numeric(),
  p = numeric(),
  N = integer(),
  stringsAsFactors = FALSE
)


############################################################
# 53. AJUSTAR TEXTURA
############################################################

if(sum(ok_textura) >= 4){
  
  textura_factor <- droplevels(
    factor(
      textura[
        ok_textura
      ]
    )
  )
  
  
  if(nlevels(textura_factor) >= 2){
    
    set.seed(4000)
    
    
    fit_textura <- tryCatch(
      
      vegan::envfit(
        coord[
          ok_textura,
          ,
          drop = FALSE
        ],
        
        textura_factor,
        
        permutations = N_PERM
      ),
      
      error = function(e){
        
        NULL
      }
    )
    
    
    if(!is.null(fit_textura)){
      
      cent <- tryCatch(
        
        as.data.frame(
          scores(
            fit_textura,
            display = "factors"
          )
        ),
        
        error = function(e){
          
          NULL
        }
      )
      
      
      if(
        !is.null(cent) &&
        nrow(cent) > 0
      ){
        
        names(cent)[1:2] <- c(
          "NMDS1",
          "NMDS2"
        )
        
        
        centroides_textura <- data.frame(
          Textura = rownames(cent),
          
          NMDS1 = cent$NMDS1,
          
          NMDS2 = cent$NMDS2,
          
          r2 = as.numeric(
            fit_textura$factors$r[1]
          ),
          
          p = as.numeric(
            fit_textura$factors$pvals[1]
          ),
          
          N = sum(ok_textura),
          
          stringsAsFactors = FALSE
        )
        
        
        resultado_textura <- data.frame(
          Variable = "Textura",
          
          r2 = as.numeric(
            fit_textura$factors$r[1]
          ),
          
          p = as.numeric(
            fit_textura$factors$pvals[1]
          ),
          
          N = sum(ok_textura),
          
          stringsAsFactors = FALSE
        )
      }
    }
  }
}


############################################################
# 54. GRÁFICO 3
# NMDS + VARIABLES EDÁFICAS
############################################################

g_suelo <- grafico_base()


if(nrow(vectores_suelo_plot) > 0){
  
  g_suelo <- g_suelo +
    
    geom_segment(
      data = vectores_suelo_plot,
      
      aes(
        x = 0,
        y = 0,
        xend = NMDS1_plot,
        yend = NMDS2_plot
      ),
      
      inherit.aes = FALSE,
      
      arrow = arrow(
        length = grid::unit(
          0.20,
          "cm"
        )
      ),
      
      linewidth = 0.8
    ) +
    
    geom_text_repel(
      data = vectores_suelo_plot,
      
      aes(
        x = NMDS1_plot,
        y = NMDS2_plot,
        label = Etiqueta
      ),
      
      inherit.aes = FALSE,
      
      size = 3.7,
      
      max.overlaps = Inf,
      
      box.padding = 0.5,
      
      min.segment.length = 0
    )
}


############################################################
# 55. AGREGAR CENTROIDES DE TEXTURA
############################################################

if(nrow(centroides_textura) > 0){
  
  g_suelo <- g_suelo +
    
    geom_point(
      data = centroides_textura,
      
      aes(
        x = NMDS1,
        y = NMDS2
      ),
      
      inherit.aes = FALSE,
      
      shape = 4,
      
      size = 3.5,
      
      stroke = 1
    ) +
    
    geom_text_repel(
      data = centroides_textura,
      
      aes(
        x = NMDS1,
        y = NMDS2,
        label = Textura
      ),
      
      inherit.aes = FALSE,
      
      size = 3,
      
      fontface = "italic",
      
      max.overlaps = Inf
    )
}


############################################################
# 56. TERMINAR GRÁFICO DE SUELO
############################################################

g_suelo <- g_suelo +
  
  labs(
    title =
      "Cuenca del Gualeguay - NMDS y variables edáficas",
    
    subtitle = paste0(
      "Bray-Curtis | Stress = ",
      round(stress, 3),
      " | Sílice log10(x+1) | * p ≤ 0,05"
    ),
    
    x = "NMDS1",
    
    y = "NMDS2",
    
    color = "Estación",
    
    shape = "Perfil"
  )


############################################################
# 57. MOSTRAR LOS TRES GRÁFICOS
############################################################

print(
  g_fito
)

print(
  g_clima
)

print(
  g_suelo
)


############################################################
# 58. EXPORTAR SVG
############################################################

ggsave(
  filename = file.path(
    carpeta_resultados,
    "NMDS_Gualeguay_01_Fitolitos.svg"
  ),
  
  plot = g_fito,
  
  width = 12,
  
  height = 8.5,
  
  units = "in"
)


ggsave(
  filename = file.path(
    carpeta_resultados,
    "NMDS_Gualeguay_02_Clima.svg"
  ),
  
  plot = g_clima,
  
  width = 11,
  
  height = 8,
  
  units = "in"
)


ggsave(
  filename = file.path(
    carpeta_resultados,
    "NMDS_Gualeguay_03_Suelo.svg"
  ),
  
  plot = g_suelo,
  
  width = 12,
  
  height = 8.5,
  
  units = "in"
)


############################################################
# 59. EXPORTAR PNG 300 DPI
############################################################

ggsave(
  filename = file.path(
    carpeta_resultados,
    "NMDS_Gualeguay_01_Fitolitos.png"
  ),
  
  plot = g_fito,
  
  width = 12,
  
  height = 8.5,
  
  units = "in",
  
  dpi = 300
)


ggsave(
  filename = file.path(
    carpeta_resultados,
    "NMDS_Gualeguay_02_Clima.png"
  ),
  
  plot = g_clima,
  
  width = 11,
  
  height = 8,
  
  units = "in",
  
  dpi = 300
)


ggsave(
  filename = file.path(
    carpeta_resultados,
    "NMDS_Gualeguay_03_Suelo.png"
  ),
  
  plot = g_suelo,
  
  width = 12,
  
  height = 8.5,
  
  units = "in",
  
  dpi = 300
)


############################################################
# 60. TABLA DE MUESTRAS
############################################################

tabla_muestras <- data.frame(
  Estación = datos_nmds$Estación,
  
  Perfil = datos_nmds$Perfil,
  
  ID = datos_nmds$ID,
  
  NMDS1 = coord[, 1],
  
  NMDS2 = coord[, 2],
  
  stringsAsFactors = FALSE
)


############################################################
# 61. CONTROL DE PERFILES
############################################################

control_perfiles <- as.data.frame(
  table(
    Estación = datos_nmds$Estación,
    Perfil = datos_nmds$Perfil
  ),
  
  stringsAsFactors = FALSE
)


control_perfiles <- control_perfiles[
  control_perfiles$Freq > 0,
  ,
  drop = FALSE
]


names(
  control_perfiles
)[3] <- "N_muestras"


############################################################
# 62. TABLA DE SÍMBOLOS
############################################################

mapa_simbolos <- data.frame(
  Perfil = names(
    simbolos_perfil
  ),
  
  Simbolo_ggplot = as.numeric(
    simbolos_perfil
  ),
  
  stringsAsFactors = FALSE
)


############################################################
# 63. TABLA DE MORFOTIPOS SIN VARIACIÓN
############################################################

tabla_sin_variacion <- data.frame(
  Morfotipo = morfotipos_sin_variacion,
  
  Motivo = rep(
    "Sin variación entre muestras",
    length(
      morfotipos_sin_variacion
    )
  ),
  
  stringsAsFactors = FALSE
)


############################################################
# 64. RESUMEN DEL NMDS
############################################################

n_perfiles_reales <- nrow(
  unique(
    datos_nmds[
      ,
      c(
        "Estación",
        "Perfil"
      )
    ]
  )
)


resumen <- data.frame(
  Cuenca = "Gualeguay",
  
  Hoja = "Gualeguay",
  
  N_muestras = nrow(coord),
  
  N_estaciones = length(
    unique(
      datos_nmds$Estación
    )
  ),
  
  N_perfiles = n_perfiles_reales,
  
  N_morfotipos = ncol(
    fito_nmds
  ),
  
  Distancia = "Bray-Curtis",
  
  Dimensiones = 2,
  
  Stress = stress,
  
  stringsAsFactors = FALSE
)


############################################################
# 65. PARÁMETROS
############################################################

parametros <- data.frame(
  Parámetro = c(
    "Base analizada",
    "Unidad gráfica",
    "Color",
    "Símbolo",
    "Estandarización",
    "Distancia",
    "Dimensiones NMDS",
    "trymax",
    "Permutaciones",
    "Alpha",
    "Vectores fitolíticos",
    "Vectores climáticos",
    "Sílice",
    "Textura"
  ),
  
  Valor = c(
    "Gualeguay",
    "Muestra",
    "Estación",
    "Perfil",
    "Abundancia relativa; suma por muestra = 1",
    "Bray-Curtis",
    "2",
    "500",
    as.character(N_PERM),
    as.character(ALPHA),
    
    paste0(
      "Significativos + morfotipos clave + top ",
      TOP_R2,
      " por r2"
    ),
    
    "TMA y PMA ajustadas sobre centroides NMDS de las estaciones",
    
    "log10(Sílice/gramos + 1)",
    
    "Factor representado mediante centroides"
  ),
  
  stringsAsFactors = FALSE
)


############################################################
# 66. EXPORTAR EXCEL
############################################################

archivo_excel <- file.path(
  carpeta_resultados,
  "Resultados_NMDS_Gualeguay.xlsx"
)


write_xlsx(
  list(
    Resumen_NMDS =
      resumen,
    
    Muestras_NMDS =
      tabla_muestras,
    
    Control_perfiles =
      control_perfiles,
    
    Simbolos_perfil =
      mapa_simbolos,
    
    Vectores_fito_todos =
      vectores_fito,
    
    Vectores_fito_OK =
      vectores_fito_ok,
    
    Vectores_fito_figura =
      vectores_fito_plot,
    
    Fito_sin_variacion =
      tabla_sin_variacion,
    
    Clima_por_estacion =
      clima_fit,
    
    Vectores_clima =
      vectores_clima,
    
    Vectores_suelo =
      vectores_suelo,
    
    Resultado_textura =
      resultado_textura,
    
    Centroides_textura =
      centroides_textura,
    
    Parametros =
      parametros
  ),
  
  archivo_excel
)


############################################################
# 66B. CONTROLAR QUE EL EXCEL SE HAYA CREADO
############################################################

if(!file.exists(archivo_excel)){
  stop(
    paste(
      "No se pudo crear el archivo Excel:",
      archivo_excel
    )
  )
}

cat(
  "\nExcel de resultados creado correctamente en:\n",
  normalizePath(archivo_excel, winslash = "/", mustWork = TRUE),
  "\n"
)


############################################################
# 67. RESULTADO FINAL
############################################################

cat(
  "\n\n============================================\n"
)

cat(
  "NMDS CUENCA DEL GUALEGUAY FINALIZADO\n"
)

cat(
  "============================================\n"
)

cat(
  "Muestras utilizadas:",
  nrow(coord),
  "\n"
)

cat(
  "Estaciones:",
  resumen$N_estaciones,
  "\n"
)

cat(
  "Perfiles:",
  resumen$N_perfiles,
  "\n"
)

cat(
  "Morfotipos utilizados:",
  ncol(fito_nmds),
  "\n"
)

cat(
  "Stress:",
  round(stress, 4),
  "\n"
)

cat(
  "Vectores fitolíticos mostrados:",
  nrow(vectores_fito_plot),
  "\n"
)

cat(
  "Vectores fitolíticos significativos:",
  sum(
    vectores_fito_ok$p <= ALPHA,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "\nVectores climáticos:\n"
)

print(
  vectores_clima[
    ,
    c(
      "Variable",
      "r2",
      "p",
      "N",
      "Estado"
    )
  ]
)

cat(
  "\nVectores edáficos:\n"
)

print(
  vectores_suelo[
    ,
    c(
      "Variable",
      "r2",
      "p",
      "N",
      "Estado"
    )
  ]
)

cat(
  "\nColor = Estación\n"
)

cat(
  "Símbolo = Perfil\n"
)

cat(
  "\nResultados guardados en:\n"
)

cat(
  carpeta_resultados,
  "\n"
)

cat(
  "\nArchivo Excel:\n",
  archivo_excel,
  "\n"
)

cat(
  "============================================\n"
)
)