# ===================================================================
# MS3 | Diversidad beta / Bray-Curtis entre estaciones — Parana
# Alcance: Entrada: hoja Filtro Paraná.
# Nombre normalizado para el repositorio; analisis sin cambios.
# ===================================================================

############################################################
# DIVERSIDAD BETA Y RECAMBIO COMPOSICIONAL ENTRE ESTACIONES
# CUENCA DEL PARANÁ - HOJA "Filtro Paraná"
#
# Unidad de comparación:
# Estaciones integradas a partir de la MEDIA de las abundancias
# relativas de las muestras de cada estación.
#
# IMPORTANTE:
# 1) Se normaliza CADA MUESTRA a suma = 1.
# 2) Luego se promedian los morfotipos por estación.
# 3) Finalmente se calcula Bray-Curtis entre estaciones.
############################################################

#-----------------------------------------------------------
# 1. PAQUETES
#-----------------------------------------------------------

paquetes <- c(
  "readxl",
  "vegan",
  "dplyr",
  "tidyr",
  "ggplot2",
  "pheatmap",
  "writexl",
  "svglite"
)

instalar <- paquetes[
  !(paquetes %in% installed.packages()[,"Package"])
]

if(length(instalar) > 0){
  install.packages(
    instalar,
    repos = "https://cloud.r-project.org"
  )
}

library(readxl)
library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pheatmap)
library(writexl)
library(svglite)


#-----------------------------------------------------------
# 2. IMPORTAR BASE FILTRADA
#-----------------------------------------------------------

archivo <- file.choose()

hojas <- excel_sheets(archivo)

if(!"Filtro Paraná" %in% hojas){
  stop("No se encontró la hoja 'Filtro Paraná'.")
}

datos <- read_excel(
  archivo,
  sheet = "Filtro Paraná",
  na = c("", "NA", "N/A", "NaN", "Inf", "-Inf")
)

datos <- as.data.frame(
  datos,
  check.names = FALSE
)

cat("\nMuestras en Filtro Paraná:", nrow(datos), "\n")
cat("Estaciones:", length(unique(datos$Estación)), "\n")


#-----------------------------------------------------------
# 3. CARPETA DE RESULTADOS
#-----------------------------------------------------------

sello <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

carpeta_resultados <- file.path(
  dirname(archivo),
  paste0("Bray_Curtis_Filtro_Parana_", sello)
)

dir.create(
  carpeta_resultados,
  showWarnings = FALSE
)

cat(
  "\nResultados en:\n",
  carpeta_resultados,
  "\n"
)


#-----------------------------------------------------------
# 4. FUNCIÓN NUMÉRICA SEGURA
#-----------------------------------------------------------

a_numerico <- function(x){
  
  if(is.numeric(x)){
    return(as.numeric(x))
  }
  
  x <- trimws(as.character(x))
  
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


#-----------------------------------------------------------
# 5. IDENTIFICAR MORFOTIPOS
#-----------------------------------------------------------

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

vars_fito <- names(datos)[inicio:fin]

cat(
  "\nMorfotipos utilizados:",
  length(vars_fito),
  "\n"
)


#-----------------------------------------------------------
# 6. MATRIZ DE SILICOFITOLITOS POR MUESTRA
#-----------------------------------------------------------

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

# En la matriz fitolítica, las celdas vacías se interpretan
# como ausencia del morfotipo.
fito[
  is.na(fito)
] <- 0

mat_fito <- as.matrix(
  fito
)

storage.mode(
  mat_fito
) <- "double"

if(any(!is.finite(mat_fito))){
  stop("Persisten NA/NaN/Inf en la matriz fitolítica.")
}

if(any(mat_fito < 0)){
  stop("Existen valores negativos en la matriz fitolítica.")
}


#-----------------------------------------------------------
# 7. ELIMINAR MUESTRAS VACÍAS
#-----------------------------------------------------------

suma_filas <- rowSums(
  mat_fito
)

usar <- is.finite(suma_filas) &
  suma_filas > 0

if(any(!usar)){
  
  cat(
    "\nMuestras eliminadas por suma cero:",
    sum(!usar),
    "\n"
  )
  
  print(
    datos[
      !usar,
      c(
        "Estación",
        "ID"
      )
    ]
  )
}

datos_bray <- datos[
  usar,
  ,
  drop = FALSE
]

mat_fito <- mat_fito[
  usar,
  ,
  drop = FALSE
]

suma_filas <- suma_filas[
  usar
]


#-----------------------------------------------------------
# 8. NORMALIZAR CADA MUESTRA
#-----------------------------------------------------------
#
# Este paso es fundamental porque la hoja contiene valores
# expresados con escalas distintas entre filas.
# Todas las muestras pasan primero a suma = 1.
#-----------------------------------------------------------

mat_rel_muestras <- decostand(
  mat_fito,
  method = "total"
)

if(any(!is.finite(mat_rel_muestras))){
  stop(
    "La matriz de abundancias relativas contiene valores no finitos."
  )
}


#-----------------------------------------------------------
# 9. MEDIA DE ABUNDANCIAS RELATIVAS POR ESTACIÓN
#-----------------------------------------------------------

tabla_rel_muestras <- data.frame(
  Estación = trimws(
    as.character(
      datos_bray$Estación
    )
  ),
  mat_rel_muestras,
  check.names = FALSE
)

matriz_estacion <- tabla_rel_muestras %>%
  
  group_by(
    Estación
  ) %>%
  
  summarise(
    across(
      all_of(vars_fito),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Guardar explícitamente los nombres de las estaciones.
# Esto evita perderlos al pasar de data.frame/tibble a matriz.

nombres_estaciones <- as.character(
  matriz_estacion$Estación
)

matriz_estacion_num <- matriz_estacion %>%
  select(
    -Estación
  )

matriz_estacion_num <- as.matrix(
  matriz_estacion_num
)

storage.mode(
  matriz_estacion_num
) <- "double"

rownames(
  matriz_estacion_num
) <- nombres_estaciones

# Control adicional: cada estación vuelve a suma = 1.
matriz_relativa <- decostand(
  matriz_estacion_num,
  method = "total"
)

# Asegurar que los nombres de estación permanezcan asociados
# a las filas de la matriz final.
rownames(
  matriz_relativa
) <- nombres_estaciones

cat(
  "\nMatriz integrada por estación:\n"
)

print(
  round(
    matriz_relativa,
    4
  )
)


#-----------------------------------------------------------
# 10. BRAY-CURTIS
#-----------------------------------------------------------

bray <- vegdist(
  matriz_relativa,
  method = "bray"
)

matriz_bray <- as.matrix(
  bray
)

cat(
  "\nDisimilitud Bray-Curtis:\n"
)

print(
  round(
    matriz_bray,
    3
  )
)


#-----------------------------------------------------------
# 11. COMPARACIONES POR PARES
#-----------------------------------------------------------

tabla_pares <- as.data.frame(
  as.table(
    matriz_bray
  )
)

names(
  tabla_pares
) <- c(
  "Estacion1",
  "Estacion2",
  "Bray"
)

tabla_pares <- tabla_pares %>%
  
  filter(
    Estacion1 != Estacion2
  ) %>%
  
  rowwise() %>%
  
  mutate(
    Par = paste(
      sort(
        c(
          as.character(Estacion1),
          as.character(Estacion2)
        )
      ),
      collapse = "_"
    )
  ) %>%
  
  ungroup() %>%
  
  distinct(
    Par,
    .keep_all = TRUE
  ) %>%
  
  select(
    -Par
  ) %>%
  
  arrange(
    Bray
  )


cat(
  "\nMayor similitud (menor Bray):\n"
)

print(
  head(
    tabla_pares,
    10
  )
)


cat(
  "\nMayor recambio (mayor Bray):\n"
)

print(
  tabla_pares %>%
    arrange(
      desc(Bray)
    ) %>%
    head(10)
)


#-----------------------------------------------------------
# 12. AGRUPAMIENTO
#-----------------------------------------------------------

arbol <- hclust(
  bray,
  method = "average"
)


#-----------------------------------------------------------
# 13. HEATMAP
#-----------------------------------------------------------

png(
  filename = file.path(
    carpeta_resultados,
    "Bray_Curtis_Parana_Heatmap.png"
  ),
  width = 2600,
  height = 2200,
  res = 300
)

pheatmap(
  matriz_bray,
  cluster_rows = arbol,
  cluster_cols = arbol,
  display_numbers = TRUE,
  number_format = "%.3f",
  main =
    "Cuenca del Paraná - Disimilitud Bray-Curtis entre estaciones"
)

dev.off()


svg(
  filename = file.path(
    carpeta_resultados,
    "Bray_Curtis_Parana_Heatmap.svg"
  ),
  width = 9,
  height = 8
)

pheatmap(
  matriz_bray,
  cluster_rows = arbol,
  cluster_cols = arbol,
  display_numbers = TRUE,
  number_format = "%.3f",
  main =
    "Cuenca del Paraná - Disimilitud Bray-Curtis entre estaciones"
)

dev.off()


#-----------------------------------------------------------
# 14. DENDROGRAMA
#-----------------------------------------------------------

png(
  filename = file.path(
    carpeta_resultados,
    "Bray_Curtis_Parana_Dendrograma.png"
  ),
  width = 2600,
  height = 1800,
  res = 300
)

plot(
  arbol,
  main =
    "Cuenca del Paraná - Agrupamiento de estaciones por Bray-Curtis",
  xlab = "",
  sub = "",
  ylab = "Disimilitud Bray-Curtis"
)

dev.off()


svg(
  filename = file.path(
    carpeta_resultados,
    "Bray_Curtis_Parana_Dendrograma.svg"
  ),
  width = 9,
  height = 6
)

plot(
  arbol,
  main =
    "Cuenca del Paraná - Agrupamiento de estaciones por Bray-Curtis",
  xlab = "",
  sub = "",
  ylab = "Disimilitud Bray-Curtis"
)

dev.off()


#-----------------------------------------------------------
# 15. TABLAS PARA EXPORTACIÓN
#-----------------------------------------------------------

tabla_matriz_relativa <- data.frame(
  Estación = nombres_estaciones,
  as.data.frame(
    matriz_relativa,
    check.names = FALSE
  ),
  check.names = FALSE
)

tabla_bray <- data.frame(
  Estación = rownames(
    matriz_bray
  ),
  as.data.frame(
    matriz_bray,
    check.names = FALSE
  ),
  check.names = FALSE
)

muestras_por_estacion <- as.data.frame(
  table(
    Estación = datos_bray$Estación
  ),
  stringsAsFactors = FALSE
)

names(
  muestras_por_estacion
)[2] <- "N_muestras"

control_muestras <- data.frame(
  Estación = datos_bray$Estación,
  ID = datos_bray$ID,
  Suma_morfotipos_antes_normalizar = suma_filas,
  stringsAsFactors = FALSE
)

parametros <- data.frame(
  Parámetro = c(
    "Cuenca",
    "Hoja",
    "Unidad de comparación",
    "Morfotipos",
    "Normalización",
    "Integración por estación",
    "Distancia",
    "Clustering"
  ),
  Valor = c(
    "Paraná",
    "Filtro Paraná",
    "Estación",
    "Acute bulbosus:Crenate",
    "Cada muestra se transforma a abundancia relativa (suma = 1)",
    "Media de abundancias relativas por morfotipo",
    "Bray-Curtis",
    "UPGMA / average linkage"
  ),
  stringsAsFactors = FALSE
)


#-----------------------------------------------------------
# 16. EXPORTAR EXCEL
#-----------------------------------------------------------

archivo_excel <- file.path(
  carpeta_resultados,
  "Bray_Curtis_Estaciones_Filtro_Parana.xlsx"
)

write_xlsx(
  list(
    Parametros =
      parametros,
    
    Muestras_por_estacion =
      muestras_por_estacion,
    
    Control_muestras =
      control_muestras,
    
    Abundancia_rel_media =
      tabla_matriz_relativa,
    
    Bray_Curtis =
      tabla_bray,
    
    Comparaciones =
      tabla_pares,
    
    Mayor_similitud =
      head(
        tabla_pares,
        10
      ),
    
    Mayor_recambio =
      tabla_pares %>%
      arrange(
        desc(Bray)
      ) %>%
      head(10)
  ),
  archivo_excel
)


#-----------------------------------------------------------
# 17. RESULTADO FINAL
#-----------------------------------------------------------

cat(
  "\n============================================\n"
)

cat(
  "BRAY-CURTIS CUENCA DEL PARANÁ FINALIZADO\n"
)

cat(
  "============================================\n"
)

cat(
  "Muestras utilizadas:",
  nrow(datos_bray),
  "\n"
)

cat(
  "Estaciones:",
  nrow(matriz_relativa),
  "\n"
)

cat(
  "Morfotipos:",
  ncol(matriz_relativa),
  "\n"
)

cat(
  "\nExcel creado en:\n",
  archivo_excel,
  "\n"
)

cat(
  "\nResultados guardados en:\n",
  carpeta_resultados,
  "\n"
)

cat(
  "============================================\n"
)

