# ===================================================================
# MS3 | Funciones de transferencia y validacion
# Alcance: MAT, PLS-R, WA-PLS: no modificar modelos sin comparar resultados.
# Nombre normalizado para el repositorio; analisis sin cambios.
# ===================================================================

# ============================================================
# FUNCIONES DE TRANSFERENCIA - TESIS FREZZIA#
# PREGUNTA CENTRAL DE VALIDACIÓN EXTERNA
# ------------------------------------------------------------
# A partir EXCLUSIVAMENTE de la asociación de silicofitolitos de
# un perfil testigo, ¿qué valor ambiental estima cada función de
# transferencia y qué tan próxima es esa estimación al valor
# ambiental realmente observado?
#
# La hoja "Validación" NUNCA interviene en el ajuste ni en la
# elección de k/componentes. Se usa solamente al final para
# comparar OBSERVADO vs. ESTIMADO.
#
# Para responder la pregunta se exportan dos niveles:
#   a) muestra por muestra: conserva la resolución de pH, MO y sílice;
#   b) resumen por perfil: valor real medio, valor estimado medio,
#      error medio, MAE y RMSE para MAT, PLS-R y WA-PLS.
#

# Versión 2: usa una hoja independiente "Validación"
# MAT vs PLS-R vs WA-PLS
#
# ESTRUCTURA DEL EXCEL
#   "Funciones"    = base moderna de entrenamiento
#   "Calibración"  = asociaciones silicofitolíticas de los perfiles testigo
#   "Validación"   = valores ambientales REALES de esos testigos
#
# La hoja Validación es la fuente de los valores observados contra
# los que se comparan las predicciones. El emparejamiento se hace
# por Estación + ID, nunca por el orden de las filas.
#
# Variables objetivo:
#   1) TMA
#   2) PMA
#   3) pH
#   4) Materia orgánica
#   5) log10(Sílice/gramos + 1)
#
# Textura NO se modela como función de transferencia porque
# en esta base es categórica/ordinal.
#
# VALIDACIÓN INTERNA
# - TMA y PMA: leave-one-station-out.
# - pH, MO y sílice: leave-one-profile-out.
#
# Esta versión EXIGE una columna "Perfil" en la base. De ese modo
# podemos comparar esta corrida con la anterior, que para las variables
# edáficas utilizó leave-one-station-out como criterio conservador.
#
# VALIDACIÓN EXTERNA
# - Predictores biológicos: hoja "Calibración".
# - Valores reales: hoja "Validación".
# - TMA/PMA: métricas externas a escala de estación.
# - pH/MO/sílice: métricas externas a escala de muestra.
#
# IMPORTANTE
# - Los NA ambientales NO se imputan.
# - Los NA biológicos se interpretan como ausencia (0).
# - La matriz biológica se renormaliza por fila a suma = 1.
# - MAT usa squared chord distance.
# ============================================================


# ------------------------------------------------------------
# 0. PAQUETES
# ------------------------------------------------------------

paquetes <- c(
  "readxl",
  "rioja",
  "dplyr",
  "writexl",
  "stringr",
  "tibble",
  "tidyr"
)

faltantes <- paquetes[!(paquetes %in% rownames(installed.packages()))]

if(length(faltantes) > 0){
  install.packages(
    faltantes,
    dependencies = TRUE,
    repos = "https://cloud.r-project.org"
  )
}

invisible(lapply(paquetes, library, character.only = TRUE))


# ------------------------------------------------------------
# 1. FUNCIONES AUXILIARES
# ------------------------------------------------------------

normalizar_texto <- function(x){
  # Evita problemas de iconv/ASCII en Windows con nombres que llevan tilde.
  # Conserva UTF-8, pasa a minúsculas, quita espacios y reemplaza
  # manualmente los caracteres acentuados más comunes.
  x <- enc2utf8(as.character(x))
  x <- trimws(tolower(x))
  x <- chartr(
    "áéíóúüñ",
    "aeiouun",
    x
  )
  x
}

a_numero <- function(x){
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub(",", ".", x, fixed = TRUE)
  x[x %in% c("", "NA", "N/A", "S/D", "SD", "N.D.", "ND", "-", "--")] <- NA
  suppressWarnings(as.numeric(x))
}

buscar_columna <- function(nombres, patrones, obligatoria = TRUE){
  
  n_norm <- normalizar_texto(nombres)
  
  for(p in patrones){
    i <- grep(p, n_norm)
    if(length(i) > 0){
      return(nombres[i[1]])
    }
  }
  
  if(obligatoria){
    stop(
      paste0(
        "No se encontró una columna compatible con: ",
        paste(patrones, collapse = " / ")
      )
    )
  }
  
  NA_character_
}

crear_clave <- function(datos, est_col, id_col){
  paste(
    trimws(as.character(datos[[est_col]])),
    trimws(as.character(datos[[id_col]])),
    sep = "||"
  )
}

metricas_basicas <- function(obs, pred){
  
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]
  pred <- pred[ok]
  
  n <- length(obs)
  
  if(n == 0){
    return(
      tibble::tibble(
        N = 0,
        RMSE = NA_real_,
        MAE = NA_real_,
        R2 = NA_real_,
        Bias_pred_menos_obs = NA_real_,
        NRMSE_SD = NA_real_,
        NRMSE_rango = NA_real_
      )
    )
  }
  
  rmse <- sqrt(mean((pred - obs)^2))
  mae <- mean(abs(pred - obs))
  bias <- mean(pred - obs)
  
  r2 <- if(
    n >= 3 &&
    sd(obs, na.rm = TRUE) > 0 &&
    sd(pred, na.rm = TRUE) > 0
  ){
    cor(obs, pred, use = "complete.obs")^2
  } else {
    NA_real_
  }
  
  sd_obs <- sd(obs, na.rm = TRUE)
  rango_obs <- diff(range(obs, na.rm = TRUE))
  
  tibble::tibble(
    N = n,
    RMSE = rmse,
    MAE = mae,
    R2 = r2,
    Bias_pred_menos_obs = bias,
    NRMSE_SD = ifelse(
      is.finite(sd_obs) && sd_obs > 0,
      rmse / sd_obs,
      NA_real_
    ),
    NRMSE_rango = ifelse(
      is.finite(rango_obs) && rango_obs > 0,
      rmse / rango_obs,
      NA_real_
    )
  )
}

metricas_pred_matrix <- function(
    obs,
    pred_matrix,
    nombres_candidatos,
    metodo,
    complejidad,
    configuracion,
    estacion = NULL,
    agregar_por_estacion = FALSE
){
  
  pred_matrix <- as.matrix(pred_matrix)
  
  # Algunos métodos pueden devolver menos columnas que las solicitadas.
  nc <- min(
    ncol(pred_matrix),
    length(nombres_candidatos)
  )
  
  pred_matrix <- pred_matrix[, seq_len(nc), drop = FALSE]
  nombres_candidatos <- nombres_candidatos[seq_len(nc)]
  complejidad <- complejidad[seq_len(nc)]
  configuracion <- configuracion[seq_len(nc)]
  
  salida <- vector("list", nc)
  
  for(j in seq_len(nc)){
    
    pred <- as.numeric(pred_matrix[, j])
    
    if(agregar_por_estacion){
      
      tmp <- tibble::tibble(
        Estacion = estacion,
        Observado = obs,
        Estimado = pred
      ) %>%
        dplyr::filter(
          !is.na(Estacion),
          is.finite(Observado),
          is.finite(Estimado)
        ) %>%
        dplyr::group_by(Estacion) %>%
        dplyr::summarise(
          Observado = mean(Observado, na.rm = TRUE),
          Estimado = mean(Estimado, na.rm = TRUE),
          .groups = "drop"
        )
      
      met <- metricas_basicas(
        tmp$Observado,
        tmp$Estimado
      )
      
    } else {
      
      met <- metricas_basicas(
        obs,
        pred
      )
    }
    
    met$Metodo <- metodo
    met$Candidato <- nombres_candidatos[j]
    met$Complejidad <- complejidad[j]
    met$Configuracion <- configuracion[j]
    
    salida[[j]] <- met
  }
  
  dplyr::bind_rows(salida) %>%
    dplyr::relocate(
      Metodo,
      Candidato,
      Complejidad,
      Configuracion
    )
}

seleccionar_parsimonioso <- function(tabla, tolerancia = 0.01){
  
  tabla_ok <- tabla %>%
    dplyr::filter(is.finite(RMSE))
  
  if(nrow(tabla_ok) == 0){
    return(NULL)
  }
  
  rmse_min <- min(tabla_ok$RMSE, na.rm = TRUE)
  limite <- rmse_min * (1 + tolerancia)
  
  candidatos <- tabla_ok %>%
    dplyr::filter(RMSE <= limite) %>%
    dplyr::arrange(
      Complejidad,
      RMSE,
      dplyr::desc(R2),
      abs(Bias_pred_menos_obs)
    )
  
  candidatos[1, , drop = FALSE]
}

extraer_fit_wapls <- function(pred_obj, componente){
  
  if(is.null(pred_obj) || is.null(pred_obj$fit)){
    return(rep(NA_real_, 0))
  }
  
  z <- pred_obj$fit
  
  if(is.matrix(z) || is.data.frame(z)){
    componente <- min(
      componente,
      ncol(z)
    )
    return(as.numeric(z[, componente]))
  }
  
  as.numeric(z)
}

hacer_grafico_obs_pred <- function(
    df,
    variable,
    archivo_png
){
  
  df <- df %>%
    dplyr::filter(
      is.finite(Observado_original),
      is.finite(Estimado_original)
    )
  
  if(nrow(df) < 2){
    return(invisible(NULL))
  }
  
  png(
    archivo_png,
    width = 1600,
    height = 1200,
    res = 180
  )
  
  rango <- range(
    c(
      df$Observado_original,
      df$Estimado_original
    ),
    na.rm = TRUE
  )
  
  plot(
    df$Observado_original,
    df$Estimado_original,
    xlab = "Observado",
    ylab = "Estimado",
    main = paste0(
      variable,
      " - validación externa"
    ),
    xlim = rango,
    ylim = rango,
    pch = 19
  )
  
  abline(
    0,
    1,
    lty = 2
  )
  
  dev.off()
}


# ------------------------------------------------------------
# 2. CARGAR ARCHIVO Y DETECTAR LAS TRES HOJAS
# ------------------------------------------------------------

archivo <- file.choose()

hojas <- readxl::excel_sheets(archivo)
hojas_norm <- normalizar_texto(hojas)

cat("\nHojas encontradas en el archivo:\n")
print(hojas)

cat("\nHojas normalizadas por el script:\n")
print(hojas_norm)

idx_funciones <- which(
  hojas_norm == "funciones"
)

idx_calibracion <- which(
  hojas_norm == "calibracion"
)

idx_validacion <- which(
  hojas_norm == "validacion"
)

if(length(idx_funciones) == 0){
  stop("No se encontró la hoja 'Funciones'.")
}

if(length(idx_calibracion) == 0){
  stop("No se encontró la hoja 'Calibración'.")
}

if(length(idx_validacion) == 0){
  stop("No se encontró la hoja 'Validación'.")
}

hoja_funciones <- hojas[idx_funciones[1]]
hoja_calibracion <- hojas[idx_calibracion[1]]
hoja_validacion <- hojas[idx_validacion[1]]

cat("\n==================================================\n")
cat("ARCHIVO:", archivo, "\n")
cat("ENTRENAMIENTO:", hoja_funciones, "\n")
cat("TESTIGOS BIOLÓGICOS:", hoja_calibracion, "\n")
cat("VALORES REALES:", hoja_validacion, "\n")
cat("==================================================\n")

train <- readxl::read_excel(
  archivo,
  sheet = hoja_funciones
) %>%
  as.data.frame(check.names = FALSE)

test <- readxl::read_excel(
  archivo,
  sheet = hoja_calibracion
) %>%
  as.data.frame(check.names = FALSE)


# ------------------------------------------------------------
# 3. LEER HOJA VALIDACIÓN
# ------------------------------------------------------------
#
# En el archivo actual la hoja Validación no tiene encabezados.
# El script lo detecta automáticamente:
#
# - si tiene encabezados -> los usa;
# - si NO tiene encabezados y tiene la misma cantidad de columnas
#   que Calibración -> le asigna los encabezados de Calibración.
# ------------------------------------------------------------

validacion_sin_header <- readxl::read_excel(
  archivo,
  sheet = hoja_validacion,
  col_names = FALSE
) %>%
  as.data.frame(check.names = FALSE)

primera_celda <- normalizar_texto(
  validacion_sin_header[1, 1]
)

if(
  primera_celda %in% c(
    "estacion",
    "estación"
  )
){
  
  validacion <- readxl::read_excel(
    archivo,
    sheet = hoja_validacion
  ) %>%
    as.data.frame(check.names = FALSE)
  
  cat(
    "\nLa hoja Validación tiene encabezados: se usaron directamente.\n"
  )
  
} else {
  
  if(ncol(validacion_sin_header) != ncol(test)){
    stop(
      paste0(
        "La hoja Validación no tiene encabezados y posee ",
        ncol(validacion_sin_header),
        " columnas, mientras que Calibración posee ",
        ncol(test),
        ". No puede homologarse automáticamente."
      )
    )
  }
  
  validacion <- validacion_sin_header
  names(validacion) <- names(test)
  
  cat(
    "\nLa hoja Validación no tiene encabezados. ",
    "Se asignaron automáticamente los encabezados de Calibración.\n"
  )
}


# ------------------------------------------------------------
# 4. DETECTAR COLUMNAS
# ------------------------------------------------------------

est_col <- buscar_columna(
  names(train),
  c("^estacion$")
)

id_col <- buscar_columna(
  names(train),
  c("^id$")
)

perfil_col <- buscar_columna(
  names(train),
  c("^perfil$"),
  obligatoria = TRUE
)

textura_col <- buscar_columna(
  names(train),
  c("^textura$")
)

ph_col <- buscar_columna(
  names(train),
  c("^ph", "ph")
)

mo_col <- buscar_columna(
  names(train),
  c("^mo$", "materia organica")
)

tma_col <- buscar_columna(
  names(train),
  c("^tma", "temperatura media anual")
)

pma_col <- buscar_columna(
  names(train),
  c("^pma", "precipitacion media anual")
)

silice_col <- buscar_columna(
  names(train),
  c("silice.*gram", "silice")
)

# Morfotipos: todas las columnas entre ID y Textura.
pos_id <- match(
  id_col,
  names(train)
)

pos_textura <- match(
  textura_col,
  names(train)
)

if(
  is.na(pos_id) ||
  is.na(pos_textura) ||
  pos_textura <= pos_id + 1
){
  stop(
    "No se pudo identificar el bloque de columnas de morfotipos."
  )
}

bio_cols <- names(train)[
  (pos_id + 1):(pos_textura - 1)
]

cat("\nColumnas detectadas:\n")
cat("Estación:", est_col, "\n")
cat("ID:", id_col, "\n")
cat(
  "Perfil:",
  ifelse(
    is.na(perfil_col),
    "NO DISPONIBLE",
    perfil_col
  ),
  "\n"
)
cat("TMA:", tma_col, "\n")
cat("PMA:", pma_col, "\n")
cat("pH:", ph_col, "\n")
cat("MO:", mo_col, "\n")
cat("Sílice:", silice_col, "\n")
cat("Morfotipos:", length(bio_cols), "\n")


# ------------------------------------------------------------
# 5. COMPROBAR COLUMNAS EN CALIBRACIÓN Y VALIDACIÓN
# ------------------------------------------------------------

columnas_test_necesarias <- c(
  est_col,
  id_col,
  bio_cols
)

faltan_test <- setdiff(
  columnas_test_necesarias,
  names(test)
)

if(length(faltan_test) > 0){
  stop(
    paste0(
      "En Calibración faltan columnas: ",
      paste(
        faltan_test,
        collapse = ", "
      )
    )
  )
}

columnas_val_necesarias <- c(
  est_col,
  id_col,
  ph_col,
  mo_col,
  tma_col,
  pma_col,
  silice_col
)

faltan_val <- setdiff(
  columnas_val_necesarias,
  names(validacion)
)

if(length(faltan_val) > 0){
  stop(
    paste0(
      "En Validación faltan columnas: ",
      paste(
        faltan_val,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 6. EMPAREJAR CALIBRACIÓN CON VALIDACIÓN POR ESTACIÓN + ID
# ------------------------------------------------------------

clave_test <- crear_clave(
  test,
  est_col,
  id_col
)

clave_val <- crear_clave(
  validacion,
  est_col,
  id_col
)

if(anyDuplicated(clave_test)){
  duplicados <- unique(
    clave_test[duplicated(clave_test)]
  )
  
  stop(
    paste0(
      "Hay claves Estación+ID duplicadas en Calibración: ",
      paste(
        duplicados,
        collapse = ", "
      )
    )
  )
}

if(anyDuplicated(clave_val)){
  duplicados <- unique(
    clave_val[duplicated(clave_val)]
  )
  
  stop(
    paste0(
      "Hay claves Estación+ID duplicadas en Validación: ",
      paste(
        duplicados,
        collapse = ", "
      )
    )
  )
}

indice_val <- match(
  clave_test,
  clave_val
)

if(any(is.na(indice_val))){
  faltantes <- clave_test[
    is.na(indice_val)
  ]
  
  stop(
    paste0(
      "Hay muestras de Calibración sin valor correspondiente ",
      "en Validación: ",
      paste(
        faltantes,
        collapse = ", "
      )
    )
  )
}

# Reordenar Validación exactamente como Calibración.
observados <- validacion[
  indice_val,
  ,
  drop = FALSE
]

# Control extra: las claves deben coincidir 1 a 1.
if(
  !identical(
    crear_clave(observados, est_col, id_col),
    clave_test
  )
){
  stop(
    "Falló el emparejamiento entre Calibración y Validación."
  )
}

cat(
  "\nEmparejamiento Calibración ↔ Validación:",
  nrow(test),
  "de",
  nrow(test),
  "muestras coincidentes.\n"
)


# ------------------------------------------------------------
# 7. ROTULAR PERFILES TESTIGO
# ------------------------------------------------------------
#
# La base actual no tiene una columna Perfil. Para la salida se
# utilizan las equivalencias definidas para los cuatro testigos.
# Esto NO interviene en el entrenamiento ni en las predicciones.
# ------------------------------------------------------------

test$Perfil_testigo <- paste0(
  test[[est_col]],
  " ",
  sub("^P", "", as.character(test[[perfil_col]]))
)


# ------------------------------------------------------------
# 8. PREPARAR MATRICES BIOLÓGICAS
# ------------------------------------------------------------

preparar_bio <- function(
    datos,
    bio_cols
){
  
  bio <- datos[
    ,
    bio_cols,
    drop = FALSE
  ]
  
  bio <- as.data.frame(
    lapply(
      bio,
      a_numero
    ),
    check.names = FALSE
  )
  
  # NA biológico = ausencia.
  bio[is.na(bio)] <- 0
  
  if(
    any(
      as.matrix(bio) < 0,
      na.rm = TRUE
    )
  ){
    stop(
      "Hay abundancias biológicas negativas."
    )
  }
  
  suma <- rowSums(
    bio,
    na.rm = TRUE
  )
  
  valida <- is.finite(suma) & suma > 0
  
  bio_rel <- bio[
    valida,
    ,
    drop = FALSE
  ]
  
  bio_rel <- sweep(
    bio_rel,
    1,
    rowSums(bio_rel),
    "/"
  )
  
  list(
    bio = bio_rel,
    valida = valida
  )
}

prep_train <- preparar_bio(
  train,
  bio_cols
)

prep_test <- preparar_bio(
  test,
  bio_cols
)

train <- train[
  prep_train$valida,
  ,
  drop = FALSE
]

test <- test[
  prep_test$valida,
  ,
  drop = FALSE
]

# Aplicar el mismo filtro y orden a los valores observados.
observados <- observados[
  prep_test$valida,
  ,
  drop = FALSE
]

bio_train_total <- prep_train$bio
bio_test_total <- prep_test$bio

rownames(bio_train_total) <- make.unique(
  paste0(
    train[[est_col]],
    "_",
    train[[id_col]]
  )
)

rownames(bio_test_total) <- make.unique(
  paste0(
    test[[est_col]],
    "_",
    test[[id_col]]
  )
)

cat("\nMuestras biológicas válidas:\n")
cat(
  "Entrenamiento:",
  nrow(bio_train_total),
  "\n"
)
cat(
  "Testigos:",
  nrow(bio_test_total),
  "\n"
)

perfiles_train_unicos <- unique(
  paste(
    train[[est_col]],
    train[[perfil_col]],
    sep = " | "
  )
)

cat(
  "Perfiles de entrenamiento:",
  length(perfiles_train_unicos),
  "\n"
)


# ------------------------------------------------------------
# 9. PREPARAR VARIABLES AMBIENTALES
# ------------------------------------------------------------

variables_ambientales <- c(
  ph_col,
  mo_col,
  tma_col,
  pma_col,
  silice_col
)

for(v in variables_ambientales){
  
  train[[v]] <- a_numero(
    train[[v]]
  )
  
  observados[[v]] <- a_numero(
    observados[[v]]
  )
}


# ------------------------------------------------------------
# 10. ESPECIFICACIONES DE LAS CINCO VARIABLES
# ------------------------------------------------------------

especificaciones <- list(
  
  TMA = list(
    columna = tma_col,
    escala = "climatica",
    transformacion = "ninguna"
  ),
  
  PMA = list(
    columna = pma_col,
    escala = "climatica",
    transformacion = "ninguna"
  ),
  
  pH = list(
    columna = ph_col,
    escala = "edafica",
    transformacion = "ninguna"
  ),
  
  MO = list(
    columna = mo_col,
    escala = "edafica",
    transformacion = "ninguna"
  ),
  
  Silice_log10 = list(
    columna = silice_col,
    escala = "edafica",
    transformacion = "log10(x + 1)"
  )
)

transformar_objetivo <- function(
    x,
    transformacion
){
  
  if(
    transformacion == "log10(x + 1)"
  ){
    
    x[x < 0] <- NA
    
    return(
      log10(x + 1)
    )
  }
  
  x
}

retrotransformar_objetivo <- function(
    x,
    transformacion
){
  
  if(
    transformacion == "log10(x + 1)"
  ){
    
    return(
      (10^x) - 1
    )
  }
  
  x
}



# ------------------------------------------------------------
# 10 bis. FUNCIONES PARA RESUMIR LA VALIDACIÓN EXTERNA
# ------------------------------------------------------------

media_finita <- function(x){
  x <- x[is.finite(x)]
  if(length(x) == 0) return(NA_real_)
  mean(x)
}

min_finito <- function(x){
  x <- x[is.finite(x)]
  if(length(x) == 0) return(NA_real_)
  min(x)
}

max_finito <- function(x){
  x <- x[is.finite(x)]
  if(length(x) == 0) return(NA_real_)
  max(x)
}

mae_vec <- function(obs, pred){
  ok <- is.finite(obs) & is.finite(pred)
  if(!any(ok)) return(NA_real_)
  mean(abs(pred[ok] - obs[ok]))
}

rmse_vec <- function(obs, pred){
  ok <- is.finite(obs) & is.finite(pred)
  if(!any(ok)) return(NA_real_)
  sqrt(mean((pred[ok] - obs[ok])^2))
}

bias_vec <- function(obs, pred){
  ok <- is.finite(obs) & is.finite(pred)
  if(!any(ok)) return(NA_real_)
  mean(pred[ok] - obs[ok])
}


# ------------------------------------------------------------
# 11. CARPETA DE SALIDA
# ------------------------------------------------------------

carpeta_base <- dirname(
  archivo
)

marca_tiempo <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

carpeta_salida <- file.path(
  carpeta_base,
  paste0(
    "Funciones_transferencia_",
    marca_tiempo
  )
)

dir.create(
  carpeta_salida,
  showWarnings = FALSE,
  recursive = TRUE
)

cat(
  "\nCarpeta de salida:\n",
  carpeta_salida,
  "\n"
)


# ------------------------------------------------------------
# 12. EVALUAR UNA VARIABLE
# ------------------------------------------------------------

evaluar_variable <- function(
    nombre_variable,
    spec
){
  
  cat("\n==================================================\n")
  cat(
    "VARIABLE:",
    nombre_variable,
    "\n"
  )
  cat("==================================================\n")
  
  col_y <- spec$columna
  
  # TRAIN usa los datos ambientales de Funciones.
  y_train_original <- train[[col_y]]
  
  # TEST usa EXCLUSIVAMENTE los valores reales de Validación.
  y_test_original <- observados[[col_y]]
  
  y_train_modelo <- transformar_objetivo(
    y_train_original,
    spec$transformacion
  )
  
  y_test_modelo <- transformar_objetivo(
    y_test_original,
    spec$transformacion
  )
  
  idx_train <- is.finite(
    y_train_modelo
  )
  
  if(sum(idx_train) < 20){
    cat(
      "Variable omitida: menos de 20 muestras con dato ambiental.\n"
    )
    return(NULL)
  }
  
  bio_train <- bio_train_total[
    idx_train,
    ,
    drop = FALSE
  ]
  
  y_train <- y_train_modelo[
    idx_train
  ]
  
  meta_train <- train[
    idx_train,
    ,
    drop = FALSE
  ]
  
  # Quitar morfotipos ausentes en el TRAIN específico.
  taxa_validos <- colSums(
    bio_train,
    na.rm = TRUE
  ) > 0
  
  bio_train <- bio_train[
    ,
    taxa_validos,
    drop = FALSE
  ]
  
  bio_test <- bio_test_total[
    ,
    colnames(bio_train),
    drop = FALSE
  ]
  
  
  # ----------------------------------------------------------
  # Grupos de validación interna
  # ----------------------------------------------------------
  
  if(
    spec$escala == "climatica"
  ){
    
    grupo_texto <- as.character(
      meta_train[[est_col]]
    )
    
    estrategia_cv <- "Leave-one-station-out"
    
  } else {
    
    grupo_texto <- interaction(
      meta_train[[est_col]],
      meta_train[[perfil_col]],
      drop = TRUE
    )
    
    estrategia_cv <- "Leave-one-profile-out"
  }
  
  grupos_cv <- as.integer(
    factor(grupo_texto)
  )
  
  n_grupos <- length(
    unique(grupos_cv)
  )
  
  cat(
    "N entrenamiento:",
    nrow(bio_train),
    "\n"
  )
  cat(
    "N grupos CV:",
    n_grupos,
    "\n"
  )
  cat(
    "Estrategia:",
    estrategia_cv,
    "\n"
  )
  cat(
    "Morfotipos:",
    ncol(bio_train),
    "\n"
  )
  cat(
    "N testigos:",
    nrow(bio_test),
    "\n"
  )
  cat(
    "Testigos con valor REAL en Validación:",
    sum(
      is.finite(y_test_modelo)
    ),
    "\n"
  )
  
  if(n_grupos < 4){
    stop(
      paste0(
        "Hay menos de 4 grupos para validación en ",
        nombre_variable,
        "."
      )
    )
  }
  
  agregar_estacion_cv <- (
    spec$escala == "climatica"
  )
  
  resultados_cv <- list()
  mejores_metodo <- list()
  predicciones_ext <- list()
  diagnosticos_mat <- list()
  
  
  # ==========================================================
  # A. MAT
  # ==========================================================
  
  k_max <- min(
    10,
    nrow(bio_train) -
      max(table(grupos_cv))
  )
  
  if(k_max >= 1){
    
    mat_fit <- tryCatch(
      rioja::MAT(
        y = bio_train,
        x = y_train,
        dist.method = "sq.chord",
        k = k_max,
        lean = FALSE
      ),
      error = function(e){
        cat(
          "Error MAT ajuste:",
          e$message,
          "\n"
        )
        NULL
      }
    )
    
    if(!is.null(mat_fit)){
      
      mat_cv <- tryCatch(
        rioja::crossval(
          mat_fit,
          cv.method = "lgo",
          ngroups = grupos_cv,
          verbose = FALSE
        ),
        error = function(e){
          cat(
            "Error MAT validación:",
            e$message,
            "\n"
          )
          NULL
        }
      )
      
      if(
        !is.null(mat_cv) &&
        !is.null(mat_cv$predicted)
      ){
        
        n_pred_mat <- ncol(
          as.matrix(mat_cv$predicted)
        )
        
        # MAT suele devolver media simple y ponderada.
        if(n_pred_mat == 2 * k_max){
          
          nombres_mat <- c(
            paste0(
              "MAT_k",
              seq_len(k_max),
              "_media"
            ),
            paste0(
              "MAT_k",
              seq_len(k_max),
              "_ponderada"
            )
          )
          
          complejidad_mat <- c(
            seq_len(k_max),
            seq_len(k_max)
          )
          
          configuracion_mat <- c(
            paste0(
              "k=",
              seq_len(k_max),
              "; media"
            ),
            paste0(
              "k=",
              seq_len(k_max),
              "; ponderada"
            )
          )
          
        } else {
          
          nombres_mat <- paste0(
            "MAT_candidato_",
            seq_len(n_pred_mat)
          )
          
          complejidad_mat <- seq_len(
            n_pred_mat
          )
          
          configuracion_mat <- nombres_mat
        }
        
        tabla_mat <- metricas_pred_matrix(
          obs = y_train,
          pred_matrix = mat_cv$predicted,
          nombres_candidatos = nombres_mat,
          metodo = "MAT",
          complejidad = complejidad_mat,
          configuracion = configuracion_mat,
          estacion = meta_train[[est_col]],
          agregar_por_estacion = agregar_estacion_cv
        )
        
        tabla_mat$Variable <- nombre_variable
        tabla_mat$Estrategia_CV <- estrategia_cv
        tabla_mat$N_grupos_CV <- n_grupos
        
        resultados_cv$MAT <- tabla_mat
        
        mejor_mat <- seleccionar_parsimonioso(
          tabla_mat,
          tolerancia = 0.01
        )
        
        mejores_metodo$MAT <- mejor_mat
        
        if(!is.null(mejor_mat)){
          
          k_opt <- as.integer(
            mejor_mat$Complejidad[1]
          )
          
          usar_ponderada <- grepl(
            "ponderada",
            mejor_mat$Configuracion[1],
            fixed = TRUE
          )
          
          pred_mat_obj <- tryCatch(
            predict(
              mat_fit,
              newdata = bio_test,
              k = k_opt,
              match.data = TRUE,
              verbose = FALSE,
              lean = FALSE
            ),
            error = function(e){
              cat(
                "Error MAT predicción externa:",
                e$message,
                "\n"
              )
              NULL
            }
          )
          
          if(!is.null(pred_mat_obj)){
            
            fit_mat <- pred_mat_obj$fit
            
            if(
              is.matrix(fit_mat) ||
              is.data.frame(fit_mat)
            ){
              
              nombres_fit <- colnames(
                fit_mat
              )
              
              if(
                usar_ponderada &&
                "MAT.wm" %in% nombres_fit
              ){
                
                pred_modelo <- as.numeric(
                  fit_mat[, "MAT.wm"]
                )
                
              } else if(
                !usar_ponderada &&
                "MAT" %in% nombres_fit
              ){
                
                pred_modelo <- as.numeric(
                  fit_mat[, "MAT"]
                )
                
              } else {
                
                col_elegida <- ifelse(
                  usar_ponderada &&
                    ncol(fit_mat) >= 2,
                  2,
                  1
                )
                
                pred_modelo <- as.numeric(
                  fit_mat[, col_elegida]
                )
              }
              
            } else {
              
              pred_modelo <- as.numeric(
                fit_mat
              )
            }
            
            pred_original <- retrotransformar_objetivo(
              pred_modelo,
              spec$transformacion
            )
            
            predicciones_ext$MAT <- tibble::tibble(
              Variable = nombre_variable,
              Metodo = "MAT",
              Configuracion = mejor_mat$Configuracion[1],
              Estacion = test[[est_col]],
              Perfil_testigo = test$Perfil_testigo,
              ID = test[[id_col]],
              Fuente_observado = "Validación",
              Observado_modelo = y_test_modelo,
              Estimado_modelo = pred_modelo,
              Observado_original = y_test_original,
              Estimado_original = pred_original,
              Error_pred_menos_obs = (
                pred_original -
                  y_test_original
              )
            )
            
            if(
              !is.null(
                pred_mat_obj$diagnostics
              )
            ){
              
              diag_mat <- as.data.frame(
                pred_mat_obj$diagnostics
              )
              
              diag_mat$Variable <- nombre_variable
              diag_mat$Metodo <- "MAT"
              diag_mat$Configuracion <- mejor_mat$Configuracion[1]
              diag_mat$Estacion <- test[[est_col]]
              diag_mat$Perfil_testigo <- test$Perfil_testigo
              diag_mat$ID <- test[[id_col]]
              
              diagnosticos_mat$MAT <- diag_mat
            }
          }
        }
      }
    }
  }
  
  
  # ==========================================================
  # B. PLS-R
  # ==========================================================
  
  npls_max <- min(
    5,
    nrow(bio_train) - 2,
    ncol(bio_train) - 1
  )
  
  if(npls_max >= 1){
    
    pls_fit <- tryCatch(
      rioja::WAPLS(
        y = bio_train,
        x = y_train,
        npls = npls_max,
        iswapls = FALSE,
        standx = FALSE,
        lean = FALSE
      ),
      error = function(e){
        cat(
          "Error PLS-R ajuste:",
          e$message,
          "\n"
        )
        NULL
      }
    )
    
    if(!is.null(pls_fit)){
      
      pls_cv <- tryCatch(
        rioja::crossval(
          pls_fit,
          cv.method = "lgo",
          ngroups = grupos_cv,
          verbose = FALSE
        ),
        error = function(e){
          cat(
            "Error PLS-R validación:",
            e$message,
            "\n"
          )
          NULL
        }
      )
      
      if(
        !is.null(pls_cv) &&
        !is.null(pls_cv$predicted)
      ){
        
        n_comp_real <- ncol(
          as.matrix(
            pls_cv$predicted
          )
        )
        
        tabla_pls <- metricas_pred_matrix(
          obs = y_train,
          pred_matrix = pls_cv$predicted,
          nombres_candidatos = paste0(
            "PLSR_comp",
            seq_len(n_comp_real)
          ),
          metodo = "PLS-R",
          complejidad = seq_len(
            n_comp_real
          ),
          configuracion = paste0(
            "Componente=",
            seq_len(n_comp_real)
          ),
          estacion = meta_train[[est_col]],
          agregar_por_estacion = agregar_estacion_cv
        )
        
        tabla_pls$Variable <- nombre_variable
        tabla_pls$Estrategia_CV <- estrategia_cv
        tabla_pls$N_grupos_CV <- n_grupos
        
        resultados_cv$PLSR <- tabla_pls
        
        mejor_pls <- seleccionar_parsimonioso(
          tabla_pls,
          tolerancia = 0.01
        )
        
        mejores_metodo$PLSR <- mejor_pls
        
        if(!is.null(mejor_pls)){
          
          comp_opt <- as.integer(
            mejor_pls$Complejidad[1]
          )
          
          pred_pls_obj <- tryCatch(
            predict(
              pls_fit,
              newdata = bio_test,
              match.data = TRUE,
              verbose = FALSE
            ),
            error = function(e){
              cat(
                "Error PLS-R predicción externa:",
                e$message,
                "\n"
              )
              NULL
            }
          )
          
          if(!is.null(pred_pls_obj)){
            
            pred_modelo <- extraer_fit_wapls(
              pred_pls_obj,
              comp_opt
            )
            
            pred_original <- retrotransformar_objetivo(
              pred_modelo,
              spec$transformacion
            )
            
            predicciones_ext$PLSR <- tibble::tibble(
              Variable = nombre_variable,
              Metodo = "PLS-R",
              Configuracion = mejor_pls$Configuracion[1],
              Estacion = test[[est_col]],
              Perfil_testigo = test$Perfil_testigo,
              ID = test[[id_col]],
              Fuente_observado = "Validación",
              Observado_modelo = y_test_modelo,
              Estimado_modelo = pred_modelo,
              Observado_original = y_test_original,
              Estimado_original = pred_original,
              Error_pred_menos_obs = (
                pred_original -
                  y_test_original
              )
            )
          }
        }
      }
    }
  }
  
  
  # ==========================================================
  # C. WA-PLS
  # ==========================================================
  
  if(npls_max >= 1){
    
    wapls_fit <- tryCatch(
      rioja::WAPLS(
        y = bio_train,
        x = y_train,
        npls = npls_max,
        iswapls = TRUE,
        standx = FALSE,
        lean = FALSE
      ),
      error = function(e){
        cat(
          "Error WA-PLS ajuste:",
          e$message,
          "\n"
        )
        NULL
      }
    )
    
    if(!is.null(wapls_fit)){
      
      wapls_cv <- tryCatch(
        rioja::crossval(
          wapls_fit,
          cv.method = "lgo",
          ngroups = grupos_cv,
          verbose = FALSE
        ),
        error = function(e){
          cat(
            "Error WA-PLS validación:",
            e$message,
            "\n"
          )
          NULL
        }
      )
      
      if(
        !is.null(wapls_cv) &&
        !is.null(wapls_cv$predicted)
      ){
        
        n_comp_real <- ncol(
          as.matrix(
            wapls_cv$predicted
          )
        )
        
        tabla_wapls <- metricas_pred_matrix(
          obs = y_train,
          pred_matrix = wapls_cv$predicted,
          nombres_candidatos = paste0(
            "WAPLS_comp",
            seq_len(n_comp_real)
          ),
          metodo = "WA-PLS",
          complejidad = seq_len(
            n_comp_real
          ),
          configuracion = paste0(
            "Componente=",
            seq_len(n_comp_real)
          ),
          estacion = meta_train[[est_col]],
          agregar_por_estacion = agregar_estacion_cv
        )
        
        tabla_wapls$Variable <- nombre_variable
        tabla_wapls$Estrategia_CV <- estrategia_cv
        tabla_wapls$N_grupos_CV <- n_grupos
        
        resultados_cv$WAPLS <- tabla_wapls
        
        mejor_wapls <- seleccionar_parsimonioso(
          tabla_wapls,
          tolerancia = 0.01
        )
        
        mejores_metodo$WAPLS <- mejor_wapls
        
        if(!is.null(mejor_wapls)){
          
          comp_opt <- as.integer(
            mejor_wapls$Complejidad[1]
          )
          
          pred_wapls_obj <- tryCatch(
            predict(
              wapls_fit,
              newdata = bio_test,
              match.data = TRUE,
              verbose = FALSE
            ),
            error = function(e){
              cat(
                "Error WA-PLS predicción externa:",
                e$message,
                "\n"
              )
              NULL
            }
          )
          
          if(!is.null(pred_wapls_obj)){
            
            pred_modelo <- extraer_fit_wapls(
              pred_wapls_obj,
              comp_opt
            )
            
            pred_original <- retrotransformar_objetivo(
              pred_modelo,
              spec$transformacion
            )
            
            predicciones_ext$WAPLS <- tibble::tibble(
              Variable = nombre_variable,
              Metodo = "WA-PLS",
              Configuracion = mejor_wapls$Configuracion[1],
              Estacion = test[[est_col]],
              Perfil_testigo = test$Perfil_testigo,
              ID = test[[id_col]],
              Fuente_observado = "Validación",
              Observado_modelo = y_test_modelo,
              Estimado_modelo = pred_modelo,
              Observado_original = y_test_original,
              Estimado_original = pred_original,
              Error_pred_menos_obs = (
                pred_original -
                  y_test_original
              )
            )
          }
        }
      }
    }
  }
  
  
  # ----------------------------------------------------------
  # UNIR RESULTADOS
  # ----------------------------------------------------------
  
  tabla_cv <- dplyr::bind_rows(
    resultados_cv
  )
  
  mejor_por_metodo <- dplyr::bind_rows(
    mejores_metodo
  )
  
  preds_ext <- dplyr::bind_rows(
    predicciones_ext
  )
  
  diag_mat <- dplyr::bind_rows(
    diagnosticos_mat
  )
  
  
  # ----------------------------------------------------------
  # RESUMEN VALIDACIÓN EXTERNA
  # ----------------------------------------------------------
  
  resumen_ext <- tibble::tibble()
  
  if(nrow(preds_ext) > 0){
    
    metodos <- unique(
      preds_ext$Metodo
    )
    
    resumen_lista <- vector(
      "list",
      length(metodos)
    )
    
    for(i in seq_along(metodos)){
      
      met <- metodos[i]
      
      dd <- preds_ext[
        preds_ext$Metodo == met,
        ,
        drop = FALSE
      ]
      
      if(
        spec$escala == "climatica"
      ){
        
        # Una estación = una unidad de evaluación climática.
        dd_eval <- dd %>%
          dplyr::filter(
            is.finite(Observado_modelo),
            is.finite(Estimado_modelo)
          ) %>%
          dplyr::group_by(Estacion) %>%
          dplyr::summarise(
            Observado_modelo = mean(
              Observado_modelo,
              na.rm = TRUE
            ),
            Estimado_modelo = mean(
              Estimado_modelo,
              na.rm = TRUE
            ),
            Observado_original = mean(
              Observado_original,
              na.rm = TRUE
            ),
            Estimado_original = mean(
              Estimado_original,
              na.rm = TRUE
            ),
            .groups = "drop"
          )
        
        escala_eval <- "estacion"
        
      } else {
        
        dd_eval <- dd
        escala_eval <- "muestra"
      }
      
      met_modelo <- metricas_basicas(
        dd_eval$Observado_modelo,
        dd_eval$Estimado_modelo
      )
      
      names(met_modelo)[
        names(met_modelo) == "RMSE"
      ] <- "RMSE_modelo"
      
      names(met_modelo)[
        names(met_modelo) == "MAE"
      ] <- "MAE_modelo"
      
      names(met_modelo)[
        names(met_modelo) == "R2"
      ] <- "R2_modelo"
      
      names(met_modelo)[
        names(met_modelo) ==
          "Bias_pred_menos_obs"
      ] <- "Bias_modelo"
      
      names(met_modelo)[
        names(met_modelo) ==
          "NRMSE_SD"
      ] <- "NRMSE_SD_modelo"
      
      names(met_modelo)[
        names(met_modelo) ==
          "NRMSE_rango"
      ] <- "NRMSE_rango_modelo"
      
      met_original <- metricas_basicas(
        dd_eval$Observado_original,
        dd_eval$Estimado_original
      )
      
      names(met_original)[
        names(met_original) == "RMSE"
      ] <- "RMSE_original"
      
      names(met_original)[
        names(met_original) == "MAE"
      ] <- "MAE_original"
      
      names(met_original)[
        names(met_original) == "R2"
      ] <- "R2_original"
      
      names(met_original)[
        names(met_original) ==
          "Bias_pred_menos_obs"
      ] <- "Bias_original"
      
      names(met_original)[
        names(met_original) ==
          "NRMSE_SD"
      ] <- "NRMSE_SD_original"
      
      names(met_original)[
        names(met_original) ==
          "NRMSE_rango"
      ] <- "NRMSE_rango_original"
      
      met_original$N <- NULL
      
      resumen_lista[[i]] <- dplyr::bind_cols(
        tibble::tibble(
          Variable = nombre_variable,
          Metodo = met,
          Configuracion = unique(
            dd$Configuracion
          )[1],
          Escala_validacion_externa = escala_eval,
          Fuente_valores_reales = "Hoja Validación",
          Transformacion = spec$transformacion
        ),
        met_modelo,
        met_original
      )
    }
    
    resumen_ext <- dplyr::bind_rows(
      resumen_lista
    )
  }
  
  
  # ----------------------------------------------------------
  # CONTROL POR VARIABLE
  # ----------------------------------------------------------
  
  control <- tibble::tibble(
    Variable = nombre_variable,
    Columna_original = col_y,
    Transformacion = spec$transformacion,
    Escala = spec$escala,
    N_train = nrow(bio_train),
    N_grupos_CV = n_grupos,
    Estrategia_CV = estrategia_cv,
    N_test_total = nrow(bio_test),
    N_test_con_observado_real = sum(
      is.finite(
        y_test_modelo
      )
    ),
    Fuente_test_biologico = "Calibración",
    Fuente_valor_real = "Validación",
    Min_train_original = min(
      y_train_original[idx_train],
      na.rm = TRUE
    ),
    Max_train_original = max(
      y_train_original[idx_train],
      na.rm = TRUE
    )
  )
  
  
  # ----------------------------------------------------------
  # GRÁFICO
  # ----------------------------------------------------------
  
  if(nrow(preds_ext) > 0){
    
    hacer_grafico_obs_pred(
      preds_ext,
      nombre_variable,
      file.path(
        carpeta_salida,
        paste0(
          "Validacion_externa_",
          nombre_variable,
          ".png"
        )
      )
    )
  }
  
  
  list(
    control = control,
    cv = tabla_cv,
    mejor_metodo = mejor_por_metodo,
    pred_ext = preds_ext,
    resumen_ext = resumen_ext,
    diagnosticos_mat = diag_mat
  )
}


# ------------------------------------------------------------
# 13. CORRER LAS CINCO VARIABLES
# ------------------------------------------------------------

resultados <- mapply(
  FUN = evaluar_variable,
  nombre_variable = names(
    especificaciones
  ),
  spec = especificaciones,
  SIMPLIFY = FALSE
)

names(resultados) <- names(
  especificaciones
)

resultados_validos <- resultados[
  !sapply(
    resultados,
    is.null
  )
]

if(length(resultados_validos) == 0){
  stop(
    "No se pudo ajustar ninguna función de transferencia."
  )
}


# ------------------------------------------------------------
# 14. COMBINAR RESULTADOS
# ------------------------------------------------------------

control_variables <- dplyr::bind_rows(
  lapply(
    resultados_validos,
    function(x) x$control
  )
)

cv_todos_candidatos <- dplyr::bind_rows(
  lapply(
    resultados_validos,
    function(x) x$cv
  )
)

mejor_config_metodo <- dplyr::bind_rows(
  lapply(
    resultados_validos,
    function(x) x$mejor_metodo
  )
)

predicciones_testigos <- dplyr::bind_rows(
  lapply(
    resultados_validos,
    function(x) x$pred_ext
  )
)

resumen_externo <- dplyr::bind_rows(
  lapply(
    resultados_validos,
    function(x) x$resumen_ext
  )
)

diagnosticos_mat <- dplyr::bind_rows(
  lapply(
    resultados_validos,
    function(x) x$diagnosticos_mat
  )
)


# ------------------------------------------------------------
# 14 bis. RESPUESTA DIRECTA A LA PREGUNTA CENTRAL
# ------------------------------------------------------------
#
# 1) Comparación muestra por muestra:
#    es la salida más importante para pH, MO y sílice porque esas
#    variables pueden cambiar con la profundidad dentro del perfil.
#
# 2) Comparación resumida por perfil:
#    permite responder en forma sintética cuánto estima cada método
#    y cuánto se aparta de la realidad del perfil testigo.
#
# Los valores reales vienen EXCLUSIVAMENTE de la hoja Validación.
# ------------------------------------------------------------

comparacion_testigos_muestra <- predicciones_testigos %>%
  dplyr::arrange(
    Perfil_testigo,
    Variable,
    Metodo,
    ID
  )


# Valor real del perfil a partir de los datos observados.
# Se usa distinct() para no repetir el mismo observado tres veces
# por la presencia de MAT, PLS-R y WA-PLS.

valores_reales_por_perfil <- comparacion_testigos_muestra %>%
  dplyr::select(
    Estacion,
    Perfil_testigo,
    Variable,
    ID,
    Observado_original
  ) %>%
  dplyr::distinct() %>%
  dplyr::group_by(
    Estacion,
    Perfil_testigo,
    Variable
  ) %>%
  dplyr::summarise(
    N_observado = sum(is.finite(Observado_original)),
    Valor_real_promedio = media_finita(Observado_original),
    Valor_real_min = min_finito(Observado_original),
    Valor_real_max = max_finito(Observado_original),
    .groups = "drop"
  )


# Resumen de la estimación de cada método dentro de cada perfil.
# Para TMA/PMA, donde el valor observado es único por estación/perfil,
# el promedio de las predicciones de las muestras del perfil constituye
# la estimación sintética del perfil.
#
# Para pH/MO/sílice se conserva además la tabla muestra por muestra;
# el promedio de perfil es sólo un resumen y NO sustituye la evaluación
# de cada profundidad.

estimaciones_por_perfil_metodo <- comparacion_testigos_muestra %>%
  dplyr::group_by(
    Estacion,
    Perfil_testigo,
    Variable,
    Metodo,
    Configuracion
  ) %>%
  dplyr::summarise(
    N_comparado = sum(
      is.finite(Observado_original) &
        is.finite(Estimado_original)
    ),
    Valor_estimado_promedio = media_finita(Estimado_original),
    Error_promedio = bias_vec(
      Observado_original,
      Estimado_original
    ),
    MAE_perfil = mae_vec(
      Observado_original,
      Estimado_original
    ),
    RMSE_perfil = rmse_vec(
      Observado_original,
      Estimado_original
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    valores_reales_por_perfil,
    by = c(
      "Estacion",
      "Perfil_testigo",
      "Variable"
    )
  ) %>%
  dplyr::relocate(
    Estacion,
    Perfil_testigo,
    Variable,
    Metodo,
    Configuracion,
    N_observado,
    N_comparado,
    Valor_real_promedio,
    Valor_real_min,
    Valor_real_max,
    Valor_estimado_promedio,
    Error_promedio,
    MAE_perfil,
    RMSE_perfil
  )


# Tabla ancha para lectura inmediata:
# una fila = un perfil testigo x una variable.
# Muestra lado a lado qué estimó MAT, PLS-R y WA-PLS.

estimaciones_wide <- estimaciones_por_perfil_metodo %>%
  dplyr::select(
    Estacion,
    Perfil_testigo,
    Variable,
    Metodo,
    Valor_estimado_promedio,
    Error_promedio,
    MAE_perfil,
    RMSE_perfil
  ) %>%
  tidyr::pivot_wider(
    names_from = Metodo,
    values_from = c(
      Valor_estimado_promedio,
      Error_promedio,
      MAE_perfil,
      RMSE_perfil
    ),
    names_glue = "{.value}_{Metodo}"
  )

tabla_pregunta_central <- valores_reales_por_perfil %>%
  dplyr::left_join(
    estimaciones_wide,
    by = c(
      "Estacion",
      "Perfil_testigo",
      "Variable"
    )
  ) %>%
  dplyr::arrange(
    Perfil_testigo,
    factor(
      Variable,
      levels = c(
        "TMA",
        "PMA",
        "pH",
        "MO",
        "Silice_log10"
      )
    )
  )


# ------------------------------------------------------------
# 15. MEJOR MÉTODO INTERNO POR VARIABLE
# ------------------------------------------------------------

mejor_modelo_interno <- mejor_config_metodo %>%
  dplyr::group_by(Variable) %>%
  dplyr::arrange(
    RMSE,
    dplyr::desc(R2),
    abs(Bias_pred_menos_obs),
    .by_group = TRUE
  ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()


# ------------------------------------------------------------
# 16. COMPARACIÓN FINAL INTERNO + EXTERNO
# ------------------------------------------------------------

comparacion_final <- mejor_config_metodo %>%
  dplyr::select(
    Variable,
    Metodo,
    Configuracion,
    RMSE_interno = RMSE,
    MAE_interno = MAE,
    R2_interno = R2,
    Bias_interno = Bias_pred_menos_obs,
    NRMSE_SD_interno = NRMSE_SD,
    NRMSE_rango_interno = NRMSE_rango,
    Estrategia_CV,
    N_grupos_CV
  ) %>%
  dplyr::left_join(
    resumen_externo %>%
      dplyr::select(
        Variable,
        Metodo,
        Escala_validacion_externa,
        N_externo = N,
        RMSE_externo = RMSE_original,
        MAE_externo = MAE_original,
        R2_externo = R2_original,
        Bias_externo = Bias_original,
        NRMSE_SD_externo = NRMSE_SD_original,
        NRMSE_rango_externo = NRMSE_rango_original
      ),
    by = c(
      "Variable",
      "Metodo"
    )
  )


# ------------------------------------------------------------
# 17. TABLA DE VALORES REALES DE LOS TESTIGOS
# ------------------------------------------------------------

tabla_validacion_real <- tibble::tibble(
  Estacion = observados[[est_col]],
  Perfil_testigo = test$Perfil_testigo,
  ID = observados[[id_col]],
  TMA_real = observados[[tma_col]],
  PMA_real = observados[[pma_col]],
  pH_real = observados[[ph_col]],
  MO_real = observados[[mo_col]],
  Silice_gramos_real = observados[[silice_col]]
)


# ------------------------------------------------------------
# 18. CONTROL DEL EMPAREJAMIENTO
# ------------------------------------------------------------

control_emparejamiento <- tibble::tibble(
  Estacion_calibracion = test[[est_col]],
  ID_calibracion = test[[id_col]],
  Estacion_validacion = observados[[est_col]],
  ID_validacion = observados[[id_col]],
  Coincide = crear_clave(
    test,
    est_col,
    id_col
  ) == crear_clave(
    observados,
    est_col,
    id_col
  )
)


# ------------------------------------------------------------
# 19. AVISOS METODOLÓGICOS
# ------------------------------------------------------------

avisos <- tibble::tibble(
  Tema = c(
    "Pregunta central",
    "Entrenamiento",
    "Testigos biológicos",
    "Valores reales",
    "Emparejamiento",
    "Variables objetivo",
    "Textura",
    "Clima - validación interna",
    "Edáficas - validación interna",
    "Clima - validación externa",
    "Sílice",
    "Sesgo",
    "Selección final"
  ),
  Criterio = c(
    "A partir exclusivamente de la asociación de silicofitolitos de un perfil testigo, se estima cada variable ambiental y se compara la estimación con el valor realmente observado.",
    "La hoja Funciones se utiliza para entrenar los modelos.",
    "La hoja Calibración aporta exclusivamente las asociaciones silicofitolíticas de los perfiles testigo.",
    "La hoja Validación es la fuente de TMA, PMA, pH, MO y sílice observados de los testigos.",
    "Calibración y Validación se unen por Estación + ID; el orden de las filas no interviene.",
    "Se modelan TMA, PMA, pH, MO y sílice amorfa.",
    "No se modela como objetivo porque su codificación es ordinal/categórica.",
    "TMA y PMA usan leave-one-station-out y las métricas internas se calculan a escala de estación.",
    "pH, MO y sílice usan leave-one-profile-out; el grupo se define como Estación + Perfil.",
    "Los perfiles testigo pertenecen a estaciones también presentes en entrenamiento; la evaluación externa climática es complementaria. La validación interna leave-one-station-out es la principal prueba climática.",
    "Se calibra en log10(x + 1) y se exporta también la estimación retrotransformada.",
    "Bias = predicción - observado; positivo = sobreestimación; negativo = subestimación.",
    "La decisión final debe integrar RMSE, R2, sesgo, validación externa y coherencia ecológica."
  )
)


# ------------------------------------------------------------
# 20. EXPORTAR EXCEL
# ------------------------------------------------------------

archivo_salida <- file.path(
  carpeta_salida,
  "Resultados_funciones_transferencia_v5_pregunta_central.xlsx"
)

salidas <- list(
  "PREGUNTA_CENTRAL" = tabla_pregunta_central,
  "Testigos_por_perfil" = estimaciones_por_perfil_metodo,
  "Testigos_por_muestra" = comparacion_testigos_muestra,
  "Resumen_externo_global" = resumen_externo,
  "Comparacion_interno_externo" = comparacion_final,
  "Mejor_config_metodo" = mejor_config_metodo,
  "Mejor_modelo_interno" = mejor_modelo_interno,
  "CV_todos_candidatos" = cv_todos_candidatos,
  "Valores_reales_testigos" = tabla_validacion_real,
  "Control_variables" = control_variables,
  "Control_emparejamiento" = control_emparejamiento,
  "Diagnosticos_MAT" = diagnosticos_mat,
  "Avisos_metodologicos" = avisos
)

salidas <- salidas[
  sapply(
    salidas,
    function(x){
      is.data.frame(x) &&
        nrow(x) > 0
    }
  )
]

writexl::write_xlsx(
  salidas,
  path = archivo_salida
)


# ------------------------------------------------------------
# 21. MOSTRAR RESULTADOS PRINCIPALES
# ------------------------------------------------------------

cat("\n==================================================\n")
cat("PROCESO TERMINADO\n")
cat("==================================================\n")

cat("\nVALORES REALES DE LOS TESTIGOS LEÍDOS DESDE VALIDACIÓN:\n")
print(
  tabla_validacion_real
)

cat("\nCONTROL DE VARIABLES:\n")
print(
  control_variables
)

cat("\nMEJOR CONFIGURACIÓN DE CADA MÉTODO:\n")
print(
  mejor_config_metodo %>%
    dplyr::select(
      Variable,
      Metodo,
      Configuracion,
      N,
      RMSE,
      MAE,
      R2,
      Bias_pred_menos_obs,
      NRMSE_SD,
      Estrategia_CV
    )
)

cat("\nMEJOR MODELO INTERNO POR VARIABLE:\n")
print(
  mejor_modelo_interno %>%
    dplyr::select(
      Variable,
      Metodo,
      Configuracion,
      RMSE,
      R2,
      Bias_pred_menos_obs
    )
)

cat("\nVALIDACIÓN EXTERNA CONTRA LA HOJA VALIDACIÓN:\n")
print(
  resumen_externo %>%
    dplyr::select(
      Variable,
      Metodo,
      Configuracion,
      Escala_validacion_externa,
      N,
      RMSE_original,
      MAE_original,
      R2_original,
      Bias_original
    )
)

cat("\n==================================================\n")
cat("RESPUESTA A LA PREGUNTA CENTRAL\n")
cat("==================================================\n")
cat(
  "\nA partir exclusivamente de los silicofitolitos de cada perfil testigo, ",
  "estas son las estimaciones y su distancia respecto de los valores reales:\n"
)
print(
  tabla_pregunta_central
)

cat("\nCOMPARACIÓN FINAL INTERNA + EXTERNA:\n")
print(
  comparacion_final
)

cat("\nARCHIVO EXCEL:\n")
cat(
  archivo_salida,
  "\n"
)

cat("\nCARPETA DE RESULTADOS:\n")
cat(
  carpeta_salida,
  "\n"
)

if(
  .Platform$OS.type == "windows" &&
  file.exists(archivo_salida)
){
  shell.exec(
    archivo_salida
  )
}

# ============================================================
# FIN
# ============================================================

