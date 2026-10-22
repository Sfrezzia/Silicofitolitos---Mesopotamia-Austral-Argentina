# ===================================================================
# MS3 | Seleccion de variables ambientales y edaficas
# Alcance: Pearson, Spearman, DCA, RDA/CCA, VIF.
# Nombre normalizado para el repositorio; analisis sin cambios.
# ===================================================================

# ============================================================
# TESIS FREZZIA - CAPÍTULO 6.1
# EXPLORACIÓN Y SELECCIÓN DE VARIABLES AMBIENTALES Y EDÁFICAS
# ============================================================
#
# Diseñado para la base actual:
#   Hoja "Funciones"    = base de calibración / entrenamiento
#   Hoja "Calibración" = perfiles testigo reservados
#
# IMPORTANTE:
# - Los testigos NO intervienen en correlaciones, DCA, RDA/CCA,
#   efectos marginales ni VIF.
# - Las asociaciones silicofitolíticas se renormalizan por muestra
#   a suma = 1, porque la base contiene filas en escalas distintas
#   (proporciones, porcentajes y sumas parciales).
# - TMA/PMA/EMA se evalúan a escala de ESTACIÓN para la selección
#   climática, evitando pseudorreplicación y ponderaciones desiguales
#   por diferente número de muestras por estación.
# - Las variables edáficas se evalúan a escala de MUESTRA.
# - La sílice se usa como log10(Sílice/g + 1), coherente con los
#   análisis previos de la tesis. Esto puede desactivarse abajo.
#
# SALIDAS:
# - Excel con diagnósticos, gradientes, correlaciones, DCA,
#   modelos restringidos, efectos marginales y VIF.
# - PNG con matrices de correlación y DCA.
# ============================================================

# ------------------------------------------------------------
# 0. CONFIGURACIÓN
# ------------------------------------------------------------

cat("\nIMPORTANTE: ejecutar este archivo COMPLETO con el botón Source.\n")
cat("No ejecutar solamente el bloque final de exportación.\n\n")

set.seed(1234)
N_PERM <- 999
USAR_SILICE_LOG <- TRUE

# Criterio orientativo DCA ya definido en la metodología:
# < 3 SD  -> RDA
# > 4 SD  -> CCA
# 3-4 SD  -> comparar ambos

# ------------------------------------------------------------
# 1. PAQUETES
# ------------------------------------------------------------

paquetes <- c(
  "readxl",
  "vegan",
  "dplyr",
  "tidyr",
  "writexl",
  "ggplot2",
  "stringr",
  "tibble"
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
# 2. ELEGIR ARCHIVO Y LEER HOJAS
# ------------------------------------------------------------
# Para el punto 6.1 sólo es obligatoria la hoja "Funciones".
# La hoja con perfiles testigo se lee únicamente como CONTROL y
# NO participa en la selección de variables. Por eso, si no está
# presente, el script continúa normalmente.
# ------------------------------------------------------------

archivo <- file.choose()
hojas <- readxl::excel_sheets(archivo)

cat("\n============================================\n")
cat("ARCHIVO SELECCIONADO\n")
cat("============================================\n")
cat(normalizePath(archivo), "\n")
cat("\nHojas disponibles:\n")
print(hojas)

# Normalizar nombres de hojas para tolerar mayúsculas, espacios
# y diferencias de acentuación.
normalizar_hoja <- function(x){
  x <- trimws(as.character(x))
  x <- gsub("[[:space:]]+", " ", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  tolower(x)
}

hojas_norm <- normalizar_hoja(hojas)

# ---- Hoja obligatoria: Funciones ----
idx_funciones <- which(hojas_norm == "funciones")
if(length(idx_funciones) == 0){
  stop(
    paste0(
      "No se encontró la hoja 'Funciones'.\n",
      "Hojas disponibles: ", paste(hojas, collapse = " | ")
    )
  )
}
hoja_funciones <- hojas[idx_funciones[1]]

# ---- Hoja opcional: Calibración / Calibracion / Testigos ----
idx_testigos <- which(
  hojas_norm %in% c("calibracion", "testigos", "perfiles testigo", "perfiles_testigo")
)

datos <- readxl::read_excel(
  archivo,
  sheet = hoja_funciones,
  na = c("", "NA", "N/A", "S/D", "SD", "N.D.", "ND")
)

datos <- as.data.frame(datos, check.names = FALSE)

if(length(idx_testigos) > 0){
  hoja_testigos <- hojas[idx_testigos[1]]
  testigos <- readxl::read_excel(
    archivo,
    sheet = hoja_testigos,
    na = c("", "NA", "N/A", "S/D", "SD", "N.D.", "ND")
  )
  testigos <- as.data.frame(testigos, check.names = FALSE)
  cat("\nHoja de testigos detectada:", hoja_testigos, "\n")
  cat("Testigos reservados - filas:", nrow(testigos),
      " columnas:", ncol(testigos), "\n")
} else {
  testigos <- NULL
  cat(
    "\nAVISO: no se encontró una hoja de testigos. ",
    "Esto NO impide ejecutar 6.1, porque los testigos no deben ",
    "participar en la selección de variables.\n"
  )
}

cat("\nHoja utilizada para selección de variables:", hoja_funciones, "\n")
cat("Base de calibración - filas:", nrow(datos),
    " columnas:", ncol(datos), "\n")

# ------------------------------------------------------------
# 3. CONTROL DE COLUMNAS DE LA BASE ACTUAL
# ------------------------------------------------------------

columnas_obligatorias <- c(
  "Estación",
  "ID",
  "Textura",
  "pH (%)",
  "MO",
  "TMA (º C)",
  "PMA (mm)",
  "Sílice/gramos"
)

faltan <- setdiff(columnas_obligatorias, names(datos))
if(length(faltan) > 0){
  stop(
    paste0(
      "Faltan columnas obligatorias en la hoja Funciones: ",
      paste(faltan, collapse = ", ")
    )
  )
}

# EMA todavía puede no estar incorporada. Se busca de forma flexible.
col_ema <- grep(
  "(^EMA$|EMA.*mm|evapotranspir)",
  names(datos),
  ignore.case = TRUE,
  value = TRUE
)

if(length(col_ema) > 0){
  col_ema <- col_ema[1]
  cat("\nEMA detectada en columna:", col_ema, "\n")
} else {
  col_ema <- NULL
  warning(
    "La base actual no contiene EMA. El análisis climático se ejecutará con TMA y PMA. ",
    "Si luego agregás EMA, el script la incorporará automáticamente."
  )
}

# Los morfotipos se identifican por posición: todo lo ubicado
# entre ID y Textura.
i_id <- match("ID", names(datos))
i_textura <- match("Textura", names(datos))

if(is.na(i_id) || is.na(i_textura) || i_textura <= i_id + 1){
  stop("No fue posible identificar el bloque de morfotipos entre ID y Textura.")
}

bio_cols <- names(datos)[(i_id + 1):(i_textura - 1)]
cat("Morfotipos detectados:", length(bio_cols), "\n")
print(bio_cols)

# ------------------------------------------------------------
# 4. FUNCIONES DE LIMPIEZA
# ------------------------------------------------------------

a_numero <- function(x){
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub(",", ".", x, fixed = TRUE)
  x[toupper(x) %in% c("", "NA", "N/A", "S/D", "SD", "N.D.", "ND")] <- NA
  suppressWarnings(as.numeric(x))
}

primero_no_na <- function(x){
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA_real_)
  x[1]
}

normalizar_textura <- function(x){
  z <- as.character(x)
  z <- iconv(z, from = "", to = "ASCII//TRANSLIT")
  z <- tolower(trimws(z))
  z <- gsub("[-_/]+", " ", z)
  z <- gsub("[[:space:]]+", " ", z)
  z[z %in% c("", "na", "n/a", "s/d", "sd", "n.d.", "nd")] <- NA
  
  dplyr::case_when(
    is.na(z) ~ NA_character_,
    z == "arenoso" ~ "Arenoso",
    z == "franco arenoso" ~ "Franco-arenoso",
    z == "franco" ~ "Franco",
    z %in% c("franco limoso", "franco limosa") ~ "Franco-limoso",
    z == "franco arcilloso" ~ "Franco-arcilloso",
    z %in% c(
      "franco arcillo limoso",
      "franco arcillo limosa",
      "franco arciillo limoso"
    ) ~ "Franco-arcillo-limoso",
    z %in% c("arcillo limoso", "arcillo limosa") ~ "Arcillo-limoso",
    z %in% c("arcilloso", "arcillosa") ~ "Arcilloso",
    TRUE ~ paste0("NO_MAPEADA: ", z)
  )
}

# Escala ordinal EXPLORATORIA de clases más gruesas a más finas.
# Se conserva además la clase textual estandarizada.
diccionario_textura <- tibble::tibble(
  Textura_std = c(
    "Arenoso",
    "Franco-arenoso",
    "Franco",
    "Franco-limoso",
    "Franco-arcilloso",
    "Franco-arcillo-limoso",
    "Arcillo-limoso",
    "Arcilloso"
  ),
  Textura_ord = 0:7
)

mapa_textura <- setNames(
  diccionario_textura$Textura_ord,
  diccionario_textura$Textura_std
)

# ------------------------------------------------------------
# 5. CONVERTIR VARIABLES Y CREAR METADATOS INTERNOS
# ------------------------------------------------------------

vars_num_origen <- c(
  "pH (%)",
  "MO",
  "TMA (º C)",
  "PMA (mm)",
  "Sílice/gramos"
)
if(!is.null(col_ema)) vars_num_origen <- c(vars_num_origen, col_ema)

for(v in vars_num_origen){
  datos[[v]] <- a_numero(datos[[v]])
  if(!is.null(testigos) && v %in% names(testigos)){
    testigos[[v]] <- a_numero(testigos[[v]])
  }
}

# Convertir también los morfotipos a numérico.
for(v in bio_cols){
  datos[[v]] <- a_numero(datos[[v]])
  if(!is.null(testigos) && v %in% names(testigos)){
    testigos[[v]] <- a_numero(testigos[[v]])
  }
}

# Metadatos con nombres simples para los modelos.
meta <- tibble::tibble(
  Estacion = as.character(datos[["Estación"]]),
  ID = as.character(datos[["ID"]]),
  TMA = datos[["TMA (º C)"]],
  PMA = datos[["PMA (mm)"]],
  pH = datos[["pH (%)"]],
  MO = datos[["MO"]],
  Silice_g = datos[["Sílice/gramos"]],
  Textura_original = as.character(datos[["Textura"]])
)

if(!is.null(col_ema)){
  meta$EMA <- datos[[col_ema]]
}

meta$log10_Silice_g <- ifelse(
  !is.na(meta$Silice_g) & meta$Silice_g >= 0,
  log10(meta$Silice_g + 1),
  NA_real_
)

meta$Textura_std <- normalizar_textura(meta$Textura_original)
meta$Textura_ord <- unname(mapa_textura[meta$Textura_std])

texturas_no_mapeadas <- unique(
  meta$Textura_std[grepl("^NO_MAPEADA:", meta$Textura_std)]
)
texturas_no_mapeadas <- texturas_no_mapeadas[!is.na(texturas_no_mapeadas)]

if(length(texturas_no_mapeadas) > 0){
  warning(
    paste0(
      "Hay texturas no mapeadas: ",
      paste(texturas_no_mapeadas, collapse = " | "),
      ". Revisarlas antes de interpretar Textura_ord."
    )
  )
}

# ------------------------------------------------------------
# 6. MATRIZ BIOLÓGICA: CONTROL DE ESCALA Y RENORMALIZACIÓN
# ------------------------------------------------------------

bio_raw <- datos[, bio_cols, drop = FALSE]
bio_raw[is.na(bio_raw)] <- 0
bio_raw <- as.matrix(bio_raw)
storage.mode(bio_raw) <- "double"

suma_original <- rowSums(bio_raw)

control_escala <- tibble::tibble(
  Estacion = meta$Estacion,
  ID = meta$ID,
  Suma_morfotipos_antes = suma_original,
  Escala_aprox = dplyr::case_when(
    abs(suma_original - 1) < 1e-4 ~ "Suma ~ 1",
    abs(suma_original - 100) < 1e-2 ~ "Suma ~ 100",
    suma_original == 0 ~ "Suma = 0",
    TRUE ~ "Suma parcial / otra escala"
  )
)

filas_validas <- suma_original > 0 & is.finite(suma_original)

if(any(!filas_validas)){
  warning(sum(!filas_validas), " filas sin información biológica fueron excluidas.")
}

bio_raw <- bio_raw[filas_validas, , drop = FALSE]
meta <- meta[filas_validas, , drop = FALSE]
control_escala <- control_escala[filas_validas, , drop = FALSE]

# Frecuencia relativa final: cada muestra suma 1.
bio_rel <- vegan::decostand(bio_raw, method = "total")
rownames(bio_rel) <- make.unique(paste(meta$Estacion, meta$ID, sep = " | "))

control_escala$Suma_despues <- rowSums(bio_rel)

cat("\nMuestras de calibración con datos biológicos válidos:", nrow(bio_rel), "\n")
cat("Morfotipos:", ncol(bio_rel), "\n")

# ------------------------------------------------------------
# 7. CONTROL DE TESTIGOS (NO SE USAN PARA SELECCIÓN)
# ------------------------------------------------------------

# La hoja de testigos es opcional para el punto 6.1.
# Se construyen SIEMPRE los objetos de control para que la exportación
# a Excel no falle aunque la hoja no exista o no se utilice.
ids_train <- unique(as.character(meta$ID))

if(!is.null(testigos) && all(c("Estación", "ID") %in% names(testigos))){
  
  testigos_control <- testigos %>%
    dplyr::group_by(.data[["Estación"]]) %>%
    dplyr::summarise(
      N_muestras = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(Estacion = `Estación`)
  
  ids_test <- unique(as.character(testigos[["ID"]]))
  solapamiento_ids <- intersect(ids_train, ids_test)
  n_testigos_total <- nrow(testigos)
  n_est_testigos <- dplyr::n_distinct(testigos[["Estación"]])
  
} else {
  
  testigos_control <- tibble::tibble(
    Estacion = character(0),
    N_muestras = integer(0)
  )
  ids_test <- character(0)
  solapamiento_ids <- character(0)
  n_testigos_total <- 0L
  n_est_testigos <- 0L
}

control_testigos <- tibble::tibble(
  Indicador = c(
    "Filas_calibracion",
    "Filas_testigos",
    "Estaciones_calibracion",
    "Estaciones_testigos",
    "IDs_solapados_train_test",
    "EMA_presente"
  ),
  Valor = as.character(c(
    nrow(meta),
    n_testigos_total,
    dplyr::n_distinct(meta$Estacion),
    n_est_testigos,
    length(solapamiento_ids),
    !is.null(col_ema)
  ))
)

if(length(solapamiento_ids) > 0){
  warning("Hay IDs presentes tanto en calibración como en testigos: ",
          paste(solapamiento_ids, collapse = ", "))
}

cat("\nTestigos reservados por estación:\n")
if(nrow(testigos_control) > 0){
  print(testigos_control)
} else {
  cat("No se usa/encuentra hoja de testigos en esta corrida de 6.1.\n")
}

# ------------------------------------------------------------
# 8. RESÚMENES DE GRADIENTES
# ------------------------------------------------------------

resumen_num <- function(x, nombre, escala){
  x <- as.numeric(x)
  valid <- x[is.finite(x)]
  
  if(length(valid) == 0){
    return(tibble::tibble(
      Variable = nombre,
      Escala = escala,
      N = 0,
      N_NA = length(x),
      Min = NA_real_, Max = NA_real_, Rango = NA_real_,
      Media = NA_real_, Mediana = NA_real_, DE = NA_real_,
      IQR = NA_real_, CV_pct = NA_real_
    ))
  }
  
  m <- mean(valid)
  
  tibble::tibble(
    Variable = nombre,
    Escala = escala,
    N = length(valid),
    N_NA = sum(!is.finite(x)),
    Min = min(valid),
    Max = max(valid),
    Rango = max(valid) - min(valid),
    Media = m,
    Mediana = median(valid),
    DE = if(length(valid) > 1) sd(valid) else NA_real_,
    IQR = IQR(valid),
    CV_pct = if(length(valid) > 1 && !is.na(m) && m != 0){
      100 * sd(valid) / abs(m)
    } else {
      NA_real_
    }
  )
}

# ------------------------------------------------------------
# 9. CLIMA A ESCALA DE ESTACIÓN
# ------------------------------------------------------------

clima_vars <- c("TMA", "PMA")
if("EMA" %in% names(meta) && sum(!is.na(meta$EMA)) > 0){
  clima_vars <- c(clima_vars, "EMA")
}

# Verificar que el clima sea constante dentro de cada estación.
clima_consistencia <- meta %>%
  dplyr::group_by(Estacion) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(clima_vars),
      ~ dplyr::n_distinct(.x[!is.na(.x)]),
      .names = "{.col}_n_valores"
    ),
    .groups = "drop"
  )

cols_cons <- setdiff(names(clima_consistencia), "Estacion")
if(any(as.matrix(clima_consistencia[, cols_cons, drop = FALSE]) > 1, na.rm = TRUE)){
  warning("Alguna estación posee más de un valor para una variable climática. Revisar Clima_consistencia.")
}

clima_estacion <- meta %>%
  dplyr::group_by(Estacion) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(clima_vars),
      ~ primero_no_na(.x)
    ),
    .groups = "drop"
  )

gradientes_clima <- dplyr::bind_rows(
  lapply(
    clima_vars,
    function(v) resumen_num(clima_estacion[[v]], v, "Estación")
  )
)

# ------------------------------------------------------------
# 10. VARIABLES EDÁFICAS A ESCALA DE MUESTRA
# ------------------------------------------------------------

silice_modelo <- if(USAR_SILICE_LOG) "log10_Silice_g" else "Silice_g"

edaf_vars_modelo <- c(
  "pH",
  "MO",
  silice_modelo,
  "Textura_ord"
)

gradientes_edaficos <- dplyr::bind_rows(
  resumen_num(meta$pH, "pH", "Muestra"),
  resumen_num(meta$MO, "MO", "Muestra"),
  resumen_num(meta$Silice_g, "Silice_g", "Muestra"),
  resumen_num(meta$log10_Silice_g, "log10_Silice_g", "Muestra"),
  resumen_num(meta$Textura_ord, "Textura_ord", "Muestra")
)

textura_frecuencias <- meta %>%
  dplyr::count(Textura_std, Textura_ord, name = "N") %>%
  dplyr::arrange(Textura_ord)

# ------------------------------------------------------------
# 11. CORRELACIONES
# ------------------------------------------------------------

mat_a_largo <- function(matriz, tipo, componente){
  df <- as.data.frame(as.table(matriz), stringsAsFactors = FALSE)
  names(df) <- c("Variable_1", "Variable_2", "Correlacion")
  df$Tipo <- tipo
  df$Componente <- componente
  df
}

# Clima: un registro por estación.
mat_clima <- as.matrix(clima_estacion[, clima_vars, drop = FALSE])
cor_pearson_clima <- cor(mat_clima, use = "pairwise.complete.obs", method = "pearson")
cor_spearman_clima <- cor(mat_clima, use = "pairwise.complete.obs", method = "spearman")

# Edáficas: Pearson sólo para continuas; Spearman incorpora textura ordinal.
edaf_cont <- c("pH", "MO", silice_modelo)
mat_edaf_cont <- as.matrix(meta[, edaf_cont, drop = FALSE])
mat_edaf_spear <- as.matrix(meta[, unique(c(edaf_cont, "Textura_ord")), drop = FALSE])

cor_pearson_edaf <- cor(mat_edaf_cont, use = "pairwise.complete.obs", method = "pearson")
cor_spearman_edaf <- cor(mat_edaf_spear, use = "pairwise.complete.obs", method = "spearman")

# ------------------------------------------------------------
# 12. GRÁFICOS DE CORRELACIÓN
# ------------------------------------------------------------

guardar_heatmap_cor <- function(matriz, titulo, ruta){
  if(ncol(matriz) < 2) return(invisible(NULL))
  
  df <- as.data.frame(as.table(matriz), stringsAsFactors = FALSE)
  names(df) <- c("Var1", "Var2", "r")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Var1, y = Var2, fill = r)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)), size = 4) +
    ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = titulo, x = NULL, y = NULL, fill = "r") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
  
  ggplot2::ggsave(ruta, p, width = 7, height = 6, dpi = 300)
}

# ------------------------------------------------------------
# 13. DCA - LONGITUD DEL GRADIENTE BIOLÓGICO
# ------------------------------------------------------------

dca <- vegan::decorana(bio_rel)
dca_sites <- vegan::scores(dca, display = "sites", choices = 1:2)

longitud_eje1 <- diff(range(dca_sites[, 1], na.rm = TRUE))

if(longitud_eje1 < 3){
  decision_dca <- "RDA"
  metodos_ordenacion <- "RDA"
} else if(longitud_eje1 > 4){
  decision_dca <- "CCA"
  metodos_ordenacion <- "CCA"
} else {
  decision_dca <- "Zona intermedia: comparar RDA y CCA"
  metodos_ordenacion <- c("RDA", "CCA")
}

evals <- rep(NA_real_, 4)
if(!is.null(dca$evals)){
  z <- as.numeric(dca$evals)
  evals[seq_len(min(4, length(z)))] <- z[seq_len(min(4, length(z)))]
}

dca_resumen <- tibble::tibble(
  Longitud_eje1_SD = longitud_eje1,
  Decision = decision_dca,
  Eigen1 = evals[1],
  Eigen2 = evals[2],
  Eigen3 = evals[3],
  Eigen4 = evals[4]
)

cat("\nDCA - longitud aproximada del eje 1:", round(longitud_eje1, 3), "SD\n")
cat("Decisión orientativa:", decision_dca, "\n")

# ------------------------------------------------------------
# 14. FUNCIÓN GENERAL PARA RDA / CCA
# ------------------------------------------------------------

anova_a_df <- function(x, modelo, metodo, tipo){
  if(is.null(x)) return(tibble::tibble())
  z <- as.data.frame(x)
  z$Termino <- rownames(z)
  rownames(z) <- NULL
  z$Modelo <- modelo
  z$Metodo <- metodo
  z$Tipo_prueba <- tipo
  z <- z %>% dplyr::relocate(Modelo, Metodo, Tipo_prueba, Termino)
  z
}

ajustar_ordenacion <- function(Y, env, vars, metodo, etiqueta, nperm = N_PERM){
  
  vars <- vars[vars %in% names(env)]
  if(length(vars) == 0) return(NULL)
  
  cc <- complete.cases(env[, vars, drop = FALSE]) & rowSums(Y, na.rm = TRUE) > 0
  Y0 <- Y[cc, , drop = FALSE]
  E0 <- as.data.frame(env[cc, vars, drop = FALSE])
  
  # Quitar variables sin variación dentro del subconjunto efectivo.
  tiene_var <- sapply(
    E0,
    function(x) length(unique(x[!is.na(x)])) > 1
  )
  E0 <- E0[, tiene_var, drop = FALSE]
  vars <- names(E0)
  
  if(length(vars) == 0 || nrow(Y0) <= length(vars) + 2){
    return(NULL)
  }
  
  Y_use <- if(metodo == "RDA"){
    vegan::decostand(Y0, method = "hellinger")
  } else {
    Y0
  }
  
  f <- stats::reformulate(vars, response = "Y_use")
  environment(f) <- environment()
  
  mod <- tryCatch(
    {
      if(metodo == "RDA"){
        vegan::rda(f, data = E0)
      } else {
        vegan::cca(f, data = E0)
      }
    },
    error = function(e){
      warning("No se pudo ajustar ", etiqueta, " - ", metodo, ": ", e$message)
      NULL
    }
  )
  
  if(is.null(mod)) return(NULL)
  
  rs <- tryCatch(vegan::RsquareAdj(mod), error = function(e) NULL)
  r2 <- if(!is.null(rs)) rs$r.squared else NA_real_
  r2adj <- if(!is.null(rs)) rs$adj.r.squared else NA_real_
  
  global <- tryCatch(
    vegan::anova.cca(mod, permutations = nperm),
    error = function(e) NULL
  )
  
  marginal <- tryCatch(
    vegan::anova.cca(mod, by = "margin", permutations = nperm),
    error = function(e) NULL
  )
  
  vif <- tryCatch(
    vegan::vif.cca(mod),
    error = function(e) NULL
  )
  
  r2_tabla <- tibble::tibble(
    Modelo = etiqueta,
    Metodo = metodo,
    N = nrow(Y0),
    N_variables = length(vars),
    Variables = paste(vars, collapse = " + "),
    R2 = r2,
    R2_ajustado = r2adj
  )
  
  vif_tabla <- if(!is.null(vif)){
    tibble::tibble(
      Modelo = etiqueta,
      Metodo = metodo,
      Variable = names(vif),
      VIF = as.numeric(vif)
    )
  } else {
    tibble::tibble()
  }
  
  list(
    modelo = mod,
    R2 = r2_tabla,
    global = anova_a_df(global, etiqueta, metodo, "Global"),
    marginal = anova_a_df(marginal, etiqueta, metodo, "Marginal"),
    VIF = vif_tabla,
    filas_usadas = which(cc),
    variables = vars
  )
}

# ------------------------------------------------------------
# 15. MODELO CLIMÁTICO - ESCALA DE ESTACIÓN
# ------------------------------------------------------------

# Promedio de la composición relativa dentro de cada estación.
bio_df <- as.data.frame(bio_rel, check.names = FALSE)
bio_df$Estacion <- meta$Estacion

bio_estacion <- bio_df %>%
  dplyr::group_by(Estacion) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(bio_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

clima_integrado <- dplyr::left_join(
  clima_estacion,
  bio_estacion,
  by = "Estacion"
)

Y_clima <- as.matrix(clima_integrado[, bio_cols, drop = FALSE])
env_clima <- as.data.frame(clima_integrado[, clima_vars, drop = FALSE])
rownames(Y_clima) <- clima_integrado$Estacion
rownames(env_clima) <- clima_integrado$Estacion

resultados_clima <- list()
for(met in metodos_ordenacion){
  resultados_clima[[met]] <- ajustar_ordenacion(
    Y = Y_clima,
    env = env_clima,
    vars = clima_vars,
    metodo = met,
    etiqueta = "Clima_estaciones"
  )
}
resultados_clima <- resultados_clima[!sapply(resultados_clima, is.null)]

clima_R2 <- dplyr::bind_rows(lapply(resultados_clima, `[[`, "R2"))
clima_global <- dplyr::bind_rows(lapply(resultados_clima, `[[`, "global"))
clima_marginal <- dplyr::bind_rows(lapply(resultados_clima, `[[`, "marginal"))
clima_VIF <- dplyr::bind_rows(lapply(resultados_clima, `[[`, "VIF"))

# Modelos univariados: cada predictor usa todos los sitios con dato disponible.
clima_univ_R2 <- list()
clima_univ_global <- list()

for(v in clima_vars){
  for(met in metodos_ordenacion){
    z <- ajustar_ordenacion(
      Y = Y_clima,
      env = env_clima,
      vars = v,
      metodo = met,
      etiqueta = paste0("Clima_univariado_", v)
    )
    if(!is.null(z)){
      rr <- z$R2
      rr$Variable <- v
      gg <- z$global
      gg$Variable <- v
      clima_univ_R2[[paste(v, met)]] <- rr
      clima_univ_global[[paste(v, met)]] <- gg
    }
  }
}

clima_univ_R2 <- dplyr::bind_rows(clima_univ_R2)
clima_univ_global <- dplyr::bind_rows(clima_univ_global)

# ------------------------------------------------------------
# 16. MODELO EDÁFICO - ESCALA DE MUESTRA
# ------------------------------------------------------------

Y_edaf <- bio_rel
env_edaf <- as.data.frame(meta[, edaf_vars_modelo, drop = FALSE])
rownames(env_edaf) <- rownames(Y_edaf)

resultados_edaf <- list()
for(met in metodos_ordenacion){
  resultados_edaf[[met]] <- ajustar_ordenacion(
    Y = Y_edaf,
    env = env_edaf,
    vars = edaf_vars_modelo,
    metodo = met,
    etiqueta = "Edafico_muestras"
  )
}
resultados_edaf <- resultados_edaf[!sapply(resultados_edaf, is.null)]

edaf_R2 <- dplyr::bind_rows(lapply(resultados_edaf, `[[`, "R2"))
edaf_global <- dplyr::bind_rows(lapply(resultados_edaf, `[[`, "global"))
edaf_marginal <- dplyr::bind_rows(lapply(resultados_edaf, `[[`, "marginal"))
edaf_VIF <- dplyr::bind_rows(lapply(resultados_edaf, `[[`, "VIF"))

# Univariados edáficos: permiten evaluar cada variable con su N disponible,
# independientemente de los NA de los demás predictores.
edaf_univ_R2 <- list()
edaf_univ_global <- list()

for(v in edaf_vars_modelo){
  for(met in metodos_ordenacion){
    z <- ajustar_ordenacion(
      Y = Y_edaf,
      env = env_edaf,
      vars = v,
      metodo = met,
      etiqueta = paste0("Edaf_univariado_", v)
    )
    if(!is.null(z)){
      rr <- z$R2
      rr$Variable <- v
      gg <- z$global
      gg$Variable <- v
      edaf_univ_R2[[paste(v, met)]] <- rr
      edaf_univ_global[[paste(v, met)]] <- gg
    }
  }
}

edaf_univ_R2 <- dplyr::bind_rows(edaf_univ_R2)
edaf_univ_global <- dplyr::bind_rows(edaf_univ_global)

# ------------------------------------------------------------
# 17. TABLA DE DISPONIBILIDAD DE DATOS
# ------------------------------------------------------------

disponibilidad <- dplyr::bind_rows(
  lapply(clima_vars, function(v){
    tibble::tibble(
      Componente = "Climático",
      Variable = v,
      Escala = "Estación",
      N_disponible = sum(!is.na(clima_estacion[[v]])),
      N_total = nrow(clima_estacion)
    )
  }),
  lapply(edaf_vars_modelo, function(v){
    tibble::tibble(
      Componente = "Edáfico",
      Variable = v,
      Escala = "Muestra",
      N_disponible = sum(!is.na(meta[[v]])),
      N_total = nrow(meta)
    )
  })
)

# ------------------------------------------------------------
# 18. CARPETA DE SALIDA Y GRÁFICOS
# ------------------------------------------------------------

carpeta_salida <- file.path(
  dirname(archivo),
  paste0("Seleccion_variables_6_1_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)

dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

guardar_heatmap_cor(
  cor_pearson_clima,
  "Variables climáticas - Pearson (escala de estación)",
  file.path(carpeta_salida, "Correlacion_clima_Pearson.png")
)

guardar_heatmap_cor(
  cor_spearman_clima,
  "Variables climáticas - Spearman (escala de estación)",
  file.path(carpeta_salida, "Correlacion_clima_Spearman.png")
)

guardar_heatmap_cor(
  cor_pearson_edaf,
  "Variables edáficas continuas - Pearson",
  file.path(carpeta_salida, "Correlacion_edaficas_Pearson.png")
)

guardar_heatmap_cor(
  cor_spearman_edaf,
  "Variables edáficas - Spearman",
  file.path(carpeta_salida, "Correlacion_edaficas_Spearman.png")
)

# DCA simple, sin sobrecargar con 25 estaciones.
dca_plot_df <- tibble::tibble(
  DCA1 = dca_sites[, 1],
  DCA2 = dca_sites[, 2],
  Estacion = meta$Estacion,
  ID = meta$ID
)

p_dca <- ggplot2::ggplot(
  dca_plot_df,
  ggplot2::aes(x = DCA1, y = DCA2)
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, linetype = 3) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.25, linetype = 3) +
  ggplot2::geom_point(alpha = 0.70, size = 1.8) +
  ggplot2::labs(
    title = "DCA de la base moderna de calibración",
    subtitle = paste0("Longitud eje 1 = ", round(longitud_eje1, 3), " SD | ", decision_dca),
    x = "DCA1",
    y = "DCA2"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  file.path(carpeta_salida, "DCA_base_calibracion.png"),
  p_dca,
  width = 7,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 19. EXPORTAR EXCEL DE RESULTADOS
# ------------------------------------------------------------

archivo_excel <- file.path(
  carpeta_salida,
  "Resultados_seleccion_variables_6_1.xlsx"
)

# Seguridad de exportación: si alguna tabla opcional no llegó a generarse,
# se reemplaza por una tabla vacía en lugar de detener toda la exportación.
objetos_exportables <- c(
  "control_testigos", "testigos_control", "control_escala", "disponibilidad",
  "gradientes_clima", "gradientes_edaficos", "textura_frecuencias",
  "diccionario_textura", "clima_consistencia", "clima_estacion",
  "clima_R2", "clima_global", "clima_marginal", "clima_VIF",
  "clima_univ_R2", "clima_univ_global", "edaf_R2", "edaf_global",
  "edaf_marginal", "edaf_VIF", "edaf_univ_R2", "edaf_univ_global"
)
for(nm in objetos_exportables){
  if(!exists(nm, inherits = FALSE)){
    assign(nm, tibble::tibble(), inherits = FALSE)
  }
}

salidas <- list(
  "Control_testigos" = control_testigos,
  "Testigos_por_estacion" = testigos_control,
  "Control_escala_biologica" = control_escala,
  "Disponibilidad" = disponibilidad,
  "Gradientes_clima" = gradientes_clima,
  "Gradientes_edaficos" = gradientes_edaficos,
  "Textura_frecuencias" = textura_frecuencias,
  "Diccionario_textura" = diccionario_textura,
  "Clima_consistencia" = clima_consistencia,
  "Clima_por_estacion" = clima_estacion,
  "Cor_Pearson_clima" = mat_a_largo(cor_pearson_clima, "Pearson", "Clima"),
  "Cor_Spearman_clima" = mat_a_largo(cor_spearman_clima, "Spearman", "Clima"),
  "Cor_Pearson_edaf" = mat_a_largo(cor_pearson_edaf, "Pearson", "Edafico"),
  "Cor_Spearman_edaf" = mat_a_largo(cor_spearman_edaf, "Spearman", "Edafico"),
  "DCA" = dca_resumen,
  "Clima_R2" = clima_R2,
  "Clima_global" = clima_global,
  "Clima_marginal" = clima_marginal,
  "Clima_VIF" = clima_VIF,
  "Clima_univ_R2" = clima_univ_R2,
  "Clima_univ_global" = clima_univ_global,
  "Edaf_R2" = edaf_R2,
  "Edaf_global" = edaf_global,
  "Edaf_marginal" = edaf_marginal,
  "Edaf_VIF" = edaf_VIF,
  "Edaf_univ_R2" = edaf_univ_R2,
  "Edaf_univ_global" = edaf_univ_global
)

# writexl no acepta elementos NULL: quitar hojas vacías si las hubiera.
salidas <- salidas[!sapply(salidas, is.null)]
salidas <- salidas[sapply(salidas, function(x) is.data.frame(x) && nrow(x) > 0)]

writexl::write_xlsx(salidas, path = archivo_excel)

# ------------------------------------------------------------
# 20. RESUMEN EN CONSOLA
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ANÁLISIS 6.1 FINALIZADO\n")
cat("============================================================\n")

cat("\nBase de calibración: ", nrow(meta), " muestras válidas\n", sep = "")
cat("Estaciones de calibración: ", dplyr::n_distinct(meta$Estacion), "\n", sep = "")
cat("Testigos reservados: ", n_testigos_total, " muestras\n", sep = "")
cat("IDs compartidos calibración/testigos: ", length(solapamiento_ids), "\n", sep = "")

cat("\nVariables climáticas efectivamente analizadas:\n")
print(clima_vars)

cat("\nVariables edáficas efectivamente analizadas:\n")
print(edaf_vars_modelo)

cat("\nDCA:\n")
print(dca_resumen)

cat("\nGradientes climáticos:\n")
print(gradientes_clima)

cat("\nGradientes edáficos:\n")
print(gradientes_edaficos)

cat("\nR2 del/los modelo/s climático/s:\n")
print(clima_R2)

cat("\nR2 del/los modelo/s edáfico/s:\n")
print(edaf_R2)

cat("\nResultados guardados en:\n", carpeta_salida, "\n")
cat("Excel:\n", archivo_excel, "\n")

# ============================================================
# FIN
# ============================================================

