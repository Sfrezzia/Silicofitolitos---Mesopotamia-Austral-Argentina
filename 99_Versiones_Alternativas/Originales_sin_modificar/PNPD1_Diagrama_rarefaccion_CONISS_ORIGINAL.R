# ============================================================
# Diagrama fitolitico PNPD1 con rioja::strat.plot
# VERSION v10c: macroafinidades + CONISS + rarefaccion, con eje Y corregido para que la primera muestra quede arriba
#
# Al correrlo, el script pide seleccionar/subir el Excel
# Hojas usadas:
#   - "Silicofitolitos (F.R alineados)"  -> diagrama principal (% por morfotipo)
#   - "Crudo (Silicofit-no silicofit)"   -> diatomeas, espiculas, estomatocistes, rarefaccion y silice
# ============================================================

# 1) Paquetes -------------------------------------------------
paquetes <- c("readxl", "rioja", "vegan", "svglite")
instalar <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(readxl)
library(rioja)
library(vegan)

# 2) Parametros editables -------------------------------------
# El script te pide seleccionar el Excel al correr.
# En RStudio se abre una ventana para elegir el archivo.
elegir_excel <- function() {
  cat("\nSelecciona/sube el archivo Excel PNPD1.xlsx cuando se abra la ventana...\n")
  
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    f <- rstudioapi::selectFile(
      caption = "Selecciona/sube el Excel PNPD1.xlsx",
      label = "Seleccionar",
      path = getwd(),
      filter = "Archivos Excel (*.xlsx)",
      existing = TRUE
    )
    if (!is.null(f) && nzchar(f)) return(f)
  }
  
  file.choose()
}

archivo <- elegir_excel()
carpeta_salida <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
# Las salidas se guardan en la carpeta de trabajo activa de RStudio, no en la carpeta del Excel.

hoja_fit_fr <- "Silicofitolitos (F.R alineados)"
hoja_cruda  <- "Crudo (Silicofit-no silicofit)"

# Modo reducido: suma los morfotipos por MACROAFINIDAD y grafica una sola curva por grupo.
# Macroafinidades principales:
#   - Pastizales = Afinidad variable + Gramineas + Celulas cortas de gramineas
#   - Gramíneas húmedas = Podostemaceae
#   - Estrato arbóreo = Dicotiledonea + Arecaceae
agrupar_morfotipos_por_afinidad <- TRUE

# Por defecto Cyperaceae queda como columna independiente, porque no fue incluida
# explicitamente en las tres macroafinidades solicitadas.
# Si queres que Cyperaceae se sume a Gramíneas húmedas, cambia a TRUE.
incluir_cyperaceae_en_gramineas_humedas <- FALSE

# Para el diagrama agrupado conviene NO filtrar antes por abundancia, asi cada afinidad
# incorpora tambien morfotipos raros o de baja frecuencia.
# Si queres ocultar una afinidad completa de muy baja presencia, podes poner TRUE.
filtrar_morfotipos <- FALSE
umbral_max <- 0

# Ordena las curvas segun el orden de afinidades definido mas abajo.
ordenar_por_afinidad <- TRUE

# CONISS: TRUE = calcula el agrupamiento con la matriz reducida por afinidades.
# FALSE = calcula el CONISS con todos los morfotipos originales, aunque el diagrama muestre afinidades.
coniss_sobre_matriz_agrupada <- TRUE

# Panel auxiliar: curvas de no fitolitos por separado.
# Se agregan solo si tienen valores mayores a cero en al menos una muestra.
# No se grafica la curva de % de fitolitos.
graficar_no_fit_individuales <- TRUE
no_fit_para_graficar <- c("Diatomeas", "Espiculas", "Estomatocistes")

# Curvas exageradas del diagrama principal.
# En el diagrama por afinidades quedan apagadas porque cada curva ya suma varios morfotipos.
usar_curva_exagerada <- FALSE
exag_multiplicador <- 3
exag_transparencia <- 0.08

# Escala para la silice amorfa. Con 1e7, el eje queda expresado como x10^7/g.
escala_silice <- 1e7
rotulo_silice <- "Silice\namorfa\n(x10^7/g)"

# Cantidad de zonas a marcar con el cluster. Poner NULL si no queres marcar zonas.
n_zonas_coniss <- 2

# Exportacion: solo el diagrama principal + paneles auxiliares ajustados.
# Se guarda en la carpeta de trabajo activa de RStudio: getwd().
salida_pdf <- file.path(carpeta_salida, "diagrama_fitolitico_macroafinidades_v10c_orden_5813_5830.pdf")
salida_svg <- file.path(carpeta_salida, "diagrama_fitolitico_macroafinidades_v10c_orden_5813_5830.svg")

# Tipografia solicitada
# IMPORTANTE: tamano_base_pt controla el tamaÃ±o base del dispositivo PDF/SVG.
# El SVG se exporta con svglite para que las etiquetas queden como palabras editables.
# Los cex de abajo controlan el tamaÃ±o REAL dentro de rioja::strat.plot().
fuente_base <- "Arial"
tamano_base_pt <- 10

# TamaÃ±os relativos de texto.
# CAMBIO RAPIDO: subi o baja factor_texto_global para agrandar/achicar todo junto.
# Ejemplo: 1.00 = base Arial 10; 1.20 = aprox. 12 pt; 1.40 = aprox. 14 pt.
factor_texto_global <- 1.00

# Ajustes finos por tipo de texto. Si queres letras mas grandes, subi estos valores.
cex_titulo <- 1.25 * factor_texto_global
cex_rotulo_eje_y <- 1.25 * factor_texto_global
cex_numeros_eje_y <- 1.20 * factor_texto_global
cex_numeros_eje_x <- 1.10 * factor_texto_global
cex_nombres_curvas <- 1.20 * factor_texto_global
cex_leyenda <- 1.10 * factor_texto_global

# Separacion de los numeros del eje X respecto de la linea del eje.
# Aumentar el segundo valor si los numeros 5, 10, 20 o los valores del CONISS
# quedan demasiado pegados al eje. Valores utiles: 1.40 a 2.00.
mgp_ejes_x <- c(3, 1.70, 0)

# Eje Y: usar nombres completos de muestra en lugar de intervalos simplificados.
usar_nombres_muestra_en_eje_y <- TRUE
rotulo_eje_y <- ""  # se deja vacio para que no aparezca "Muestra / profundidad"

# Margen interno del eje Y.
# Sirve para que la primera o la ultima muestra del perfil, por ejemplo 5830,
# no quede pegada al borde superior/inferior del panel.
margen_y_relativo <- 0.08
margen_y_minimo <- 0.80

# Orden de muestras:
# TRUE = si la columna Muestra es numerica, ordena de menor a mayor.
# Asi, por ejemplo, 5813 queda arriba y 5830 queda abajo.
# Si tus muestras tienen nombres tipo "H.O PNPD 1", el script conserva el orden del Excel.
ordenar_muestras_numericas_ascendente <- TRUE

# 3) Funciones auxiliares -------------------------------------
limpiar_nombres <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\s+", " ", x)
  x
}

leer_hoja_limpia <- function(archivo, hoja) {
  dat <- readxl::read_excel(archivo, sheet = hoja)
  dat <- as.data.frame(dat, check.names = FALSE)
  names(dat) <- limpiar_nombres(names(dat))
  dat <- dat[, !is.na(names(dat)) & names(dat) != "", drop = FALSE]
  dat <- dat[, colSums(!is.na(dat)) > 0, drop = FALSE]
  names(dat)[1] <- "Muestra"
  dat$Muestra <- as.character(dat$Muestra)
  dat
}

as_numeric_df <- function(x) {
  x <- as.data.frame(x, check.names = FALSE)
  x[] <- lapply(x, function(z) suppressWarnings(as.numeric(as.character(z))))
  x[is.na(x)] <- 0
  x
}

calcular_rango_y_con_margen <- function(y, margen_relativo = margen_y_relativo, margen_minimo = margen_y_minimo) {
  y <- y[is.finite(y)]
  if (length(y) == 0) return(NULL)
  r <- range(y, na.rm = TRUE)
  amplitud <- diff(r)
  if (!is.finite(amplitud) || amplitud == 0) amplitud <- 1
  margen <- max(amplitud * margen_relativo, margen_minimo)
  
  # IMPORTANTE:
  # No invierto el ylim aca. El eje se invierte una sola vez con y.rev = TRUE
  # dentro de rioja::strat.plot(). Esto evita que se invierta dos veces.
  c(r[1] - margen, r[2] + margen)
}

ordenar_tabla_por_muestra_numerica <- function(dat) {
  if (!isTRUE(ordenar_muestras_numericas_ascendente)) return(dat)
  if (!"Muestra" %in% names(dat)) return(dat)
  
  muestras_txt <- trimws(as.character(dat$Muestra))
  muestras_num <- suppressWarnings(as.numeric(muestras_txt))
  
  # Solo ordena automaticamente si TODAS las muestras son numericas puras
  # como 5813, 5814, ..., 5830. Si hay nombres como H.O., conserva el orden del Excel.
  if (all(is.finite(muestras_num))) {
    dat <- dat[order(muestras_num), , drop = FALSE]
  }
  
  dat
}

alinear_raw_con_spec <- function(raw, ids_spec) {
  if (!all(ids_spec %in% rownames(raw))) {
    faltantes <- setdiff(ids_spec, rownames(raw))
    if (length(faltantes) > 0) {
      message(
        "Atencion: estas muestras estan en la hoja de F.R. alineados pero no se encontraron igual en la hoja cruda: ",
        paste(faltantes, collapse = ", ")
      )
    }
    return(raw)
  }
  raw[ids_spec, , drop = FALSE]
}

# Orden y afinidades definidos a partir del Excel Libro1.xlsx.
# El orden de esta lista es el orden en que apareceran las curvas del diagrama.
orden_morfotipos <- c(
  "Acute bulbosus",
  "Blocky",
  "Elongate entire",
  "Elongate facetate",
  "Elongate",
  "Short elongate",
  "Elongate dentate",
  "Enlongate sinuate",
  "Bulliform flabellate",
  "Cross",
  "Cross Pan",
  "Saddle",
  "Saddle Chl",
  "Saddle Bamb",
  "Bilobate",
  "Bilobate Pan",
  "Bilobate Stip",
  "Bilobate Chl",
  "Crenate",
  "Polylobate",
  "Rondel",
  "Rondell",
  "Papillate",
  "PAPILLATE",
  "Papilate",
  "PAPILATE",
  "Polygonal",
  "POLYGONAL",
  "Fusiform anullate",
  "Longolitum",
  "Asterolitum",
  "Tracheary",
  "Spheroid ornate",
  "Spheroid psilate",
  "Irregular anticlinal",
  "Spheroid echinate",
  "Baciliforms",
  "Bacilliforms",
  "Otros"
)

# Asignacion editable de afinidades.
# Respeta las afinidades nuevas definidas en el Excel de orden.
asignar_afinidad <- function(nms) {
  out <- rep("Sin asignacion", length(nms))
  names(out) <- nms
  
  afinidad_variable <- c(
    "Acute bulbosus",
    "Blocky",
    "Elongate entire",
    "Elongate facetate"
  )
  
  gramineas <- c(
    "Elongate",
    "Short elongate",
    "Elongate dentate",
    "Enlongate sinuate",
    "Bulliform flabellate"
  )
  
  celulas_cortas_gramineas <- c(
    "Cross",
    "Cross Pan",
    "Saddle",
    "Saddle Chl",
    "Saddle Bamb",
    "Bilobate",
    "Bilobate Pan",
    "Bilobate Stip",
    "Bilobate Chl",
    "Crenate",
    "Polylobate",
    "Rondel",
    "Rondell"
  )
  
  cyperaceae <- c(
    "Papillate",
    "PAPILLATE",
    "Papilate",
    "PAPILATE",
    "Polygonal",
    "POLYGONAL"
  )
  
  podostemaceae <- c(
    "Fusiform anullate",
    "Longolitum",
    "Asterolitum"
  )
  
  dicotiledonea <- c(
    "Tracheary",
    "Spheroid ornate",
    "Spheroid psilate",
    "Irregular anticlinal"
  )
  
  arecaceae <- c(
    "Spheroid echinate"
  )
  
  sin_asignacion <- c(
    "Baciliforms",
    "Bacilliforms",
    "Otros",
    "Other"
  )
  
  out[nms %in% afinidad_variable] <- "Afinidad variable"
  out[nms %in% gramineas] <- "Gramineas"
  out[nms %in% celulas_cortas_gramineas] <- "Celulas cortas de gramineas"
  out[nms %in% cyperaceae] <- "Cyperaceae"
  out[nms %in% podostemaceae] <- "Podostemaceae"
  out[nms %in% dicotiledonea] <- "Dicotiledonea"
  out[nms %in% arecaceae] <- "Arecaceae"
  out[nms %in% sin_asignacion] <- "Sin asignacion"
  
  out
}

# Tonos pastel por macroafinidad.
colores_grupo <- c(
  "Pastizales" = "#A8D8A8",            # verde pastel
  "Gramíneas húmedas" = "#A9DDF3",    # celeste pastel
  "Cyperaceae" = "#CDB4DB",           # violeta pastel, se usa si queda como grupo separado
  "Estrato arbóreo" = "#C9A27E",      # marron claro
  "Sin asignacion" = "#BFBFBF"        # gris medio
)

orden_grupos <- c(
  "Pastizales",
  "Gramíneas húmedas",
  "Cyperaceae",
  "Estrato arbóreo",
  "Sin asignacion"
)

# Reagrupamiento interpretativo de afinidades en macroafinidades.
recodificar_macroafinidad <- function(afinidades_originales) {
  out <- afinidades_originales
  
  out[afinidades_originales %in% c(
    "Afinidad variable",
    "Gramineas",
    "Celulas cortas de gramineas"
  )] <- "Pastizales"
  
  out[afinidades_originales %in% c("Podostemaceae")] <- "Gramíneas húmedas"
  
  if (isTRUE(incluir_cyperaceae_en_gramineas_humedas)) {
    out[afinidades_originales %in% c("Cyperaceae")] <- "Gramíneas húmedas"
  } else {
    out[afinidades_originales %in% c("Cyperaceae")] <- "Cyperaceae"
  }
  
  out[afinidades_originales %in% c(
    "Dicotiledonea",
    "Arecaceae"
  )] <- "Estrato arbóreo"
  
  out[afinidades_originales %in% c("Sin asignacion")] <- "Sin asignacion"
  out
}

# 4) Lectura del diagrama principal ---------------------------
fit_fr <- leer_hoja_limpia(archivo, hoja_fit_fr)
fit_fr <- ordenar_tabla_por_muestra_numerica(fit_fr)
rownames(fit_fr) <- fit_fr$Muestra

message("Orden de muestras usado en el diagrama principal: ", paste(rownames(fit_fr), collapse = " -> "))

spec <- fit_fr[, setdiff(names(fit_fr), "Muestra"), drop = FALSE]
spec <- as_numeric_df(spec)
spec <- spec[, colSums(spec, na.rm = TRUE) > 0, drop = FALSE]

# La hoja tiene frecuencias relativas 0-1. Para strat.plot las paso a porcentaje 0-100.
if (max(as.matrix(spec), na.rm = TRUE) <= 1.5) {
  spec <- spec * 100
}

# Matriz completa de frecuencias relativas (%), sin filtrar.
# Esta matriz se conserva para que las afinidades sumen TODOS los morfotipos, incluso los de baja frecuencia.
spec_completa <- spec

# Afinidad original asignada a cada morfotipo.
afinidad_morfotipos <- asignar_afinidad(colnames(spec_completa))

# Macroafinidad interpretativa usada para sumar columnas del diagrama.
macroafinidad_morfotipos <- recodificar_macroafinidad(afinidad_morfotipos)

# Aviso de morfotipos que no esten en el orden predefinido.
morfotipos_no_listados <- setdiff(colnames(spec_completa), orden_morfotipos)
if (length(morfotipos_no_listados) > 0) {
  message(
    "Atencion: estos morfotipos no estan en orden_morfotipos y se asignan segun la funcion asignar_afinidad(); si no coinciden quedan como Sin asignacion: ",
    paste(morfotipos_no_listados, collapse = ", ")
  )
}

# Construccion del diagrama principal.
if (isTRUE(agrupar_morfotipos_por_afinidad)) {
  
  # Orden final de macroafinidades: respeta orden_grupos y agrega cualquier grupo extra al final.
  grupos_presentes <- orden_grupos[orden_grupos %in% unique(macroafinidad_morfotipos)]
  grupos_extra <- setdiff(unique(macroafinidad_morfotipos), grupos_presentes)
  grupos_finales <- c(grupos_presentes, grupos_extra)
  
  # Suma fila por fila los morfotipos pertenecientes a cada macroafinidad.
  spec_plot <- as.data.frame(
    sapply(grupos_finales, function(g) {
      cols_g <- names(macroafinidad_morfotipos)[macroafinidad_morfotipos == g]
      if (length(cols_g) == 0) {
        rep(0, nrow(spec_completa))
      } else {
        rowSums(spec_completa[, cols_g, drop = FALSE], na.rm = TRUE)
      }
    }),
    check.names = FALSE
  )
  rownames(spec_plot) <- rownames(spec_completa)
  
  # Elimina macroafinidades completamente vacias.
  spec_plot <- spec_plot[, colSums(spec_plot, na.rm = TRUE) > 0, drop = FALSE]
  
  # Filtro opcional aplicado a macroafinidades ya sumadas, no a morfotipos individuales.
  if (isTRUE(filtrar_morfotipos)) {
    spec_plot <- spec_plot[, apply(spec_plot, 2, max, na.rm = TRUE) >= umbral_max, drop = FALSE]
  }
  
  # En el diagrama reducido, cada columna ya es una macroafinidad.
  afinidad <- colnames(spec_plot)
  names(afinidad) <- colnames(spec_plot)
  
} else {
  
  # Modo clasico: grafica morfotipos individuales.
  if (filtrar_morfotipos) {
    spec_plot <- spec_completa[, apply(spec_completa, 2, max, na.rm = TRUE) >= umbral_max, drop = FALSE]
  } else {
    spec_plot <- spec_completa
  }
  
  afinidad <- recodificar_macroafinidad(asignar_afinidad(colnames(spec_plot)))
  
  if (ordenar_por_afinidad) {
    morfotipos_en_orden <- orden_morfotipos[orden_morfotipos %in% colnames(spec_plot)]
    morfotipos_no_listados <- setdiff(colnames(spec_plot), orden_morfotipos)
    nuevo_orden <- c(morfotipos_en_orden, morfotipos_no_listados)
    spec_plot <- spec_plot[, nuevo_orden, drop = FALSE]
    afinidad <- recodificar_macroafinidad(asignar_afinidad(colnames(spec_plot)))
  }
}

col_main <- unname(colores_grupo[afinidad])
col_main[is.na(col_main)] <- colores_grupo[["Sin asignacion"]]

# 5) Lectura de datos crudos y paneles auxiliares -------------
raw <- leer_hoja_limpia(archivo, hoja_cruda)
raw <- ordenar_tabla_por_muestra_numerica(raw)
rownames(raw) <- raw$Muestra
raw <- alinear_raw_con_spec(raw, rownames(spec_plot))

message("Orden de muestras usado en paneles auxiliares: ", paste(rownames(raw), collapse = " -> "))

# Columnas no fitoliticas disponibles en la base.
cols_no_fit <- intersect(c("Espiculas", "Radiolarios", "Diatomeas", "Estomatocistes"), names(raw))

# Las columnas fitoliticas crudas son las ubicadas entre Muestra y la primera columna no fitolitica.
# Se excluyen tambien columnas operativas/cuantitativas posteriores.
idx_no_fit <- match(cols_no_fit[1], names(raw))
cols_fit_raw <- names(raw)[2:(idx_no_fit - 1)]
cols_fit_raw <- setdiff(cols_fit_raw, c("Campos fm", "Campos ff", "Silice amorfa por gramo"))

fit_counts <- as_numeric_df(raw[, cols_fit_raw, drop = FALSE])
no_fit_counts <- as_numeric_df(raw[, cols_no_fit, drop = FALSE])

n_fit <- rowSums(fit_counts, na.rm = TRUE)
n_no_fit <- rowSums(no_fit_counts, na.rm = TRUE)
total_contado <- n_fit + n_no_fit
fitolitos_pct <- ifelse(total_contado > 0, 100 * n_fit / total_contado, NA_real_)

# Rarefaccion: riqueza esperada de morfotipos fitoliticos para un mismo N de conteo.
# Esto usa vegan::rarefy; no es una funcion de rioja, pero se integra al diagrama con strat.plot.
row_tot_fit <- rowSums(fit_counts, na.rm = TRUE)
n_rarefaccion <- min(row_tot_fit[row_tot_fit > 0], na.rm = TRUE)
riqueza_rarefactada <- vegan::rarefy(fit_counts, sample = n_rarefaccion)
riqueza_observada <- rowSums(fit_counts > 0, na.rm = TRUE)

silice <- suppressWarnings(as.numeric(as.character(raw[["Silice amorfa por gramo"]])))
silice_x <- silice / escala_silice

# Panel auxiliar compacto.
# No se grafica % de fitolitos. En su lugar se incorporan, si tienen datos,
# curvas individuales para Diatomeas, Espiculas y Estomatocistes.
no_fit_disponibles <- if (isTRUE(graficar_no_fit_individuales)) {
  intersect(no_fit_para_graficar, names(no_fit_counts))
} else {
  character(0)
}
no_fit_seleccionados <- no_fit_disponibles[
  vapply(no_fit_counts[, no_fit_disponibles, drop = FALSE], function(z) {
    any(is.finite(z) & z > 0, na.rm = TRUE)
  }, logical(1))
]

if (length(no_fit_seleccionados) > 0) {
  no_fit_aux <- no_fit_counts[, no_fit_seleccionados, drop = FALSE]
  nombres_aux <- c(
    "Diatomeas" = "Diatomeas\n(N)",
    "Espiculas" = "Espiculas\n(N)",
    "Estomatocistes" = "Estomatocistes\n(N)"
  )
  colnames(no_fit_aux) <- unname(nombres_aux[colnames(no_fit_aux)])
} else {
  no_fit_aux <- data.frame(row.names = rownames(no_fit_counts))
}

aux_plot <- data.frame(
  no_fit_aux,
  "Rarefaccion\n(S)" = as.numeric(riqueza_rarefactada),
  check.names = FALSE
)
aux_plot[[rotulo_silice]] <- silice_x

# Colores pastel del panel auxiliar.
# Importante: queda definido fuera de graficar_diagrama(), porque rioja::strat.plot()
# puede evaluar argumentos graficos en un entorno distinto al de la funcion local.
colores_aux_map <- c(
  "Diatomeas\n(N)" = "#A9DDF3",
  "Espiculas\n(N)" = "#CFCFCF",
  "Estomatocistes\n(N)" = "#CDB4DB",
  "Rarefaccion\n(S)" = "#B8D8B8"
)
colores_aux_map[rotulo_silice] <- "#E6C29F"
col_aux <- unname(colores_aux_map[colnames(aux_plot)])
col_aux[is.na(col_aux)] <- "#D0D0D0"

# Dejo tambien una copia en .GlobalEnv para evitar problemas de evaluacion diferida
# en algunas versiones/instalaciones de rioja.
assign("col_aux", col_aux, envir = .GlobalEnv)

# Guardar tabla de indicadores para revisar valores.
aux_out <- data.frame(
  Muestra = raw$Muestra,
  Fitolitos_N = n_fit,
  No_fitolitos_N = n_no_fit,
  Total_contado_N = total_contado,
  Fitolitos_pct = fitolitos_pct,
  Diatomeas_N = if ("Diatomeas" %in% names(no_fit_counts)) no_fit_counts[["Diatomeas"]] else NA_real_,
  Espiculas_N = if ("Espiculas" %in% names(no_fit_counts)) no_fit_counts[["Espiculas"]] else NA_real_,
  Estomatocistes_N = if ("Estomatocistes" %in% names(no_fit_counts)) no_fit_counts[["Estomatocistes"]] else NA_real_,
  Riqueza_observada = riqueza_observada,
  Rarefaccion_S = as.numeric(riqueza_rarefactada),
  N_rarefaccion = n_rarefaccion,
  Silice_amorfa_por_gramo = silice,
  Silice_escalada = silice_x,
  check.names = FALSE
)
# La tabla aux_out queda disponible en memoria para revisar en consola,
# pero no se exporta para que la salida quede limpia.

# 6) Eje estratigrafico ---------------------------------------
# Mantengo las posiciones ya usadas para PNPD1:
# H.O. arriba; Sed. H.O. debajo; luego puntos medios de intervalos sedimentarios.
if (nrow(spec_plot) == 7) {
  profundidad <- c(0, 1, 2.5, 7.5, 12.5, 17.5, 22.5)
  
  if (usar_nombres_muestra_en_eje_y) {
    # Muestra los nombres tal como vienen en la primera columna del Excel
    # por ejemplo: H.O PNPD 1, 4874, 4875, etc.
    etiquetas_y <- rownames(spec_plot)
  } else {
    # Etiquetas estratigraficas simplificadas
    etiquetas_y <- c("H.O.", "Sed. H.O.", "0-5", "5-10", "10-15", "15-20", "20-25")
  }
} else {
  profundidad <- seq_len(nrow(spec_plot)) - 1
  etiquetas_y <- rownames(spec_plot)
}

# Rango del eje Y con margen interno, sin doble inversion del eje.
rango_y <- calcular_rango_y_con_margen(profundidad)

# 7) Cluster CONISS y broken-stick ----------------------------
clust <- NULL
matriz_coniss <- if (isTRUE(coniss_sobre_matriz_agrupada)) spec_plot else spec_completa
matriz_coniss <- matriz_coniss[, colSums(matriz_coniss, na.rm = TRUE) > 0, drop = FALSE]

if (nrow(matriz_coniss) >= 3 && ncol(matriz_coniss) >= 2) {
  # Distancia tipo Hellinger/chord: sqrt(p/100), frecuente en diagramas paleoecologicos.
  diss <- dist(sqrt(matriz_coniss / 100))
  clust <- rioja::chclust(diss, method = "coniss")
  
  # Broken-stick queda disponible si queres revisarlo manualmente en consola:
  # rioja::bstick(clust, ng = min(10, nrow(matriz_coniss) - 1), plot = TRUE)
}

# 8) Funcion de graficado -------------------------------------
graficar_diagrama <- function() {
  par(mar = c(5.2, 4, 4, 2), xpd = NA, family = fuente_base, mgp = mgp_ejes_x, tcl = -0.25)
  
  # Diagrama principal de silicofitolitos.
  force(col_main)
  force(col_aux)
  
  x_main <- rioja::strat.plot(
    d = spec_plot,
    yvar = profundidad,
    y.rev = TRUE,
    ylim = rango_y,
    scale.percent = TRUE,
    title = "",
    ylabel = "",
    y.tks = profundidad,
    y.tks.labels = etiquetas_y,
    x.names = colnames(spec_plot),
    
    xLeft = 0.06,
    xRight = 0.72,
    yBottom = 0.29,
    yTop = 0.82,
    
    # Aspecto general
    plot.line = TRUE,
    lwd.line = 0.75,
    col.line = col_main,
    plot.poly = TRUE,
    col.poly = col_main,
    col.poly.line = NA,
    plot.bar = FALSE,
    
    # Curvas exageradas para morfotipos de baja abundancia
    # Sombreado suavizado para que no parezca abundancia real.
    exag = usar_curva_exagerada,
    exag.mult = exag_multiplicador,
    col.exag = "auto",
    exag.alpha = exag_transparencia,
    
    # Escalas y textos
    min.width = 5,
    x.pc.inc = 10,
    x.pc.lab = TRUE,
    x.pc.omit0 = TRUE,
    # Con pointsize = 10 en el dispositivo: cex = 1 equivale aprox. a Arial 10.
    cex.title = 0.001,
    cex.ylabel = 0.001,
    cex.yaxis = cex_numeros_eje_y,
    cex.axis = cex_numeros_eje_x,
    cex.xlabel = cex_nombres_curvas,
    srt.xlabel = 60,
    xSpace = 0.004
  )
  
  # Paneles auxiliares: no fitolitos individuales N, rarefaccion y silice.
  
  max_aux <- apply(aux_plot, 2, function(z) {
    zz <- z[is.finite(z)]
    if (length(zz) == 0 || max(zz, na.rm = TRUE) == 0) return(1)
    max(zz, na.rm = TRUE)
  })
  minmax_aux <- cbind(Min = rep(0, ncol(aux_plot)), Max = max_aux)
  rownames(minmax_aux) <- colnames(aux_plot)
  
  # Mantengo la misma separacion de numeros del eje X en paneles auxiliares
  # y en la escala del CONISS.
  par(mgp = mgp_ejes_x, tcl = -0.25)
  
  x_aux <- rioja::strat.plot(
    d = aux_plot,
    yvar = unname(profundidad),
    y.rev = TRUE,
    ylim = rango_y,
    scale.percent = FALSE,
    minmax = minmax_aux,
    scale.minmax = TRUE,
    title = "",
    ylabel = "",
    y.axis = FALSE,
    x.names = colnames(aux_plot),
    
    xLeft = 0.735,
    xRight = 0.955,
    yBottom = 0.29,
    yTop = 0.82,
    add = TRUE,
    
    plot.line = TRUE,
    lwd.line = 0.85,
    col.line = col_aux,
    plot.poly = TRUE,
    col.poly = col_aux,
    col.poly.line = NA,
    plot.bar = FALSE,
    plot.symb = TRUE,
    symb.pch = 21,
    symb.cex = 0.6,
    
    cex.title = 0.001,
    cex.ylabel = 0.001,
    cex.axis = cex_numeros_eje_x,
    cex.xlabel = cex_nombres_curvas,
    srt.xlabel = 60,
    xSpace = 0.006,
    
    clust = clust,
    clust.width = if (!is.null(clust)) 0.055 else 0
  )
  
  # Marcar zonas del CONISS, si se pidio.
  if (!is.null(clust) && !is.null(n_zonas_coniss)) {
    rioja::addClustZone(x_aux, clust, nZone = n_zonas_coniss, col = "#B07AA1", lty = 2, lwd = 1.2)
  }
  
  # Rotulo del agrupamiento CONISS.
  # Se dibuja sobre toda la pagina para que no dependa del panel interno de rioja.
  if (!is.null(clust)) {
    par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), family = fuente_base)
    plot.new()
    text(0.935, 0.855, "CONISS", cex = cex_nombres_curvas * 0.95, font = 2)
  }
  
  # Leyenda de macroafinidades.
  # Se coloca en una franja inferior propia, en dos filas, para que no se superponga
  # con lineas del diagrama, etiquetas o curvas auxiliares.
  par(fig = c(0.03, 0.97, 0.02, 0.16), new = TRUE, mar = c(0, 0, 0, 0), family = fuente_base)
  plot.new()
  grupos_usados <- orden_grupos[orden_grupos %in% unique(afinidad)]
  legend(
    "center",
    legend = grupos_usados,
    fill = colores_grupo[grupos_usados],
    border = NA,
    bty = "n",
    horiz = FALSE,
    ncol = 4,
    cex = cex_leyenda * 0.90,
    x.intersp = 0.55,
    y.intersp = 0.85
  )
  
  invisible(list(main = x_main, aux = x_aux))
}

# 9) Exportar solo el diagrama principal + auxiliares ----------
# PDF: uso cairo_pdf si esta disponible porque respeta mejor fuentes como Arial.
if (capabilities("cairo")) {
  grDevices::cairo_pdf(
    filename = salida_pdf,
    width = 18,
    height = 8.4,
    family = fuente_base,
    pointsize = tamano_base_pt
  )
} else {
  grDevices::pdf(
    file = salida_pdf,
    width = 18,
    height = 8.4,
    family = fuente_base,
    pointsize = tamano_base_pt,
    useDingbats = FALSE
  )
}
graficar_diagrama()
dev.off()

# SVG editable: uso svglite para que los textos queden como <text>,
# es decir, como palabras/frases editables, no como letras sueltas o trazados.
# IMPORTANTE: no usar showtext ni cairo_svg para esta salida SVG.
svglite::svglite(
  file = salida_svg,
  width = 18,
  height = 8.4,
  bg = "white",
  pointsize = tamano_base_pt,
  system_fonts = list(
    sans = fuente_base,
    serif = fuente_base,
    mono = "Courier New"
  ),
  fix_text_size = FALSE
)
graficar_diagrama()
dev.off()

# 10) Diagnostico rapido en consola ---------------------------
message("Listo. Diagrama por macroafinidades creado en la carpeta de trabajo activa de RStudio: ", carpeta_salida)
message("- ", salida_pdf)
message("- ", salida_svg)

if (isTRUE(agrupar_morfotipos_por_afinidad)) {
  cat("\nMacroafinidades graficadas y morfotipos incluidos en cada una:\n")
  mapa_macroafinidades <- split(names(macroafinidad_morfotipos), macroafinidad_morfotipos)
  mapa_macroafinidades <- mapa_macroafinidades[c(
    intersect(orden_grupos, names(mapa_macroafinidades)),
    setdiff(names(mapa_macroafinidades), orden_grupos)
  )]
  print(mapa_macroafinidades)
  
  cat("\nAfinidad original de cada morfotipo:\n")
  print(data.frame(
    Morfotipo = names(afinidad_morfotipos),
    Afinidad_original = unname(afinidad_morfotipos),
    Macroafinidad = unname(macroafinidad_morfotipos),
    check.names = FALSE
  ))
} else {
  cat("\nMorfotipos graficados y afinidad asignada:\n")
  print(data.frame(Morfotipo = colnames(spec_plot), Afinidad = unname(afinidad), check.names = FALSE))
}

cat("\nSuma por nivel del diagrama principal, en % del total agrupado/alineado:\n")
print(round(rowSums(spec_plot), 2))

cat("\nCurvas auxiliares graficadas:\n")
print(colnames(aux_plot))

cat("\nIndicadores auxiliares:\n")
print(aux_out)


