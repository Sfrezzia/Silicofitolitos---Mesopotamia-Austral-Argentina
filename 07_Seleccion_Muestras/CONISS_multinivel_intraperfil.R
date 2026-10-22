# ===================================================================
# MS3 | CONISS multinivel intraperfil
# Alcance: Version historica: asume seis niveles; requiere validacion.
# Nombre normalizado para el repositorio; analisis sin cambios.
# ===================================================================

############################################################
# CONISS MULTINIVEL - PARANA FITOLITOS
# Selección de muestras para funciones de transferencia
############################################################

#------------------------------------------------------------
# 1. PAQUETES
#------------------------------------------------------------

paquetes <- c(
  "readxl",
  "rioja",
  "vegan",
  "dplyr",
  "writexl"
)

instalar <- paquetes[
  !(paquetes %in% installed.packages()[,"Package"])
]

if(length(instalar)>0){
  install.packages(instalar)
}

library(readxl)
library(rioja)
library(vegan)
library(dplyr)
library(writexl)

#------------------------------------------------------------
# 2. CARGAR EXCEL
#------------------------------------------------------------

archivo <- file.choose()

hojas <- excel_sheets(archivo)

cat("\nHojas disponibles:\n")
print(hojas)

hoja <- readline(
  prompt="\nEscriba el nombre exacto de la hoja con silicofitolitos: "
)

datos <- read_excel(
  archivo,
  sheet = hoja
)

datos <- as.data.frame(
  datos,
  check.names = FALSE
)

#------------------------------------------------------------
# 3. IDENTIFICAR PERFILES
#------------------------------------------------------------

if(!"Estación" %in% names(datos)){
  stop(
    "No existe la columna 'Estación'. Revisar nombre."
  )
}

datos$Perfil <- trimws(
  as.character(datos$Estación)
)

cat("\nPERFILES ENCONTRADOS:\n")
print(unique(datos$Perfil))

cat("\nMUESTRAS POR PERFIL:\n")
print(table(datos$Perfil))

#------------------------------------------------------------
# 4. ORDEN ESTRATIGRÁFICO
#------------------------------------------------------------

datos <- datos %>%
  group_by(Perfil) %>%
  mutate(
    Orden = row_number()
  ) %>%
  ungroup()

datos$Profundidad <- case_when(
  datos$Orden == 1 ~ "H.O.",
  datos$Orden == 2 ~ "0-5 cm",
  datos$Orden == 3 ~ "5-10 cm",
  datos$Orden == 4 ~ "10-15 cm",
  datos$Orden == 5 ~ "15-20 cm",
  datos$Orden == 6 ~ "20-25 cm",
  TRUE ~ NA_character_
)

#------------------------------------------------------------
# 5. VARIABLES DE FITOLITOS
#------------------------------------------------------------

# toma todas las columnas numéricas
vars_fito <- names(datos)[
  sapply(datos, is.numeric)
]

# eliminar columnas que no son fitolitos
vars_fito <- setdiff(
  vars_fito,
  c("Orden")
)

cat("\nVariables usadas:\n")
print(vars_fito)

#------------------------------------------------------------
# 6. LOOP CONISS
#------------------------------------------------------------

resultado <- list()
perfiles <- unique(datos$Perfil)

for(p in perfiles){
  cat("\n============================\n")
  cat("Procesando:",p,"\n")
  cat("============================\n")

  sub <- datos %>%
    filter(Perfil == p)

  if(nrow(sub)!=6){
    cat("Se omite porque no tiene 6 muestras\n")
    next
  }

  matriz <- sub[,vars_fito]

  matriz <- data.frame(
    lapply(matriz, as.numeric)
  )

  matriz[is.na(matriz)] <- 0

  # eliminar variables sin variación
  matriz <- matriz[,
    apply(matriz, 2, sd)>0,
    drop=FALSE
  ]

  if(ncol(matriz)<2){
    cat("Sin variación suficiente\n")
    next
  }

  #------------------------------------------------
  # MISMA TRANSFORMACIÓN QUE EL DIAGRAMA
  #------------------------------------------------

  matriz_rel <- decostand(
    matriz,
    method="total"
  )

  diss <- dist(
    sqrt(matriz_rel)
  )

  # CONISS
  clust <- rioja::chclust(
    diss,
    method="coniss"
  )

  # diferentes niveles
  k2 <- cutree(clust, k=2)
  k3 <- cutree(clust, k=3)
  k4 <- cutree(clust, k=4)

  tabla <- data.frame(
    Perfil=p,
    Muestra=sub$Orden,
    Profundidad=sub$Profundidad,
    CONISS_k2=k2,
    CONISS_k3=k3,
    CONISS_k4=k4
  )

  resultado[[length(resultado)+1]] <- tabla

  #------------------------------------------------
  # DENDROGRAMA
  #------------------------------------------------

  try({
    pdf(
      paste0(
        "CONISS_",
        gsub(" ", "_", p),
        ".pdf"
      ),
      width=7,
      height=5
    )

    plot(
      clust,
      main=paste("CONISS", p)
    )

    rect.hclust(
      clust,
      k=2,
      border="red"
    )

    rect.hclust(
      clust,
      k=3,
      border="blue"
    )

    rect.hclust(
      clust,
      k=4,
      border="darkgreen"
    )

    dev.off()
  })
}

#------------------------------------------------------------
# EXPORTAR RESULTADOS
#------------------------------------------------------------

CONISS_final <- bind_rows(
  resultado
)

write_xlsx(
  CONISS_final,
  "CONISS_multinivel_intraperfil.xlsx"
)

cat("\n\n================================\n")
cat("ANÁLISIS TERMINADO\n")
cat("Archivo creado:\n")
cat("CONISS_multinivel_intraperfil.xlsx\n")
cat("================================\n")
