# ===================================================================
# MS3 | Bray-Curtis intraperfil
# Alcance: Version historica: asume seis niveles; requiere validacion.
# Nombre normalizado para el repositorio; analisis sin cambios.
# ===================================================================

############################################################
# BRAY-CURTIS DENTRO DE PERFILES
# PARANÁ - SILICOFITOLITOS
# Evaluación de divergencia respecto a:
# 1) Horizonte orgánico (H.O.)
# 2) Horizonte superficial 0-5 cm
############################################################

# 1. PAQUETES
paquetes <- c("readxl", "vegan", "dplyr", "stringr", "writexl")
instalar <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(instalar)>0){ install.packages(instalar) }
library(readxl)
library(vegan)
library(dplyr)
library(stringr)
library(writexl)

# 2. IMPORTAR BASE
archivo <- file.choose()
datos <- read_excel(archivo)
datos <- as.data.frame(datos)
cat("\nColumnas disponibles:\n")
print(names(datos))

# 3. CREAR IDENTIFICADOR DE PERFIL
# La columna debe llamarse Estación
datos$Perfil_ID <- as.character(datos$Estación)
cat("\nPerfiles encontrados:\n")
print(unique(datos$Perfil_ID))

# 4. ORDEN DE MUESTRAS
# Se asume que cada perfil tiene 6 filas en orden vertical
datos <- datos %>%
  group_by(Perfil_ID) %>%
  mutate(Numero_muestra = row_number()) %>%
  ungroup()
datos$Profundidad <- case_when(
  datos$Numero_muestra == 1 ~ "H.O.",
  datos$Numero_muestra == 2 ~ "0-5 cm",
  datos$Numero_muestra == 3 ~ "5-10 cm",
  datos$Numero_muestra == 4 ~ "10-15 cm",
  datos$Numero_muestra == 5 ~ "15-20 cm",
  datos$Numero_muestra == 6 ~ "20-25 cm"
)

# 5. VARIABLES FITOLÍTICAS
inicio <- which(names(datos)=="Acute bulbosus")
fin <- which(names(datos)=="Crenate")
if(length(inicio)==0 | length(fin)==0){
  stop("No se encontraron las columnas de fitolitos")
}
vars_fito <- names(datos)[inicio:fin]
cat("\nFitolitos utilizados:\n")
print(vars_fito)

# 6. FUNCIÓN BRAY-CURTIS
calcular_bray <- function(x, referencia){
  matriz <- rbind(referencia, x)
  matriz <- as.data.frame(lapply(matriz, as.numeric))
  matriz[is.na(matriz)] <- 0
  matriz_rel <- decostand(matriz, method="total")
  distancia <- vegdist(matriz_rel, method="bray")
  as.numeric(distancia[1])
}

# 7. CALCULAR POR PERFIL
resultados <- list()
perfiles <- unique(datos$Perfil_ID)
for(p in perfiles){
  sub <- datos %>% filter(Perfil_ID == p)
  if(nrow(sub)!=6){
    warning(paste("Perfil", p, "no tiene 6 muestras"))
  }
  matriz <- sub[,vars_fito]
  HO <- matriz[1,]
  superficial <- matriz[2,]
  for(i in 1:nrow(matriz)){
    resultados[[length(resultados)+1]] <- data.frame(
      Perfil=p,
      Muestra=i,
      Profundidad=sub$Profundidad[i],
      Bray_HO=calcular_bray(matriz[i,], HO),
      Bray_0_5=calcular_bray(matriz[i,], superficial)
    )
  }
}
tabla <- bind_rows(resultados)

# 8. PROMEDIO DE REFERENCIAS
tabla <- tabla %>%
  mutate(Bray_promedio=(Bray_HO + Bray_0_5)/2)

# 9. INTERPRETACIÓN
tabla <- tabla %>%
  mutate(Interpretacion=case_when(
    Bray_promedio < 0.20 ~ "Baja diferenciación",
    Bray_promedio < 0.40 ~ "Diferenciación moderada",
    TRUE ~ "Alta diferenciación composicional"
  ))

# 10. RANKING DE MUESTRAS DIVERGENTES
divergentes <- tabla %>%
  filter(Muestra > 2) %>%
  arrange(desc(Bray_promedio))

# 11. RESUMEN POR PERFIL
resumen <- divergentes %>%
  group_by(Perfil) %>%
  slice_max(Bray_promedio, n=1)

# 12. EXPORTAR
write_xlsx(
  list(
    Bray_por_muestra=tabla,
    Muestras_divergentes=divergentes,
    Resumen_perfiles=resumen
  ),
  "Bray_Curtis_intraperfil_referencias.xlsx"
)
cat("\n====================================\n")
cat("ANÁLISIS FINALIZADO\n")
cat("Archivo generado:\n")
cat("Bray_Curtis_intraperfil_referencias.xlsx\n")
cat("====================================\n")
