# ===================================================================
# MS3 | Seleccion de muestras: Bray-Curtis y CONISS k=5
# Alcance: NO VALIDADO: error de indexacion en distancias Bray-Curtis. No usar para filtrar datos sin auditoria.
# Nombre normalizado para el repositorio; analisis sin cambios.
# ===================================================================

############################################################
# FILTRADO FUNCIONES DE TRANSFERENCIA
# BRAY-CURTIS + CONISS k=5
#
# CRITERIO:
#
# Excluir solamente:
# 15-20 cm y 20-25 cm
# Bray >= 0.60
# CONISS separado del grupo superficial
#
############################################################


#------------------------------------------------------------
# PAQUETES
#------------------------------------------------------------

paquetes <- c(
  "readxl",
  "dplyr",
  "vegan",
  "rioja",
  "writexl"
)


instalar <- paquetes[
  !(paquetes %in% installed.packages()[,"Package"])
]


if(length(instalar)>0){
  install.packages(instalar)
}


library(readxl)
library(dplyr)
library(vegan)
library(rioja)
library(writexl)


#------------------------------------------------------------
# IMPORTAR BASE
#------------------------------------------------------------

archivo <- file.choose()

datos <- read_excel(
  archivo
)

datos <- as.data.frame(datos)

cat("Filas importadas:")
print(nrow(datos))


#------------------------------------------------------------
# CREAR PERFILES
# 7 FILAS POR PERFIL
#------------------------------------------------------------

datos$Cuenca <- datos[[1]]

if(nrow(datos) %% 7 != 0){
  stop(
    "La base no tiene múltiplos de 7 filas"
  )
}

n_perfiles <- nrow(datos)/7

datos$Perfil <- rep(
  1:n_perfiles,
  each=7
)

datos$Orden_perfil <- rep(
  1:7,
  times=n_perfiles
)


#------------------------------------------------------------
# PROFUNDIDAD
#------------------------------------------------------------

datos$Profundidad <- c(
  "H.O",
  "Sedimento H.O",
  "0-5 cm",
  "5-10 cm",
  "10-15 cm",
  "15-20 cm",
  "20-25 cm"
)[datos$Orden_perfil]


#------------------------------------------------------------
# VARIABLES FITOLÍTICAS
#------------------------------------------------------------

inicio <- which(
  names(datos)=="Acute bulbosus"
)

fin <- which(
  names(datos)=="Crenate"
)

vars <- names(datos)[inicio:fin]


#------------------------------------------------------------
# OBJETOS
#------------------------------------------------------------

bray_tabla <- data.frame()
coniss_tabla <- data.frame()


#------------------------------------------------------------
# BRAY CURTIS
#------------------------------------------------------------

for(p in 1:n_perfiles){

  perfil <- datos %>%
    filter(
      Perfil==p
    )

  referencias <- perfil[1:3,vars]

  for(i in 4:7){

    matriz <- rbind(
      referencias,
      perfil[i,vars]
    )

    matriz <- data.frame(
      lapply(
        matriz,
        as.numeric
      )
    )

    matriz[is.na(matriz)] <- 0

    bc <- vegdist(
      matriz,
      method="bray"
    )

    v <- as.numeric(bc)

    bray_tabla <- rbind(
      bray_tabla,
      data.frame(
        Cuenca=perfil$Cuenca[1],
        Perfil=p,
        Orden_perfil=i,
        Profundidad=perfil$Profundidad[i],
        Bray_HO=v[1],
        Bray_SedHO=v[2],
        Bray_0_5=v[3],
        Bray_promedio=
          mean(v[1:3])
      )
    )
  }
}


#------------------------------------------------------------
# CONISS k=5
#------------------------------------------------------------

for(p in 1:n_perfiles){

  perfil <- datos %>%
    filter(
      Perfil==p
    )

  matriz <- perfil[,vars]

  matriz <- data.frame(
    lapply(
      matriz,
      as.numeric
    )
  )

  matriz[is.na(matriz)] <- 0

  modelo <- chclust(
    dist(
      sqrt(matriz),
      method="euclidean"
    ),
    method="coniss"
  )

  grupos <- cutree(
    modelo,
    k=5
  )

  grupo_superficial <- names(
    sort(
      table(grupos[1:3]),
      decreasing=TRUE
    )
  )[1]

  coniss_tabla <- rbind(
    coniss_tabla,
    data.frame(
      Cuenca=perfil$Cuenca[1],
      Perfil=p,
      Orden_perfil=1:7,
      CONISS_k5=grupos,
      Grupo_superficial=grupo_superficial
    )
  )
}


#------------------------------------------------------------
# INTEGRAR
#------------------------------------------------------------

calculos <- left_join(
  bray_tabla,
  coniss_tabla,
  by=c(
    "Cuenca",
    "Perfil",
    "Orden_perfil"
  )
)


#------------------------------------------------------------
# AGREGAR A BASE ORIGINAL
#------------------------------------------------------------

base_final <- left_join(
  datos,
  calculos,
  by=c(
    "Cuenca",
    "Perfil",
    "Orden_perfil"
  )
)


#------------------------------------------------------------
# DECISION FINAL
#------------------------------------------------------------

base_final$Decision <- "Mantener"

# referencias actuales
base_final$Decision[
  base_final$Orden_perfil <=3
] <-
  "Mantener referencia actual"

# criterio de exclusión
base_final$Decision[
  base_final$Orden_perfil >=6 &
    base_final$Bray_promedio >=0.60 &
    base_final$CONISS_k5 != base_final$Grupo_superficial
] <-
  "Excluir"


#------------------------------------------------------------
# EXPORTAR
#------------------------------------------------------------

write_xlsx(
  list(
    Base_completa=base_final,
    Muestras_excluir=
      base_final %>%
      filter(
        Decision=="Excluir"
      )
  ),
  "Base_general_Bray_CONISS_k5_060.xlsx"
)

write_xlsx(
  list(
    Bray=bray_tabla,
    CONISS_k5=coniss_tabla,
    Calculos_integrados=calculos
  ),
  "Calculos_CONISS_k5_Bray060.xlsx"
)

cat("\n============================\n")
cat("FINALIZADO\n")
cat("CONISS k=5 + Bray >=0.60\n")
cat("============================\n")
