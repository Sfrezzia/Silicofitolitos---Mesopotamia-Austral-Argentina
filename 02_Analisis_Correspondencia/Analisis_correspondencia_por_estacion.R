# ============================================================
# MS3 | ANALISIS DE CORRESPONDENCIA (AC) - POR ESTACION
# Versión GENERALIZADA de la lógica del AC de PNPD
# Unidad de análisis: cada Excel de estación (puede tener 1 o
# varios perfiles y una cantidad variable de muestras).
#
# ENTRADA: Excel con columna Muestra (o ID) y columnas de
# grupos fitolíticos. Se puede incluir la columna Perfil.
# Si no existe Perfil, se reconstruye a partir de filas que
# comienzan con H.O. (p. ej. H.O PNPD 1), sin asumir 7 filas.
# Si no existen marcadores H.O., se asume un perfil en el
# archivo; para más de uno es necesario agregar Perfil.
#
# SALIDAS: 3 figuras (SVG) + 1 PDF de muestras y grupos,
# 3 CSV de coordenadas / contribución y tabla de muestras.
#
# IMPORTANTE: los segmentos de los grupos representan las
# coordenadas de las columnas de un AC, NO son los vectores
# de ajuste ambiental calculados con envfit().
# ============================================================

# 1. Paquetes -----------------------------------------------
paquetes <- c('readxl','FactoMineR','ggplot2','ggrepel','dplyr','svglite')
por_instalar <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if(length(por_instalar)) install.packages(por_instalar, repos='https://cloud.r-project.org')
library(readxl)
library(FactoMineR)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(svglite)

# 2. Configuración ------------------------------------------
# Un Excel por estación; no se introduce número fijo de perfiles.
archivo <- file.choose()
hojas <- readxl::excel_sheets(archivo)
if('Hoja1' %in% hojas){
  hoja <- 'Hoja1'
} else if(length(hojas) == 1){
  hoja <- hojas[1]
} else {
  cat('Hojas disponibles: ', paste(hojas, collapse=' | '), '\n')
  hoja <- readline('Escribí el nombre de la hoja con el AC: ')
  if(!hoja %in% hojas) stop('La hoja indicada no existe en el Excel.')
}

# Grupos de mayor contribución conjunta a Dim 1 + Dim 2
n_grupos_destacados <- 12L
# Si el Excel contiene varias estaciones y se quiere un AC por
# estación, poner TRUE. Por defecto se hace uno por archivo.
separar_por_estacion <- FALSE

# Carpeta de salida, adyacente al Excel y sin sobreescribir
# resultados de otras estaciones/perfiles.
nombre_archivo <- tools::file_path_sans_ext(basename(archivo))
normalizar_archivo <- function(x){
  x <- iconv(as.character(x), from='', to='ASCII//TRANSLIT')
  x <- gsub('[^A-Za-z0-9_-]+','_',x)
  x <- gsub('^_+|_+$','',x)
  if(is.na(x) || !nzchar(x)) 'Estacion' else x
}
identificador <- normalizar_archivo(nombre_archivo)
carpeta_salida <- file.path(dirname(archivo), paste0('AC_',identificador))
dir.create(carpeta_salida, recursive=TRUE, showWarnings=FALSE)

# 3. Datos y control de nombres ------------------------------
datos <- as.data.frame(readxl::read_excel(archivo, sheet=hoja), check.names=FALSE)
names(datos) <- trimws(names(datos))
if(anyDuplicated(names(datos))) stop('Hay encabezados duplicados: revisá el Excel.')
if(nrow(datos) < 2) stop('El Excel debe tener al menos dos muestras.')

buscar_campo <- function(nms, posibles){
  norm <- tolower(iconv(nms,from='',to='ASCII//TRANSLIT'))
  candidatos <- which(norm %in% posibles)
  if(length(candidatos)) nms[candidatos[1]] else NA_character_
}
col_muestra <- buscar_campo(names(datos),c('muestra','id','codigo_muestra','codigo'))
if(is.na(col_muestra)) {
  stop('Falta una columna Muestra o ID. Agregala para identificar cada fila.')
}
col_perfil <- buscar_campo(names(datos),c('perfil','perfil_id'))
col_estacion <- buscar_campo(names(datos),c('estacion','estacion_id'))

muestras <- trimws(as.character(datos[[col_muestra]]))
if(anyNA(muestras) || any(!nzchar(muestras))) stop('Hay identificadores de muestra vacíos.')

# 4. Reconstrucción de perfiles, si no existe Perfil ----------
if(!is.na(col_perfil)){
  perfiles <- trimws(as.character(datos[[col_perfil]]))
  if(anyNA(perfiles) || any(!nzchar(perfiles))) stop('Hay filas sin Perfil.')
} else {
  # Detecta H.O., HO, H.O PNPD 1, H.O_1.
  es_ho <- grepl('^\\s*H\\s*\\.?\\s*O\\.?($|[[:space:]_])',
                 muestras, ignore.case=TRUE, perl=TRUE)
  perfiles <- character(length(muestras))
  if(!any(es_ho)){
    warning('No se encontraron cabeceras H.O.: se asigna un solo perfil. Si hay más de uno, incorporá la columna Perfil.')
    perfiles[] <- 'Perfil_1'
  } else {
    perfil_activo <- NA_character_
    contador <- 0L
    for(i in seq_along(muestras)){
      if(es_ho[i]){
        contador <- contador+1L
        sufijo <- sub('^\\s*H\\s*\\.?\\s*O\\.?\\s*[_[:space:]]*',
                     '',muestras[i],ignore.case=TRUE,perl=TRUE)
        sufijo <- trimws(sufijo)
        perfil_activo <- if(nzchar(sufijo)) sufijo else paste0('Perfil_',contador)
      }
      if(is.na(perfil_activo)) stop('Hay muestras anteriores al primer H.O.; agregá Perfil o corregí el orden.')
      perfiles[i] <- perfil_activo
    }
  }
}

# No se presupone un número constante de muestras por perfil.
if(!is.na(col_estacion)){
  estaciones <- trimws(as.character(datos[[col_estacion]]))
  if(anyNA(estaciones) || any(!nzchar(estaciones))) stop('Hay filas sin Estación.')
} else {
  estaciones <- rep(nombre_archivo, nrow(datos))
}

# 5. Reconocer grupos fitolíticos y excluir metadatos --------
# El AC original tenía primera columna Muestra y luego grupos.
# Se excluyen explícitamente columnas ambientales, identificadores
# y metadatos, incluso cuando R las interprete como numéricas.
es_meta <- function(nm){
  v <- tolower(iconv(nm, from='', to='ASCII//TRANSLIT'))
  grepl('^(muestra|id|codigo|perfil|estacion|cuenca|fisionomia|profundidad|orden|nivel|textura|latitud|longitud|altitud|tma|pma|ema|ph|mo)([ _.(]|$)',v) |
    grepl('silice|materia organica|temperatura|precipitacion|coordenada|fecha|codigo',v)
}
vars_candidatas <- names(datos)[!vapply(names(datos), es_meta, logical(1))]
if(length(vars_candidatas) < 2) stop('No se identificaron al menos dos columnas de grupos fitolíticos.')

a_numero <- function(x){
  z <- trimws(as.character(x))
  faltante <- is.na(z) | z %in% c('','NA','N/A','ND','S/D','-')
  z <- gsub(',','.',z,fixed=TRUE)
  valor <- suppressWarnings(as.numeric(z))
  if(any(!faltante & is.na(valor))) return(NULL)
  valor
}
columnas_numericas <- lapply(datos[,vars_candidatas,drop=FALSE],a_numero)
es_numerica <- !vapply(columnas_numericas,is.null,logical(1))
excluidas <- vars_candidatas[!es_numerica]
if(length(excluidas)){
  message('Columnas no numéricas omitidas: ',paste(excluidas,collapse=', '))
}
if(sum(es_numerica) < 2) stop('Faltan dos columnas numéricas de grupos fitolíticos.')
matriz_todos <- as.data.frame(columnas_numericas[es_numerica],check.names=FALSE)
names(matriz_todos) <- vars_candidatas[es_numerica]
matriz_todos[is.na(matriz_todos)] <- 0
if(any(as.matrix(matriz_todos) < 0)) stop('Hay abundancias negativas.')

cat('\nPerfiles detectados y cantidad de muestras:\n')
print(table(perfiles))
cat('\nGrupos fitolíticos candidatos: ',ncol(matriz_todos),'\n')

# 6. AC por archivo o por estación ---------------------------
indices_listado <- if(separar_por_estacion) {
  split(seq_len(nrow(datos)),estaciones)
} else {
  list(Total=seq_len(nrow(datos)))
}

for(etiqueta in names(indices_listado)){
  ii <- indices_listado[[etiqueta]]
  dat <- matriz_todos[ii,,drop=FALSE]
  rownames(dat) <- make.unique(muestras[ii])
  # Eliminar grupos y muestras con total cero, sin asumir 7 filas.
  dat <- dat[,colSums(dat)>0,drop=FALSE]
  validas <- rowSums(dat)>0
  if(sum(validas)<2 || ncol(dat)<2){
    warning('Se omite ',etiqueta,': faltan muestras o grupos con recuentos positivos.')
    next
  }
  dat <- dat[validas,,drop=FALSE]
  muestras_sub <- muestras[ii][validas]
  perfiles_sub <- perfiles[ii][validas]
  estaciones_sub <- estaciones[ii][validas]
  if(nrow(dat)<3) warning('La ordenación AC tiene menos de tres muestras; interpretar con cautela.')
  res.ca <- FactoMineR::CA(dat, graph=FALSE)
  if(nrow(res.ca$eig) < 2){
    warning('Se omite ',etiqueta,': no hay dos dimensiones disponibles.')
    next
  }
  porcentaje <- as.numeric(res.ca$eig[1:2,2])
  coord_m <- as.data.frame(res.ca$row$coord[,1:2,drop=FALSE])
  names(coord_m) <- c('Dim1','Dim2')
  coord_m$Muestra <- rownames(coord_m)
  coord_m$Perfil <- as.factor(perfiles_sub)
  coord_m$Estacion <- estaciones_sub
  coord_f <- as.data.frame(res.ca$col$coord[,1:2,drop=FALSE])
  names(coord_f) <- c('Dim1','Dim2')
  coord_f$Grupo_fitolitico <- rownames(coord_f)
  coord_f$Contribucion_Dim1_Dim2 <- as.numeric(
    res.ca$col$contrib[,1]+res.ca$col$contrib[,2])
  ord <- order(coord_f$Contribucion_Dim1_Dim2,decreasing=TRUE)
  n_mostrar <- min(as.integer(n_grupos_destacados), nrow(coord_f))
  top_f <- coord_f[ord[seq_len(n_mostrar)],,drop=FALSE]

  # Escalado SOLO VISUAL: los puntos de grupos son coordenadas
  # del AC, no regresiones ambientales ni biplot de PCA.
  rm <- max(abs(as.matrix(coord_m[,c('Dim1','Dim2')])))
  rf <- max(abs(as.matrix(top_f[,c('Dim1','Dim2')])))
  escala <- if(is.finite(rf) && rf>0) 0.85*rm/rf else 1
  top_f$X <- top_f$Dim1*escala
  top_f$Y <- top_f$Dim2*escala
  eje_x <- sprintf('Dim 1 (%.1f%%)',porcentaje[1])
  eje_y <- sprintf('Dim 2 (%.1f%%)',porcentaje[2])

  tema <- ggplot2::theme_minimal(base_size=11) +
    ggplot2::theme(legend.position='right')
  ejes <- list(
    ggplot2::geom_hline(yintercept=0,linetype='dashed',color='grey70'),
    ggplot2::geom_vline(xintercept=0,linetype='dashed',color='grey70'))
  p_muestras <- ggplot(coord_m,aes(Dim1,Dim2,color=Perfil)) + ejes +
    geom_point(size=2.6) +
    ggrepel::geom_text_repel(aes(label=Muestra),size=3,show.legend=FALSE) +
    labs(title='AC - muestras por perfil',x=eje_x,y=eje_y,color='Perfil') + tema
  p_grupos <- ggplot(coord_f,aes(Dim1,Dim2)) + ejes +
    geom_point(size=2) +
    ggrepel::geom_text_repel(aes(label=Grupo_fitolitico),size=2.7) +
    labs(title='AC - grupos fitolíticos',x=eje_x,y=eje_y)+tema
  p_completo <- ggplot() + ejes +
    geom_point(data=coord_m,aes(Dim1,Dim2,color=Perfil),size=2.6)+
    ggrepel::geom_text_repel(data=coord_m,
             aes(Dim1,Dim2,label=Muestra,color=Perfil),size=3,show.legend=FALSE)+
    geom_segment(data=top_f,aes(x=0,y=0,xend=X,yend=Y),
                 arrow=grid::arrow(length=grid::unit(0.22,'cm')),
                 color='black',linewidth=0.5)+
    ggrepel::geom_text_repel(data=top_f,
             aes(X,Y,label=Grupo_fitolitico),color='black',size=3)+
    labs(title='AC - muestras y grupos fitolíticos',x=eje_x,y=eje_y,
         color='Perfil')+tema

  sufijo <- if(separar_por_estacion) paste0('_',normalizar_archivo(etiqueta)) else ''
  ggsave(file.path(carpeta_salida,paste0('AC_muestras',sufijo,'.svg')),
         p_muestras,width=8,height=6)
  ggsave(file.path(carpeta_salida,paste0('AC_muestras_y_grupos',sufijo,'.svg')),
         p_completo,width=10,height=7)
  ggsave(file.path(carpeta_salida,paste0('AC_grupos',sufijo,'.svg')),
         p_grupos,width=10,height=7)
  ggsave(file.path(carpeta_salida,paste0('AC_muestras_y_grupos',sufijo,'.pdf')),
         p_completo,width=10,height=7)
  write.csv(coord_m,file.path(carpeta_salida,
            paste0('AC_coordenadas_muestras',sufijo,'.csv')),row.names=FALSE)
  write.csv(coord_f,file.path(carpeta_salida,
            paste0('AC_coordenadas_grupos',sufijo,'.csv')),row.names=FALSE)
  write.csv(top_f,file.path(carpeta_salida,
            paste0('AC_grupos_mayor_contribucion',sufijo,'.csv')),row.names=FALSE)
  cat('AC exportado en:',carpeta_salida,' | Estación:',etiqueta,
      ' | Muestras:',nrow(dat),' | Grupos:',ncol(dat),'\n')
}
cat('\nAnálisis de correspondencia finalizado.\n')
