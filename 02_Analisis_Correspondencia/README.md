# Análisis de correspondencia (AC) — por estación

**Script principal:** `Analisis_correspondencia_por_estacion.R`.

A partir del AC de PNPD recibido por texto en la conversación, se preparó una **adaptación generalizada**, no una copia estadísticamente validada: se lee un Excel de una estación, que puede contener **un número variable de perfiles y de muestras**, y se realiza un AC conjunto de las muestras de esa estación. Produce figuras con muestras, grupos fitolíticos y ambas categorías, además de tablas de coordenadas.

## Excel de entrada

- Una fila por muestra; columna `Muestra` o `ID`.
- Una columna numérica por **grupo fitolítico**; los nombres originales se conservan.
- **Recomendado:** columna `Perfil` para identificar inequívocamente cada perfil.
- Si no hay `Perfil`, el script intenta reconstruir los bloques con filas encabezadas por H.O. (por ejemplo, `H.O PNPD 1`). Si no encuentra dichos marcadores, interpreta que el Excel contiene **un solo perfil**; si no fuera así, agregá `Perfil` antes de correrlo.
- Si el archivo contiene varias estaciones, se puede activar `separar_por_estacion <- TRUE`; por defecto analiza todo el Excel como una unidad, por lo que conviene **un archivo por estación**.
- Se usa la hoja `Hoja1` cuando existe, o la única hoja, o se solicita la hoja cuando hay varias.

**No presupone cuatro perfiles, ni siete muestras por perfil, ni utiliza los nombres PNPD de manera fija.** Tampoco deben incluirse columnas no biológicas como si fueran grupos fitolíticos; las variables ambientales más habituales se excluyen por nombre, pero se debe revisar la tabla que imprime R.

## Salidas

En una carpeta nueva `AC_<nombre del Excel>/` se exportan: `AC_muestras.svg`, `AC_muestras_y_grupos.svg` y `.pdf`, `AC_grupos.svg`, y CSV de muestras, grupos y contribuciones.

**Cuidado metodológico:** los segmentos que parten del origen visualizan coordenadas de columnas del AC con una escala gráfica. No son vectores de `envfit` ni resultados de un análisis de correspondencia canónica. **Falta ejecutar y cotejar** este script contra el AC de PNPD y los demás conjuntos antes de publicar el repositorio.
