[README.txt](https://github.com/user-attachments/files/34163939/README.txt)
ANÁLISIS FITOLÍTICOS DE LA MESOPOTAMIA AUSTRAL
Repositorio de scripts de R

OBJETIVO
Este repositorio organiza códigos utilizados para explorar asociaciones de
silicofitolitos, su variación espacial y su relación con variables ambientales.
Los archivos permiten consultar los procedimientos de análisis, el flujo de
trabajo, los paquetes y funciones empleados, las entradas requeridas y las
salidas previstas.

ORGANIZACIÓN
01_Diagramas_CONISS/         Diagramas fitolíticos, rarefacción y CONISS.
02_Analisis_Correspondencia/ Análisis de correspondencia por estación.
03_NMDS/                    Ordenaciones NMDS por cuenca.
04_Diversidad_Beta/         Disimilitud de Bray-Curtis y diversidad beta.
05_Mapas_IDW/               Interpolaciones espaciales exploratorias.
06_Seleccion_Variables/     Gradientes, correlaciones y ordenación restringida.
07_Seleccion_Muestras/      Evaluación de muestras mediante Bray-Curtis y CONISS.
08_Funciones_Transferencia/  Calibración y validación de modelos predictivos.

REQUISITOS Y ARCHIVOS DE ENTRADA
- Entorno de referencia: R 4.1.2 y RStudio.
- Los análisis utilizan paquetes de R especificados en los respectivos scripts.
- Cada script requiere uno o más archivos de Microsoft Excel (.xlsx) con una
  estructura de entrada particular: nombres de hojas, columnas, identificadores
  y disposición de los datos. Deben respetarse esos formatos para ejecutarlo.
- Algunos scripts permiten seleccionar el archivo Excel mediante file.choose().
- Ciertos análisis requieren también información cartográfica.
- Las matrices de entrada y los resultados no se consideran automáticamente
  incluidos por el solo hecho de publicar los scripts.

ALCANCE Y PRECAUCIONES
Los códigos se comparten para documentar los procedimientos, sus dependencias
y la relación entre datos de entrada, operaciones y salidas. Las rutinas
corresponden a distintas escalas de análisis (perfil, estación, cuenca o región),
por lo que no deben considerarse intercambiables.

La disponibilidad de los scripts no implica que todos hayan sido ejecutados
con los archivos publicados ni que sus resultados se hayan contrastado con
las figuras y tablas definitivas. Algunas rutinas, especialmente las de
selección de muestras, requieren revisión antes de reutilizar sus salidas.
