# Auditoría del filtrado integrado de muestras (uso interno)

**Estado:** script original recibido y conservado. **No verificado mediante ejecución sobre la matriz utilizada en la tesis.** No reemplazar la base moderna ni recalcular funciones de transferencia sin comparar los ID y los resultados documentados.

## Hallazgo crítico: asignación incorrecta de distancias Bray–Curtis

En cada iteración se construye una matriz de **4 filas**: 1 = H.O., 2 = HOS, 3 = 0–5 cm, 4 = muestra profunda. Después se hace `v <- as.numeric(vegan::vegdist(matriz, method="bray"))`.

El vector de seis distancias (`dist`) tiene este orden:

| Índice | Par comparado |
|---|---|
| v[1] | fila 1 vs 2 (H.O. vs HOS) |
| v[2] | fila 1 vs 3 (H.O. vs 0–5) |
| v[3] | fila 1 vs 4 (H.O. vs profunda) |
| v[4] | fila 2 vs 3 (HOS vs 0–5) |
| v[5] | fila 2 vs 4 (HOS vs profunda) |
| v[6] | fila 3 vs 4 (0–5 vs profunda) |

**Corrección mínima para las tres referencias usadas en el script original (sin ejecutarla automáticamente):**

```r
D <- as.matrix(vegan::vegdist(matriz, method = "bray"))
Bray_HO    <- D[4, 1]
Bray_SedHO <- D[4, 2]
Bray_0_5   <- D[4, 3]
Bray_promedio <- mean(c(Bray_HO, Bray_SedHO, Bray_0_5))
```

**Si se sigue literalmente el apartado 4.4.5**, este menciona dos referencias sedimentarias: HOS y 0–5 cm. En ese caso, el promedio que correspondería contrastar sería `mean(D[4, 2:3])`, pero esta decisión requiere cotejar la versión realmente aplicada en los resultados.

## Diferencias con el apartado 4.4.5

1. La metodología menciona como referencias **HOS y 0–5 cm**, mientras que el código incorpora también **H.O.**; son materiales diferentes.
2. La metodología indica evaluación de los niveles **5–10, 10–15, 15–20 y 20–25 cm**. El código calcula Bray para esos cuatro, pero **solo puede excluir 15–20 y 20–25 cm**.
3. El apartado metodológico no documenta explícitamente aquí un corte **CONISS k=5** ni un umbral numérico **Bray ≥0,60**; no incorporar esos parámetros al texto sin verificar si efectivamente sustentan las 18 exclusiones.
4. El script usa Bray–Curtis sobre la matriz original, **sin normalizar cada fila**, y CONISS a partir de `dist(sqrt(matriz))`; las versiones anteriores usan otras transformaciones. Verificar si la base ya contenía frecuencias relativas y si todos los perfiles tienen la misma escala.

## Controles de integridad

5. `Perfil <- rep(1:n_perfiles, each=7)` presupone que el Excel está ordenado en bloques exactos de siete filas por perfil. Validar contra `Estación + Perfil + ID` del MS1, porque `Perfil` numérico interno no es el identificador real.
6. `datos$Cuenca <- datos[[1]]` supone que la primera columna contiene la cuenca. Debe verificarse el encabezado y su consistencia.
7. El bloque de morfotipos debe existir, ser contiguo y no incluir metadatos; el script no comprueba que `Acute bulbosus` aparezca antes de `Crenate`.
8. La moda de los tres grupos superficiales con CONISS k=5 puede estar empatada; `sort(table(...))` elige un grupo en un empate sin dejar constancia. Exigir decisión explícita o reportar empates.
9. Exportar una tabla de control con **Estación, Perfil, ID, Profundidad, Bray_HOS, Bray_0_5, grupo CONISS, decisión**. Comparar los 18 ID y los nueve perfiles de la tesis con la hoja final efectivamente usada para calibración.

## Recomendación de cierre

**No puede afirmarse aún que este script reproduzca exactamente la exclusión de las 18 muestras.** Conservar la versión original, ejecutar una versión auditada solo sobre la matriz histórica (sin sobrescribir archivos), inspeccionar diferencias y validar la lista de códigos de colección frente a la base final. No modificar modelos ya publicados por una discrepancia de código sin primero establecer qué archivo/salida fue la base real de entrenamiento.
