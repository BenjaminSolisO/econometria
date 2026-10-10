# Plan: curso "Álgebra matricial aplicada" (14 clases)

## Contexto

El sitio Quarto `econometria-cursos` (Pages: benjaminsoliso.github.io/econometria) tiene Eco 1–4. Eco 1, Clase 1 resume el álgebra en una sola clase, y el usuario quiere dos cursos de base: **Álgebra matricial aplicada** (estilo ingeniería/física, se hace primero) y, después, **Álgebra lineal pura** (nivel licenciatura en matemáticas). Eco 5 (finanzas) queda para más adelante.

Decisiones del usuario:
- 14 clases: las 10 del programa base, más Jordan/funciones de matrices, matrices complejas, optimización lineal y convexa, y mínimos cuadrados con restricciones y problemas inversos.
- FFT, Kronecker/vec y sistemas grandes y dispersos **se van al curso puro**.
- **Enunciados generales (n×n)**, con demostraciones generales escritas **paso a paso, sin saltarse nada**. **Todos los ejemplos, ilustraciones y ejercicios en 2D y 3D** (matrices 2×2 y 3×3, vectores de R² y R³). Lo abstracto (espacios vectoriales generales, dimensión infinita) queda para el curso puro.
- Muchas figuras estáticas: 3–5 por clase.

## Normas de escritura (valen para las 14 clases)

- **Estructura y convenciones del sitio:** las mismas de Eco 1–4. Recuadro `callout-note "Borrador"` antes de cada demostración, `.proof`, notas al pie al margen y ejercicios con `callout-tip collapse="true"`. Crossrefs `{#thm-…}` y `@thm-…` solo dentro de la misma clase; entre clases se cita "Clase N".
- **Sección ★:** se titula "★ Para ir más allá" (no "Postgrado"; el curso es de pregrado). Usa `callout-important`.
- **Algoritmos:** recuadro `callout-caution title="Algoritmo"` con los pasos numerados, el costo en operaciones y cuándo falla.
- **Demostraciones sin atajos:** prohibidos "es fácil ver" y "análogamente" sin escribir el caso. Cada igualdad de una cadena lleva su justificación. Los índices y las sumas se expanden cuando ayuda.
- **Ejemplos:** cada resultado tiene al menos un ejemplo 2×2 y uno 3×3, con **toda la aritmética escrita**, números chicos y, de preferencia, enteros. Aplicaciones de ingeniería y física en 2D/3D: circuitos de 2 o 3 mallas, armaduras planas, masas y resortes con 2 o 3 grados de libertad, rotaciones en R³, tensor de inercia, ajuste de curvas con 3 o 4 puntos.
- **Ejercicios:** 7 por clase (6 de práctica y 1 ★), todos 2D o 3D, con la solución completa paso a paso.
- **Recuadro final "En más dimensiones":** dice qué se generaliza y remite al curso de Álgebra lineal pura.
- **Notas al pie:** solo hechos históricos verificables (lección de la revisión de Eco 2–4).
- **Gotchas de LaTeX:** no usar `%` dentro de `$…$`; escribir "5 %" fuera de math (regla de la memoria del sitio).

## Programa

| N.º | Clase | Ejes del contenido | ★ |
|---|---|---|---|
| 1 | Sistemas lineales y eliminación gaussiana | 3 lecturas de Ax=b (filas = rectas o planos que se cortan; columnas = combinación; transformación); operaciones elementales; escalonada y escalonada reducida; pivotes, variables libres; existencia y unicidad; costo ~⅔n³. Kirchhoff con 2 y 3 mallas; balance químico | Matrices elementales |
| 2 | LU, inversas y bloques | A=LU como registro; PA=LU y pivoteo parcial; varios lados derechos; Gauss–Jordan; por qué no invertir; bloques 2×2 de bloques, complemento de Schur, Sherman–Morrison (2D/3D). Armadura plana | Unicidad de LU y menores principales |
| 3 | Los cuatro subespacios | Col, Nul, Fila, Nul izq. en R²/R³; independencia, base, dimensión; rango–nulidad; solución general = particular + homogénea; ortogonalidad entre subespacios. Matriz de incidencia de un grafo de 3 nodos | Rango fila = rango columna |
| 4 | Determinantes | 3 propiedades que lo definen; cálculo por eliminación; cofactores, Sarrus y Cramer; det(AB); área y volumen orientados; jacobiano (polares y esféricas); bloques | Unicidad por Leibniz (expansión 2×2/3×3 explícita + general) |
| 5 | Ortogonalidad y QR | Producto interno, Cauchy–Schwarz, ángulos; bases ortonormales; rotaciones y reflexiones en R²/R³; Gram–Schmidt clásico y modificado; QR; Householder | Pérdida de ortogonalidad (ejemplo numérico casi colineal) |
| 6 | Mínimos cuadrados | Proyección sobre recta y plano; ecuaciones normales; QR; ajuste de recta, parábola y exponencial con 3–4 puntos; ponderados; Tikhonov | κ(A'A)=κ(A)² |
| 7 | Valores propios y dinámica | Polinomio característico; diagonalización; Aᵏ y estabilidad de xₖ₊₁=Axₖ; Markov 2 y 3 estados; Fibonacci; x'=Ax y e^{At} diagonalizable; retratos de fase 2D (nodo, silla, foco, centro) | Adelanto: el bloque 2×2 no diagonalizable (→ Clase 12) |
| 8 | Simétricas y definidas positivas | Teorema espectral; formas cuadráticas y ejes principales (elipses, elipsoides); criterios (autovalores, pivotes, menores); Cholesky; Rayleigh; hessiano; modos normales 2 y 3 masas; tensor de inercia | Courant–Fischer |
| 9 | SVD | Existencia; círculo/esfera → elipse/elipsoide; rango y bases; pseudoinversa y norma mínima; rango bajo (imagen como matriz 3×3 de juguete); PCA en nube 2D/3D | Eckart–Young |
| 10 | Normas, condicionamiento y métodos iterativos | Normas 1, 2, ∞ y sus bolas unitarias; normas matriciales inducidas; κ(A) y sensibilidad (sistema 2×2 casi singular); error hacia atrás; método de la potencia; Jacobi y Gauss–Seidel; radio espectral; cálculo matricial básico (∂(a'x), ∂(x'Ax)) | Convergencia de los iterativos ⇔ ρ<1 |
| 11 | Matrices complejas | C², C³; producto hermitiano; hermitianas (autovalores reales), unitarias; rotaciones como e^{iθ}; Pauli 2×2 y spin; circulantes 3×3 y DFT de 3 puntos (raíces cúbicas de la unidad). Nota: la FFT va en el curso puro | Descomposición de Schur (A=UTU*) y teorema espectral para normales (de ahí sale gratis; la Clase 15 la usa para QZ) |
| 12 | Jordan y funciones de matrices | Cayley–Hamilton; Jordan 2×2 y 3×3; e^{At} general (t·e^{λt}: amortiguamiento crítico, resonancia); A^{1/2}, log A; Perron–Frobenius para positivas; Leontief 2–3 sectores; PageRank 3 páginas | Perron–Frobenius (demostración para matrices positivas) |
| 13 | Optimización lineal y convexa | Conjuntos y funciones convexas en R²/R³; PL en forma estándar; método gráfico; vértices y teorema fundamental; simplex en tableau (2 y 3 variables); dualidad débil y fuerte; holgura complementaria; Lagrange y KKT en 2D/3D; programación cuadrática simple | Dualidad fuerte vía Farkas |
| 14 | MC con restricciones y problemas inversos | MC con restricciones de igualdad (Lagrange, sistema KKT); desigualdades y mínimos cuadrados no negativos 2D; mal condicionamiento; SVD truncada; Tikhonov y curva L; deconvolución/inversión de juguete 3×3 | Equivalencia Tikhonov ↔ filtro de valores singulares |
| 15 | Schur generalizado, QZ y expectativas racionales | Valores propios generalizados Ax=λBx con B singular (valores propios infinitos); regular pencil; teorema QZ (S=QAZ, T=QBZ triangulares) y reordenamiento; Blanchard–Kahn y método de Klein con modelo de 2 y 3 variables resuelto a mano; se lee después de 11 y 12 | Unicidad e indeterminación (casos sin solución o con múltiples) |

Para mantener las 14 clases en 2D/3D: el laplaciano discreto y los métodos de Krylov quedan para el curso puro (nota en la Clase 10). Kronecker y vec también.

## Archivos

Archivos nuevos, siguiendo la estructura de `econometria-4/`:
- `algebra-matricial/_metadata.yml` (copia de `econometria-4/_metadata.yml` con `sidebar: am`).
- `algebra-matricial/index.qmd` (formato de `econometria-4/index.qmd`: requisitos, lista `ul.clases`, notación y bibliografía: Strang, *Introduction to Linear Algebra*; Lay; Trefethen–Bau; Boyd–Vandenberghe; Hansen, *Discrete Inverse Problems*).
- `algebra-matricial/clase-01.qmd` … `clase-14.qmd`. Encabezado idéntico al de las otras clases (`format: html default / pdf include-in-header ../assets/preambulo.tex`, enlace de descarga del PDF).
- `assets/figuras_algebra.R`: mismo estilo y paleta que `assets/figuras.R` (función `abrir()`, colores tinta/teal/rojo/ocre/gris/fondo). Salida en `assets/fig/am/`. 3D con `persp()` de R base. Unas 50 figuras.

Archivos que se modifican:
- `_quarto.yml`: `render: algebra-matricial/*.qmd`; navbar con "Álgebra matricial" antes de Eco 1; `sidebar id: am` con las 14 clases.
- `index.qmd` (portada): nueva sección "Fundamentos matemáticos" con la tarjeta del curso, ajustar el subtítulo "Cuatro cursos" y la descripción del sitio.
- `README.md`: estructura y lista de cursos.
- `econometria-1/clase-01.qmd`: una línea que remita a Álgebra matricial como base. Nada más.

## Ejecución

1. **Infraestructura:** metadata, índice del curso, `_quarto.yml`, portada y script de figuras (todavía vacío). Render de la portada y del índice para verificar.
2. **Clases en tandas de 3 o 4:** 1–4, 5–8, 9–11, 12–14. Por cada clase:
   1. Agregar sus figuras a `figuras_algebra.R` y correrlo.
   2. Escribir el `.qmd`.
   3. Correr un script R de verificación en el scratchpad que recalcule todo número de ejemplos y soluciones (det, inversas, autovalores, SVD, soluciones de PL con un simplex manual o `optim`/búsqueda en vértices, mínimos cuadrados).
   4. Hacer `quarto render algebra-matricial/clase-NN.qmd`.
3. Después de cada tanda: commit y push (regla de la memoria). Los mensajes de commit llevan la línea Co-Authored-By.
4. Al final: render de los índices, actualizar la memoria (`project_sitio_econometria_github.md` y `MEMORY.md`) con el curso nuevo y su estado.

Notas operativas:
- Quarto está en `C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe` y R en `C:/Program Files/R/R-4.4.3/bin/Rscript.exe`.
- Los renders van en segundo plano y de a uno: dos renders simultáneos chocan en `docs/`.

## Verificación

- **Numérica:** para cada clase, un script R que recalcula todos los números del texto (ejemplos y soluciones), con cero discrepancias antes de renderizar.
- **Render:** el log de cada clase sin "Unable to resolve crossref", "error" ni "warning". Script de crossrefs (`{#id}` contra `@id` por archivo), el mismo usado en la revisión de Eco 2–4.
- **Visual:** `pdftools::pdf_convert` de cada página con figura o tabla de cada PDF, revisando que no haya contenido cortado, superposiciones ni figuras mal escaladas. Muestreo de las páginas con ecuaciones largas (matrices 3×3 desplegadas).
- **Sitio:** la navbar y la sidebar muestran el curso, los enlaces de la portada funcionan y el PDF de descarga existe para las 14 clases.
