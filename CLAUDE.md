# Sitio "Econometría" (Quarto → GitHub Pages)

Repo público `BenjaminSolisO/econometria`, rama `main`, Pages desde `main:/docs`.
URL: https://benjaminsoliso.github.io/econometria/. Idioma: español. Contenido teórico y matemático (pregrado + postgrado), demostraciones paso a paso, ejercicios resueltos.

## Cursos y estado (actualizar al cerrar cada clase)

| Carpeta | Contenido | Estado |
|---|---|---|
| `algebra-matricial/` | Álgebra matricial aplicada, 15 clases (plan en `.claude/plan-algebra-matricial.md`) | Clases 1–7 publicadas; falta 8 a 15 |
| `econometria-1/` | 10 clases (MCO, inferencia, MCG, VI) | Completo |
| `econometria-2/` | 13 clases (M-estimadores, MV, GMM, elección discreta, panel, cuantílica) | Completo |
| `econometria-3/` | 13 clases de series de tiempo | Completo |
| `econometria-4/` | 10 clases (causal, no paramétrica, ML, bootstrap) | Completo |

Pendientes: Econometría 5 (financiera: valoración con GMM, factores, volatilidad realizada, eventos, VaR/ES) y el curso de Álgebra lineal pura (FFT, Kronecker/vec, sistemas dispersos van ahí). El usuario aún no revisa personalmente el contenido de Eco 2–4 (hubo una revisión exhaustiva el 2026-10-05, ~60 correcciones).

## Herramientas (Windows)

- Quarto: `C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe` (no está en PATH).
- R: `C:/Program Files/R/R-4.4.3/bin/Rscript.exe` (no está en PATH). `pdftools` y `magick` instalados.
- **No hay Python ni `jq`.** Para editar usa Edit/Write; `sed` con patrones complejos o `&` falla en silencio (verifica con grep). `Rscript -e '...'` rompe con `>` en el código: usa un archivo `.R`.
- Render de UNA clase: `quarto render <carpeta>/clase-NN.qmd` (~1 min, xelatex, Cambria). No renderices el sitio completo (~8 min). Nunca dos renders a la vez ni con un `http-server` sobre `docs/`.
- Figuras de álgebra: `Rscript assets/figuras_algebra.R` desde la raíz → `assets/fig/am/`.

## Flujo por clase (usar la skill `/clase-sitio`)

1. Leer la fila del plan y la clase anterior (estilo).
2. Agregar figuras a `assets/figuras_algebra.R` y correrlo.
3. Script R de verificación `vNN.R` (en el scratchpad) que recalcula **todo** número de ejemplos y soluciones; cero discrepancias antes de renderizar.
4. Escribir `clase-NN.qmd`.
5. `Rscript scripts/check_clase.R <carpeta>/clase-NN.qmd` hasta que diga `TODO OK` (lint + render + log + detector de desbordes). Mirar visualmente las páginas con figuras o ecuaciones largas (`pdftools::pdf_convert` → imagen).
6. Actualizar estado (esta tabla, README, índice si cambia).
7. **Commit y push después de CADA clase** (el usuario tiene límite de uso; nunca dejar commits solo locales). Mensaje: `Álgebra matricial: clase N (tema)`.

Hook del proyecto (`.claude/settings.json`): al editar un `.qmd` corre `scripts/lint_qmd.R` y, si falla, me devuelve los errores. Si no dispara, abrir `/hooks` una vez para recargar.

## Reglas de las clases de Álgebra matricial

- Enunciados y demostraciones **generales (n×n)**, cada igualdad justificada. Prohibido "es fácil ver" y "análogamente" sin escribir el caso.
- **Todo ejemplo, figura y ejercicio en 2D y 3D** (matrices 2×2 y 3×3), con la aritmética completa y números chicos (enteros si se puede). Al menos un ejemplo 2×2 y uno 3×3 por resultado.
- Estructura: título "Clase N · Tema" + subtítulo + descripción; enlace de descarga del PDF; secciones `#`; recuadro `callout-note "Borrador"` antes de cada demostración; `.proof`; algoritmos en `callout-caution title="Algoritmo"` (pasos numerados, costo, cuándo falla); 3–5 figuras; sección `# ★ Para ir más allá` con `callout-important`; recuadro final `callout-caution "En más dimensiones"` (remite al curso de Álgebra lineal pura); `# Resumen`; `# Ejercicios`: 7 por clase (6 + 1 ★), todos 2D/3D, solución plegada `callout-tip collapse="true" title="Solución N.k"`.
- Notas al pie: solo hechos históricos verificables.
- Referencias cruzadas `{#thm-…}` y `@thm-…` **solo dentro de la misma clase**; entre clases se cita "Clase N" en texto.

## Gotchas conocidos

- `%` dentro de `$…$` es comentario LaTeX y `\%` rompe xelatex con babel español: escribir "5 %" fuera del math (el lint lo detecta).
- Quarto solo avisa en el log "Unable to resolve crossref"; no deja clase `unresolved` en el HTML (el lint compara `{#id}` con `@id`).
- Cambria no tiene `✓`: está arreglado con `\newunicodechar` en `assets/preambulo.tex`; las clases de Eco 3–4 renderizadas antes del arreglo (12) necesitan re-render para que les llegue.
- Opciones del PDF van top-level en `<carpeta>/_metadata.yml`, y en cada clase `format: {html, pdf: include-in-header: ../assets/preambulo.tex}`.
- Detector de desbordes del PDF: palabras de alto ≥ 9 con `x + width > 412` pt se salen (el cuerpo termina en ~408; las notas al margen miden 8). Partir ecuaciones anchas con `aligned` o varias líneas `$$`.
- `qr()` de R con tol 1e-7 declara rango 1 el caso Läuchli: usar `tol = 1e-14` o `LAPACK = TRUE`.
- Prohibido nombrar a Jay Cummings en cualquier parte del sitio (README incluido).
- Si el push falla por red (github.com:443), reintentar con espera.

## Cómo sacarle provecho a Claude Code en este repo

- **Una clase completa, con final verificable:** `/goal clase-NN.qmd existe, scripts/check_clase.R da TODO OK y el commit está pusheado a main` (una clase por vez: cada turno consume uso).
- **Esfuerzo:** `/effort` alto para demostraciones y teoría pesada (Schur/QZ, Blanchard–Kahn, Perron–Frobenius, Courant–Fischer); bajo para formato, render y commits.
- **Render en paralelo:** `/fork` copia la sesión a una de fondo (con su worktree) para renderizar o revisar la clase N mientras se escribe la N+1. `claude agents` (en PowerShell) muestra todas las sesiones.
- **Revisión:** el agente `revisor-clase` recalcula en R los números de una clase y reporta errores sin editar. Lanzarlo por bloque de clases, no con todo el sitio a la vez (gasta mucho uso). Pendiente: Eco 2–4 completos al terminar Eco 5.
- **Gasto:** `/usage` para ver qué consume el límite; `/skill-doctor` para apagar skills sin uso.
- Abrir Claude **dentro de esta carpeta** (no en `C:\Users\solis`) para que cargue este archivo y los worktrees sean livianos.
