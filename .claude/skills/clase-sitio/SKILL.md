---
name: clase-sitio
description: Escribe, verifica, renderiza y publica UNA clase del sitio Quarto "Econometría" (álgebra matricial, Eco 1-5, álgebra lineal pura). Usar con /clase-sitio o cuando se pida "escribe la clase N de álgebra matricial", "haz la clase siguiente del sitio", "agrega una clase a econometria-N". Cubre figuras, verificación numérica en R, .qmd, check completo, commit y push.
---

# Clase del sitio Econometría

Argumentos esperados: curso (`algebra-matricial`, `econometria-N`, …) y número de clase. Si faltan, deducir la siguiente clase pendiente de la tabla de estado de `CLAUDE.md`.

Hacer UNA clase por invocación. Reglas completas, rutas y gotchas: `CLAUDE.md`. Programa de Álgebra matricial: `.claude/plan-algebra-matricial.md`.

## Pasos

1. **Contexto.** Leer la fila de la clase en el plan (o en el `index.qmd` del curso) y la clase anterior completa para igualar estilo, notación y convenciones. Revisar qué prometieron clases previas que esta debe cumplir ("se demuestra en la Clase N").
2. **Figuras** (3–5 por clase). Agregar un bloque `# Clase N` a `assets/figuras_algebra.R` con la función `abrir()` y la paleta existente (tinta, teal, rojo, ocre, gris, azul, fondo), nombres `cNN_*.png`; 3D con `marco3d()`/`persp()`. Correr `Rscript assets/figuras_algebra.R` y mirar cada PNG.
3. **Verificación numérica ANTES de escribir.** Crear `vNN.R` en el scratchpad que recalcule en R cada número que aparecerá (determinantes, inversas, autovalores, factorizaciones, soluciones de ejercicios) usando los mismos datos del texto. Cero discrepancias. Usar `tol = 1e-14` con `qr()` en casos mal condicionados.
4. **Escribir `<curso>/clase-NN.qmd`** con la estructura obligatoria de `CLAUDE.md`: encabezado idéntico al de las otras clases (title `"Clase N · …"`, subtitle, description, `format: html + pdf include-in-header: ../assets/preambulo.tex`), enlace `descarga-pdf` a `clase-NN.pdf`, secciones, Borrador antes de cada demostración, ejemplos 2×2 y 3×3 con toda la aritmética, ★ Para ir más allá, En más dimensiones, Resumen, 7 ejercicios con solución plegada.
   - Cada resultado se enuncia en general (n×n) y se demuestra sin saltos. Todo ejemplo y ejercicio en 2D/3D.
   - Referencias `@id` solo dentro de la clase; entre clases, "Clase N".
   - `%` fuera de math. Ecuaciones anchas con `aligned`.
   - Con el hook activo, el lint corre solo tras cada edición del `.qmd`: corregir lo que reporte.
5. **Chequeo completo:** `Rscript scripts/check_clase.R <curso>/clase-NN.qmd`. Repetir hasta `TODO OK`. Luego revisar a ojo las páginas del PDF con figuras y ecuaciones largas (`pdftools::pdf_convert` a PNG en el scratchpad y leerlos): nada cortado, superpuesto ni mal escalado.
6. **Cierre.** Si la descripción de la clase cambió, actualizar el `index.qmd` del curso; actualizar la tabla de estado en `CLAUDE.md` y el README si corresponde. Limpiar archivos temporales (no dejar `.png` de revisión en el repo).
7. **Commit y push (obligatorio, después de CADA clase).** `git add` solo los archivos de la clase (qmd, figuras, índices, `docs/` regenerado, CLAUDE.md/README); mensaje `Álgebra matricial: clase N (tema)` con la línea Co-Authored-By vigente; `git push origin main` y confirmar que subió.
8. **Reportar** en pocas líneas: qué quedó, números verificados, páginas, commit, y cualquier decisión tomada. Si algo no se pudo verificar, decirlo.

## No hacer

- No renderizar el sitio completo ni dos clases a la vez.
- No inventar hechos históricos en notas al pie; solo los verificables.
- No nombrar a Jay Cummings en ningún lugar del sitio.
- No saltarse la verificación numérica ni el `check_clase.R` para ir más rápido.
