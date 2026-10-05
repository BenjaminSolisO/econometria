# Econometría

Sitio de los cursos **Econometría 1, 2 y 3**: teoría, demostraciones paso a paso y ejercicios resueltos.

Sitio publicado: <https://benjaminsoliso.github.io/econometria/>

## Estructura

```
index.qmd                 Landing page
econometria-1/            10 clases teóricas (HTML + PDF)
econometria-2/            13 clases teóricas (HTML + PDF)
econometria-3/            13 clases de series de tiempo (HTML + PDF)
assets/                   Estilos, preámbulo LaTeX y figuras (figuras.R las regenera)
docs/                     Sitio renderizado (lo sirve GitHub Pages)
```

## Compilar

Requiere [Quarto](https://quarto.org) y una distribución LaTeX con `xelatex`. Las fuentes del PDF son Cambria / Cambria Math / Consolas (Windows).

```sh
Rscript assets/figuras.R   # solo si cambian las figuras
quarto render
```

El resultado queda en `docs/`, que GitHub Pages publica desde la rama `main`.

## Convenciones de las clases

- `::: {.callout-note title="Borrador"}`: razonamiento previo a una demostración.
- `::: {.proof}`: demostración formal.
- `::: {.callout-important title="★ Postgrado"}` y secciones con ★: material de postgrado.
- Notas al pie → aparecen al margen (HTML y PDF).
- Soluciones: `::: {.callout-tip collapse="true" title="Solución ..."}`.
