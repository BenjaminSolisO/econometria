# Lint rápido (segundos, sin render) de archivos .qmd del sitio.
#
# Uso manual:   Rscript scripts/lint_qmd.R archivo.qmd [otro.qmd ...]   (sale con 1 si hay errores)
# Uso en hook:  sin argumentos lee el JSON de PostToolUse por stdin, toma tool_input.file_path y,
#               si el archivo es .qmd y tiene errores, responde {"decision":"block","reason":...}
#               para que Claude los corrija antes de seguir.
#
# Revisa lo que ya nos rompió el render:
#   1. "%" dentro de math ($...$ o $$...$$): es comentario LaTeX y "\%" rompe xelatex con babel español.
#   2. Referencias @thm-..., @fig-..., etc. sin su {#id} en el mismo archivo (Quarto solo avisa en el log).
#   3. Ids {#...} duplicados.
#   4. Clases de Álgebra matricial: la frase prohibida "es fácil ver".
#   5. Clases (clase-NN.qmd): título "Clase N", enlace de descarga clase-NN.pdf y preámbulo del PDF coherentes con el nombre.

args <- commandArgs(trailingOnly = TRUE)
modo_hook <- length(args) == 0

if (modo_hook) {
  entrada <- paste(readLines(file("stdin"), warn = FALSE), collapse = "\n")
  m <- regmatches(entrada, regexec('"file_path"\\s*:\\s*"((?:[^"\\\\]|\\\\.)*)"', entrada, perl = TRUE))[[1]]
  if (length(m) < 2) quit(status = 0)
  archivos <- gsub("\\\\+", "/", m[2])
} else {
  archivos <- args
}

# Reemplaza por espacios (conservando los saltos de línea) lo que coincide con el patrón
borrar <- function(txt, patron) {
  m <- gregexpr(patron, txt, perl = TRUE)
  regmatches(txt, m) <- lapply(regmatches(txt, m), function(s) gsub("[^\n]", " ", s))
  txt
}
nlinea <- function(txt, pos) vapply(pos, function(p) if (p <= 1) 1L else 1L + sum(utf8ToInt(substr(txt, 1, p - 1)) == 10L), integer(1))

lint <- function(ruta) {
  if (!grepl("\\.qmd$", ruta) || !file.exists(ruta)) return(character())
  nombre <- basename(ruta)
  crudo <- paste(readLines(ruta, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  errs <- character()
  err <- function(linea, msg) errs <<- c(errs, sprintf("%s:%d: %s", nombre, linea, msg))

  # Texto sin encabezado YAML, bloques de código, código en línea ni \$ escapados
  txt <- borrar(crudo, "(?s)\\A---\n.*?\n---")
  txt <- borrar(txt, "(?s)```.*?```")
  txt <- borrar(txt, "`[^`\n]*`")
  txt <- gsub("\\\\\\$", "  ", txt)

  # 1. "%" dentro de math
  resto <- txt
  for (patron in c("(?s)\\$\\$.*?\\$\\$", "\\$[^$\n]+\\$")) {
    g <- gregexpr(patron, resto, perl = TRUE)[[1]]
    if (g[1] != -1) {
      trozos <- regmatches(resto, list(g))[[1]]
      for (i in seq_along(trozos)) {
        if (grepl("%", trozos[i], fixed = TRUE)) {
          pos <- g[i] + regexpr("%", trozos[i], fixed = TRUE) - 1
          err(nlinea(resto, pos), "'%' dentro de math (comentario LaTeX; '\\%' rompe xelatex con babel). Escribe 'N %' fuera del math.")
        }
      }
    }
    resto <- borrar(resto, patron)
  }

  # 2 y 3. Ids y referencias cruzadas
  prefijos <- "thm|lem|prp|cor|def|exm|exr|fig|tbl|eq|sec|cnj|alg|nte|tip"
  g <- gregexpr("\\{#([A-Za-z]+-[A-Za-z0-9_-]+)", txt, perl = TRUE)[[1]]
  ids <- character(); pos_ids <- integer()
  if (g[1] != -1) {
    ids <- sub("^\\{#", "", regmatches(txt, list(g))[[1]]); ids <- sub("-+$", "", ids); pos_ids <- as.integer(g)
  }
  dup <- ids[duplicated(ids)]
  for (d in unique(dup)) err(nlinea(txt, pos_ids[which(ids == d)[2]]), sprintf("id duplicado {#%s}", d))
  g <- gregexpr(sprintf("@((?:%s)-[A-Za-z0-9_-]+)", prefijos), txt, perl = TRUE)[[1]]
  if (g[1] != -1) {
    refs <- sub("^@", "", regmatches(txt, list(g))[[1]]); refs <- sub("-+$", "", refs)
    for (i in which(!(refs %in% ids)))
      err(nlinea(txt, as.integer(g)[i]), sprintf("referencia @%s sin {#%s} en este archivo (entre clases se cita 'Clase N')", refs[i], refs[i]))
  }

  # 4. Frase prohibida en las clases de Álgebra matricial
  if (grepl("algebra-matricial", normalizePath(ruta, winslash = "/", mustWork = FALSE)) && grepl("^clase-", nombre)) {
    g <- gregexpr("es f[aá]cil ver", txt, perl = TRUE, ignore.case = TRUE)[[1]]
    if (g[1] != -1) for (p in g) err(nlinea(txt, p), "frase prohibida 'es fácil ver': escribe el paso")
  }

  # 5. Coherencia de clase-NN.qmd
  if (grepl("^clase-[0-9]+\\.qmd$", nombre)) {
    n <- as.integer(sub("^clase-([0-9]+)\\.qmd$", "\\1", nombre)); nn <- sprintf("%02d", n)
    tit <- regmatches(crudo, regexec('(?m)^title:\\s*"Clase ([0-9]+)', crudo, perl = TRUE))[[1]]
    if (length(tit) < 2 || as.integer(tit[2]) != n) err(2, sprintf("el título debe empezar con \"Clase %d\"", n))
    if (!grepl(sprintf('href="clase-%s.pdf"', nn), crudo, fixed = TRUE)) err(1, sprintf("falta el enlace de descarga href=\"clase-%s.pdf\"", nn))
    if (!grepl("include-in-header: ../assets/preambulo.tex", crudo, fixed = TRUE)) err(1, "falta 'include-in-header: ../assets/preambulo.tex' en el encabezado del PDF")
  }
  errs
}

todos <- unlist(lapply(archivos, lint))

if (modo_hook) {
  if (length(todos)) {
    msg <- paste0("Lint de .qmd falló; corrige antes de seguir:\n", paste(todos, collapse = "\n"))
    esc <- gsub("\n", "\\\\n", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", msg)))
    cat(sprintf('{"decision":"block","reason":"%s"}', esc))
  }
  quit(status = 0)
} else {
  if (length(todos)) { cat(todos, sep = "\n"); quit(status = 1) }
  cat("lint OK (", length(archivos), " archivo(s))\n", sep = "")
}
