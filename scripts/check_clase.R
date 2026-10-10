# Chequeo completo de UNA clase: lint, render, log y detector de desbordes del PDF.
#
# Uso (desde cualquier carpeta):
#   Rscript scripts/check_clase.R algebra-matricial/clase-08.qmd
#   Rscript scripts/check_clase.R algebra-matricial/clase-08.qmd --sin-render   # solo lint + PDF ya renderizado
#
# Sale con 0 si todo está limpio y con 1 si algo falla. Renderizar toma ~1 min por clase.
# No renderices dos clases a la vez ni con un http-server abierto sobre docs/ (chocan en docs/).

quarto <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"

todos <- commandArgs(FALSE)
f <- sub("^--file=", "", todos[grep("^--file=", todos)])
setwd(dirname(dirname(normalizePath(f, winslash = "/"))))   # raíz del repo

args <- commandArgs(trailingOnly = TRUE)
ruta <- args[!startsWith(args, "--")][1]
sin_render <- "--sin-render" %in% args
if (is.na(ruta) || !file.exists(ruta)) stop("Uso: Rscript scripts/check_clase.R <carpeta>/clase-NN.qmd [--sin-render]")
ruta <- sub("^\\./", "", gsub("\\\\", "/", ruta))

problemas <- character()
paso <- function(txt) cat("\n== ", txt, "\n", sep = "")

# 1. Lint rápido
paso("1/3 Lint")
lint <- system2(file.path(R.home("bin"), "Rscript.exe"), c("scripts/lint_qmd.R", ruta), stdout = TRUE, stderr = TRUE)
cat(lint, sep = "\n")
if (!is.null(attr(lint, "status")) && attr(lint, "status") != 0) problemas <- c(problemas, "lint")

# 2. Render y revisión del log
paso("2/3 Render")
if (sin_render) {
  cat("(omitido por --sin-render)\n")
} else {
  log <- system2(quarto, c("render", ruta), stdout = TRUE, stderr = TRUE)
  malas <- grep("Unable to resolve crossref|WARNING|ERROR|Missing character|Overfull \\\\hbox \\([0-9.]{3,}pt", log, value = TRUE)
  if (!is.null(attr(log, "status")) && attr(log, "status") != 0) problemas <- c(problemas, "quarto render falló")
  if (length(malas)) { cat(malas, sep = "\n"); problemas <- c(problemas, "avisos en el log del render") }
  else cat("log limpio\n")
}

# 3. Detector de desbordes en el PDF (palabras de cuerpo que pasan x = 412 pt; las notas al margen miden 8 pt de alto)
paso("3/3 Desbordes en el PDF")
pdf <- file.path("docs", dirname(ruta), sub("\\.qmd$", ".pdf", basename(ruta)))
if (!file.exists(pdf)) {
  cat("no existe", pdf, "(renderiza primero)\n"); problemas <- c(problemas, "falta el PDF")
} else {
  paginas <- pdftools::pdf_data(pdf)
  hay <- FALSE
  for (p in seq_along(paginas)) {
    x <- paginas[[p]]
    mal <- x[x$height >= 9 & (x$x + x$width) > 412, ]
    if (nrow(mal)) {
      hay <- TRUE
      cat(sprintf("p. %d: %s\n", p, paste(utils::head(mal$text, 8), collapse = " ")))
    }
  }
  if (hay) problemas <- c(problemas, "texto fuera del ancho (partir ecuaciones con aligned o en varias líneas $$)")
  else cat(sprintf("%d páginas sin desbordes\n", length(paginas)))
}

cat("\n")
if (length(problemas)) {
  cat("FALLA:", paste(problemas, collapse = "; "), "\n"); quit(status = 1)
}
cat("TODO OK:", ruta, "\n")
