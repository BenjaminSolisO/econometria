# Genera las figuras estáticas de Álgebra matricial aplicada (PNG para HTML y PDF).
# Uso: Rscript assets/figuras_algebra.R  (desde la raíz del proyecto)
# Misma paleta y estilo que assets/figuras.R.

tinta  <- "#1f2430"
teal   <- "#0f5e6b"
rojo   <- "#b5452f"
ocre   <- "#b8892b"
gris   <- "#8a8577"
azul   <- "#5b7f95"
fondo  <- "#fbf8f1"

dir.create("assets/fig/am", showWarnings = FALSE, recursive = TRUE)

# SOLO=c08 Rscript assets/figuras_algebra.R regenera únicamente las figuras c08_* (las demás se dibujan en un dispositivo nulo)
solo <- Sys.getenv("SOLO")
abrir <- function(archivo, w = 7, h = 4.6) {
  destino <- if (nzchar(solo) && !startsWith(archivo, solo)) nullfile() else file.path("assets/fig/am", archivo)
  png(destino, width = w, height = h, units = "in",
      res = 200, bg = fondo)
  par(family = "serif", mar = c(4, 4, 1.5, 1), col.axis = tinta, col.lab = tinta,
      fg = gris)
}

# Ejes cartesianos limpios con cuadrícula suave
ejes2d <- function(xlim, ylim, xlab = expression(x[1]), ylab = expression(x[2]), ...) {
  plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, las = 1, asp = 1, ...)
  abline(h = pretty(ylim), v = pretty(xlim), col = adjustcolor(gris, 0.18))
  abline(h = 0, v = 0, col = adjustcolor(tinta, 0.55))
}

flecha <- function(x0, y0, x1, y1, col = tinta, lwd = 2.5, ...) {
  arrows(x0, y0, x1, y1, col = col, lwd = lwd, length = 0.11, ...)
}

# Utilidades 3D sobre persp(): devuelve la matriz de proyección y funciones para
# trazar puntos, segmentos y polígonos en coordenadas del espacio.
marco3d <- function(xlim, ylim, zlim, theta = 35, phi = 22, xlab = "x1", ylab = "x2",
                    zlab = "x3", ...) {
  par(mar = c(1.2, 2.4, 1.6, 1.2))
  persp(xlim, ylim, matrix(zlim[1], 2, 2), zlim = zlim, theta = theta, phi = phi,
        col = NA, border = NA, box = TRUE, axes = TRUE, ticktype = "detailed", r = 6,
        xlab = xlab, ylab = ylab, zlab = zlab, expand = 0.75, cex.axis = 0.7, ...)
}
p3 <- function(pm, x, y, z) trans3d(x, y, z, pm)
seg3 <- function(pm, a, b, ...) {
  P <- trans3d(c(a[1], b[1]), c(a[2], b[2]), c(a[3], b[3]), pm)
  lines(P, ...)
}
flecha3 <- function(pm, a, b, col = tinta, lwd = 2.5) {
  P <- trans3d(c(a[1], b[1]), c(a[2], b[2]), c(a[3], b[3]), pm)
  arrows(P$x[1], P$y[1], P$x[2], P$y[2], col = col, lwd = lwd, length = 0.1)
}
poli3 <- function(pm, X, col, border = NA, ...) {
  P <- trans3d(X[, 1], X[, 2], X[, 3], pm)
  polygon(P, col = col, border = border, ...)
}
# Plano a x + b y + c z = d recortado a la caja (c != 0): devuelve la malla como polígonos
plano3 <- function(pm, a, b, c, d, xlim, ylim, col, n = 12) {
  xs <- seq(xlim[1], xlim[2], length.out = n)
  ys <- seq(ylim[1], ylim[2], length.out = n)
  for (i in seq_len(n - 1)) for (j in seq_len(n - 1)) {
    X <- cbind(c(xs[i], xs[i + 1], xs[i + 1], xs[i]), c(ys[j], ys[j], ys[j + 1], ys[j + 1]))
    Z <- (d - a * X[, 1] - b * X[, 2]) / c
    poli3(pm, cbind(X, Z), col = col)
  }
}

# Cada clase agrega aquí sus figuras, en secciones marcadas.

# ===========================================================================
# Clase 1: sistemas lineales
# ---------------------------------------------------------------------------
# 1a. Lectura por filas en 2D: los tres casos
abrir("c01_rectas.png", w = 9, h = 3.4)
par(mfrow = c(1, 3), mar = c(4, 4, 2.2, 0.8))
recta <- function(a, b, c, col, lty = 1) {  # a x1 + b x2 = c, con b != 0
  curve((c - a * x) / b, from = -1, to = 6, add = TRUE, col = col, lwd = 2.6, lty = lty)
}
casos <- list(
  list(t = "Una solución",        l2 = c(3, -1, 1),  p = c(1, 2)),
  list(t = "Ninguna solución",    l2 = c(2, 4, 4),   p = NULL),
  list(t = "Infinitas soluciones", l2 = c(2, 4, 10), p = NULL))
for (cs in casos) {
  ejes2d(c(-1, 6), c(-1, 4.5), main = "")
  title(cs$t, col.main = tinta, font.main = 1, cex.main = 1.25)
  recta(1, 2, 5, teal)
  recta(cs$l2[1], cs$l2[2], cs$l2[3], rojo, lty = if (cs$t == "Infinitas soluciones") 2 else 1)
  if (!is.null(cs$p)) points(cs$p[1], cs$p[2], pch = 21, bg = ocre, cex = 1.8)
}
dev.off()

# 1b. Lectura por columnas: b = 1 a1 + 2 a2
abrir("c01_columnas.png", w = 6.2, h = 5)
ejes2d(c(-0.5, 6), c(-2.5, 4))
a1 <- c(1, 3); a2 <- c(2, -1); b <- c(5, 1)
flecha(0, 0, a1[1], a1[2], col = teal)
flecha(0, 0, a2[1], a2[2], col = rojo)
flecha(0, 0, 2 * a2[1], 2 * a2[2], col = adjustcolor(rojo, 0.45), lty = 2)
flecha(a1[1], a1[2], b[1], b[2], col = rojo)
flecha(0, 0, b[1], b[2], col = tinta, lwd = 3)
text(0.25, 1.9, expression(a[1]), col = teal, cex = 1.3)
text(1.25, -0.95, expression(a[2]), col = rojo, cex = 1.3)
text(3.1, 2.55, expression(2 * a[2]), col = rojo, cex = 1.2)
text(5.15, 1.35, expression(b == a[1] + 2 * a[2]), col = tinta, cex = 1.2, adj = 0)
dev.off()

# 1c. Lectura por filas en 3D: tres planos que se cortan en un punto
abrir("c01_planos.png", w = 6.4, h = 5.4)
xl <- c(0, 2); yl <- c(1, 3); zl <- c(-2, 8)
pm <- marco3d(xl, yl, zl, theta = 30, phi = 20)
plano3(pm, 1, 1, 1, 6,  xl, yl, adjustcolor(teal, 0.28))
plano3(pm, 2, 3, 1, 11, xl, yl, adjustcolor(rojo, 0.22))
plano3(pm, 1, -1, 2, 5, xl, yl, adjustcolor(ocre, 0.30))
P <- p3(pm, 1, 2, 3); points(P, pch = 21, bg = tinta, cex = 1.8)
text(P$x, P$y, "  (1, 2, 3)", adj = 0, col = tinta, cex = 1.1)
dev.off()

# 1d. Tres planos: una recta común o ningún punto común
abrir("c01_planos_casos.png", w = 9.5, h = 4.6)
par(mfrow = c(1, 2))
for (caso in 1:2) {
  pm <- marco3d(xl, yl, c(-2, 8), theta = 30, phi = 20)
  plano3(pm, 1, 1, 1, 6,  xl, yl, adjustcolor(teal, 0.28))
  plano3(pm, 2, 3, 1, 11, xl, yl, adjustcolor(rojo, 0.22))
  plano3(pm, 3, 4, 2, if (caso == 1) 17 else 20, xl, yl, adjustcolor(ocre, 0.30))
  if (caso == 1) {
    t <- c(2.5, 3.5)
    seg3(pm, c(7 - 2 * t[1], -1 + t[1], t[1]), c(7 - 2 * t[2], -1 + t[2], t[2]),
         col = tinta, lwd = 4)
  }
  mtext(if (caso == 1) "Una recta común: infinitas soluciones" else "Sin punto común: ninguna solución",
        side = 3, line = 0.1, col = tinta, cex = 1.05)
}
dev.off()

# 1e. Equilibrio de dos mercados relacionados (café y té)
abrir("c01_mercados.png", w = 9, h = 4.2)
par(mfrow = c(1, 2), mar = c(4, 4.2, 2.4, 0.8))
mercado <- function(titulo, dem, ofe, q0, p0, etiq_d, etiq_o, ylab) {
  plot(NA, xlim = c(0, 10), ylim = c(0, 7), xlab = "Cantidad Q", ylab = ylab, las = 1)
  abline(h = 0:7, v = seq(0, 10, 2), col = adjustcolor(gris, 0.18))
  title(titulo, col.main = tinta, font.main = 1, cex.main = 1.15)
  curve(dem(x), from = 0, to = 10, add = TRUE, col = teal, lwd = 2.6)
  curve(ofe(x), from = 0, to = 10, add = TRUE, col = rojo, lwd = 2.6)
  segments(q0, 0, q0, p0, lty = 2, col = gris); segments(0, p0, q0, p0, lty = 2, col = gris)
  points(q0, p0, pch = 21, bg = ocre, cex = 1.8)
  text(q0, p0, sprintf("  (Q, P) = (%d, %d)", q0, p0), adj = 0, pos = 4, col = tinta, cex = 0.95)
  text(etiq_d[1], etiq_d[2], "demanda", col = teal); text(etiq_o[1], etiq_o[2], "oferta", col = rojo)
}
mercado(expression("Café, con " * P[2] == 3), function(q) (13 - q) / 2, function(q) q - 1, 5, 4,
        c(1.6, 6.4), c(8.6, 6.5), expression("Precio " * P[1]))
mercado(expression("Té, con " * P[1] == 4), function(q) 9 - q, function(q) q - 3, 6, 3,
        c(3.9, 6.6), c(7.0, 6.4), expression("Precio " * P[2]))
dev.off()

# ===========================================================================
# Clase 2: LU, inversas y bloques
# ---------------------------------------------------------------------------
# 2a. Cadena de tres masas y cuatro resortes entre dos paredes
abrir("c02_resortes.png", w = 8, h = 2.6)
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(-0.6, 10.6), ylim = c(-1.3, 1.6), axes = FALSE, xlab = "", ylab = "", asp = 1)
pared <- function(x, lado) {
  segments(x, -0.9, x, 0.9, lwd = 3, col = tinta)
  for (y in seq(-0.85, 0.75, by = 0.2))
    segments(x, y, x + lado * 0.25, y + 0.2, col = gris, lwd = 1.2)
}
resorte <- function(x0, x1, y = 0) {
  xs <- seq(x0 + 0.2, x1 - 0.2, length.out = 13)
  ys <- y + c(0, rep(c(0.22, -0.22), length.out = 11), 0)
  lines(c(x0, xs, x1), c(y, ys, y), col = tinta, lwd = 1.8)
}
pared(0, -1); pared(10, 1)
xm <- c(2.5, 5, 7.5)
bordes <- c(0, xm, 10)
for (k in 1:4) resorte(bordes[k] + ifelse(k == 1, 0, 0.4), bordes[k + 1] - ifelse(k == 4, 0, 0.4))
for (k in 1:3) {
  rect(xm[k] - 0.4, -0.4, xm[k] + 0.4, 0.4, col = adjustcolor(teal, 0.85), border = tinta)
  text(xm[k], 0, paste0("m", k), col = "white", cex = 1.05)
  flecha(xm[k], 0.75, xm[k] + 0.9, 0.75, col = rojo, lwd = 2)
  text(xm[k] + 0.45, 1.15, bquote(f[.(k)]), col = rojo, cex = 1.15)
  segments(xm[k], -0.55, xm[k], -0.8, col = ocre, lwd = 1.5)
  flecha(xm[k], -0.7, xm[k] + 0.6, -0.7, col = ocre, lwd = 1.6)
  text(xm[k] + 0.3, -1.05, bquote(u[.(k)]), col = ocre, cex = 1.05)
}
for (k in 1:4) text((bordes[k] + bordes[k + 1]) / 2, 0.55, "k", col = gris, font = 3)
dev.off()

# 2b. A = LU como patrón de entradas
abrir("c02_patron_lu.png", w = 7.5, h = 2.6)
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(0, 15.5), ylim = c(-0.3, 3.4), axes = FALSE, xlab = "", ylab = "", asp = 1)
matriz <- function(x0, M, colfun, titulo) {
  for (i in 1:3) for (j in 1:3) {
    col <- colfun(i, j)
    rect(x0 + j - 1, 3 - i, x0 + j, 4 - i, col = col, border = fondo, lwd = 2)
    text(x0 + j - 0.5, 3.5 - i, M[i, j], cex = 0.95,
         col = if (col == fondo) gris else "white")
  }
  text(x0 + 1.5, 3.3, titulo, col = tinta, cex = 1.1)
}
A <- matrix(c("1","1","1","2","3","1","1","-1","2"), 3, byrow = TRUE)
L <- matrix(c("1","0","0","2","1","0","1","-2","1"), 3, byrow = TRUE)
U <- matrix(c("1","1","1","0","1","-1","0","0","-1"), 3, byrow = TRUE)
matriz(0, A, function(i, j) azul, "A")
text(3.75, 1.5, "=", cex = 2, col = tinta)
matriz(4.5, L, function(i, j) if (i > j) rojo else if (i == j) gris else fondo, "L")
text(8.25, 1.5, "×", cex = 1.6, col = tinta)
matriz(9, U, function(i, j) if (i <= j) teal else fondo, "U")
text(13.6, 2.2, "multiplicadores", col = rojo, cex = 0.95, adj = 0)
text(13.6, 1.6, "debajo de la", col = rojo, cex = 0.95, adj = 0)
text(13.6, 1.0, "diagonal de L", col = rojo, cex = 0.95, adj = 0)
dev.off()

# 2c. Pivote pequeño: la solución correcta y la que entrega la eliminación sin pivoteo
abrir("c02_pivoteo.png", w = 6.4, h = 4.6)
ejes2d(c(-0.6, 2.6), c(-0.4, 2.4))
eps <- 1e-4
curve((1 - eps * x) / 1, from = -0.6, to = 2.6, add = TRUE, col = teal, lwd = 2.6)
curve(2 - x, from = -0.6, to = 2.6, add = TRUE, col = rojo, lwd = 2.6)
points(1.0001, 0.9999, pch = 21, bg = ocre, cex = 2)
points(0, 1, pch = 4, col = tinta, cex = 2, lwd = 3)
text(1.15, 1.6, "solución exacta (1,0001; 0,9999)", adj = 0, col = tinta, cex = 0.95)
text(-0.55, 0.72, "sin pivoteo, 3 cifras: (0; 1)", adj = 0, col = tinta, cex = 0.95)
text(2.0, 0.15, expression(x[1] + x[2] == 2), col = rojo, cex = 1.05, adj = 0)
text(1.5, 1.14, expression("0,0001" * x[1] + x[2] == 1), col = teal, cex = 1.05, adj = 0)
dev.off()

# ===========================================================================
# Clase 3: los cuatro subespacios
# ---------------------------------------------------------------------------
# Plano generado por dos vectores (por el origen), como paralelogramo recortado
plano_gen <- function(pm, u, v, s = 1, col) {
  X <- rbind(-s * u - s * v, s * u - s * v, s * u + s * v, -s * u + s * v)
  poli3(pm, X, col = col, border = adjustcolor(tinta, 0.25))
}
# 3a. Subespacios de R^3: un plano y una recta por el origen; un plano desplazado no lo es
abrir("c03_subespacios.png", w = 6.6, h = 5.4)
pm <- marco3d(c(-2, 2), c(-2, 2), c(-2, 3.5), theta = 35, phi = 22)
u <- c(1, 0, 0.3); v <- c(0, 1, 0.3)
plano_gen(pm, u, v, s = 1.7, adjustcolor(teal, 0.30))
Xd <- rbind(-1.7 * u - 1.7 * v, 1.7 * u - 1.7 * v, 1.7 * u + 1.7 * v, -1.7 * u + 1.7 * v)
Xd[, 3] <- Xd[, 3] + 2
poli3(pm, Xd, col = adjustcolor(rojo, 0.18), border = adjustcolor(rojo, 0.5))
d <- c(-0.5, 0.9, 1)
seg3(pm, -1.9 * d, 1.9 * d, col = ocre, lwd = 3.5)
P0 <- p3(pm, 0, 0, 0); points(P0, pch = 21, bg = tinta, cex = 1.4)
t1 <- p3(pm, -1.7, 1.7, 0); text(t1, "plano por el origen", col = teal, cex = 1, pos = 2)
t2 <- p3(pm, -1.7, 1.7, 2); text(t2, "plano desplazado", col = rojo, cex = 1, pos = 2)
t3 <- p3(pm, 1.9 * d[1], 1.9 * d[2], 1.9 * d[3]); text(t3, "recta por el origen", col = ocre, cex = 1, pos = 4)
dev.off()

# 3b. Los cuatro subespacios de A = [1 2; 2 4] en el plano de entrada y el de salida
abrir("c03_cuatro_2d.png", w = 9, h = 4.4)
par(mfrow = c(1, 2), mar = c(4, 4, 2.2, 1))
ejes2d(c(-2.5, 2.5), c(-2.5, 2.5)); title("Entrada: R² (vectores x)", col.main = tinta, font.main = 1)
abline(0, 2, col = teal, lwd = 3); abline(0, -0.5, col = rojo, lwd = 3)
text(0.95, 2.3, "espacio fila", col = teal, adj = 1); text(-2.4, 1.55, "espacio nulo", col = rojo, adj = 0)
flecha(0, 0, 1, 2, col = teal); flecha(0, 0, -2, 1, col = rojo)
ejes2d(c(-2.5, 2.5), c(-2.5, 2.5), xlab = expression(b[1]), ylab = expression(b[2])); title("Salida: R² (vectores b = Ax)", col.main = tinta, font.main = 1)
abline(0, 2, col = azul, lwd = 3); abline(0, -0.5, col = ocre, lwd = 3)
text(0.95, 2.3, "espacio columna", col = azul, adj = 1); text(-2.4, 1.55, "nulo izquierdo", col = ocre, adj = 0)
flecha(0, 0, 1, 2, col = azul); flecha(0, 0, -2, 1, col = ocre)
dev.off()

# 3c. Espacio fila (plano) y espacio nulo (recta perpendicular) de B = [1 1 1; 2 3 1; 3 4 2]
abrir("c03_fila_nulo.png", w = 6.4, h = 5.4)
pm <- marco3d(c(-2.2, 2.2), c(-2.2, 2.2), c(-2.2, 2.2), theta = 25, phi = 18)
r1 <- c(1, 0, 2) / sqrt(5); r2 <- c(0, 1, -1); r2 <- r2 - sum(r2 * r1) * r1; r2 <- r2 / sqrt(sum(r2^2))
plano_gen(pm, r1, r2, s = 1.5, adjustcolor(teal, 0.28))
n <- c(-2, 1, 1) / sqrt(6)
seg3(pm, -2 * n, 2 * n, col = rojo, lwd = 3.5)
flecha3(pm, c(0, 0, 0), c(1, 0, 2) * 0.7, col = teal)
flecha3(pm, c(0, 0, 0), c(0, 1, -1) * 0.9, col = teal)
flecha3(pm, c(0, 0, 0), c(-2, 1, 1) * 0.55, col = rojo)
text(p3(pm, 2 * n[1], 2 * n[2], 2 * n[3]), "espacio nulo", col = rojo, pos = 2)
text(p3(pm, 0.75, 0, 1.45), "espacio fila", col = teal, pos = 4)
dev.off()

# 3d. Estructura de las soluciones: recta solución paralela al espacio nulo
abrir("c03_soluciones.png", w = 6.6, h = 5.4)
pm <- marco3d(c(-3, 7), c(-2, 3), c(-1.5, 4), theta = 30, phi = 20)
d <- c(-2, 1, 1)
seg3(pm, -1.4 * d, 1.4 * d, col = rojo, lwd = 3.2)
xp <- c(7, -1, 0)
seg3(pm, xp + 0 * d, xp + 4 * d, col = teal, lwd = 3.2)
flecha3(pm, c(0, 0, 0), xp, col = gris, lwd = 2)
points(p3(pm, 7, -1, 0), pch = 21, bg = teal, cex = 1.5)
points(p3(pm, 1, 2, 3), pch = 21, bg = ocre, cex = 1.5)
points(p3(pm, 0, 0, 0), pch = 21, bg = tinta, cex = 1.3)
text(p3(pm, 7, -1, 0), expression(x[p] == (7 * "," * -1 * "," * 0)), pos = 4, col = teal, cex = 0.95)
text(p3(pm, 1, 2, 3), "(1, 2, 3)", pos = 4, col = ocre, cex = 0.95)
text(p3(pm, -2.8, 1.4, 1.4), "N(A)", col = rojo, pos = 3)
text(p3(pm, -1, 3, 4), expression(x[p] + N(A)), col = teal, pos = 4)
dev.off()

# 3e. Red de tres nodos: potenciales, diferencias y un lazo
abrir("c03_red.png", w = 6, h = 4.6)
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(-0.6, 4.6), ylim = c(-0.7, 3.9), axes = FALSE, xlab = "", ylab = "", asp = 1)
N <- rbind(c(0, 0), c(4, 0), c(2, 3.3))
arista <- function(a, b, et, off) {
  p <- N[a, ]; q <- N[b, ]; u <- (q - p) / sqrt(sum((q - p)^2))
  arrows(p[1] + 0.35 * u[1], p[2] + 0.35 * u[2], q[1] - 0.35 * u[1], q[2] - 0.35 * u[2],
         col = tinta, lwd = 2.5, length = 0.13)
  m <- (p + q) / 2 + off; text(m[1], m[2], et, col = tinta, cex = 1.15)
}
arista(1, 2, expression(a[1]), c(0, -0.35))
arista(2, 3, expression(a[2]), c(0.45, 0.1))
arista(1, 3, expression(a[3]), c(-0.45, 0.1))
for (k in 1:3) {
  symbols(N[k, 1], N[k, 2], circles = 0.3, inches = FALSE, add = TRUE, bg = teal, fg = tinta)
  text(N[k, 1], N[k, 2], k, col = "white", cex = 1.1)
}
text(N[1, 1] - 0.2, N[1, 2] - 0.55, expression(x[1]), col = teal)
text(N[2, 1] + 0.2, N[2, 2] - 0.55, expression(x[2]), col = teal)
text(N[3, 1] + 0.55, N[3, 2] + 0.25, expression(x[3]), col = teal)
th <- seq(-0.3, 2 * pi - 1.1, length.out = 80)
lines(2 + 0.55 * cos(th), 1.15 + 0.55 * sin(th), col = ocre, lwd = 2)
arrows(2 + 0.55 * cos(th[79]), 1.15 + 0.55 * sin(th[79]), 2 + 0.55 * cos(th[80]),
       1.15 + 0.55 * sin(th[80]), col = ocre, lwd = 2, length = 0.1)
text(2, 1.15, "lazo", col = ocre, cex = 0.95)
dev.off()

# ===========================================================================
# Clase 4: determinantes
# ---------------------------------------------------------------------------
# 4a. Área del paralelogramo por el rectángulo que lo contiene
abrir("c04_area.png", w = 6, h = 4.8)
ejes2d(c(-0.3, 4.5), c(-0.3, 3.4))
rect(0, 0, 4, 3, border = gris, lty = 2, lwd = 1.5)
polygon(c(0, 3, 4, 1), c(0, 1, 3, 2), col = adjustcolor(teal, 0.3), border = teal, lwd = 2)
flecha(0, 0, 3, 1, col = rojo); flecha(0, 0, 1, 2, col = ocre)
polygon(c(0, 3, 3), c(0, 0, 1), col = adjustcolor(gris, 0.18), border = NA)
polygon(c(1, 4, 1), c(3, 3, 2), col = adjustcolor(gris, 0.18), border = NA)
polygon(c(0, 0, 1), c(0, 2, 2), col = adjustcolor(gris, 0.30), border = NA)
polygon(c(4, 4, 3), c(3, 1, 1), col = adjustcolor(gris, 0.30), border = NA)
rect(0, 2, 1, 3, col = adjustcolor(ocre, 0.15), border = NA)
rect(3, 0, 4, 1, col = adjustcolor(ocre, 0.15), border = NA)
text(2.1, 0.35, expression((a * "," * c) == (3 * "," * 1)), col = rojo, cex = 0.95)
text(0.3, 1.15, expression((b * "," * d)), col = ocre, cex = 0.95, adj = 0)
text(2.05, 1.55, "área = ad − bc = 5", col = tinta, cex = 1.05)
dev.off()

# 4b. Cómo transforma una matriz el cuadrado unitario: área y orientación
abrir("c04_transformacion.png", w = 9.5, h = 3.9)
par(mfrow = c(1, 3), mar = c(4, 4, 2.4, 0.8))
cuadro <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
dibuja <- function(M, titulo, lim, p1 = 4, p2 = 3) {
  ejes2d(lim, lim); title(titulo, col.main = tinta, font.main = 1, cex.main = 1.15)
  P <- t(M %*% t(cuadro))
  polygon(P, col = adjustcolor(teal, 0.3), border = teal, lwd = 2)
  flecha(0, 0, P[2, 1], P[2, 2], col = rojo); flecha(0, 0, P[4, 1], P[4, 2], col = ocre)
  text(P[2, 1], P[2, 2], expression(Ae[1]), pos = p1, col = rojo)
  text(P[4, 1], P[4, 2], expression(Ae[2]), pos = p2, col = ocre)
}
dibuja(diag(2), "Cuadrado unitario: área 1", c(-0.5, 4.2))
dibuja(matrix(c(3, 1, 1, 2), 2), "det = 5: área ×5", c(-0.5, 4.2))
dibuja(matrix(c(0, 2, 1, 0), 2), "det = −2: área ×2, giro invertido", c(-0.5, 4.2), p1 = 2, p2 = 1)
dev.off()

# 4c. Paralelepípedo generado por las columnas de [1 1 0; 0 1 1; 1 0 1] (volumen 2)
abrir("c04_paralelepipedo.png", w = 6.4, h = 5.4)
pm <- marco3d(c(0, 2), c(0, 2), c(0, 2), theta = 35, phi = 20)
v1 <- c(1, 0, 1); v2 <- c(1, 1, 0); v3 <- c(0, 1, 1)
caras <- list(c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1))
vert <- function(s1, s2, s3) s1 * v1 + s2 * v2 + s3 * v3
cara <- function(a, b, fijo, val, col) {
  X <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
  Q <- t(apply(X, 1, function(r) { s <- c(0, 0, 0); s[a] <- r[1]; s[b] <- r[2]; s[fijo] <- val; vert(s[1], s[2], s[3]) }))
  poli3(pm, Q, col = col, border = adjustcolor(teal, 0.8))
}
for (val in c(0, 1)) {
  cara(1, 2, 3, val, adjustcolor(teal, 0.16))
  cara(1, 3, 2, val, adjustcolor(teal, 0.16))
  cara(2, 3, 1, val, adjustcolor(teal, 0.16))
}
flecha3(pm, c(0, 0, 0), v1, col = rojo); flecha3(pm, c(0, 0, 0), v2, col = ocre); flecha3(pm, c(0, 0, 0), v3, col = azul)
text(p3(pm, v1[1], v1[2], v1[3]), expression(a[1]), pos = 2, col = rojo)
text(p3(pm, v2[1], v2[2], v2[3]), expression(a[2]), pos = 4, col = ocre)
text(p3(pm, v3[1], v3[2], v3[3]), expression(a[3]), pos = 3, col = azul)
dev.off()

# 4d. Coordenadas polares: el elemento de área r dr dθ
abrir("c04_polares.png", w = 5.6, h = 5.2)
ejes2d(c(-0.2, 3.2), c(-0.2, 3.2))
for (r in 1:3) { th <- seq(0, pi / 2, length.out = 100); lines(r * cos(th), r * sin(th), col = adjustcolor(gris, 0.6)) }
for (th in seq(0, pi / 2, by = pi / 12)) segments(0, 0, 3.1 * cos(th), 3.1 * sin(th), col = adjustcolor(gris, 0.45))
r0 <- 2; dr <- 0.5; t0 <- pi / 6; dt <- pi / 12
th <- seq(t0, t0 + dt, length.out = 40)
polygon(c((r0) * cos(th), rev((r0 + dr) * cos(th))), c((r0) * sin(th), rev((r0 + dr) * sin(th))),
        col = adjustcolor(teal, 0.45), border = teal, lwd = 2)
text(1.25, 3.05, "lados ≈ dr y r dθ", col = teal, adj = 0, cex = 0.95)
text(1.25, 2.8, "área ≈ r dr dθ", col = teal, adj = 0, cex = 0.95)
dev.off()

# ===========================================================================
# Clase 5: ortogonalidad y QR
# ---------------------------------------------------------------------------
angulo_recto <- function(P, u, w, s = 0.18, col = tinta) {  # marca de ángulo recto en P entre u y w
  u <- u / sqrt(sum(u^2)) * s; w <- w / sqrt(sum(w^2)) * s
  lines(rbind(P + u, P + u + w, P + w), col = col, lwd = 1.2)
}
# 5a. Proyección de b = (1,3) sobre la recta de a = (2,1)
abrir("c05_proyeccion.png", w = 6, h = 4.8)
ejes2d(c(-1.5, 4), c(-0.5, 3.4))
a <- c(2, 1); b <- c(1, 3); p <- c(2, 1); e <- b - p
abline(0, 0.5, col = adjustcolor(teal, 0.5), lwd = 1.5, lty = 2)
flecha(0, 0, b[1], b[2], col = tinta)
flecha(0, 0, a[1], a[2], col = teal, lwd = 3.2)
segments(p[1], p[2], b[1], b[2], col = rojo, lwd = 2.5, lty = 2)
angulo_recto(p, -a, e, s = 0.25)
points(p[1], p[2], pch = 21, bg = ocre, cex = 1.6)
text(b[1], b[2], "b = (1, 3)", pos = 4, col = tinta)
text(2.05, 0.65, "p = a = (2, 1)", pos = 4, col = teal)
text(1.65, 2.1, "e = b − p = (−1, 2)", pos = 4, col = rojo)
text(3.75, 1.55, "recta de a", col = teal, cex = 0.9)
dev.off()

# 5b. Rotación y reflexión del plano: la reflexión invierte la orientación
abrir("c05_rot_refl.png", w = 9, h = 4.4)
par(mfrow = c(1, 2), mar = c(4, 4, 2.4, 0.8))
L <- rbind(c(0.4, 0.15), c(1.6, 0.15), c(1.6, 0.5), c(0.75, 0.5), c(0.75, 1.3), c(0.4, 1.3))
th <- pi / 3
Rm <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
Hm <- matrix(c(cos(th), sin(th), sin(th), -cos(th)), 2)
ejes2d(c(-1.6, 1.8), c(-0.3, 1.9)); title("Rotación en 60°", col.main = tinta, font.main = 1, cex.main = 1.15)
polygon(L, col = adjustcolor(gris, 0.25), border = gris, lwd = 1.5)
polygon(t(Rm %*% t(L)), col = adjustcolor(teal, 0.35), border = teal, lwd = 2)
arc <- seq(0.25, 0.25 + th, length.out = 40); lines(1.25 * cos(arc), 1.25 * sin(arc), col = ocre, lwd = 1.8)
text(1.3, 1.05, "60°", col = ocre)
ejes2d(c(-0.6, 1.9), c(-0.3, 1.9)); title("Reflexión en la recta de 30°", col.main = tinta, font.main = 1, cex.main = 1.15)
abline(0, tan(pi / 6), col = rojo, lwd = 1.8, lty = 2)
polygon(L, col = adjustcolor(gris, 0.25), border = gris, lwd = 1.5)
polygon(t(Hm %*% t(L)), col = adjustcolor(teal, 0.35), border = teal, lwd = 2)
text(1.75, 0.83, "espejo", col = rojo, cex = 0.95)
dev.off()

# 5c. Gram–Schmidt: en el plano y en el espacio
abrir("c05_gram_schmidt.png", w = 9.6, h = 4.7)
par(mfrow = c(1, 2))
par(mar = c(4, 4, 2.2, 0.8))
ejes2d(c(-0.4, 3.4), c(-0.9, 4.3)); title("En el plano", col.main = tinta, font.main = 1, cex.main = 1.15)
a1 <- c(3, 4); a2 <- c(2, 1); q1 <- a1 / 5; pr <- 2 * q1; v2 <- a2 - pr
flecha(0, 0, a1[1], a1[2], col = teal); flecha(0, 0, a2[1], a2[2], col = ocre)
segments(pr[1], pr[2], a2[1], a2[2], col = rojo, lwd = 2, lty = 2)
flecha(0, 0, v2[1], v2[2], col = rojo); flecha(0, 0, q1[1], q1[2], col = tinta, lwd = 3.2)
angulo_recto(pr, -a1, a2 - pr, s = 0.2)
points(pr[1], pr[2], pch = 21, bg = ocre, cex = 1.3)
text(a1[1], a1[2], expression(a[1]), pos = 3, col = teal)
text(a2[1], a2[2], expression(a[2]), pos = 4, col = ocre)
text(pr[1], pr[2], expression(2 * q[1]), pos = 2, col = ocre)
text(q1[1], q1[2], expression(q[1]), pos = 2, col = tinta)
text(v2[1], v2[2], expression(q[2] == a[2] - 2 * q[1]), pos = 4, col = rojo, cex = 0.95)
A1 <- c(1, 2, 2); A2 <- c(4, 5, 2); PR <- 2 * A1; V2 <- A2 - PR
pm <- marco3d(c(0, 4.5), c(0, 5.5), c(-2, 4.5), theta = 40, phi = 18)
title("En el espacio", col.main = tinta, font.main = 1, cex.main = 1.15)
seg3(pm, c(0, 0, 0), 2.3 * A1, col = adjustcolor(teal, 0.5), lty = 2)
flecha3(pm, c(0, 0, 0), A1, col = teal); flecha3(pm, c(0, 0, 0), A2, col = ocre)
seg3(pm, PR, A2, col = rojo, lwd = 2, lty = 2)
flecha3(pm, c(0, 0, 0), V2, col = rojo)
points(p3(pm, PR[1], PR[2], PR[3]), pch = 21, bg = ocre, cex = 1.3)
text(p3(pm, A1[1], A1[2], A1[3]), expression(a[1]), pos = 2, col = teal)
text(p3(pm, A2[1], A2[2], A2[3]), expression(a[2]), pos = 1, col = ocre)
text(p3(pm, PR[1], PR[2], PR[3]), expression(6 * q[1]), pos = 2, col = ocre)
text(p3(pm, V2[1], V2[2], V2[3]), expression(v[2] == 3 * q[2]), pos = 4, col = rojo)
dev.off()

# 5d. Proyección de b = (3,0,0) sobre el plano generado por q1 y q2
abrir("c05_proy_plano.png", w = 6.6, h = 5.4)
u <- c(1, 2, 2) / 3; v <- c(2, 1, -2) / 3; B <- c(3, 0, 0); P <- u + 2 * v
S <- rbind(c(-0.6, -0.4), c(2.2, -0.4), c(2.2, 2.6), c(-0.6, 2.6))
X <- t(apply(S, 1, function(s) s[1] * u + s[2] * v))
pm <- marco3d(range(c(X[, 1], 0, 3)), range(c(X[, 2], 0)), range(c(X[, 3], 0, 1)), theta = -35, phi = 22)
poli3(pm, X, col = adjustcolor(teal, 0.25), border = adjustcolor(teal, 0.6))
flecha3(pm, c(0, 0, 0), B, col = tinta)
flecha3(pm, c(0, 0, 0), P, col = teal)
seg3(pm, P, B, col = rojo, lwd = 2.5, lty = 2)
flecha3(pm, c(0, 0, 0), u, col = ocre, lwd = 2); flecha3(pm, c(0, 0, 0), v, col = ocre, lwd = 2)
text(p3(pm, B[1], B[2], B[3]), "b = (3, 0, 0)", pos = 4, col = tinta)
text(p3(pm, P[1], P[2], P[3]), "p", pos = 2, col = teal, cex = 1.2)
M <- (P + B) / 2; text(p3(pm, M[1], M[2], M[3]), "e = 2q3", pos = 3, col = rojo)
text(p3(pm, u[1], u[2], u[3]), expression(q[1]), pos = 3, col = ocre)
text(p3(pm, v[1], v[2], v[3]), expression(q[2]), pos = 1, col = ocre)
dev.off()

# 5e. Reflexión de Householder que lleva a = (3,4) a (5,0)
abrir("c05_householder.png", w = 6, h = 4.9)
ejes2d(c(-2.6, 5.6), c(-0.8, 4.6))
a <- c(3, 4); Ha <- c(5, 0); vv <- a - Ha
abline(0, 0.5, col = rojo, lwd = 1.8, lty = 2)
flecha(0, 0, a[1], a[2], col = teal); flecha(0, 0, Ha[1], Ha[2], col = teal)
flecha(0, 0, vv[1], vv[2], col = ocre)
segments(a[1], a[2], Ha[1], Ha[2], col = adjustcolor(ocre, 0.7), lty = 3, lwd = 1.8)
text(a[1], a[2], "a = (3, 4)", pos = 4, col = teal)
text(Ha[1], Ha[2], "Ha = (5, 0)", pos = 3, col = teal)
text(vv[1], vv[2], "v = a − 5e1 = (−2, 4)", pos = 4, col = ocre)
text(3.0, 0.7, "espejo (perpendicular a v)", col = rojo, cex = 0.9)
dev.off()

# ===========================================================================
# Clase 6: mínimos cuadrados
# ---------------------------------------------------------------------------
ejesdat <- function(xlim, ylim, xlab = "t", ylab = "b", ...) {
  plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, las = 1, ...)
  abline(h = pretty(ylim), v = pretty(xlim), col = adjustcolor(gris, 0.18))
  abline(h = 0, col = adjustcolor(tinta, 0.4))
}
# 6a. Recta de mínimos cuadrados por (0,6), (1,0), (2,0)
abrir("c06_recta.png", w = 6, h = 4.6)
ejesdat(c(-0.3, 2.4), c(-2, 7))
tt <- 0:2; bb <- c(6, 0, 0); ff <- 5 - 3 * tt
abline(5, -3, col = teal, lwd = 2.6)
segments(tt, bb, tt, ff, col = rojo, lwd = 2.2, lty = 2)
points(tt, ff, pch = 21, bg = "white", col = teal, cex = 1.3)
points(tt, bb, pch = 21, bg = ocre, cex = 1.7)
text(0.07, 5.5, "e1 = 1", col = rojo, adj = 0); text(0.93, 1, "e2 = −2", col = rojo, adj = 1); text(2.07, -0.5, "e3 = 1", col = rojo, adj = 0)
text(1.55, 3.4, "b = 5 − 3t", col = teal, cex = 1.1)
dev.off()

# 6b. b = (6,0,0) proyectado sobre el plano C(A) generado por (1,1,1) y (0,1,2)
abrir("c06_proy_plano.png", w = 6.6, h = 5.4)
a1 <- c(1, 1, 1); a2 <- c(0, 1, 2); B <- c(6, 0, 0); P <- c(5, 2, -1)
S <- rbind(c(-0.4, -3.6), c(5.8, -3.6), c(5.8, 1.2), c(-0.4, 1.2))
X <- t(apply(S, 1, function(s) s[1] * a1 + s[2] * a2))
pm <- marco3d(range(c(X[, 1], 0, 6)), range(c(X[, 2], 0)), range(c(X[, 3], 0)), theta = 100, phi = 18)
poli3(pm, X, col = adjustcolor(teal, 0.22), border = adjustcolor(teal, 0.6))
flecha3(pm, c(0, 0, 0), a1, col = ocre, lwd = 2); flecha3(pm, c(0, 0, 0), a2, col = ocre, lwd = 2)
flecha3(pm, c(0, 0, 0), B, col = tinta); flecha3(pm, c(0, 0, 0), P, col = teal)
seg3(pm, P, B, col = rojo, lwd = 2.5, lty = 2)
text(p3(pm, B[1], B[2], B[3]), "b = (6, 0, 0)", pos = 2, col = tinta)
text(p3(pm, P[1], P[2], P[3]), "p = (5, 2, −1)", pos = 4, col = teal)
M <- (P + B) / 2; text(p3(pm, M[1], M[2], M[3]), "e = (1, −2, 1)", pos = 1, col = rojo)
text(p3(pm, a1[1], a1[2], a1[3]), expression(a[1]), pos = 3, col = ocre)
text(p3(pm, a2[1], a2[2], a2[3]), expression(a[2]), pos = 2, col = ocre)
dev.off()

# 6c. Parábola de mínimos cuadrados por cuatro puntos
abrir("c06_parabola.png", w = 6, h = 4.6)
ejesdat(c(-1.4, 2.4), c(-2, 7))
tt <- -1:2; bb <- c(1, 4, -1, 6); ff <- 1 + tt^2
curve(1 + x^2, from = -1.4, to = 2.4, add = TRUE, col = teal, lwd = 2.6)
segments(tt, bb, tt, ff, col = rojo, lwd = 2.2, lty = 2)
points(tt, ff, pch = 21, bg = "white", col = teal, cex = 1.3)
points(tt, bb, pch = 21, bg = ocre, cex = 1.7)
text(1.2, 5.3, expression(b == 1 + t^2), col = teal, cex = 1.1)
dev.off()

# 6d. Ajuste exponencial: escala original y escala logarítmica
abrir("c06_exponencial.png", w = 9.4, h = 4.2)
par(mfrow = c(1, 2), mar = c(4, 4.2, 2.4, 0.8))
tt <- 0:2; yy <- exp(c(0, 1, 3))
ejesdat(c(-0.2, 2.3), c(0, 24), ylab = "y"); title("Escala original", col.main = tinta, font.main = 1, cex.main = 1.15)
curve(exp(-1 / 6 + 1.5 * x), from = -0.2, to = 2.3, add = TRUE, col = teal, lwd = 2.6)
points(tt, yy, pch = 21, bg = ocre, cex = 1.7)
text(0.85, 15, expression(y == e^{-1/6} * e^{1.5 * t}), col = teal, cex = 1.05)
ejesdat(c(-0.2, 2.3), c(-0.6, 3.4), ylab = "ln y"); title("Escala logarítmica", col.main = tinta, font.main = 1, cex.main = 1.15)
abline(-1 / 6, 1.5, col = teal, lwd = 2.6)
segments(tt, c(0, 1, 3), tt, -1 / 6 + 1.5 * tt, col = rojo, lwd = 2, lty = 2)
points(tt, c(0, 1, 3), pch = 21, bg = ocre, cex = 1.7)
text(0.55, 2.4, expression(ln ~ y == -1/6 + 1.5 * t), col = teal, cex = 1.05)
dev.off()

# 6e. Tres rectas para los mismos datos: MC, ponderados y Tikhonov
abrir("c06_comparacion.png", w = 6.2, h = 4.6)
ejesdat(c(-0.3, 2.4), c(-2.5, 7))
abline(5, -3, col = teal, lwd = 2.6)
abline(4.5, -3, col = ocre, lwd = 2.6, lty = 2)
abline(2.4, -1.2, col = rojo, lwd = 2.6, lty = 4)
points(0:2, c(6, 0, 0), pch = 21, bg = tinta, cex = 1.6)
legend("topright", c("mínimos cuadrados: 5 − 3t", "ponderados W = diag(1, 2, 1): 4,5 − 3t",
       "Tikhonov λ = 1: 2,4 − 1,2t"), col = c(teal, ocre, rojo), lty = c(1, 2, 4), lwd = 2.4,
       bg = fondo, box.col = gris, cex = 0.85)
dev.off()

# ---------------------------------------------------------------------------
# Clase 7: valores propios y sistemas dinámicos
# ---------------------------------------------------------------------------
# Solución exacta de x' = A x con descomposición espectral (A diagonalizable, valores propios posiblemente complejos)
sol_edo <- function(A, x0, t) {
  e <- eigen(A); V <- e$vectors; cc <- solve(V, as.complex(x0))
  sapply(t, function(tt) Re(V %*% (exp(e$values * tt) * cc)))
}

# 7a. A = [[4,1],[2,3]]: los vectores propios solo se estiran; los demás giran
abrir("c07_propios_2d.png", w = 6.4, h = 4.8)
ejes2d(c(-1.5, 5.8), c(-4.6, 5.6))
abline(0, 1, col = adjustcolor(teal, 0.35), lty = 2, lwd = 1.5)
abline(0, -2, col = adjustcolor(rojo, 0.35), lty = 2, lwd = 1.5)
flecha(0, 0, 1, 1, col = teal, lwd = 3); flecha(0, 0, 5, 5, col = teal, lwd = 1.6, lty = 2)
flecha(0, 0, 1, -2, col = rojo, lwd = 3); flecha(0, 0, 2, -4, col = rojo, lwd = 1.6, lty = 2)
flecha(0, 0, 1, 0, col = ocre, lwd = 3); flecha(0, 0, 4, 2, col = ocre, lwd = 1.6, lty = 2)
text(1, 1.1, "v1 = (1, 1)", pos = 2, col = teal, cex = 0.9); text(5, 5, "A v1 = 5 v1", pos = 4, col = teal, cex = 0.9)
text(1.05, -2, "v2 = (1, −2)", pos = 4, col = rojo, cex = 0.9); text(2, -4, "A v2 = 2 v2", pos = 4, col = rojo, cex = 0.9)
text(1, -0.35, "x = (1, 0)", pos = 1, col = ocre, cex = 0.9); text(4, 2, "A x = (4, 2)", pos = 4, col = ocre, cex = 0.9)
dev.off()

# 7b. Cadena de Markov de dos ciudades: x0 = (0, 1)
abrir("c07_markov.png", w = 9.4, h = 4.4)
par(mfrow = c(1, 2), mar = c(4, 4.2, 2.4, 0.8))
Mk <- matrix(c(.9, .1, .2, .8), 2); xs <- matrix(0, 2, 13); xs[, 1] <- c(0, 1)
for (k in 2:13) xs[, k] <- Mk %*% xs[, k - 1]
plot(NA, xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05), xlab = "fracción en la ciudad 1", ylab = "fracción en la ciudad 2", las = 1, asp = 1)
title("Trayectoria en el plano", col.main = tinta, font.main = 1, cex.main = 1.15)
abline(h = pretty(c(0, 1)), v = pretty(c(0, 1)), col = adjustcolor(gris, 0.18))
abline(1, -1, col = adjustcolor(tinta, 0.5), lty = 2)
lines(xs[1, ], xs[2, ], col = teal, lwd = 1.8); points(xs[1, ], xs[2, ], pch = 21, bg = ocre, cex = 1.1)
points(2/3, 1/3, pch = 24, bg = rojo, cex = 1.7)
text(0.05, 0.93, "x0 = (0, 1)", pos = 4, col = tinta, cex = 0.9); text(2/3, 0.28, "(2/3, 1/3)", pos = 1, col = rojo, cex = 0.95)
text(0.85, 0.28, "x1 + x2 = 1", col = tinta, cex = 0.85, srt = -45)
plot(NA, xlim = c(0, 12), ylim = c(0, 1), xlab = "paso k", ylab = "fracción", las = 1)
title("Cada ciudad en el tiempo", col.main = tinta, font.main = 1, cex.main = 1.15)
abline(h = pretty(c(0, 1)), v = pretty(c(0, 12)), col = adjustcolor(gris, 0.18))
abline(h = c(1/3, 2/3), col = adjustcolor(tinta, 0.5), lty = 2)
lines(0:12, xs[1, ], col = teal, lwd = 2); points(0:12, xs[1, ], pch = 21, bg = teal, cex = 0.9)
lines(0:12, xs[2, ], col = rojo, lwd = 2); points(0:12, xs[2, ], pch = 21, bg = rojo, cex = 0.9)
text(9.5, 0.78, "ciudad 1 → 2/3", col = teal, cex = 0.95); text(9.5, 0.22, "ciudad 2 → 1/3", col = rojo, cex = 0.95)
dev.off()

# 7c. Retratos de fase
retrato <- function(A, titulo, lim = 3, n0 = 8, vp = TRUE) {
  plot(NA, xlim = c(-lim, lim), ylim = c(-lim, lim), xlab = expression(x[1]), ylab = expression(x[2]), las = 1, asp = 1)
  title(titulo, col.main = tinta, font.main = 1, cex.main = 1.1)
  abline(h = 0, v = 0, col = adjustcolor(tinta, 0.4))
  e <- eigen(A)
  if (vp && all(abs(Im(e$values)) < 1e-12)) for (j in 1:2) { v <- Re(e$vectors[, j]); abline(0, v[2] / v[1], col = adjustcolor(rojo, 0.55), lty = 2, lwd = 1.5) }
  tt <- seq(0, 6, length.out = 1500)
  ang <- seq(0, 2 * pi, length.out = n0 + 1)[-1] + 0.3
  for (r in c(0.8, 1.9)) for (a in ang) {
    x0 <- r * c(cos(a), sin(a)); S <- sol_edo(A, x0, tt)
    fuera <- abs(S[1, ]) > lim | abs(S[2, ]) > lim
    if (any(fuera)) S[, which(fuera)[1]:ncol(S)] <- NA
    ok <- which(!is.na(S[1, ]))
    if (length(ok) > 10) {
      lines(S[1, ], S[2, ], col = adjustcolor(teal, 0.8), lwd = 1.3)
      m <- ok[min(length(ok) - 3, 40)]
      arrows(S[1, m], S[2, m], S[1, m + 3], S[2, m + 3], col = teal, length = 0.07, lwd = 1.3)
    }
  }
  points(0, 0, pch = 21, bg = ocre, cex = 1.2)
}
abrir("c07_retratos.png", w = 8.6, h = 8.2)
par(mfrow = c(2, 2), mar = c(4, 4, 2.6, 0.8))
retrato(matrix(c(-3, 2, 1, -2), 2), "Nodo estable: λ = −1, −4")
retrato(matrix(c(1, 2, 2, 1), 2), "Silla: λ = 3, −1")
retrato(matrix(c(0, -2, 1, -2), 2), "Foco estable: λ = −1 ± i", n0 = 6)
retrato(matrix(c(0, -4, 1, 0), 2), "Centro: λ = ±2i", n0 = 5)
dev.off()

# 7d. Plano traza-determinante
abrir("c07_traza_det.png", w = 7, h = 4.8)
plot(NA, xlim = c(-5, 5), ylim = c(-3.5, 6.5), xlab = "traza  τ", ylab = "determinante  Δ", las = 1)
abline(h = 0, v = 0, col = adjustcolor(tinta, 0.5))
tx <- seq(-5, 5, length.out = 300)
polygon(c(tx, rev(tx)), c(tx^2 / 4, rep(7, length(tx))), col = adjustcolor(azul, 0.16), border = NA)
polygon(c(-5, 5, 5, -5), c(0, 0, -3.5, -3.5), col = adjustcolor(rojo, 0.14), border = NA)
polygon(c(tx, rev(tx)), c(pmin(tx^2 / 4, 7), rep(0, length(tx))), col = adjustcolor(ocre, 0.14), border = NA)
lines(tx, tx^2 / 4, col = tinta, lwd = 1.8)
text(-3.4, 1.0, "nodo estable", col = tinta, cex = 0.9); text(3.4, 1.0, "nodo inestable", col = tinta, cex = 0.9)
text(-3.4, 4.6, "foco estable", col = tinta, cex = 0.9); text(3.4, 4.6, "foco inestable", col = tinta, cex = 0.9)
text(0, 5.6, "centro (τ = 0)", col = tinta, cex = 0.9); text(0, -2, "silla (siempre inestable)", col = tinta, cex = 0.95)
text(2.9, 3.0, "Δ = τ²/4", col = tinta, cex = 0.8, adj = 0)
pts <- rbind(c(-5, 4), c(2, -3), c(-2, 2), c(0, 4))
points(pts[, 1], pts[, 2], pch = 21, bg = rojo, cex = 1.3)
text(pts[1, 1] + 0.1, pts[1, 2] - 0.55, "(−5, 4)", cex = 0.8, col = rojo, adj = 0); text(pts[2, 1] + 0.1, pts[2, 2] - 0.4, "(2, −3)", cex = 0.8, col = rojo, adj = 0)
text(pts[3, 1] + 0.1, pts[3, 2] - 0.45, "(−2, 2)", cex = 0.8, col = rojo, adj = 0); text(pts[4, 1] + 0.1, pts[4, 2] - 0.45, "(0, 4)", cex = 0.8, col = rojo, adj = 0)
dev.off()

# 7e. Euler: R = R1 R3 es la rotación de 120 grados alrededor de (1, -1, 1)
abrir("c07_euler.png", w = 6.6, h = 5.4)
ax <- c(1, -1, 1); u0 <- c(1, 1, 0); u1 <- c(-1, 0, 1); u2 <- c(0, -1, -1)
pm <- marco3d(c(-1.6, 1.6), c(-1.6, 1.6), c(-1.6, 1.6), theta = -35, phi = 22)
B1 <- c(1, 1, 0) / sqrt(2); B2 <- c(1, -1, -2) / sqrt(6)   # base ortonormal del plano perpendicular al eje
pl <- t(sapply(list(c(-1.5, -1.5), c(1.5, -1.5), c(1.5, 1.5), c(-1.5, 1.5)), function(s) s[1] * B1 + s[2] * B2))
poli3(pm, pl, col = adjustcolor(teal, 0.16), border = adjustcolor(teal, 0.5))
seg3(pm, -1.4 * ax, 1.4 * ax, col = rojo, lwd = 1.6, lty = 2)
flecha3(pm, c(0, 0, 0), 1.2 * ax, col = rojo, lwd = 2.6)
flecha3(pm, c(0, 0, 0), u0, col = tinta); flecha3(pm, c(0, 0, 0), u1, col = ocre); flecha3(pm, c(0, 0, 0), u2, col = azul)
seg3(pm, u0, u1, col = gris, lty = 3); seg3(pm, u1, u2, col = gris, lty = 3); seg3(pm, u2, u0, col = gris, lty = 3)
text(p3(pm, 0.55, -0.55, 0.55), "eje (1, −1, 1)", pos = 3, col = rojo, cex = 0.9)
text(p3(pm, 1, 1, 0), "u", pos = 4, col = tinta); text(p3(pm, -1, 0, 1), "Ru", pos = 2, col = ocre); text(p3(pm, 0, -1, -1), "R²u", pos = 1, col = azul)
dev.off()

# 7f. Matriz defectuosa A = [[3,1],[0,3]]: un solo vector propio, nodo degenerado
abrir("c07_defectivo.png", w = 6, h = 4.8)
ejes2d(c(-3, 3), c(-3, 3))
abline(h = 0, col = adjustcolor(rojo, 0.55), lwd = 1.8, lty = 2)
tt <- seq(-4, 1.2, length.out = 800)
for (cc in c(-2.2, -1.2, -0.5, 0.5, 1.2, 2.2)) for (c1 in c(0, 1.5, -1.5)) {
  x1 <- exp(3 * tt) * (c1 + cc * tt); x2 <- exp(3 * tt) * cc
  ok <- abs(x1) < 3 & abs(x2) < 3
  lines(x1[ok], x2[ok], col = adjustcolor(teal, 0.8), lwd = 1.2)
  nr <- sqrt(x1^2 + x2^2); nr[!ok] <- Inf; m <- which.min(abs(nr - 1.6)); if (is.finite(nr[m]) && m < 797) arrows(x1[m], x2[m], x1[m + 3], x2[m + 3], col = teal, length = 0.08, lwd = 1.3)
}
points(0, 0, pch = 21, bg = ocre, cex = 1.3)
text(2.9, 0.2, "único vector propio (1, 0)", pos = 2, col = rojo, cex = 0.85)
dev.off()

# ---------------------------------------------------------------------------
# Clase 8: matrices simétricas y definidas positivas
# ---------------------------------------------------------------------------
s2 <- sqrt(2)

# 8a. Tres cónicas x'Ax = 1: elipse, hipérbola y rectas paralelas
abrir("c08_cuadricas_2d.png", w = 9.8, h = 3.8)
par(mfrow = c(1, 3), mar = c(4, 4, 2.6, 0.8))
# (a) elipse A = [[2,1],[1,2]]
ejes2d(c(-1.3, 1.3), c(-1.3, 1.3))
title("Elipse: λ = 1, 3", col.main = tinta, font.main = 1, cex.main = 1.1)
tt <- seq(0, 2 * pi, length.out = 300)
Ee <- cbind(c(1, -1) / s2, c(1, 1) / s2) %*% rbind(1 * cos(tt), 1 / sqrt(3) * sin(tt))
lines(Ee[1, ], Ee[2, ], col = teal, lwd = 2.4)
abline(0, 1, col = adjustcolor(rojo, 0.5), lty = 2); abline(0, -1, col = adjustcolor(rojo, 0.5), lty = 2)
flecha(0, 0, 1 / s2, -1 / s2, col = rojo, lwd = 2.4); flecha(0, 0, 1 / sqrt(3) / s2, 1 / sqrt(3) / s2, col = ocre, lwd = 2.4)
text(0.75, -0.95, "semieje 1", col = rojo, cex = 0.9); text(0.55, 0.72, expression("semieje " * 1 / sqrt(3)), col = ocre, cex = 0.9)
# (b) hipérbola A = [[1,2],[2,1]]: 3 y1^2 - y2^2 = 1
ejes2d(c(-2.6, 2.6), c(-2.6, 2.6))
title("Hipérbola: λ = −1, 3", col.main = tinta, font.main = 1, cex.main = 1.1)
ss <- seq(-1.9, 1.9, length.out = 300)
for (sg in c(-1, 1)) {
  y1 <- sg * cosh(ss) / sqrt(3); y2 <- sinh(ss)
  Hh <- cbind(c(1, 1) / s2, c(1, -1) / s2) %*% rbind(y1, y2)
  lines(Hh[1, ], Hh[2, ], col = teal, lwd = 2.4)
}
for (m in c(-1, 1)) { as <- cbind(c(1, 1) / s2, c(1, -1) / s2) %*% rbind(c(-3, 3), m * sqrt(3) * c(-3, 3)); lines(as[1, ], as[2, ], col = adjustcolor(gris, 0.9), lty = 3) }
# (c) rectas A = [[1,1],[1,1]]: (x+y)^2 = 1
ejes2d(c(-1.6, 1.6), c(-1.6, 1.6))
title("Rectas: λ = 0, 2", col.main = tinta, font.main = 1, cex.main = 1.1)
abline(1, -1, col = teal, lwd = 2.4); abline(-1, -1, col = teal, lwd = 2.4)
flecha(0, 0, 0.9, -0.9, col = rojo, lwd = 2.2)
text(0.95, -0.62, "λ = 0", col = rojo, cex = 0.95, adj = 0)
dev.off()

# 8b. Elipsoide x'Ax = 1 para A = I + J (valores propios 4, 1, 1): esferoide con eje corto (1,1,1)
ELIP_TH <- as.numeric(Sys.getenv("ELIP_TH", "-40")); ELIP_PH <- as.numeric(Sys.getenv("ELIP_PH", "35"))
abrir("c08_elipsoide.png", w = 6.6, h = 5.4)
q1 <- c(1, 1, 1) / sqrt(3); q2 <- c(1, -1, 0) / sqrt(2); q3 <- c(1, 1, -2) / sqrt(6)
pm <- marco3d(c(-1.1, 1.1), c(-1.1, 1.1), c(-1.1, 1.1), theta = ELIP_TH, phi = ELIP_PH)
punto_elip <- function(th, ph) { u <- c(sin(th) * cos(ph), sin(th) * sin(ph), cos(th)); 0.5 * u[1] * q1 + 1 * u[2] * q2 + 1 * u[3] * q3 }
for (th in seq(0.35, pi - 0.35, length.out = 7)) {
  P <- t(sapply(seq(0, 2 * pi, length.out = 90), function(ph) punto_elip(th, ph)))
  lines(trans3d(P[, 1], P[, 2], P[, 3], pm), col = adjustcolor(teal, 0.55), lwd = 1)
}
for (ph in seq(0, pi, length.out = 9)[-9]) {
  P <- t(sapply(seq(0, 2 * pi, length.out = 90), function(th) punto_elip(th, ph)))
  lines(trans3d(P[, 1], P[, 2], P[, 3], pm), col = adjustcolor(teal, 0.55), lwd = 1)
}
flecha3(pm, c(0, 0, 0), 0.5 * q1, col = rojo, lwd = 3); flecha3(pm, c(0, 0, 0), q2, col = ocre, lwd = 2.4); flecha3(pm, c(0, 0, 0), q3, col = ocre, lwd = 2.4)
seg3(pm, -0.5 * q1, 0.5 * q1, col = rojo, lwd = 1.4, lty = 2)
text(p3(pm, 0.5 * q1[1] + 0.05, 0.5 * q1[2], 0.5 * q1[3] + 0.18), "semieje 1/2", col = rojo, cex = 0.9)
text(p3(pm, 1.12 * q2[1], 1.12 * q2[2], 1.12 * q2[3]), "semieje 1", col = ocre, cex = 0.9, pos = 4)
text(p3(pm, 1.3 * q3[1], 1.3 * q3[2], 1.3 * q3[3]), "semieje 1", col = ocre, cex = 0.9, pos = 1)
dev.off()

# 8c. Cociente de Rayleigh y la imagen del círculo unidad por A = [[2,1],[1,2]]
abrir("c08_rayleigh.png", w = 9.4, h = 4.4)
par(mfrow = c(1, 2), mar = c(4, 4.2, 2.4, 0.8))
Ar <- matrix(c(2, 1, 1, 2), 2)
th <- seq(0, pi, length.out = 300)
plot(NA, xlim = c(0, 180), ylim = c(0.6, 3.4), xlab = "ángulo θ del vector unitario (grados)", ylab = "cociente de Rayleigh R(x)", las = 1)
title("R(cos θ, sen θ) = 2 + sen 2θ", col.main = tinta, font.main = 1, cex.main = 1.1)
abline(h = pretty(c(0.6, 3.4)), v = seq(0, 180, 45), col = adjustcolor(gris, 0.18))
abline(h = c(1, 3), col = adjustcolor(rojo, 0.6), lty = 2)
lines(th * 180 / pi, 2 + sin(2 * th), col = teal, lwd = 2.4)
points(c(45, 135), c(3, 1), pch = 21, bg = c(rojo, ocre), cex = 1.5)
text(45, 3.18, "máximo λ₂ = 3 en (1, 1)", cex = 0.85, col = rojo); text(135, 0.82, "mínimo λ₁ = 1 en (1, −1)", cex = 0.85, col = ocre)
ejes2d(c(-3.6, 3.6), c(-3.6, 3.6))
title("El círculo unidad y su imagen A x", col.main = tinta, font.main = 1, cex.main = 1.1)
tt <- seq(0, 2 * pi, length.out = 300); U <- rbind(cos(tt), sin(tt)); W <- Ar %*% U
lines(U[1, ], U[2, ], col = azul, lwd = 2); lines(W[1, ], W[2, ], col = teal, lwd = 2.4)
for (a in seq(0.15, 2 * pi, length.out = 11)) { x <- c(cos(a), sin(a)); y <- Ar %*% x; segments(x[1], x[2], y[1], y[2], col = adjustcolor(gris, 0.7), lwd = 0.9) }
flecha(0, 0, 3 / s2, 3 / s2, col = rojo, lwd = 2.6); flecha(0, 0, 1 / s2, -1 / s2, col = ocre, lwd = 2.6)
text(2.4, 2.9, "A q = 3 q", col = rojo, cex = 0.9); text(1.5, -1.0, "A q = q", col = ocre, cex = 0.9)
legend("bottomright", c("círculo unidad", "imagen: semiejes 3 y 1"), col = c(azul, teal), lwd = 2.4, bty = "n", cex = 0.85)
dev.off()

# 8d. Puntos críticos y Hessiano: f = x^3 - 3x + y^2 y g = x^3 + y^3 - 3xy
abrir("c08_hessiano.png", w = 9.4, h = 4.4)
par(mfrow = c(1, 2), mar = c(4, 4.2, 2.6, 0.8))
xs <- seq(-2.4, 2.4, length.out = 220); ys <- seq(-2.2, 2.2, length.out = 220)
Fz <- outer(xs, ys, function(x, y) x^3 - 3 * x + y^2)
contour(xs, ys, Fz, levels = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 2, 3, 4, 6), col = adjustcolor(teal, 0.85), lwd = 1.2, labcex = 0.7, xlab = expression(x), ylab = expression(y), las = 1, drawlabels = TRUE)
title("f = x³ − 3x + y²", col.main = tinta, font.main = 1, cex.main = 1.1)
points(1, 0, pch = 24, bg = teal, cex = 1.7); points(-1, 0, pch = 21, bg = rojo, cex = 1.7)
legend("topright", c("mínimo, H = diag(6, 2)", "silla, H = diag(−6, 2)"), pch = c(24, 21), pt.bg = c(teal, rojo), bty = "o", box.col = gris, bg = fondo, cex = 0.82)
xs <- seq(-1.4, 2.0, length.out = 220); ys <- seq(-1.4, 2.0, length.out = 220)
Gz <- outer(xs, ys, function(x, y) x^3 + y^3 - 3 * x * y)
contour(xs, ys, Gz, levels = c(-1, -0.75, -0.5, -0.25, 0, 0.5, 1, 2, 4), col = adjustcolor(teal, 0.85), lwd = 1.2, labcex = 0.7, xlab = expression(x), ylab = expression(y), las = 1, drawlabels = TRUE)
title("g = x³ + y³ − 3xy", col.main = tinta, font.main = 1, cex.main = 1.1)
points(1, 1, pch = 24, bg = teal, cex = 1.7); points(0, 0, pch = 21, bg = rojo, cex = 1.7)
legend("bottomright", c("mínimo, g = −1", "silla, g = 0"), pch = c(24, 21), pt.bg = c(teal, rojo), bty = "o", box.col = gris, bg = fondo, cex = 0.82)
dev.off()

# 8e. Modos normales: dos masas (evolución temporal) y los tres modos de tres masas
abrir("c08_modos.png", w = 11.2, h = 3.7)
par(mfrow = c(1, 4), mar = c(4, 4.6, 2.8, 0.8))
tt <- seq(0, 22, length.out = 700)
plot(NA, xlim = c(0, 22), ylim = c(-1.1, 1.1), xlab = "tiempo t", ylab = "desplazamiento", las = 1)
title("Dos masas: x(0) = (1, 0)", col.main = tinta, font.main = 1, cex.main = 1.05)
abline(h = 0, col = adjustcolor(tinta, 0.5)); abline(h = pretty(c(-1, 1)), col = adjustcolor(gris, 0.18))
lines(tt, 0.5 * (cos(tt) + cos(sqrt(3) * tt)), col = teal, lwd = 1.8); lines(tt, 0.5 * (cos(tt) - cos(sqrt(3) * tt)), col = rojo, lwd = 1.8)
legend("bottomright", c("masa 1", "masa 2"), col = c(teal, rojo), lwd = 2, bty = "n", cex = 0.8)
modo <- function(a, titulo) {
  plot(NA, xlim = c(0, 4), ylim = c(-1.9, 1.9), xlab = "posición de reposo", ylab = "desplazamiento", las = 1, xaxt = "n")
  axis(1, at = 0:4, labels = c("pared", "1", "2", "3", "pared"))
  title(titulo, col.main = tinta, font.main = 1, cex.main = 1.05)
  abline(h = 0, col = adjustcolor(tinta, 0.4)); abline(h = pretty(c(-1.9, 1.9)), col = adjustcolor(gris, 0.15))
  lines(0:4, c(0, a, 0), col = teal, lwd = 2); points(0:4, c(0, a, 0), pch = 21, bg = c(tinta, rep(ocre, 3), tinta), cex = c(1, rep(1.6, 3), 1))
}
modo(c(1, sqrt(2), 1), "Modo 1: ω ≈ 0,77")
modo(c(1, 0, -1), "Modo 2: ω ≈ 1,41")
modo(c(1, -sqrt(2), 1), "Modo 3: ω ≈ 1,85")
dev.off()

# ---------------------------------------------------------------------------
# Clase 9: descomposición en valores singulares
# ---------------------------------------------------------------------------
s3 <- sqrt(3); s5 <- sqrt(5); s6 <- sqrt(6); s10 <- sqrt(10)

# 9a. El círculo unidad y su imagen por A = [[3,0],[4,5]]
abrir("c09_circulo_elipse.png", w = 9.6, h = 4.5)
par(mfrow = c(1, 2), mar = c(4, 4, 2.6, 0.8))
A9 <- matrix(c(3, 0, 4, 5), 2, byrow = TRUE)
v19 <- c(1, 1) / s2; v29 <- c(1, -1) / s2; u19 <- c(1, 3) / s10; u29 <- c(3, -1) / s10
tt <- seq(0, 2 * pi, length.out = 400); Uc <- rbind(cos(tt), sin(tt)); Wc <- A9 %*% Uc
ejes2d(c(-1.6, 1.6), c(-1.6, 1.6))
title("Círculo unidad: |x| = 1", col.main = tinta, font.main = 1, cex.main = 1.1)
lines(Uc[1, ], Uc[2, ], col = azul, lwd = 2.4)
ang <- seq(0.3, 2 * pi, length.out = 9)[-9]
points(cos(ang), sin(ang), pch = 21, bg = adjustcolor(gris, 0.9), cex = 0.9)
flecha(0, 0, v19[1], v19[2], col = rojo, lwd = 2.8); flecha(0, 0, v29[1], v29[2], col = ocre, lwd = 2.8)
text(0.95, 1.0, expression(v[1]), col = rojo, cex = 1.15); text(1.02, -1.0, expression(v[2]), col = ocre, cex = 1.15)
ejes2d(c(-7.2, 7.2), c(-7.2, 7.2))
title("Imagen: elipse de semiejes 3√5 y √5", col.main = tinta, font.main = 1, cex.main = 1.1)
abline(0, 3, col = adjustcolor(rojo, 0.4), lty = 2); abline(0, -1 / 3, col = adjustcolor(ocre, 0.5), lty = 2)
lines(Wc[1, ], Wc[2, ], col = teal, lwd = 2.4)
Pa <- A9 %*% rbind(cos(ang), sin(ang)); points(Pa[1, ], Pa[2, ], pch = 21, bg = adjustcolor(gris, 0.9), cex = 0.9)
flecha(0, 0, 3 * s5 * u19[1], 3 * s5 * u19[2], col = rojo, lwd = 2.8); flecha(0, 0, s5 * u29[1], s5 * u29[2], col = ocre, lwd = 2.8)
text(-1.6, 6.7, expression(sigma[1] * u[1]), col = rojo, cex = 1.15); text(4.6, 0.5, expression(sigma[2] * u[2]), col = ocre, cex = 1.15)
dev.off()

# 9b. La esfera unidad y su imagen por A = [[2,3,3],[3,2,3],[2,2,0]]: un elipsoide
Ai9 <- matrix(c(2, 3, 3, 3, 2, 3, 2, 2, 0), 3, byrow = TRUE)
w19 <- c(1, 1, 1) / s3; w29 <- c(1, 1, -2) / s6; w39 <- c(1, -1, 0) / s2
x19 <- c(2, 2, 1) / 3; x29 <- c(-1, -1, 4) / (3 * s2); x39 <- c(-1, 1, 0) / s2
ESF_TH <- as.numeric(Sys.getenv("ESF_TH", "-35")); ESF_PH <- as.numeric(Sys.getenv("ESF_PH", "25"))
abrir("c09_esfera_elipsoide.png", w = 9.8, h = 4.8)
par(mfrow = c(1, 2))
esfera <- function(pm, M) {
  for (th in seq(0.4, pi - 0.4, length.out = 6)) {
    P <- t(M %*% sapply(seq(0, 2 * pi, length.out = 90), function(ph) c(sin(th) * cos(ph), sin(th) * sin(ph), cos(th))))
    lines(trans3d(P[, 1], P[, 2], P[, 3], pm), col = adjustcolor(teal, 0.5), lwd = 0.9)
  }
  for (ph in seq(0, pi, length.out = 9)[-9]) {
    P <- t(M %*% sapply(seq(0, 2 * pi, length.out = 90), function(th) c(sin(th) * cos(ph), sin(th) * sin(ph), cos(th))))
    lines(trans3d(P[, 1], P[, 2], P[, 3], pm), col = adjustcolor(teal, 0.5), lwd = 0.9)
  }
}
pm <- marco3d(c(-1.2, 1.2), c(-1.2, 1.2), c(-1.2, 1.2), theta = ESF_TH, phi = ESF_PH)
title("Esfera unidad y sus ejes v", col.main = tinta, font.main = 1, cex.main = 1.05, line = 0)
esfera(pm, diag(3))
flecha3(pm, c(0, 0, 0), w19, col = rojo, lwd = 3); flecha3(pm, c(0, 0, 0), w29, col = ocre, lwd = 3); flecha3(pm, c(0, 0, 0), w39, col = azul, lwd = 3)
text(p3(pm, 1.2 * w19[1], 1.2 * w19[2], 1.2 * w19[3]), expression(v[1]), col = rojo, cex = 1.1)
text(p3(pm, 1.25 * w29[1], 1.25 * w29[2], 1.25 * w29[3]), expression(v[2]), col = ocre, cex = 1.1)
text(p3(pm, 1.25 * w39[1], 1.25 * w39[2], 1.25 * w39[3]), expression(v[3]), col = azul, cex = 1.1)
pm <- marco3d(c(-5, 5), c(-5, 5), c(-5, 5), theta = ESF_TH, phi = ESF_PH)
title("Elipsoide imagen y sus semiejes", col.main = tinta, font.main = 1, cex.main = 1.05, line = 0)
esfera(pm, Ai9)
s1 <- 4 * s3; flecha3(pm, c(0, 0, 0), s1 * x19, col = rojo, lwd = 3); flecha3(pm, c(0, 0, 0), s3 * x29, col = ocre, lwd = 3); flecha3(pm, c(0, 0, 0), 1 * x39, col = azul, lwd = 3)
text(p3(pm, 1.12 * s1 * x19[1], 1.12 * s1 * x19[2], 1.12 * s1 * x19[3]), expression(sigma[1] * u[1]), col = rojo, cex = 1.05)
text(p3(pm, 1.9 * s3 * x29[1], 1.9 * s3 * x29[2], 1.9 * s3 * x29[3]), expression(sigma[2] * u[2]), col = ocre, cex = 1.05)
text(p3(pm, 3.2 * x39[1], 3.2 * x39[2], 3.2 * x39[3]), expression(sigma[3] * u[3]), col = azul, cex = 1.05)
dev.off()

# 9c. Pseudoinversa y norma mínima, M = [[1,1],[2,2]]
abrir("c09_pseudoinversa.png", w = 9.6, h = 4.5)
par(mfrow = c(1, 2), mar = c(4, 4, 2.8, 0.8))
ejes2d(c(-0.9, 1.7), c(-0.9, 1.7))
title("Mx = (1, 2): infinitas soluciones", col.main = tinta, font.main = 1, cex.main = 1.1)
abline(1, -1, col = teal, lwd = 2.6)
abline(0, 1, col = adjustcolor(rojo, 0.55), lty = 2, lwd = 1.6)
tc <- seq(0, 2 * pi, length.out = 300)
lines(sqrt(0.5) * cos(tc), sqrt(0.5) * sin(tc), col = adjustcolor(gris, 0.8), lty = 3)
points(c(1, 0, 1.3), c(0, 1, -0.3), pch = 21, bg = adjustcolor(azul, 0.9), cex = 1.2)
points(0.5, 0.5, pch = 21, bg = rojo, cex = 1.9)
text(0.6, 0.36, expression(x^"+"), col = rojo, cex = 1.15, adj = 0)
text(-0.85, 0.55, "soluciones: x₁ + x₂ = 1", col = teal, cex = 0.88, adj = 0)
text(1.2, 1.55, "espacio fila", col = rojo, cex = 0.88, adj = 1)
text(0.45, -0.62, "círculo de radio |x⁺|", col = gris, cex = 0.82, adj = 0)
ejes2d(c(-0.5, 0.9), c(-0.5, 0.9))
title("Mx = (1, 0): sin solución exacta", col.main = tinta, font.main = 1, cex.main = 1.1)
niv <- c(0.8, 1, 1.5, 2.5)
for (cc in niv) {
  ss <- (2 + c(-1, 1) * sqrt(4 - 20 * (1 - cc))) / 10
  for (s in unique(ss)) abline(s, -1, col = adjustcolor(if (cc == 0.8) teal else gris, 0.9), lwd = if (cc == 0.8) 2.8 else 1.2)
}
abline(0, 1, col = adjustcolor(rojo, 0.55), lty = 2, lwd = 1.6)
points(0.1, 0.1, pch = 21, bg = rojo, cex = 1.9)
text(0.16, 0.02, expression(x^"+"), col = rojo, cex = 1.15, adj = 0)
legend("topright", c("mínimos cuadrados: x₁ + x₂ = 1/5 (residuo² = 0,8)", "otras rectas: residuo² = 1; 1,5; 2,5", "espacio fila"), col = c(teal, gris, adjustcolor(rojo, 0.7)), lwd = c(2.8, 1.2, 1.6), lty = c(1, 1, 2), bty = "o", box.col = gris, bg = fondo, cex = 0.75)
dev.off()

# 9d. Aproximación de rango bajo de una imagen de 3x3 y los valores singulares
abrir("c09_imagen_rango.png", w = 10.4, h = 3.5)
par(mfrow = c(1, 4), mar = c(1.2, 1, 2.6, 1))
paleta <- colorRampPalette(c(tinta, "#6a7a80", "#f3ead5"))(101)
pixel <- function(vals, etiquetas, titulo) {
  plot(NA, xlim = c(0, 3), ylim = c(3, 0), asp = 1, axes = FALSE, xlab = "", ylab = "")
  title(titulo, col.main = tinta, font.main = 1, cex.main = 1.1, line = 0.6)
  for (i in 1:3) for (j in 1:3) {
    v <- vals[i, j]; col <- paleta[1 + round(100 * v / 3)]
    rect(j - 1, i - 1, j, i, col = col, border = fondo, lwd = 2)
    text(j - 0.5, i - 0.5, etiquetas[i, j], col = if (v < 1.6) "#fbf8f1" else tinta, cex = 1.15)
  }
}
P19 <- (4 / 3) * matrix(c(2, 2, 2, 2, 2, 2, 1, 1, 1), 3, byrow = TRUE)
P29 <- matrix(c(2.5, 2.5, 3, 2.5, 2.5, 3, 2, 2, 0), 3, byrow = TRUE)
pixel(Ai9, matrix(as.character(Ai9), 3), "Imagen original A (rango 3)")
pixel(P19, matrix(c("8/3", "8/3", "8/3", "8/3", "8/3", "8/3", "4/3", "4/3", "4/3"), 3, byrow = TRUE), expression("Rango 1: " * A[1] * " (92 %)"))
pixel(P29, matrix(c("5/2", "5/2", "3", "5/2", "5/2", "3", "2", "2", "0"), 3, byrow = TRUE), expression("Rango 2: " * A[2] * " (98 %)"))
par(mar = c(3.4, 4.2, 2.6, 0.8))
sg2 <- c(48, 3, 1)
bp <- barplot(sg2, names.arg = c("σ₁²", "σ₂²", "σ₃²"), col = c(rojo, ocre, azul), border = NA, ylim = c(0, 58), las = 1, ylab = "")
title("Valores singulares al cuadrado", col.main = tinta, font.main = 1, cex.main = 1.1, line = 0.6)
text(bp, sg2 + 3.2, c("48", "3", "1"), col = tinta, cex = 1)
text(bp[1], 40, "92,3 %", col = "#fbf8f1", cex = 0.9)
dev.off()

# 9e. Componentes principales: nube 2D (con la recta de MCO) y nube 3D
abrir("c09_pca.png", w = 10.4, h = 4.9)
par(mfrow = c(1, 2), mar = c(4, 4, 2.6, 0.8))
P2 <- rbind(c(2, 1), c(1, 2), c(1, -1)); X2 <- rbind(P2, -P2, c(0, 0))
ejes2d(c(-3.2, 3.2), c(-3.2, 3.2))
title("Nube 2D: σ₁² = 18, σ₂² = 6", col.main = tinta, font.main = 1, cex.main = 1.1)
abline(0, 1, col = adjustcolor(rojo, 0.45), lty = 2); abline(0, -1, col = adjustcolor(ocre, 0.55), lty = 2)
abline(0, 0.5, col = azul, lwd = 2.2)
for (i in 1:7) { d <- sum(X2[i, ] * c(1, 1)) / 2 * c(1, 1); segments(X2[i, 1], X2[i, 2], d[1], d[2], col = adjustcolor(gris, 0.8), lwd = 0.9) }
points(X2[, 1], X2[, 2], pch = 21, bg = teal, cex = 1.5)
flecha(0, 0, sqrt(3) * 1.5 / s2, sqrt(3) * 1.5 / s2, col = rojo, lwd = 3); flecha(0, 0, 1.5 / s2, -1.5 / s2, col = ocre, lwd = 3)
text(2.05, 2.55, "1.ª componente", col = rojo, cex = 0.85); text(1.95, -1.9, "2.ª", col = ocre, cex = 0.85)
text(3.0, 1.0, "MCO: y = x/2", col = azul, cex = 0.85)
legend("bottomleft", c("distancias perpendiculares a la 1.ª componente", "recta de mínimos cuadrados"), col = c(adjustcolor(gris, 0.9), azul), lwd = c(1, 2.2), bty = "o", box.col = gris, bg = fondo, cex = 0.72)
P3 <- rbind(c(1, -2, -2), c(2, 0, -2), c(2, 0, 0)); X3 <- rbind(P3, -P3, c(0, 0, 0))
pc1 <- c(2, -1, -2) / 3; pc2 <- c(2, 2, 1) / 3; pc3 <- c(1, -2, 2) / 3
PCA_TH <- as.numeric(Sys.getenv("PCA_TH", "-40")); PCA_PH <- as.numeric(Sys.getenv("PCA_PH", "22"))
pm <- marco3d(c(-3.4, 3.4), c(-3.4, 3.4), c(-3.4, 3.4), theta = PCA_TH, phi = PCA_PH)
title("Nube 3D: σ² = 32, 8, 2", col.main = tinta, font.main = 1, cex.main = 1.1, line = 0)
L <- 2.5; poli3(pm, rbind(L * pc1 + L * pc2, L * pc1 - L * pc2, -L * pc1 - L * pc2, -L * pc1 + L * pc2), col = adjustcolor(teal, 0.13), border = adjustcolor(teal, 0.5))
for (i in 1:7) { x <- X3[i, ]; f <- x - sum(x * pc3) * pc3; seg3(pm, x, f, col = adjustcolor(gris, 0.9), lwd = 0.9) }
PP <- trans3d(X3[, 1], X3[, 2], X3[, 3], pm); points(PP, pch = 21, bg = teal, cex = 1.5)
flecha3(pm, c(0, 0, 0), 3.4 * pc1, col = rojo, lwd = 3); flecha3(pm, c(0, 0, 0), 1.7 * pc2, col = ocre, lwd = 3); flecha3(pm, c(0, 0, 0), 1.7 * pc3, col = azul, lwd = 3)
text(p3(pm, 3.7 * pc1[1], 3.7 * pc1[2], 3.7 * pc1[3]), "1.ª", col = rojo, cex = 0.95)
text(p3(pm, 2.0 * pc2[1], 2.0 * pc2[2], 2.0 * pc2[3]), "2.ª", col = ocre, cex = 0.95)
text(p3(pm, 2.0 * pc3[1], 2.0 * pc3[2], 2.0 * pc3[3] + 0.15), "3.ª", col = azul, cex = 0.95)
dev.off()
