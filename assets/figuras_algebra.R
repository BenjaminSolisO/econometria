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

abrir <- function(archivo, w = 7, h = 4.6) {
  png(file.path("assets/fig/am", archivo), width = w, height = h, units = "in",
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

# 1e. Circuito de dos mallas
abrir("c01_circuito.png", w = 7, h = 3.8)
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(-1.2, 9.6), ylim = c(-0.7, 3.8), axes = FALSE, xlab = "", ylab = "", asp = 1)
resistor <- function(p, q, etiqueta, lado = c(0, 0.45)) {
  d <- q - p; L <- sqrt(sum(d^2)); u <- d / L; n <- c(-u[2], u[1])
  a <- p + u * (L * 0.3); z <- p + u * (L * 0.7)
  segments(p[1], p[2], a[1], a[2], lwd = 2, col = tinta)
  segments(z[1], z[2], q[1], q[2], lwd = 2, col = tinta)
  k <- 6; pts <- t(sapply(0:k, function(j) a + (z - a) * j / k +
                          n * 0.22 * c(0, 1, -1, 1, -1, 1, 0)[j + 1]))
  lines(pts, lwd = 2, col = tinta)
  m <- (p + q) / 2 + lado
  text(m[1], m[2], etiqueta, col = teal, cex = 1.05)
}
cable <- function(p, q) segments(p[1], p[2], q[1], q[2], lwd = 2, col = tinta)
cable(c(0, 0), c(8, 0)); cable(c(4, 3), c(8, 3))
resistor(c(0, 3), c(4, 3), expression(R[1] == 1 * Omega))
resistor(c(4, 3), c(4, 0), expression(R[2] == 2 * Omega), lado = c(0.85, 0.9))
resistor(c(8, 3), c(8, 0), expression(R[3] == 2 * Omega), lado = c(0.95, 0))
# batería en la rama izquierda
cable(c(0, 0), c(0, 1.3)); cable(c(0, 1.7), c(0, 3))
segments(-0.45, 1.7, 0.45, 1.7, lwd = 3, col = tinta)
segments(-0.22, 1.3, 0.22, 1.3, lwd = 5, col = tinta)
text(-0.95, 1.5, "8 V", col = rojo, cex = 1.1)
# corrientes de malla
for (cx in c(2, 6.3)) {
  th <- seq(0.35, 2 * pi - 0.35, length.out = 60)
  lines(cx + 0.62 * cos(-th + pi / 2), 1.5 + 0.62 * sin(-th + pi / 2), col = ocre, lwd = 2)
  arrows(cx + 0.62 * cos(-th[59] + pi / 2), 1.5 + 0.62 * sin(-th[59] + pi / 2),
         cx + 0.62 * cos(-th[60] + pi / 2), 1.5 + 0.62 * sin(-th[60] + pi / 2),
         col = ocre, lwd = 2, length = 0.1)
}
text(2, 1.5, expression(i[1]), col = ocre, cex = 1.3)
text(6.3, 1.5, expression(i[2]), col = ocre, cex = 1.3)
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
