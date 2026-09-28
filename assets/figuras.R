# Genera las figuras estáticas de Econometría 1 (PNG, sirven para HTML y PDF).
# Uso: Rscript assets/figuras.R  (desde la raíz del proyecto)

tinta  <- "#1f2430"
teal   <- "#0f5e6b"
rojo   <- "#b5452f"
ocre   <- "#b8892b"
gris   <- "#8a8577"
fondo  <- "#fbf8f1"

abrir <- function(archivo, w = 7, h = 4.6) {
  png(file.path("assets/fig", archivo), width = w, height = h, units = "in",
      res = 200, bg = fondo)
  par(family = "serif", mar = c(4, 4, 1.5, 1), col.axis = tinta, col.lab = tinta,
      fg = gris)
}

# ---------------------------------------------------------------------------
# Clase 2: CEF no lineal vs mejor predictor lineal
abrir("cef_vs_blp.png")
set.seed(7)
x <- runif(400, -1, 2)
y <- x^2 + rnorm(400, sd = 0.35)
plot(x, y, pch = 16, cex = 0.55, col = adjustcolor(gris, 0.55),
     xlab = "x", ylab = "y", las = 1)
curve(x^2, add = TRUE, lwd = 3, col = teal)
# BLP poblacional para X ~ U(-1, 2): E[X] = 1/2, Var(X) = 3/4, E[X^2] = 1,
# Cov(X, X^2) = E[X^3] - E[X]E[X^2] = 5/4 - 1/2 = 3/4  =>  pendiente 1, intercepto 1/2
abline(a = 1/2, b = 1, lwd = 3, col = rojo, lty = 2)
legend("topleft", bty = "n", text.col = tinta, lwd = 3, lty = c(1, 2), col = c(teal, rojo),
       legend = c("CEF: m(x) = x²", "Mejor predictor lineal: 1/2 + x"))
dev.off()

# ---------------------------------------------------------------------------
# Clase 3: geometría de MCO (proyección de y sobre el espacio columna de X)
abrir("proyeccion.png", w = 7, h = 5)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(NA, xlim = c(0, 10), ylim = c(0, 7.2), axes = FALSE, xlab = "", ylab = "", asp = 1)
# plano col(X) en perspectiva
polygon(c(0.4, 7.6, 9.6, 2.4), c(0.6, 0.6, 3.2, 3.2),
        col = adjustcolor(teal, 0.10), border = teal, lwd = 1.5)
text(8.9, 1.0, "col(X)", col = teal, cex = 1.3, font = 3)
O  <- c(2.2, 1.3)
yh <- c(6.6, 2.4)
yv <- c(6.6, 6.6)
arrows(O[1], O[2], yv[1], yv[2], lwd = 3, col = tinta, length = 0.14)
arrows(O[1], O[2], yh[1], yh[2], lwd = 3, col = teal, length = 0.14)
arrows(yh[1], yh[2], yv[1], yv[2], lwd = 3, col = rojo, length = 0.14)
# ángulo recto
segments(yh[1] - 0.35, yh[2] - 0.08, yh[1] - 0.35, yh[2] + 0.30, col = tinta)
segments(yh[1] - 0.35, yh[2] + 0.30, yh[1], yh[2] + 0.38, col = tinta)
text(4.1, 4.4, "y", cex = 1.5, font = 4, col = tinta)
text(4.4, 1.55, expression(hat(y) == Py), cex = 1.3, col = teal)
text(7.55, 4.5, expression(hat(u) == My), cex = 1.3, col = rojo)
points(O[1], O[2], pch = 16, col = tinta)
text(O[1] - 0.25, O[2] - 0.25, "0", col = tinta)
dev.off()

# ---------------------------------------------------------------------------
# Clase 6: teorema central del límite con datos exponenciales
abrir("tcl.png")
set.seed(11)
R <- 20000
ns <- c(1, 3, 10, 50)
cols <- c(ocre, rojo, gris, teal)
plot(NA, xlim = c(-3.5, 4), ylim = c(0, 1.05), las = 1,
     xlab = expression(sqrt(n) * (bar(X)[n] - mu) / sigma), ylab = "densidad")
for (j in seq_along(ns)) {
  n <- ns[j]
  z <- sqrt(n) * (colMeans(matrix(rexp(n * R), n)) - 1)
  d <- density(z, from = -3.5, to = 4, adjust = 1.2)
  lines(d, lwd = 2.4, col = cols[j])
}
curve(dnorm(x), add = TRUE, lwd = 2, lty = 2, col = tinta)
legend("topright", bty = "n", text.col = tinta, lwd = c(2.4, 2.4, 2.4, 2.4, 2),
       lty = c(1, 1, 1, 1, 2), col = c(cols, tinta),
       legend = c("n = 1", "n = 3", "n = 10", "n = 50", "N(0, 1)"))
dev.off()

# ---------------------------------------------------------------------------
# Clase 8: sesgo de atenuación por error de medida
abrir("atenuacion.png")
set.seed(3)
n <- 300
xs <- rnorm(n)
y  <- 1 + 2 * xs + rnorm(n, sd = 0.8)
xo <- xs + rnorm(n, sd = 1)       # lambda = 1 / (1 + 1) = 1/2
plot(xo, y, pch = 16, cex = 0.55, col = adjustcolor(rojo, 0.45), las = 1,
     xlab = "regresor", ylab = "y")
points(xs, y, pch = 16, cex = 0.55, col = adjustcolor(teal, 0.45))
abline(lm(y ~ xs), lwd = 3, col = teal)
abline(lm(y ~ xo), lwd = 3, col = rojo, lty = 2)
legend("topleft", bty = "n", text.col = tinta, pch = 16, lwd = 3, lty = c(1, 2), col = c(teal, rojo),
       legend = c("x* verdadero (pendiente ≈ 2)", "x = x* + v observado (pendiente ≈ 1)"))
dev.off()

# ---------------------------------------------------------------------------
# Clase 10: instrumentos débiles — distribución de MC2E vs MCO
abrir("iv_debil.png")
set.seed(21)
sim <- function(pi, R = 4000, n = 200) {
  out <- matrix(NA, R, 2)
  for (r in 1:R) {
    z <- rnorm(n); v <- rnorm(n)
    u <- 0.8 * v + sqrt(1 - 0.64) * rnorm(n)
    x <- pi * z + v
    y <- 1 * x + u
    out[r, ] <- c(sum(z * y) / sum(z * x), sum(x * y) / sum(x * x))
  }
  out
}
fuerte <- sim(0.5); debil <- sim(0.05)
plot(NA, xlim = c(-1, 3), ylim = c(0, 7.5), las = 1, xlab = "estimación de β (verdadero = 1)",
     ylab = "densidad")
lines(density(fuerte[, 1], from = -1, to = 3), lwd = 2.6, col = teal)
lines(density(debil[, 1][abs(debil[, 1]) < 10], from = -1, to = 3), lwd = 2.6, col = rojo)
lines(density(fuerte[, 2], from = -1, to = 3), lwd = 2.2, col = gris, lty = 2)
abline(v = 1, col = tinta, lty = 3)
legend("topleft", bty = "n", text.col = tinta, lwd = c(2.6, 2.6, 2.2), lty = c(1, 1, 2),
       col = c(teal, rojo, gris),
       legend = c("VI, instrumento fuerte (π = 0,5)", "VI, instrumento débil (π = 0,05)", "MCO"))
dev.off()
