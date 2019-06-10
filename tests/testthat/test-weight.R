library(ggplot2)

# Scale_x_mass

labels_x <- function(g) {
  ggplot_build(g)$layout$coord$labels(ggplot_build(g)$layout$panel_params)[[1]]$x.labels
}

labels_y <- function(g) {
  ggplot_build(g)$layout$coord$labels(ggplot_build(g)$layout$panel_params)[[1]]$y.labels
}

test_that("scale_x_mass works", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(x, 1)) +
    scale_x_mass()

  expect_equal(
    labels_x(ggg),
    c("0", "500g", "1kg", "1.5kg")
  )
})

test_that("base argument works in scale_x_mass", {
  data <- data.frame(x = c(0, 1500))

  ggg_kg <- ggplot(data, aes(x, 1)) +
    scale_x_mass(base = "kilogram")
  ggg_mg <- ggplot(data, aes(x, 1)) +
    scale_x_mass(base = "milligram")

  expect_equal(
    labels_x(ggg_kg),
    c("0", "500kg", "1t", "1.5t")
  )
  expect_equal(
    labels_x(ggg_mg),
    c("0", "500mg", "1g", "1.5g")
  )
})

test_that("system argument works in scale_x_mass", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(x, 1)) +
    scale_x_mass(system = "imperial", base = "pound")

  expect_equal(
    labels_x(ggg),
    c("0", "500lb", "1000lb", "1500lb")
  )
})

test_that("convertion is done right in scale_x_mass", {
  data <- data.frame(x = c(0, 1500))

  ggg_1 <- ggplot(data, aes(x, 1)) +
    scale_x_mass(system = c("metric", "imperial"))
  ggg_2 <- ggplot(data, aes(x, 1)) +
    scale_x_mass(base = "kilogram", system = c("metric", "imperial"))

  expect_equal(
    labels_x(ggg_1),
    c("0", "1.1lb", "2.2lb", "3.31lb")
  )
  expect_equal(
    labels_x(ggg_2),
    c("0", "1102.31lb", "2204.62lb", "1.48t")
  )
})

# Scale_y_mass

test_that("scale_y_mass works", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(1, x)) +
    scale_y_mass()

  expect_equal(
    labels_y(ggg),
    c("0", "500g", "1kg", "1.5kg")
  )
})

test_that("base argument works in scale_y_mass", {
  data <- data.frame(x = c(0, 1500))

  ggg_kg <- ggplot(data, aes(1, x)) +
    scale_y_mass(base = "kilogram")
  ggg_mg <- ggplot(data, aes(1, x)) +
    scale_y_mass(base = "milligram")

  expect_equal(
    labels_y(ggg_kg),
    c("0", "500kg", "1t", "1.5t")
  )
  expect_equal(
    labels_y(ggg_mg),
    c("0", "500mg", "1g", "1.5g")
  )
})

test_that("system argument works in scale_y_mass", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(1, x)) +
    scale_y_mass(system = "imperial", base = "pound")

  expect_equal(
    labels_y(ggg),
    c("0", "500lb", "1000lb", "1500lb")
  )
})

test_that("convertion is done right in scale_y_mass", {
  data <- data.frame(x = c(0, 1500))

  ggg_1 <- ggplot(data, aes(1, x)) +
    scale_y_mass(system = c("metric", "imperial"))
  ggg_2 <- ggplot(data, aes(1, x)) +
    scale_y_mass(base = "kilogram", system = c("metric", "imperial"))

  expect_equal(
    labels_y(ggg_1),
    c("0", "1.1lb", "2.2lb", "3.31lb")
  )
  expect_equal(
    labels_y(ggg_2),
    c("0", "1102.31lb", "2204.62lb", "1.48t")
  )
})
