library(ggplot2)

# Scale_x_length

labels_x <- function(g) {
  ggplot_build(g)$layout$coord$labels(ggplot_build(g)$layout$panel_params)[[1]]$x.labels
}

labels_y <- function(g) {
  ggplot_build(g)$layout$coord$labels(ggplot_build(g)$layout$panel_params)[[1]]$y.labels
}

test_that("scale_x_length works", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(x, 1)) +
    scale_x_length()

  expect_equal(
    labels_x(ggg),
    c("0", "500m", "1km", "1.5km")
  )
})

test_that("base argument works in scale_x_length", {
  data <- data.frame(x = c(0, 1500))

  ggg_kg <- ggplot(data, aes(x, 1)) +
    scale_x_length(base = "kilometre")
  ggg_mg <- ggplot(data, aes(x, 1)) +
    scale_x_length(base = "millimetre")

  expect_equal(
    labels_x(ggg_kg),
    c("0", "500km", "1000km", "1500km")
  )
  expect_equal(
    labels_x(ggg_mg),
    c("0", "50cm", "1m", "1.5m")
  )
})

test_that("system argument works in scale_x_length", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(x, 1)) +
    scale_x_length(system = "imperial", base = "foot")

  expect_equal(
    labels_x(ggg),
    c("0", "166.67yd", "333.33yd", "500yd")
  )
})

test_that("convertion is done right in scale_x_length", {
  data <- data.frame(x = c(0, 1500))

  ggg_1 <- ggplot(data, aes(x, 1)) +
    scale_x_length(system = c("metric", "imperial"))
  ggg_2 <- ggplot(data, aes(x, 1)) +
    scale_x_length(base = "kilometre", system = c("metric", "imperial"))

  expect_equal(
    labels_x(ggg_1),
    c("0", "546.81yd", "1093.61yd", "1640.42yd")
  )
  expect_equal(
    labels_x(ggg_2),
    c("0", "310.69mi", "621.37mi", "932.06mi")
  )
})

# Scale_y_length

test_that("scale_y_length works", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(1, x)) +
    scale_y_length()

  expect_equal(
    labels_y(ggg),
    c("0", "500m", "1km", "1.5km")
  )
})

test_that("base argument works in scale_y_length", {
  data <- data.frame(x = c(0, 1500))

  ggg_kg <- ggplot(data, aes(1, x)) +
    scale_y_length(base = "kilometre")
  ggg_mg <- ggplot(data, aes(1, x)) +
    scale_y_length(base = "millimetre")

  expect_equal(
    labels_y(ggg_kg),
    c("0", "500km", "1000km", "1500km")
  )
  expect_equal(
    labels_y(ggg_mg),
    c("0", "50cm", "1m", "1.5m")
  )
})

test_that("system argument works in scale_y_length", {
  data <- data.frame(x = c(0, 1500))

  ggg <- ggplot(data, aes(1, x)) +
    scale_y_length(system = "imperial", base = "yard")

  expect_equal(
    labels_y(ggg),
    c("0", "500yd", "1000yd", "1500yd")
  )
})

test_that("convertion is done right in scale_y_length", {
  data <- data.frame(x = c(0, 1500))

  ggg_1 <- ggplot(data, aes(1, x)) +
    scale_y_length(system = c("metric", "imperial"))
  ggg_2 <- ggplot(data, aes(1, x)) +
    scale_y_length(base = "kilometre", system = c("metric", "imperial"))

  expect_equal(
    labels_y(ggg_1),
    c("0", "546.81yd", "1093.61yd", "1640.42yd")
  )
  expect_equal(
    labels_y(ggg_2),
    c("0", "310.69mi", "621.37mi", "932.06mi")
  )
})
