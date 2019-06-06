#' Position scales for mass and weight data
#'
#' This scale allow for easy labeling of data in weight measurements. The scale
#' has two major arguments, `base` and `system`. `system` determine the
#' measurement system, currently "imperial" and "metric" is implemented. For
#' `base` please refer to details.
#'
#' For "metric", base must be one of
#' \itemize{
#' \item gigatonne
#' \item megatonne
#' \item tonne
#' \item kilogram
#' \item gram
#' \item milligram
#' \item microgram
#' \item nanogram
#' \item picogram
#' }
#'
#' And for "imperial", base must be one of
#' \itemize{
#' \item ton
#' \item pound
#' }
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#' @param base Character, dertermine what the base unit is, defaults to "gram".
#' @param system Character, dertermine what measurement system to use is,
#'     defaults to "metric".
#' @param ignore Character, which units to ignore.
#' @family position scales
#' @examples
#' library(ggplot2)
#' set.seed(1)
#' df <- data.frame(
#'   x = 1:100,
#'   y = cumsum(rnorm(100)) * 100
#' )
#'
#' base <- ggplot(df, aes(x, y)) +
#'   geom_line()
#'
#' # Will default to grams
#' base +
#'   scale_y_mass()
#'
#' # Different baseline unit can be set with `base` argument
#' base +
#'   scale_y_mass(base = "kilogram")
#'
#' # Imperial system also implemented
#' base +
#'   scale_y_mass(base = "ton", system = "imperial")
#'
#' # Transformation between systems can be done on the fly. First element
#' # of system denotes the system you are moving from, and the second element
#' # denotes the system you are moving to.
#' base +
#'   scale_y_mass(base = "kilogram", system = c("metric", "imperial"))
#' @name scale_mass
#' @aliases NULL
NULL

#' @rdname scale_mass
#' @importFrom ggplot2 waiver
#' @importFrom scales censor
#' @export
scale_x_mass <- function (name = waiver(), breaks = waiver(),
                          minor_breaks = waiver(), labels = waiver(),
                          limits = NULL, expand = waiver(), oob = censor,
                          na.value = NA_real_, position = "bottom",
                          sec.axis = waiver(), base = "gram",
                          system = "metric", ignore = NULL) {

  label_fun <- wrapper_scaler(scaler = mass_scaler, base = base, system = system,
                              ignore = ignore)

  ggplot2::scale_x_continuous(
    name = name, breaks = breaks, labels = label_fun,
    minor_breaks = minor_breaks, limits = limits, expand = expand,
    oob = oob, na.value = na.value, position = position,
    trans = "identity", sec.axis = sec.axis
  )
}

#' @rdname scale_mass
#' @importFrom ggplot2 waiver
#' @importFrom scales censor
#' @export
scale_y_mass <- function (name = waiver(), breaks = waiver(),
                          minor_breaks = waiver(), labels = waiver(),
                          limits = NULL, expand = waiver(), oob = censor,
                          na.value = NA_real_, position = "left",
                          sec.axis = waiver(), base = "gram",
                          system = "metric", ignore = NULL) {

  label_fun <- wrapper_scaler(scaler = mass_scaler, base = base, system = system,
                              ignore = ignore)

  ggplot2::scale_y_continuous(
    name = name, breaks = breaks, labels = label_fun,
    minor_breaks = minor_breaks, limits = limits, expand = expand,
    oob = oob, na.value = na.value, position = position,
    trans = "identity", sec.axis = sec.axis
  )
}

#' Internal Functions
#'
#' These are not to be used directly by the users.
#' @export
#' @keywords internal
mass_scaler <- function(x, base, system, ignore) {
  if (is.na(x)) {
    return(NA_character_)
  }
  if (x == 0) {
    return("0")
  }
  convert_scale(x, base, system, mass_table, mass_convertion, ignore)
}

#' @importFrom tibble tribble
mass_table <- list(
  metric = tibble::tribble(
    ~name, ~unit, ~multiple,
    "gigatonne", "Gt",      1000000000000000,
    "megatonne", "Mt",      1000000000000,
    "tonne",     "t",       1000000,
    "kilogram",  "kg",      1000,
    "gram",      "g",       1,
    "milligram", "mg",      0.001,
    "microgram", "\u00b5g", 0.000001,
    "nanogram",  "ng",      0.000000001,
    "picogram",  "pg",      0.000000000001
  ),
  imperial = tibble::tribble(
    ~name, ~unit, ~multiple,
    "ton", "t", 2240,
    "pound", "lb", 1
  )
)

mass_convertion <- list(
  metric = 1,
  imperial = 453.59237
)
