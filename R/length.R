#' Position scales for length or distance data
#'
#' This scale allow for easy labeling of data in distance measurements.
#' The scale has two major arguments, `base` and `system`. `system` determine
#' the measurement system, currently "imperial" and "metric" is implemented.
#' For `base` please refer to details.
#'
#' For "metric", base must be one of
#' \itemize{
#' \item kilometre
#' \item metre
#' \item centimetre
#' \item millimetre
#' \item micrometre
#' \item nanometre
#' }
#'
#' And for "imperial", base must be one of
#' \itemize{
#' \item mile
#' \item yard
#' \item foot
#' \item inch
#' }
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#' @param base Character, dertermine what the base unit is, defaults to "metre".
#' @param system Character, dertermine what measurement system to use is,
#'     defaults to "metric".
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
#'   scale_y_length()
#'
#' # Different baseline unit can be set with `base` argument
#' base +
#'   scale_y_length(base = "millimetre")
#'
#' # Imperial system also implemented
#' base +
#'   scale_y_length(base = "inch", system = "imperial")
#'
#' # Transformation between systems can be done on the fly. First element
#' # of system denotes the system you are moving from, and the second element
#' # denotes the system you are moving to.
#' base +
#'   scale_y_length(base = "millimetre", system = c("metric", "imperial"))
#' @name scale_length
#' @aliases NULL
NULL

#' @rdname scale_length
#' @importFrom ggplot2 waiver
#' @importFrom scales censor
#' @export
scale_x_length <- function (name = waiver(), breaks = waiver(),
                          minor_breaks = waiver(), labels = waiver(),
                          limits = NULL, expand = waiver(), oob = censor,
                          na.value = NA_real_, position = "bottom",
                          sec.axis = waiver(), base = "metre",
                          system = "metric") {

  label_fun <- wrapper_scaler(scaler = length_scaler, base = base, system = system)

  ggplot2::scale_x_continuous(
    name = name, breaks = breaks, labels = label_fun,
    minor_breaks = minor_breaks, limits = limits, expand = expand,
    oob = oob, na.value = na.value, position = position,
    trans = "identity", sec.axis = sec.axis
  )
}

#' @rdname scale_length
#' @importFrom ggplot2 waiver
#' @importFrom scales censor
#' @export
scale_y_length <- function (name = waiver(), breaks = waiver(),
                          minor_breaks = waiver(), labels = waiver(),
                          limits = NULL, expand = waiver(), oob = censor,
                          na.value = NA_real_, position = "left",
                          sec.axis = waiver(), base = "metre",
                          system = "metric") {

  label_fun <- wrapper_scaler(scaler = length_scaler, base = base, system = system)

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
length_scaler <- function(x, base, system) {
  if (is.na(x)) {
    return(NA_character_)
  }
  if (x == 0) {
    return("0")
  }
  convert_scale(x, base, system, length_table, length_convertion)
}

#' @importFrom tibble tribble
length_table <- list(
  metric = tibble::tribble(
    ~name, ~unit, ~multiple,
    "kilometre",  "km",     1000,
    "metre",      "m",      1,
    "centimetre", "cm",     10^(-2),
    "millimetre", "mm",     10^(-3),
    "micrometre", "u00b5m", 10^(-6),
    "nanometre",  "nm",     10^(-9)
  ),
  imperial = tibble::tribble(
    ~name, ~unit, ~multiple,
    "mile", "mi", 5280,
    "yard", "yd", 3,
    "foot", "ft", 1,
    "inch", "in", 1/12
  )
)

length_convertion <- list(
  metric = 1,
  imperial = 0.3048
)
