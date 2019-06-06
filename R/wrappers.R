#' Internal Functions
#'
#' These are not to be used directly by the users.
#' @export
#' @keywords internal
wrapper_scaler <- function(x, scaler, base, system) {
  function(x) vapply(x, scaler, base = base, system = system,
                     FUN.VALUE = character(1))
}

convert_scale <- function(x, base, system, table, conversion) {
  system <- vapply(system, match.arg, choices = names(table), FUN.VALUE = character(1))
  data_before <- table[[system[1]]]
  x <- x * data_before$multiple[match.arg(base, data_before$name) == data_before$name]
  if (length(system) == 2) {
    x <- x * conversion[[system[1]]] / conversion[[system[2]]]
  }
  data2 <- table[[ifelse(length(system) == 2, system[2], system)]]
  index <- which.min(x < data2$multiple)
  paste0(round(x / data2$multiple[index], 2), data2$unit[index])
}
