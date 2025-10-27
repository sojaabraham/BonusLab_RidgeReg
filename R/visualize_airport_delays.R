#' Visualize Mean Airport Delays
#'
#' This function produces a scatter plot that visualizes the mean departure delay
#' for flights departing from New York City airports. The plot is based on
#' aggregated flight delay data from the \code{flights} dataset in the
#' \code{nycflights13} package and airport information from the \code{airports}
#' data set in the same package.
#'
#' The function uses \pkg{dplyr} for data transformation and \pkg{ggplot2} for
#' visualization. Airports are plotted according to their longitude and latitude,
#' with both color and point size representing the mean departure delay in minutes.
#'
#' This function has no arguments and returns a ggplot object.
#'
#' @return A \code{ggplot} scatter plot representing mean departure delays.
#' @examples
#' visualize_airport_delays()
#'
#' @import dplyr
#' @import ggplot2
#' @import nycflights13
#'
#' @export
visualize_airport_delays <- function(){
  delay_data <- nycflights13::flights %>%
    dplyr::group_by(.data$origin) %>%
    dplyr::summarise(mean_delay = mean(.data$dep_delay, na.rm = TRUE)) %>%
    dplyr::inner_join(nycflights13::airports, by = c("origin" = "faa"))

  ggplot2::ggplot(delay_data, ggplot2::aes(
    x = .data$lon,
    y = .data$lat,
    size = .data$mean_delay,
    color = .data$mean_delay
  )) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_viridis_c(option = "plasma") +
    ggplot2::labs(
      title = "Mean Departure Delay by NYC Airports",
      x = "Longitude",
      y = "Latitude",
      size = "Mean Delay (min)",
      color = "Mean Delay (min)"
    ) +
    ggplot2::theme_minimal()
}
