
#' Visualizes the mean delay for airports with positioning on longitude, latitude
#' @export
visualize_airport_delays <- function() {
  devtools::install_github("hadley/nycflights13")

  flights <- nycflights13::flights
  airports <- nycflights13::airports

  df <- flights %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(mean_delay = mean(df$arr_delay, na.rm=TRUE)) %>%
    dplyr::left_join(.,airports, by = c("dest" = "faa"))

  ggplot2::ggplot(df, aes(lon, lat), show.legend=FALSE) +
    ggplot2::geom_point(aes(size=mean_delay, alpha=mean_delay, color=mean_delay), show.legend=FALSE)
}
