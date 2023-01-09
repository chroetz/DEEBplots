#' @export
plotDistances <- function(truth, esti = NULL, smooth = NULL, obs = NULL, timeRange=NULL, title = "") {

  x <- prepareTrajs(truth, esti, smooth, obs, timeRange)
  if (is.null(x)) {
    warning("Returning empty plot.")
    return(ggplot())
  }
  truth <- x$truth
  esti <- x$esti
  smooth <- x$smooth
  obs <- x$obs

  # TODO: remove code duplications
  data <-
    bind_rows(
      tibble(
        trajId = truth$trajId,
        time = truth$time,
        distance =
          sqrt(rowSums((interpolateTrajs(obs, truth$time)$state - truth$state)^2)),
        kind = "obs"),
      tibble(
        trajId = truth$trajId,
        time = truth$time,
        distance =
          sqrt(rowSums((interpolateTrajs(esti, truth$time)$state - truth$state)^2)),
        kind = "esti"),
      tibble(
        trajId = truth$trajId,
        time = truth$time,
        distance =
          sqrt(rowSums((interpolateTrajs(smooth, truth$time)$state - truth$state)^2)),
        kind = "smooth")) |>
    mutate(kind = factor(.data$kind, c("truth", "esti", "smooth", "obs"))) |>
    arrange(.data$kind)

  plt <-
    ggplot(data, aes(
      x = .data$time,
      y = .data$distance,
      color = .data$kind,
      group = paste0(.data$trajId, .data$kind)
    )) +
    geom_line() +
    baseTheme() +
    ggtitle(title)
  return(plt)
}
