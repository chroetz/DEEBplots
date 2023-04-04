#' @export
plotStateSpace <- function(truth, esti = NULL, smooth = NULL, obs = NULL, timeRange = NULL, title = "", obsAlpha = 0.4, obsSize = 0.2) {

  x <- prepareTrajs(truth, esti, smooth, obs, timeRange)
  if (is.null(x)) {
    warning("Returning empty plot.")
    return(ggplot())
  }
  truth <- x$truth
  esti <- x$esti
  smooth <- x$smooth
  obs <- x$obs

  d <- getDim(truth)
  if (d == 2) {
    projection2D <- getIdentityProjection()
  } else if (d > 2) {
    projection2D <- calculateProjection(truth$state, dim = 2)
  } else {
    stop("d invalid: ", d)
  }
  data <- bind_rows(
    truth |> mutate(kind = "truth", state2D = projection2D$project(truth$state)),
    esti |> mutate(kind = "esti", state2D = projection2D$project(esti$state)),
    smooth |> mutate(kind = "smooth", state2D = projection2D$project(smooth$state))
  ) |>
    mutate(kind = factor(.data$kind, c("truth", "esti", "smooth", "obs"))) |>
    arrange(.data$kind)
  rangeData <-
    bind_rows(
      data |> group_by(.data$kind) |> filter(.data$time == max(.data$time, na.rm=TRUE)) |> mutate(range = "max"),
      data |> group_by(.data$kind) |> filter(.data$time == min(.data$time, na.rm=TRUE)) |> mutate(range = "min")
    )
  obs <-
    obs |>
    mutate(kind = "obs", state2D = projection2D$project(obs$state))

  plt <-
    ggplot(
      data,
      aes(
        x = .data$state2D[, 1],
        y = .data$state2D[, 2],
        color = .data$kind,
        group = paste0(.data$trajId, .data$kind)
      )) +
    geom_path() +
    geom_point(data = rangeData, mapping = aes(shape = .data$range), size = 3, color = "black") +
    geom_point(data = rangeData, mapping = aes(shape = .data$range), size = 2) +
    geom_point(
      data = obs,
      mapping = aes(group = NULL, color = NULL),
      alpha = obsAlpha,
      size = obsSize,
      color = kindColors["obs"]
    ) +
    baseTheme() +
    coord_fixed(ratio = 1) +
    ggtitle(title)

  return(plt)
}


