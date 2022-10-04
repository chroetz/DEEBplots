#' @export
plotStateSpace <- function(truth, esti = NULL, obs = NULL, timeRange = NULL, title = "", nBasePoints=1e3) {

  if (is.null(truth)) {
    warning("truth is NULL. Returning empty plot.")
    return(ggplot())
  }
  if (length(timeRange) == 0) {
    timeRange <- range(truth$time)
  }
  if (is.null(obs)) {
    obs <- truth[0,]
  } else {
    obs <- filter(obs, between(.data$time, timeRange[1], timeRange[2]))
  }

  # TODO: dont interpolate / make nBasePoints an opts that is set individually per model
  #times <- seq(timeRange[1], timeRange[2], length.out = nBasePoints)
  #truth <- interpolateTrajs(truth, times)
  truth <- truth |> filter(between(.data$time, timeRange[1], timeRange[2]))
  if (!is.null(esti)) {
    #esti <- interpolateTrajs(esti, times)
    esti |> filter(between(.data$time, timeRange[1], timeRange[2]))
  } else {
    esti <- truth[0,]
  }

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
    esti |> mutate(kind = "esti", state2D = projection2D$project(esti$state))
  ) |>
    mutate(kind = factor(.data$kind, c("truth", "esti", "obs"))) |>
    arrange(.data$kind)
  rangeData <-
    bind_rows(
      data |> filter(.data$time == max(.data$time)) |> mutate(range = "max"),
      data |> filter(.data$time == min(.data$time)) |> mutate(range = "min")
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
        group = paste0(.data$trajId, .data$kind),
        color = .data$kind
      )) +
    geom_path() +
    geom_point(data = rangeData, mapping = aes(shape = .data$range), size = 3) +
    geom_point(
      data = obs,
      mapping = aes(group = NULL, color = NULL),
      alpha = 0.4,
      size = 0.2
    ) +
    xlab(NULL) + ylab(NULL) +
    theme(legend.position = "none") +
    coord_fixed(ratio = 1) +
    ggtitle(title)

  return(plt)
}


