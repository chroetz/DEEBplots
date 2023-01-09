#' @export
plotStateSpace <- function(truth, esti = NULL, smooth = NULL, obs = NULL, timeRange = NULL, title = "") {

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

  truth <- truth |> filter(between(.data$time, timeRange[1], timeRange[2]))
  if (!is.null(esti)) {
    esti |> filter(between(.data$time, timeRange[1], timeRange[2]))
  } else {
    esti <- truth[0,]
  }
  if (!is.null(smooth)) {
    smooth |> filter(between(.data$time, timeRange[1], timeRange[2]))
  } else {
    smooth <- truth[0,]
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
    esti |> mutate(kind = "esti", state2D = projection2D$project(esti$state)),
    smooth |> mutate(kind = "smooth", state2D = projection2D$project(smooth$state))
  ) |>
    mutate(kind = factor(.data$kind, c("truth", "esti", "smooth", "obs"))) |>
    arrange(.data$kind)
  rangeData <-
    bind_rows(
      data |> filter(.data$time == max(.data$time)) |> mutate(range = "max"),
      data |> filter(.data$time == min(.data$time)) |> mutate(range = "min")
    )
  obs <-
    obs |>
    mutate(kind = "obs", state2D = projection2D$project(obs$state))

  cols <- c("truth" = "#D81B60", "esti" = "#1E88E5", "smooth" = "#FFA507", "obs" = "#004D40")
  plt <-
    ggplot(
      data,
      aes(
        x = .data$state2D[, 1],
        y = .data$state2D[, 2],
        group = paste0(.data$trajId, .data$kind),
        color = .data$kind
      )) +
    scale_colour_manual(values = cols) +
    geom_path() +
    geom_point(data = rangeData, mapping = aes(shape = .data$range), size = 3) +
    geom_point(
      data = obs,
      mapping = aes(group = NULL, color = NULL),
      alpha = 0.4,
      size = 0.2
    ) +
    xlab(NULL) + ylab(NULL) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8)) +
    coord_fixed(ratio = 1) +
    ggtitle(title)

  return(plt)
}


