#' @export
getStateSpacePlot <- function(truth, esti, obs = NULL, timeRange, title = "", nBasePoints=1e3) {

  if (is.null(truth) || is.null(esti)) {
    warning("Empty trajs. Returning empty plot.")
    return(ggplot())
  }
  if (is.null(obs)) {
    obs <- truth[0,]
  } else {
    obs <- filter(obs, between(.data$time, timeRange[1], timeRange[2]))
  }

  truth <- interpolateTrajs(truth, seq(timeRange[1], timeRange[2], length.out = nBasePoints))
  esti <- interpolateTrajs(esti, seq(timeRange[1], timeRange[2], length.out = nBasePoints))

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
    geom_point(
      data = obs,
      mapping = aes(group = NULL, color = NULL),
      alpha = 0.1
    ) +
    xlab(NULL) + ylab(NULL) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, vjust = -2)) +
    coord_fixed(ratio = 1) +
    ggtitle(title)

  return(plt)
}


