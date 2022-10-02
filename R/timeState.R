#' @export
getTimeDependencePlot <- function(truth, esti, obs = NULL, timeRange, title = "", nBasePoints = 1e3) {

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

  data <-
    bind_rows(
      truth |> mutate(kind = "truth"),
      esti |> mutate(kind = "esti")) |>
    unpackStateLong()

  obs <-
    obs |>
    mutate(kind = "obs") |>
    unpackStateLong()

  plt <-
    ggplot(data, aes(
      x = .data$time,
      y = .data$state,
      color = .data$kind,
      group = paste0(.data$trajId, .data$kind)
    )) +
    geom_path() +
    geom_point(
      data = obs,
      mapping = aes(color = NULL, group = NULL),
      alpha = 0.1
    ) +
    facet_wrap(vars(dim), ncol = 1, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    theme(legend.position = "none") +
    ggtitle(title)
  return(plt)
}


unpackStateLong <- function(trajs) {
  trajs |>
    mutate(tmp = as_tibble(structure(.data$state, dimnames = list(
      NULL, paste0("state", 1:ncol(.data$state))
    )))) |>
    unpack(.data$tmp) |>
    select(-.data$state) |>
    pivot_longer(
      starts_with("state"),
      names_to = "dim",
      values_to = "state",
      names_transform =  ~ str_sub(., start = 6)
    ) |>
    mutate(dim = as.integer(.data$dim))
}

