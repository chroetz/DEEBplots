#' @export
plotTimeState <- function(truth, esti = NULL, obs = NULL, timeRange=NULL, title = "", nBasePoints = 1e3) {

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
      alpha = 0.4,
      size = 0.2
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

