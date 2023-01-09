#' @export
plotDistances <- function(truth, esti = NULL, smooth = NULL, obs = NULL, timeRange=NULL, title = "") {

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
    arrange(.data$kind) |>
    unpackStateLong()

  cols <- c("truth" = "#D81B60", "esti" = "#1E88E5", "smooth" = "#FFA507", "obs" = "#004D40")
  plt <-
    ggplot(data, aes(
      x = .data$time,
      y = .data$distance,
      color = .data$kind,
      group = paste0(.data$trajId, .data$kind)
    )) +
    scale_colour_manual(values = cols) +
    geom_line() +
    xlab(NULL) + ylab(NULL) +
    theme(legend.position = "none", plot.title = element_text(size = 8)) +
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

