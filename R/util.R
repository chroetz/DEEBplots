kindColors <- c("truth" = "#D81B60", "esti" = "#1E88E5", "smooth" = "#FFA507", "obs" = "#004D40")


baseTheme <- function() {
  list(
    scale_colour_manual(values = kindColors),
    xlab(NULL),
    ylab(NULL),
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8))
  )
}


prepareTrajs <- function(truth, esti, smooth, obs, timeRange) {

  if (is.null(truth)) {
    return(NULL)
  }

  if (length(timeRange) == 0) {
    timeRange <- range(truth$time)
  }

  truth <- truth |> filter(between(.data$time, timeRange[1], timeRange[2]))

  if (is.null(obs)) {
    obs <- truth[0,]
  } else {
    obs <- obs |> filter(between(.data$time, timeRange[1], timeRange[2]))
  }

  if (is.null(esti)) {
    esti <- truth[0,]
  } else {
    esti <- esti |> filter(between(.data$time, timeRange[1], timeRange[2]))
  }
  if (is.null(smooth)) {
    smooth <- truth[0,]
  } else {
    smooth <- smooth |> filter(between(.data$time, timeRange[1], timeRange[2]))
  }

  list(
    truth = truth,
    esti = esti,
    smooth = smooth,
    obs = obs)
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


