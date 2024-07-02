#' @export
plotTimeState <- function(truth, esti = NULL, smooth = NULL, obs = NULL, timeRange=NULL, title = "", obsAlpha = 0.4, obsSize = 0.2) {

  x <- prepareTrajs(truth, esti, smooth, obs, timeRange)
  if (is.null(x)) {
    warning("Returning empty plot.")
    return(ggplot())
  }
  truth <- x$truth
  esti <- x$esti
  smooth <- x$smooth
  obs <- x$obs

  data <-
    bind_rows(
      truth |> mutate(kind = "truth"),
      esti |> mutate(kind = "esti"),
      smooth |> mutate(kind = "smooth")) |>
    mutate(kind = factor(.data$kind, c("truth", "esti", "smooth", "obs"))) |>
    arrange(.data$kind) |>
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
      alpha = obsAlpha,
      size = obsSize,
      color = kindColors["obs"]
    ) +
    facet_wrap(vars(dim), ncol = 1, scales = "free_y") +
    theme(strip.text = element_blank()) +
    baseTheme() +
    ggtitle(title)
  return(plt)
}
