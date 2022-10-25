#' @export
plotFollowTime <- function(followTime, title) {

  if (length(followTime) > 1) warning("Cannot yet plot followTime for multiple trajs. Plotting first.")
  followTime <- followTime[[1]]

  data <- tibble(
    time = followTime$time,
    distance = followTime$distance)

  plt <-
    ggplot(data, aes(
      x = .data$time,
      y = .data$distance
    )) +
    geom_line() +
    geom_hline(yintercept=followTime$radius, linetype="dashed", color = "red") +
    geom_vline(xintercept=followTime$followTime, color="blue", linetype="dashed") +
    xlab(NULL) + ylab(NULL) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8)) +
    ggtitle(title)

  return(plt)
}
