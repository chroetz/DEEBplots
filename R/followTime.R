#' @export
plotFollowTime <- function(followTime, title) {

  if (length(followTime) > 1) warning("Cannot yet plot followTime for multiple trajs. Plotting first.")
  followTime <- followTime[[1]]

  data <- tibble(
    time = followTime$time,
    maxAllowedDistance = pmax(followTime$diffConst * followTime$constFraction, followTime$closeToConstThreshold),
    distance = followTime$diffFollower,
    cumTimeLost = followTime$cumTimeLost)

  plt1 <-
    ggplot(data, aes(
      x = .data$time,
      y = .data$distance
    )) +
    geom_line() +
    geom_line(aes(y = .data$maxAllowedDistance), linetype="dashed", color = "red")
  plt2 <-
    data |>
    ggplot(aes(
      x = .data$time,
      y = .data$cumTimeLost
    )) +
    geom_line() +
    geom_hline(yintercept=followTime$lostTimeThreshold, linetype="dashed", color = "red") +
    geom_vline(xintercept=followTime$followTime, color="blue", linetype="dashed")
  plt <- gridExtra::arrangeGrob(plt1, plt2, ncol=1, top=title)

  return(plt)
}
