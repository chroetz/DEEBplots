#' @export
writePlot <- function(plt, path) {
  ggplot2::ggsave(path, plt, width = 3, height = 3)
}
