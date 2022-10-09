#' @export
createShowPlots <- function(path) {
  file.copy(
    system.file("showPlots.html", package="DEEBplots"),
    path,
    overwrite=TRUE)
  writePlotDirInventory(file.path(path, "plots"))
}

writePlotDirInventory <- function(path) {
  json <-
    DEEBpath::getParenthesisFileNameData(path) |>
    jsonlite::toJSON(pretty=TRUE, auto_unbox=TRUE)
  txt <- paste0("var plots = ",json,";")
  writeLines(txt, file.path(path, "plots.js"))
}
