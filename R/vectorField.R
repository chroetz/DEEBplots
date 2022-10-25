#' @export
plotStateSpaceWithVectorField <- function(trajs, fun, parms, title) {
  d <- ncol(trajs$state)

  if (d == 2) {
    projection2D <- getIdentityProjection()
  } else if (d > 2) {
    projection2D <- calculateProjection(trajs$state, dim = 2)
  } else {
    stop("d invalid: ", d)
  }
  traj2D <- projection2D$project(trajs$state)

  nArrows <- 20
  rangeLenX <- diff(range(traj2D[,1]))
  rangeLenY <- diff(range(traj2D[,2]))
  maxLen <- pmax(rangeLenX, rangeLenY)
  s <- 0.1
  xGrid <- seq(
    mean(range(traj2D[,1])) - (0.5+s) * maxLen,
    mean(range(traj2D[,1])) + (0.5+s) * maxLen,
    length.out = nArrows)
  yGrid <- seq(
    mean(range(traj2D[,2])) - (0.5+s) * maxLen,
    mean(range(traj2D[,2])) + (0.5+s) * maxLen,
    length.out = nArrows)
  grid2D <- as.matrix(expand.grid(xGrid, yGrid))
  grid <- projection2D$embed(grid2D)
  field <-
    sapply(
      seq_len(nrow(grid)),
      \(i) unlist(fun(u = grid[i,], parms = parms))
    ) |>
    t()
  field2D <- projection2D$project(field)

  pltData <- tibble::tibble(
    x = grid2D[,1],
    y = grid2D[,2],
    vx = field2D[,1],
    vy = field2D[,2])
  trajData <- tibble::tibble(
    x = traj2D[,1],
    y = traj2D[,2],
    trajId = trajs$trajId)

  pltData <-
    pltData |>
    mutate(speed = sqrt(.data$vx^2 + .data$vy^2))

  maxSpeed <- max(pltData$speed)
  if (!is.finite(maxSpeed) || maxSpeed <= .Machine$double.eps)
    maxSpeed <- .Machine$double.eps
  sizeX <- max(diff(range(xGrid))) / nArrows
  sizeY <- max(diff(range(yGrid))) / nArrows
  size <- mean(c(sizeX, sizeY)) * sqrt(2)

  plt <- ggplot(pltData) +
    geom_path(
      data = trajData,
      mapping = aes(
        x = .data$x,
        y = .data$y,
        group = .data$trajId)) +
    geom_segment(
      aes(
        x = .data$x,
        y = .data$y,
        color = .data$speed,
        xend = .data$x+.data$vx/maxSpeed*size,
        yend = .data$y+.data$vy/maxSpeed*size),
      arrow = arrow(length = grid::unit(0.1, "cm")),
      size = 0.6) +
    scale_colour_continuous(
      low = "grey80",
      high = "darkred") +
    xlab(NULL) +
    ylab(NULL)
  plt <-
    plt +
    theme(legend.position = "none", plot.title = element_text(size = 8)) +
    coord_fixed(ratio = 1) +
    ggtitle(title)

  return(plt)
}

#' @export
plotVectorField <- function(derivTrajs, title) {

  subtitle <- NULL

  d <- ncol(derivTrajs$state)
  if (d == 2) {
    state <- derivTrajs$state
    deriv <- derivTrajs$deriv
  } else {
    id <- do.call(paste, c(as.list(as.data.frame(derivTrajs$state[,1:2])), sep="_"))
    resState <- by(derivTrajs$state[,1:2], id, \(df) lapply(df, mean))
    resDeriv <- by(derivTrajs$deriv[,1:2], id, \(df) lapply(df, mean))
    state <- matrix(unlist(resState), ncol=2, byrow=TRUE)
    deriv <- matrix(unlist(resDeriv), ncol=2, byrow=TRUE)
    subtitle <- "dim3 mean for dim1&2"
  }

  pltData <- tibble::tibble(
    x = state[,1],
    y = state[,2],
    vx = deriv[,1],
    vy = deriv[,2])

  pltData <-
    pltData |>
    mutate(speed = sqrt(.data$vx^2 + .data$vy^2))

  nArrows <- 20
  maxSpeed <- max(pltData$speed)
  if (!is.finite(maxSpeed) || maxSpeed <= .Machine$double.eps)
    maxSpeed <- .Machine$double.eps
  sizeX <- max(diff(range(pltData$x))) / nArrows
  sizeY <- max(diff(range(pltData$y))) / nArrows
  size <- mean(c(sizeX, sizeY)) * sqrt(2)

  plt <-
    ggplot(pltData) +
    geom_segment(
      aes(
        x = .data$x,
        y = .data$y,
        color = .data$speed,
        xend = .data$x+.data$vx/maxSpeed*size,
        yend = .data$y+.data$vy/maxSpeed*size),
      arrow = arrow(length = grid::unit(0.1, "cm")),
      size = 0.6) +
    scale_colour_continuous(
      low = "grey80",
      high = "darkred") +
    xlab(NULL) +
    ylab(NULL)
  plt <-
    plt +
    theme(legend.position = "none") +
    coord_fixed(ratio = 1) +
    ggtitle(
      title,
      subtitle = subtitle
    )

  return(plt)
}
