#' Create decent LHS
#'
#' Generate random Latin hypercube samples with a
#' given budget and return the best one according to
#' the MaxPro criterion.
#'
#' @param n Number of points
#' @param d Number of dimensions
#' @param ndes Max number of random designs to generate
#' @param max.time Max amount of time
#'
#' @return Matrix with rows of points
#' @export
#'
#' @examples
#' decentLHS(20,2, ndes=10)
decentLHS <- function(n, d, ndes, max.time) {
  if (missing(ndes) && missing(max.time)) {
    stop("Must give ndes or max.time to decentLHS")
  }
  start.time <- Sys.time()
  bestdes <- NULL
  bestcrit <- Inf
  i <- 1
  while (TRUE) {
    # Make new LHS
    x <- lhs::randomLHS(n, d)
    crit <- MaxPro::MaxProMeasure(x)

    # Check if best yet
    if (crit < bestcrit) {
      bestcrit <- crit
      bestdes <- x
    }

    # Check if done
    i <- i+1
    if (!missing(ndes) && (i > ndes)) {
      break
    }
    if (!missing(max.time) && (as.numeric(Sys.time() - start.time, units="secs") > max.time)) {
      break
    }
  }
  # Return bestdes
  bestdes
}

