#' Computes the greatest common divisor (GCD) of two integers (numbers),
#'
#' @param a A number.
#' @param b A number.
#' @return the largest number that divides \code{a} and \code{b}.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#'
#' @references
#' \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' @export

euclidean <- function(a, b){
  stopifnot(is.numeric(a), is.numeric(b), length(a) == 1, length(b) == 1)
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

