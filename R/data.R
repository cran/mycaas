#' Example of a test to showcase the Adaptive Assessment tools
#'
#' The dataset contains a six items test concerning basic knowledge on computerized adaptive assessment.
#' This test has been developed during the creation of this R package.
#'
#'
#' @format A assessment object based on six multiple choice items.
#' \describe{
#' \item{states}{Structure with six items and twelve states.}
#' \item{likelihood}{Uniformly distributed across the states.}
#' \item{Stopping_rule}{The stopping rules is "likelihood_maximization" with a criterion of .5.}
#' \item{Question}{Six multiple choice items.}
#' \item{adaptive}{The value is set on TRUE allowing adaptive administration.}
#' \item{repetition}{The value is set on FALSE not allowing items repetition.}
#' }
#'@source {Created to serve as an Example for this package.}
#'@examples
#'data(AA_knowledge_test) #Lazy loading
"AA_knowledge_test"
