#' Stopping rule
#'
#' Rule to decide when terminate the assessment
#' @param likelihood A vector of the likelihood distribution on the states in the structure.
#' @param states A state-by-problem matrix representing the structure, where an element is one if the item is included in the state, and zero otherwise.
#' @param termination Define and select one of the termination criteria:
#' "likelihood_maximization" the assessment terminates when the likelihood of a knowledge
#'  state in a knowledge structure became higher of the termination criteria (Heller, and Repitsch, 2012).
#' "items_discrimination" the assessment terminates if the marginal likelihood of
#'  all the items is outside the interval of the stopping criteria (Donadello, Spoto, Sambo, Badaloni, Granziol, Vidotto, 2017).
#' @param SC The Stopping criterion for the assessment is a numeric vector of values between 0 and 1.
#'  When the "termination" parameter is "likelihood_maximization" this is a single scalar that corresponds to the likelihood that a knowledge state needed to terminates the assessment.
#'  When the "termination" parameter is "items_discrimination" this is a numeric vector of length two, the assessment terminate if the the marginal likelihood of each item is outside of the interval between the two elements.
#' @return Return TRUE if the assessment should terminates under the criteria, otherwise  FALSE
#' @export
#' @references
#' Donadello, I., Spoto, A., Sambo, F., Badaloni, S., Granziol, U., & Vidotto, G. (2017). ATS-PD: An adaptive testing system for psychological disorders. Educational and psychological measurement, 77(5), 792-815.
#'
#' Heller, J., & Repitsch, C. (2012). Exploiting prior information in stochastic knowledge assessment. Methodology.
#' @examples
#' # Consider the knowledge space and the parameters used in Brancaccio,
#' # de Chiusole, Stefanutti (2023) in Example 1
#'
#' states<-matrix(c( 0,0,0,0,0,
#'	                   0,0,0,0,1,
#'	                   0,0,1,0,1,
#'	                   0,0,0,1,1,
#'	                   0,0,1,1,1,
#'	                   1,0,1,0,1,
#'	                   0,1,0,1,1,
#'	                   1,0,1,1,1,
#'	                   0,1,1,1,1,
#'	                   1,1,0,1,1,
#'	                   1,1,1,1,1), byrow=TRUE, ncol=5)
#' beta  <-c(.004,.03,.02,.01,.007)
#' eta   <-c(5e-06, 5e-05, 4e-05,.007,.08)
#' likelihood <-c(0,0,0,0,0,0,0,.49,0,0,.51)
#' #stopping criterion based on the likelihood mode
#' stopping_criterion(likelihood,states, termination="likelihood_maximization" ,SC=c(0.5))
#' #stopping criterion based on the items marginal probabilities
#' stopping_criterion(likelihood,states, termination="items_discrimination" ,SC=c(0.2,0.8))
stopping_criterion<-function(likelihood, states, termination="likelihood_maximization" ,SC=c(0.8)){
if(any(SC>1 | SC<0))
{
  stop("The stopping criteria need to be between 0 and 1.\n")
}else if(length(SC)!=1 && termination=="likelihood_maximization")
{
  warning("likelihood_maximization stopping criteria required only one value in SC, therefore only the first elements is used.\n")
}else if(length(SC)!=2  && termination=="items_discrimination")  {
  stop("items_discrimination stopping criteria required exactly two values in SC.\n")
}

if(termination=="likelihood_maximization"){
  stopping<-any(likelihood>SC[1])
}else if(termination=="items_discrimination"){
  #Computing the marginal likelihood of each items
  L_Kg <- colSums(likelihood*states)
  stopping<-all(L_Kg>SC[2]|L_Kg<SC[1])
}
return(stopping)
}
