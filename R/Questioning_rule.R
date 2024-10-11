#' Questioning rule (half split)
#'
#' Rule to select the most informative item at each step of the assessment.
#' @param likelihood A vector of the likelihood distribution on the states in the structure.
#' @param states A state-by-problem matrix representing the structure, where an element is one if the item is included in the state, and zero otherwise.
#' @param beta Vector of careless error probabilities.
#' @param eta Vector of lucky guess error probabilities.
#' @param Q_pool A vector contains the pool of items for the assessment in this moment of the procedure.
#' @return The item that maximizes the information (for details see, Doignon and Falmagne, 2012).
#' @export
#' @references
#' Doignon, J.-P., & Falmagne, J.-C. (1999). Knowledge spaces. Berlin: Springer.
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
#' likelihood <-c(0,0,0,0,0,1/3,0,1/3,0,0,1/3)
#' Q_pool <- c(2,4,5)
#' half_split(likelihood,states,beta,eta,Q_pool)
half_split<-function(likelihood,states,beta,eta,Q_pool=NA){
  if(any(eta>1 | eta<0 |beta>1 | beta<0  ))
  {
    stop("The error parameters eta and beta need to be between 0 and 1.\n")
  }

  if(ncol(states)!=length(beta))
  {
    stop("The number of items in the knowledge structure is different from the number of beta parameters\n")
  }

  if(ncol(states)!=length(eta))
  {
    stop("The number of items in the knowledge structure is different from the number of eta parameters\n")
  }
  if(nrow(states)!=length(likelihood) || round(sum(likelihood),0)!=1)
  {
    stop(paste0("The cardinality of  the knowledge structure is ",nrow(states)," while the length of the likelihood is ",length(likelihood),
                " which sums to ",round(sum(likelihood),0),".\n"))
  }
  ifelse(any(is.na(Q_pool)),{it<-rep(1,ncol(states)); Q_pool<-which(it==1)},{it<-integer(ncol(states));it[Q_pool]<-1})
  L_Kg<-sapply(1:ncol(states), function(item,Likelhood,K,it) sum(Likelhood[K[,item]==1]*it[item]), Likelhood=likelihood, K=states, it=it)
    d<-intersect(which(round(abs(2*L_Kg-1),4)==round(min(abs(2*L_Kg-1)),4)),Q_pool)
    if(length(d)>1){
      n<-sample(d,1)
    }else{
      n<-d
    }
  return(n)
}
