#' Updating rules
#'
#' Multiplicative rule as in Falmagne and Doignon 2010 (Chapter 13, Section 10)
#' @param likelihood A vector of the likelihood distribution on the states in the structure.
#' @param states A state-by-problem matrix representing the structure, where an element is one if the item is included in the state, and zero otherwise.
#' @param q Last administered item
#' @param r_q Observed response to item q
#' @param beta Vector of careless error probabilities
#' @param eta Vector of lucky guess error probabilities
#' @return The updated likelihood distribution on the knowledge states
#' @export
#'
#' @examples
#' # Let consider the knowledge space and the parameters used in Brancaccio,
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
#' likelihood_0 <-rep(1,nrow(states))/nrow(states)
#' # Item asked
#' q = 3
#' # response observed
#' r_q = 1
#' likelihood_1 <- updating(likelihood_0,states,q,r_q,beta,eta)
updating<-function(likelihood,states,q,r_q,beta,eta){
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

    Pq_k=rep(0,length(likelihood))
    if (r_q==1){
      Pq_k[states[,q]==1]=as.numeric(1-beta[q])
      Pq_k[states[,q]==0]=as.numeric(eta[q])
    }else{
      Pq_k[states[,q]==1]=as.numeric(beta[q])
      Pq_k[states[,q]==0]=as.numeric(1-eta[q])
    }
    likelihood<-sapply(likelihood*Pq_k,function(x,y){x/y},y=sum(Pq_k*likelihood))

  return(likelihood)
}
