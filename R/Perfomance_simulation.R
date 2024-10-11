#' Performance indexes
#'
#' Simulation producing efficiency and accuracy performance curves and indexes based on simulated assessment of all the states in structure
#' @param states A state-by-problem matrix representing the structure, where an element is one if the item is included in the state, and zero otherwise.
#' @param beta Vector of careless error probabilities.
#' @param eta Vector of lucky guess error probabilities.
#' @param likelihood A vector of the likelihood for each state, if omitted the initial likelihood is assumed equally distributed.
#' @param questioning_rule A function which is used a questioning rules for the assessment.
#'  the default questioning rule is 'half_split'.
#' @param nrep Number of times in which each state is simulated. The default value is one.
#' @return Efficiency and Accuracy curves plots.
#' @importFrom rlang .data
#' @export
#'
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
#' performance_simulation(states,beta,eta)
performance_simulation<-function(states,beta,eta,likelihood=NA, questioning_rule = "half_split",nrep=1)
{
  if(any(eta>=1 | eta<0 |beta>=1 | beta<0  ))
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
  if(is.na(likelihood[1]))
  {
    likelihood <-rep(1,nrow(states))/nrow(states)
  }
  if(nrow(states)!=length(likelihood) || round(sum(likelihood),0)!=1)
  {
    stop(paste0("The cardinality of  the knowledge structure is ",nrow(states)," while the length of the likelihood is ",length(likelihood),
                " which sums to ",round(sum(likelihood),0),".\n"))
  }

  nstates<-nrow(states)
  true_states <- 1:nstates
  stopping <- seq(.5,1,by=.1)
  termination<-c("likelihood_maximization","items_discrimination")
  df<-expand.grid(true_states,stopping,termination)
  names(df) <- c("true_states","stopping","termination_rule" )
  df<-df[!(df$termination_rule=="items_discrimination"& df$stopping<.7),]
  df$nquestion <- numeric(nrow(df))
  df$likelihood <- numeric(nrow(df))
  df$recovered <- numeric(nrow(df))

  if(nrep>1)
  {
    df_token<-df
    for(i in 2:nrep)
    {
      df<-rbind(df,df_token)
    }
  }

  for(i in 1:nrow(df))
  {
    if(df$termination_rule[i]=="items_discrimination")
    {
      SC<-c(1-df$stopping[i],df$stopping[i])
    }else{
      SC<-df$stopping[i]
    }
    token <- assessment(likelihood,states,beta,eta,ki=df$true_states[i], questioning_rule =  questioning_rule, termination = df$termination_rule[i], SC=SC)
    df$nquestion[i] <- token$n
    df$likelihood[i] <- token$P_kstate
    df$recovered[i] <- token$kstate
   }
  df$hamming <- rowSums(abs(states[df$true_states,] -states[df$recovered ,]))

  averages <- stats::aggregate(cbind(nquestion,hamming, likelihood) ~ stopping + termination_rule , df, mean)
  minaverage <- stats::aggregate(cbind(nquestion,hamming, likelihood) ~ stopping + termination_rule , df, min)
  maxaverage <- stats::aggregate(cbind(nquestion,hamming, likelihood) ~ stopping + termination_rule , df, max)

  g<-ggplot2::ggplot(averages,
                     ggplot2::aes(x=.data$stopping, y =.data$nquestion, color=.data$termination_rule, group=.data$termination_rule)) +
    ggplot2::geom_line(linewidth=1, position=ggplot2::position_dodge(0.05)) +  ggplot2::geom_point(position=ggplot2::position_dodge(0.05),size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin= minaverage$nquestion, ymax=maxaverage$nquestion), position=ggplot2::position_dodge(0.05)) +
    ggplot2::geom_text(ggplot2::aes(label=round(.data$likelihood,2)), vjust=2,position =ggplot2::position_dodge(0.05)) + ggplot2::ylim(0,max(df$nquestion)+1) + ggplot2::labs(title = "Efficiency plot",
                                                                                            x = "Termination criterion",y = "Number of questions asked"  )
  p<-ggplot2::ggplot(averages,
                     ggplot2::aes(x=.data$stopping, y =.data$hamming, color=.data$termination_rule, group=.data$termination_rule)) +
    ggplot2::geom_line(linewidth=1, position=ggplot2::position_dodge(0.05)) +  ggplot2::geom_point(position=ggplot2::position_dodge(0.05),size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin= minaverage$hamming, ymax=maxaverage$hamming), position=ggplot2::position_dodge(0.05)) +
    ggplot2::geom_text(ggplot2::aes(label=round(.data$likelihood,2)), vjust=2,position =ggplot2::position_dodge(0.05)) +
    ggplot2::ylim(0,ncol(states))  + ggplot2::labs(title = "Accuracy plot",
                                                                                     x = "Termination criterion",y = "Hamming Distance"  )
  print(g)
  print(p)
  return(df)
}
