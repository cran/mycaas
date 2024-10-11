#' Assessment function
#'
#' Function that performs computerized assessment
#' @param likelihood A vector of the likelihood distribution on the states in the structure.
#' @param states A state-by-problem matrix representing the structure, where an element is one if the item is included in the state, and zero otherwise.
#' @param beta Vector of careless error probabilities.
#' @param eta Vector of lucky guess error probabilities.
#' @param questioning_rule A function which is used a questioning rules for the assessment.
#'  the default questioning rule is 'half_split'.
#' @param termination Define and select one of the termination criteria:
#' "likelihood_maximization" the assessment terminates when the likelihood of a knowledge
#'  state in a knowledge structure became higher of the termination criteria (Heller, and Repitsch, 2012).
#' "items_discrimination" the assessment terminates if the marginal likelihood of
#'  all the items is outside the interval of the stopping criteria (Donadello, Spoto, Sambo, Badaloni, Granziol, Vidotto, 2017).
#' @param SC The Stopping criterion for the assessment is a numeric vector of values between 0 and 1.
#'  When the "termination" parameter is "likelihood_maximization" this is a single scalar that corresponds to the likelihood that a knowledge state needed to terminates the assessment.
#'  When the "termination" parameter is "items_discrimination" this is a numeric vector of length two, the assessment terminate if the the marginal likelihood of each item is outside of the interval between the two elements.
#' @param ki  A number indicating the row in the structure to simulate as the true knowledge states.
#' @param textq  A character vector containing the text of the questions.
#' @param textr  A list containing for each question the correct and incorrect answers.
#' @param repetition Logical value. When the value is TRUE the assessment procedure is allowed to administer the same item more then one time.
#' @param adaptive Logical value. When the value is TRUE the assessment proceed with an adaptive procedure, otherwise the items presentation is randomized
#' @param N_items Number of items in the test. Optional entry in case of adaptive = FALSE.
#' @param simulation Logical value. When the value is TRUE the assessment proceed with simulating a complete assessment with a user knowledge state define in variable ki.
#' Otherwise the assessment collect answer from the user.
#' @param interactiveplot Logical value. When the value is TRUE the knowledge structure is plot using a color thermometer scale to represents
#' the likelihood of the states.
#' @return The outcome of a single assessment.
#' @export
#' @references
#' Doignon, J.-P., & Falmagne, J.-C. (1999). Knowledge spaces. Springer.
#'
#' Donadello, I., Spoto, A., Sambo, F., Badaloni, S., Granziol, U., & Vidotto, G. (2017). ATS-PD: An adaptive testing system for psychological disorders. Educational and psychological measurement, 77(5), 792-815.
#'
#' Heller, J., & Repitsch, C. (2012). Exploiting prior information in stochastic knowledge assessment. Methodology.
#' @examples
#' # Example 1: From Brancaccio,de Chiusole, Stefanutti (2023)
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
#' likelihood <-rep(1,nrow(states))/nrow(states)
#' assessment(likelihood,states,beta,eta)
#'
#'
#' # Example 2: Random items presentation
#'
#' assessment(N_items = 5 ,adaptive = FALSE)
#'
#' # Example 3: Interactive mode on in the console
#'
#' TextQuestion<-c("I frequently use computers.",
#' "I know how to build an assessment instrument (i.e., a paper-and-pencil questionnaire).",
#' "I can code and implement a 'for' loop (independently of its purpose).",
#' "When you administer an adaptive assessment, each question: ",
#' "I usually programme or code my assessment tools ",
#' "I add my assessment in a programme (i.e , google form) ")
#'
#' TextResponse<-list(
#' correct=list("Yes","Yes","Yes","depend from the previous questions","Yes","Yes"),
#' incorrect=list(c("No"),c("No"),c("No"),c("is independent from the others questions",
#' "depend from the gender of the interviewed"),c("No"),c("No"))
#' )
#'
#' map <- matrix(c(1,0,0,0,
#'              0,1,0,0,
#'              1,0,1,0,
#'              0,1,0,1,
#'              1,1,1,0,
#'              1,1,0,0), ncol=4)
#'
#' skillmap<-cbind(1:6,map)
#'
#' states<-pks::delineate(skillmap)$K
#' rownames(states)<-NULL
#'
#' eta <- rep(0.1,ncol(states))
#' beta <- rep(0.1,ncol(states))
#' likelihood <-rep(1,nrow(states))/nrow(states)
#'
#' if(interactive()){token <- assessment(likelihood,states,beta,eta,textq=TextQuestion,
#'                     textr=TextResponse,simulation=FALSE)}
assessment<-function(likelihood = NA , states, beta, eta, questioning_rule = "half_split", termination="likelihood_maximization", SC = .5,
                     N_items=NULL,  ki=1,textq= NULL, textr=NULL,
                     repetition=FALSE, adaptive=TRUE,simulation=TRUE, interactiveplot= FALSE)
{
  if(adaptive==FALSE)
  {
    if(N_items<26){
      states<-expand.grid(rep(list(0:1),N_items))
      beta <- rep(0, ncol(states))
      eta <- rep(0, ncol(states))
      termination="likelihood_maximization"
      SC <- 1
    }else{
      states<-matrix(0, ncol=N_items,nrow=2)
      states[2,]<-1
      eta<-rep(.5,N_items)
      beta<-rep(.5,N_items)
      repetition <- FALSE
      likelihood<-rep(1/nrow(states),nrow(states))
    }

  }
  #initializing the likelihood as a uniform distribution
  if(is.na(likelihood[1]))
  {
    likelihood<-rep(1/nrow(states),nrow(states))
  }
  # Insert external questioning rule
  questioning <- get(questioning_rule)
  #this is a variable for control the item repetition
  it=rep(1,ncol(states))
  qmemo<-NULL
  rmemo<-NULL
  while(stopping_criterion(likelihood,states, termination ,SC)==FALSE && sum(it)>0)
  {
    if(interactiveplot==TRUE)
    {
      token<-list(likelihood = likelihood,
                  qorder = qmemo,
                  rorder = rmemo,
                  kstate = which.max(likelihood),
                  P_kstate = max(likelihood),
                  n=length(rmemo),
                  states=states,
                  Stopping_rule=list(rule=termination,criterion=SC),
                  Question=list(),
                  eta = eta,
                  beta = beta,
                  Entropy=-sum(likelihood*log(likelihood,2)),
                  Std_entropy=-sum(likelihood*log(likelihood,2))/-log(1/nrow(states),2),
                  adaptive=adaptive,
                  repetition=repetition)
      attr(token, "class") <- "assessment"
      plot(token)
      Sys.sleep(2)
    }
    q <- questioning(likelihood,states,beta,eta,Q_pool=which(it==1))
    if(repetition==FALSE)
    {
      it[q]<-0
    }
    # Simulating the responses from the true knowledge state of the student
    if(simulation==TRUE){
      r_q<- ifelse(states[ki,q]==1,as.numeric(stats::runif(1)<1-beta[q]),as.numeric(stats::runif(1)<eta[q]))
    }else{
      r_q<-print_question(textq[q], list(correct=textr$correct[[q]],incorrect=textr$incorrect[[q]]))
    }
    qmemo<- c(qmemo,q)
    rmemo<-c(rmemo,as.numeric(r_q))
    likelihood<-updating(likelihood,states,q,r_q,beta,eta)
  }

  token<-list(likelihood = likelihood,
              qorder = qmemo,
              rorder = rmemo,
              kstate = which.max(likelihood),
              P_kstate = max(likelihood),
              n=length(rmemo),
              states=states,
              Stopping_rule=list(rule=termination,criterion=SC),
              Question=list(problems_list=textq,response_lists=textr),
              eta = eta,
              beta = beta,
              Entropy=-sum(likelihood*log(likelihood,2)),
              Std_entropy=-sum(likelihood*log(likelihood,2))/-log(1/nrow(states),2),
              adaptive=adaptive,
              repetition=repetition
  )
  attr(token, "class") <- "assessment"
  if(interactiveplot==TRUE){plot(token)}
  return(token)
}
