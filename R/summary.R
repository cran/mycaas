#' Summary (Method)
#'
#' Function used to produce result summaries of an assessment object.
#'
#' @param object Assessment object to display
#' @param ... Additional arguments affecting the summary produced.
#' @return The function return a summary of the information in the a assessment object
#' @export summary.assessment
#' @export
#'
#' @examples
#' #   Example of random items presentation
#'
#' token<- assessment(N_items = 5 ,adaptive = FALSE, ki=15)
#' summary(token)
summary.assessment<- function(object,...) {
  if(is.null(object$states))
  {
    stop("There is not states field in the object")
  }

  if(!all(lengths(object$Question)==0))
  {
    ask<-paste("####################################################\n",
               "Answered correct items:\n",paste("\n",object$Question$problems_list[object$qorder[object$rorder==1]],"\n",collapse = ""),"\n",
               "####################################################\n",
               "Inferred correct items:\n",paste("\n",object$Question$problems_list[setdiff(which(object$states[object$kstate,]==1), object$qorder[object$rorder==1])],"\n",collapse = ""),"\n")
  }else{
    if(sum(object$qorder[object$rorder==1])!=0)
    {
      first_sentece <- paste("The answered correct items were the ",paste(object$qorder[object$rorder==1],collapse=", "),"\n")
    }else{
      first_sentece <- paste("No correct items were observed!\n")
    }
    if(sum(setdiff(which(object$states[object$kstate,]==1), object$qorder[object$rorder==1]))!=0)
    {
      second_sentece <- paste("The inferred correct items were the ",paste(setdiff(which(object$states[object$kstate,]==1), object$qorder[object$rorder==1]),collapse=", "),"\n")
    }else{
      second_sentece <- paste("No inferred correct items!\n")
    }
    ask<-paste("####################################################\n",
               first_sentece,
               "####################################################\n",
               second_sentece)
  }

  Output<-paste(
                "#######################ASSESSMENT SUMMARY############################\n","\n",
                "Your assessment ended with ",object$n," of ",ncol(object$states)," question.\n\n",
                "The state is ",object$kstate," with a probability of ",round(object$P_kstate,3),".\n\n",
                "The items currently in the state are ", sum(object$states[object$kstate,]),"\n\n\n",collapse="")
  return(cat(Output,ask))
}
