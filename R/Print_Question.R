#' Print the question
#'
#' Function to print in the console the item and collect the answer.
#' @param TextQuestion A character contain the text of the question.
#' @param TextResponse A list containing for each question the correct and incorrect answers.
#' @return The answers of the question as numeric (1) if is correct and (0) if is false.
#' @export
print_question<-function(TextQuestion, TextResponse=list(correct="correct",incorrect=c("wrong1","wrong2","wrong3")) ){
  if(length(TextResponse$correct)==0 || length(TextResponse$incorrect)==0)
  {
    stop("You need both correct and incorrect responses.")
  }
  Answer<-"Sweet and Angry Pink Rabbits"
  nresponse <- length(TextResponse$correct)+length(TextResponse$incorrect)
  responses_temp <- sample(c(unlist(TextResponse$correct),TextResponse$incorrect),nresponse)
  responses <- paste(paste0(1:nresponse,")"),responses_temp)
  if(any(TextResponse$incorrect==unlist(TextResponse$correct)))
  {
    warning("An incorrect answer is identical to the correct one.")
  }
  while(all(!grepl(Answer,tolower(responses))) ){

    text<-paste0(TextQuestion," \n\n",paste(responses,collapse="\n\n"),"\n")
    Answer <- readline(text)
  }
  cat("\014")
  answer <- responses_temp[grepl(Answer,tolower(responses))]
  ifelse(any(tolower(answer)==tolower(TextResponse$correct)),return(1),return(0))
}
