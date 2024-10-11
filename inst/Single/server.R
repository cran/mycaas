suppressPackageStartupMessages(library(shiny, warn.conflicts = FALSE))
library(shinythemes, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(rhandsontable, warn.conflicts = FALSE)
library(markdown, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)

loadRData <- function(fileName) {
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

server <- function(input, output,file) {

  mydf <- reactive({

    tryCatch(!is.null(file),{

      if(is(file, 'assessment'))
      {
        mydf_0 <- file
      }else{
        sessionEnvir <- sys.frame()
        mydf_0 <- loadRData(file)
      }
      return(mydf_0)
    }, error <- function(e){
      cat("Data are not OK")
    }, warning <- function(w) {
      if(grepl("Something should be changed", w, fixed = TRUE)){return(w$message)}
    }
    )
  })

  textq <- NULL

  ###Data Upload----

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('test_', Sys.Date(), ".RData", sep = "")
    },
    content = function(file) {
      token <- token_plot
      attr(token, "class") <- "assessment"
      save(token, file = file)
    }
  )

  token_plot <- NULL
  observe({
    req(mydf())
    token <- mydf()
    ### RDA case ----

    if (is(token, "assessment"))  # The variable token contains the assessment object and get redistributed on the operative variables
    {
      adaptive <- token$adaptive
      states <- token$states
      beta <- token$beta
      eta <- token$eta
      adaptive <- token$adaptive
      repetition <- token$repetition
      SC <- token$Stopping_rule$criterion
      termination <- token$Stopping_rule$rule
      textq <- token$Question$problems_list
      textr <- token$Question$response_lists
      og_likelihood <- token$likelihood
    }  else{
      adaptive <- NULL
    }



    # Beginning of the assessment Engine
    observe({
      req(adaptive)
      ## REACTIVE VALUES
      likelihood <- reactiveValues()
      qmemo <- reactiveValues()
      rmemo <- reactiveValues()
      it <- reactiveValues()
      q <- reactiveValues()
      r_q <- reactiveValues()
      #Beginning of the Assessment
      observeEvent(input$Restart,
                   {
                     likelihood$a <- og_likelihood
                     #this is a variable for control the item repetition
                     it$a <- rep(1, length(states[1,]))
                     qmemo$a <- NULL
                     rmemo$a <- NULL

                     # first item of the assessment
                     q$a <-
                       half_split(likelihood$a, states, beta, eta, Q_pool = which(it$a == 1))
                     if (repetition == FALSE)
                     {
                       it$a[q$a] <- 0
                     }
                   },
                   ignoreNULL = FALSE,
                   ignoreInit = FALSE)

      ########################################

      output$panelCondition <- reactive({
        stopping_criterion(likelihood$a, states, termination , SC) == FALSE &&
          sum(it$a) >= 0
      })
      outputOptions(output, "panelCondition", suspendWhenHidden = FALSE)

      ########################################

      output$textitem <-
        renderUI(withMathJax(helpText(textq[q$a])))

      output$textanswer <-
        renderUI(radioButtons(
          "answer",
          label = "",
          choices =  unlist(list(textr[["correct"]][q$a], textr[["incorrect"]][q$a])),
          selected = NULL
        ))


      ## The assessment start here with update every time that the answer is confirmed
      observeEvent(input$Go, {
        r_q$a <- as.numeric(textr$correct[[q$a]] == input$answer)
        likelihood$a <-
          updating(likelihood$a, states, q$a, r_q$a, beta, eta)
        rmemo$a <- c(rmemo$a, as.numeric(r_q$a))
        qmemo$a <- c(qmemo$a, q$a)
        #Object for the plot
        token_plot <<- list(
          likelihood = likelihood$a,
          qorder = qmemo$a,
          rorder = rmemo$a,
          kstate = which.max(likelihood$a),
          P_kstate = max(likelihood$a),
          n = length(rmemo$a),
          states = states,
          Stopping_rule = list(),
          Question = list(problems_list = textq,response_lists=textr),
          eta = eta,
          beta = beta,
          Entropy = NA,
          Std_entropy = NA,
          adaptive = adaptive,
          repetition = repetition
        )
        attr(token_plot, "class") <- "assessment"

        ###Plot the hasse diagram only for small structure ----
        if(nrow(states)<1000)
        {
          output$panelCondition2 <- reactive({
            TRUE
          })
          outputOptions(output, "panelCondition2", suspendWhenHidden = FALSE)
          output$hasse <- renderPlot({
            plot(
              token_plot,
              bg_color = "#2B3E50",
              verices_color = "white"
            )
          })
        }


        if (stopping_criterion(likelihood$a, states, termination , SC) == FALSE &&
            sum(it$a) > 0)
        {
          q$a <-
            half_split(likelihood$a, states, beta, eta, Q_pool = which(it$a == 1))
          if (repetition == FALSE)
          {
            it$a[q$a] <- 0
          }
          item <- reactive(textq[q$a])
          output$textitem <-
            renderUI(withMathJax(helpText(item())))

          resps <- reactiveValues()
          resps$a <- unlist(list(textr[["correct"]][q$a], textr[["incorrect"]][q$a]))

          output$textanswer <-
            renderUI(radioButtons(
              "answer",
              label = "",
              choices =  sample(resps$a),
              selected = NULL
            ))

        }else{
          it$a[1] <- -1
        }
      })
      # End of the assessment Engine
    })

  })

}
