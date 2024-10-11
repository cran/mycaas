library(shiny, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(rhandsontable, warn.conflicts = FALSE)
library(markdown, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)

#Complementary function for the ShinyApp ----
loadRData <- function(fileName) {
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

my_unlist <- function(x) {
  y <- c(unlist(x))
  names(y) <- NULL
  return(y[y != "" & !is.na(y)  ])
}

#Beginning of the Shiny App ----
shinyApp(
  ## UI interface ----
  ui = navbarPage(
    "Practice Assessment",
    theme = shinythemes::shinytheme("superhero"),
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    ## Introduction Panel ----
    shiny::tabPanel("Introduction",
                    fluidRow(
                      column(9,
                             h2("Welcome to the Adaptive Assessment Tools App"),
                             p("This application allows you to conduct adaptive assessments using the continuous Markov procedure by Falmagne and Doignon (CMP; 1988) from Knowledge Space Theory (KST) and/or Adaptive Testing System for Psychological Disorders (ATS-PD; Donatello et al., 2017) from Formal Psychological Assessment (FPA). Whether you are assessing knowledge, evaluating clinical conditions, or any other application, this app provides a structured, efficient, and accurate way to collect your data."),

                             h3("What is Adaptive Testing in KST and FPA?"),
                             p("Adaptive testing is a dynamic process that tailors the assessment to each individual based on their responses. In both Knowledge Space Theory and Formal Psychological Assessment, this means recovering the state that represents the learner's or patient's abilities, among the collection of possible states on the structure."),

                             h3("The Underlying Structure"),
                             p("Both KST and FPA rely on an underlying structure—be it a knowledge structure in KST or a clinical structure in FPA. This structure guides the adaptive process, ensuring that each question or evaluation step is meaningful and relevant. The app can also accommodate a random presentation of questions, providing flexibility in how the assessment is conducted."),

                             h3("How the App Works"),
                             tags$ol(
                               tags$li(strong("Upload Your Assessment:"),"On the second page, you can upload your assessment data. This includes questions and a skill map or clinical context, which should be in a CSV file format. Alternatively, if you have pre-created an assessment in R, you can upload an RDA file containing a variable of the 'assessment' type."),
                               tags$li(strong("Adaptive Assessment:"), "The third page dynamically displays questions based on the adaptive algorithm. You can visualize the progress through a graph diagram, which highlights the likelihood of different states with color coding. This diagram helps to understand the current state. At the top of the diagram are the states that contain most of the items (with the whole collection of items on top), and at the bottom of the diagram are the states with fewer items (with the empty set on the bottom)."),
                               tags$li(strong("Results:"), "The final page provides a detailed outcome of the assessment. You'll see the cardinality of the recovered state (the score), the identified state (knowledge or clinical), and insights on potential changes in status. This comprehensive result allows for informed decision-making or further evaluation. Moreover, the result of the test can be downloaded and analyzed using R.")
                             ),
                             p("This app is designed to be a powerful tool for educators, psychologists, and clinicians, providing deep insights through adaptive assessments tailored to each individual's responses. Explore the pages and see how adaptive testing can enhance your understanding of knowledge or clinical states!"),

                             h3("References"),
                             p("Donadello, I., Spoto, A., Sambo, F., Badaloni, S., Granziol, U., & Vidotto, G. (2017). ATS-PD: An Adaptive Testing System for Psychological Disorders. Educational and Psychological Measurement, 77(5), 792-815. ", a(href = "https://doi.org/10.1177/0013164416652188", "https://doi.org/10.1177/0013164416652188")),
                             p("Falmagne, J.C., & Doignon, J.P. (1988). A class of stochastic procedures for the assessment of knowledge. British Journal of Mathematical and Statistical Psychology, 41, 1–23. ", a(href = "https://doi.org/10.1111/j.2044-8317.1988.tb00884.x", "https://doi.org/10.1111/j.2044-8317.1988.tb00884.x"))
                      )
                    )
                    ),

    ###Insert Data Panel ----
    shiny::tabPanel("Upload your Test",
             sidebarLayout(
               sidebarPanel(
                # conditionalPanel(condition = "output.panelCondition2",
                 radioButtons(
                   "data_type",
                   "Which dataset to use?",
                   choices = c("RData" = "rd",
                               "Upload own test (CSV file)" = "upload"),
                   selected = "upload"
                 ),




                 tags$hr(),

                 conditionalPanel(
                   "input.data_type == 'rd'",

                   fileInput('file2', 'Choose a RData File', accept =
                               c('.RData'))

                 ),


                 conditionalPanel(
                   "input.data_type == 'upload'",

                   fileInput(
                     "file1",
                     "Choose CSV File",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                   ),

                   tags$hr(),

                   checkboxInput("rand", "Random", FALSE),


                   checkboxInput("rep", "Repetition", FALSE),

                   tags$hr(),
                   checkboxInput("header", "Header", TRUE),


                   radioButtons(
                     "sep",
                     "Separator",
                     choices = c(
                       Comma = ",",
                       Semicolon = ";",
                       Tab = "\t"
                     ),
                     selected = ";"
                   ),


                   radioButtons(
                     "quote",
                     "Quote",
                     choices = c(
                       None = "",
                       "Double Quote" = '"',
                       "Single Quote" = "'"
                     ),
                     selected = '"'
                   ),


                   radioButtons(
                     "deci",
                     "Decimal",
                     choices = c("Comma" = ',',
                                 "Period" = '.'),
                     selected = '.'
                   ),
                   br(),
                   h3("Download Test .RData"),
                   downloadButton("downloadTest", "Download")

                 ),
               ),

               mainPanel(
                 verbatimTextOutput("data_warning"),
                 conditionalPanel(
                   "input.data_type != 'rd'",
                   h2("Your data"),
                   tableOutput("contents"),
                   tags$hr()
                 ),
                 h2("Test Parameters"),
                 fluidRow(
                   column(3,
                          verbatimTextOutput("structure")),
                   column(3,
                          verbatimTextOutput("parameters")),
                   column(6,
                          verbatimTextOutput("criteria"))
                 ),

                 verbatimTextOutput("item"),
                 fluidRow(
                   column(6,
                          verbatimTextOutput("response_correct")),
                   column(6,
                          verbatimTextOutput("response_incorrect"))
                 )
               )
             )),
    ###Adaptive Assessment Panel ----
    shiny::tabPanel("Adaptive Assessment",
             sidebarLayout(
               sidebarPanel(
                 # Section: Adaptive Assessment Introduction
                 p("Welcome on the adaptive assessment page, you will engage in an adaptive assessment where questions are presented based on your previous answers."),

                 p("At any point, you may choose to restart the assessment by clicking the \"Restart Assessment\" button.", strong("Please note that restarting the assessment will delete all previous responses, resetting the evaluation process entirely"),". This allows you to begin the assessment anew, free from any prior input or results."),

                 actionButton("Restart", "Restart Assessment"),
                 tags$hr(),
                 br(),
                 conditionalPanel(
                   condition = "output.panelCondition2",
                   h3("Visualizing Your Progress"),
                   p("A key feature of this page is the interactive graph that visualizes the structure underlying the assessment. This graph represents each possible state, with color-coding to indicate the probability of each state being your current one based on the responses observed so far."),

                   tags$ul(
                     tags$li("Lighter colors represent states with a lower probability, suggesting they are less likely to match your current knowledge or clinical status."),
                     tags$li("Darker colors represent states with a higher probability, indicating a closer match to your current knowledge or clinical status.")
                   ),

                   p("As you continue to answer questions, the colors of these states will dynamically update, providing you with a real-time visualization of the system's evolving understanding of your current state.")
                 )

               ),
               mainPanel(
                 h2("Adaptive Assessment"),
                 fluidRow(column(
                   6,
                   conditionalPanel(
                     condition = "output.panelCondition",
                     uiOutput("textitem"),
                     uiOutput("textanswer"),
                     actionButton("Go", "Confirm")
                   ),
                   conditionalPanel(
                     condition = "!output.panelCondition",
                     h4("The assessment reached the termination criteria")
                   )
                 )),

                 tags$hr(),
                 br(),
                 conditionalPanel(
                   condition = "output.panelCondition2",
                   h2("Dynamic plot")
                 ),
                 fluidRow(column(6,
                                 plotOutput("hasse")))
               )
             )),
    ###Outcome Panel ----
    shiny::tabPanel("Results",
             sidebarLayout(
               sidebarPanel(p("Welcome to the Results page. Here, you will find a comprehensive summary of the assessment you have completed."),
                            p("The outcome of the assessment is displayed below, providing detailed insights into your performance or condition. You can review your results, analyze your scores, and download them."),
                            tags$hr(),
                            br(),
                            actionButton("O", "Show Me the Output")),
               mainPanel(
                 conditionalPanel("!input.O",
                   h2("Press the 'Show Me the Output' button to display the Report")),
                         fluidRow(
                           column(
                             6,
                             h3("The score"),
                             uiOutput("score"),
                             br(),
                             h3("Your current status"),
                             uiOutput("State"),
                             br(),
                             h3("What can change in your status"),
                             uiOutput("Fringe"),
                             br(),
                             h3("Download"),
                             downloadButton("downloadData", "Download")
                           )
                         ))
             ))

  ),

##Server Side ----
  server = function(input, output, session) {

    mydf <- reactive({

      tryCatch({
        if (input$data_type == "upload") {
          req(input$file1)
          mydf_0 = read.table(
            input$file1$datapath,
            header = input$header,
            sep = input$sep,
            quote = input$quote,
            dec = input$deci
          )

        } else if (input$data_type == "rd") {
          req(input$file2)
          #sessionEnvir <- sys.frame()
          mydf_0 = loadRData(input$file2$datapath)
        }
        return(mydf_0)
      }, error=function(e){
        #cat("Data are not OK")
      }, warning = function(w) {
        if(grepl("Something should be changed", w, fixed = TRUE)){return(w$message)}
      }
      )
    })

    textq <- NULL

    mydfname = reactive({
      req(input$file1)
      gsub("\\.csv$", "", basename(input$file1$name))
    })


    output$data_warning <- renderPrint({
      if(is.null(mydf())){
        print("Data are not ok")
      }else{
        print("Data are ok")
      }
    })

    ############## Once the data are uploaded, they are showed in a nice format
    output$contents <-
      renderTable(if (input$data_type == "upload") {
        mydf()
      } else{
        if (is.null(mydf())) {
          return()
        } else{
          mydf()
        }
      })

    ###Data Upload----

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Result_",  Sys.Date(), ".RData", sep = "")
      },
      content = function(file) {
        token <- token_result
        attr(token, "class") <- "assessment"
        save(token, file = file)
      }
    )
    output$downloadTest <- downloadHandler(
      filename = function() {
        paste("test", Sys.Date(), ".RData", sep = "")
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
      token <<- mydf()
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
        } else if (is.data.frame(token) &
                   ncol(token) > 1 &
                   any(grepl('correct', tolower(names(token)))))  # The variable token contains the list of the question and eventually  the skill map
        {

          adaptive <- TRUE
          repetition <- input$rep
          labels <- names(token)
          index <- min(which(grepl('correct', tolower(labels)))) - 1

          #Definition of the items and responses text. Since we used delineate it might be that some items are repeat in the csv, therefore the control.
          textq <- token[, 1]
          textr <- list(correct = token[, "correct"],
                        incorrect = apply(token[, grepl('incorrect', tolower(labels))], 1, my_unlist))
          textr$correct <-
            textr$correct[length(textq) - match(unique(textq), rev(textq)) + 1]
          textr$incorrect <-
            textr$incorrect[length(textq) - match(unique(textq), rev(textq)) + 1]
          textq <-
            textq[length(textq) - match(unique(textq), rev(textq)) + 1]


          if(input$rand==TRUE | index==1) # The random presentation started iif the tick "random" have been selected or no skillmap as been presented (index==2)
          {
            if(length(textq)<26)
            { # Over 25 items it became difficult to compute the powerset therefore the trivial full/empty set structure is used instead.
              states<-expand.grid(rep(list(0:1),length(textq)))
              beta <- rep(0, ncol(states))
              eta <- rep(0, ncol(states))
              SC <- 1
            }else{
              states<-matrix(0, ncol=length(textq),nrow=2)
              states[2,]<-1
              beta <- rep(.5, ncol(states))
              eta <- rep(.5, ncol(states))
              SC <- 1
            }
            repetition <- FALSE
            colnames(states) <- 1:ncol(states)
          }else{
            states <- pks::delineate(token[, 1:index])$K
            colnames(states) <- 1:ncol(states)
            beta <- rep(.15, ncol(states))
            eta <- rep(.15, ncol(states))
            SC <- 0.5
          }

          termination <- "likelihood_maximization"
          og_likelihood <- rep(1, nrow(states)) / nrow(states)
          #Creation of the token object
          token_plot <<- list(
            likelihood = og_likelihood,
            qorder = NULL,
            rorder = NULL,
            kstate = which.max(og_likelihood),
            P_kstate = max(og_likelihood),
            n = 0,
            states = states,
            Stopping_rule = list(rule = termination, criterion = SC),
            Question = list(problems_list = textq, response_lists = textr),
            eta = eta,
            beta = beta,
            Entropy = NA,
            Std_entropy = NA,
            adaptive = adaptive,
            repetition = repetition
          )
          attr(token_plot, "class") <- "assessment"
          #
        } else{
          adaptive <- NULL
        }



      observe({
        req(textq)
        output$structure <- renderPrint({
          cat("Knowledge Structure \n")
          if(nrow(states)<30)
          {
            states
          }else{
            head(states)
          }

        })
        output$item <- renderPrint({
          cat("\nItems\n")
          format(data.frame(question = unlist(textq)),   justify = c("left"))
        })
        output$response_correct <- renderPrint({
          cat("\nCorrect responses\n")
          format(data.frame(answer = unlist(textr$correct)),  justify = c("left"))
        })
        output$response_incorrect <- renderPrint({
          cat("\nIncorrect responses\n")
          textr$incorrect
        })
        output$parameters <- renderPrint({
          cat("Parameters \n")
          format(data.frame(beta, eta),  justify = c("left"))
        })
        output$criteria <- renderPrint({
          cat("Termination criterion: ",
              termination,
              "\n\n with values of ",
              SC,
              "\n\n")

        })
      })

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
            #choices =   sample(resps$a),
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
          token_result<<-token_plot
          ###Plot the hasse diagram only for small structure ----
          if(nrow(states)<200)
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

      observeEvent(input$O, {
        pr_kstate <- which(token_plot$states[token_plot$kstate, ] == 1)
        pr_notkstate <- setdiff(1:ncol(token_plot$states), pr_kstate)
        output$score <-
          renderUI({
            y <- paste0(
              "The assessment ended with  ",
              token_plot$n,
              " questions.<br><strong>The score of the individual is <strong>",
              sum(token_plot$states[token_plot$kstate, ]),
              "<strong> out of <strong>",
              ncol(token_plot$states),
              ".<br> <strong> The score has been estimate on <strong>",
              sum(token_plot$rorder),
              " <strong> observed correct responses and<strong> ",
              length (token_plot$rorder) - sum(token_plot$rorder),
              " <strong>incorrect ones.<strong>"
            )
            HTML(y)
          })

        output$State <- renderUI({
          y <-
            paste0(
              "<strong> The individual current status have a probability of <strong> ",
              round(token_plot$P_kstate, 4),
              "<strong>",
              "<br> <strong> and it contains the following problems:<br><br>",
              paste(token_plot$Question$problems_list[pr_kstate], collapse = "<br><br>"),
              "
                                          <strong>"
            )
          HTML(y)
        })

        output$Fringe <- renderUI({
          y <-
            paste0(
              "<strong> The problems that are not in the current status:<br><br>",
              paste(token_plot$Question$problems_list[pr_notkstate], collapse = "<br><br>"),
              "
                                          <strong>"
            )
          HTML(y)
        })
      })


    })
  }
)
