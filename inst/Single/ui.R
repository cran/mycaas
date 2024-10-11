ui <-  shiny::navbarPage(
  " ",
  theme = shinythemes::shinytheme("superhero"),
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  ###Adaptive Assessment Panel ----
  shiny::tabPanel("AA",
                  shiny::sidebarLayout(
                    shiny:: sidebarPanel(
                      shiny::p("Welcome on this assessment session!"),
                      shiny::p("Please take your time to carefully consider each question. Once you confirm your response, you will not be able to go back or change your answer. Good luck, and let’s get started!"),
                      shiny::tags$hr()
                    ),
                    shiny::mainPanel(
                      shiny::h2("Adaptive Assessment"),
                      shiny::fluidRow(shiny::column(
                        6,
                        shiny::conditionalPanel(
                          condition = "output.panelCondition",
                          shiny::uiOutput("textitem"),
                          shiny::uiOutput("textanswer"),
                          shiny::actionButton("Go", "Confirm")
                        ),
                        shiny::conditionalPanel(
                          condition = "!output.panelCondition",
                          shiny::h4("The assessment reached the termination criteria"),
                          shiny::br(),
                          shiny::h3("Download"),
                          shiny::downloadButton("downloadData", "Download")
                        )
                      )),

                      shiny::tags$hr(),
                      shiny::br()
                    )
                  ))

)
