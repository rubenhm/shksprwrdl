source('global.R')

ui <- fluidPage(
  title = 'shksprwordl',
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet"),
  ),
  titlePanel("Shksprwordl: A wordle clone with Shakespeare's words"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column = 4,
        hr(),
        p("Shksprwordl is a clone of the wordle game that uses",
          "words from Shakespeare's plays",br(),
          "the game uses the same color scoring as wordle",br(),
          "green denotes characters in the correct position",br(),
          "yellow denotes characters in the word but in the wrong position"),
        wellPanel(
          id = "leftPanel",
          selectInput("wordSize", "Word length",allowed_sizes),
          selectInput("trySize", "Number of attempts",attempts_sizes),
          actionButton(inputId = "newgameButton",
                       label = "New game", class="btn btn-success action-button")
        ),
        wellPanel(
          tags$br(),
          tags$button("Restart", id="restart", type="button", class="btn btn-danger action-button", onclick="history.go(0)"),
        ),
      )
    ),
  mainPanel(
    conditionalPanel(
      'input.newgameButton != 0',
      uiOutput("game")
    )
  )
  )
)


server <- function(input, output, session) {
  output$game <- renderUI(
    tagList(
      column(4,
             textInput("tryword", "Try word", value = "", width = NULL, placeholder = NULL),
             actionButton(inputId = "submit",
                   label = "Submit word", class="btn btn-success action-button"),
             hr(),
             p("Number of attempts:"),
             textOutput('numberAttempts')
      ),
      column(6, dataTableOutput("table"))
    )
  )

  initial_table <- reactive({
    # Initialize table
    n_rows <- input$trySize %>% as.numeric()
    n_cols <- input$wordSize %>% as.numeric()
    dt <- matrix(data = rep('', n_rows * n_cols), nrow = n_rows) %>%
      tibble::as_tibble(.name_repair = 'unique')
  })

  # select word
  true_word <- reactiveValues(word = '')


  attempts <- reactiveValues(countervalue = 0)

  RV <- reactiveValues(data = NULL)

  observeEvent(input$newgameButton,{

               #browser()
               # Initialize data table
               RV$data <- initial_table()

               # Initialize true word
               n_cols <- input$wordSize %>% as.numeric()
               short_list <- plays_tokens %>%
                 dplyr::filter(nchar == n_cols)
               # select one word at random
               true_word$word <- sample(short_list$word, 1)

               print(true_word$word)
              })

  score <- reactiveValues(value = '')

  # update the data
  observeEvent(input$submit, {

    # collect try word
    try_word <- input$tryword

    # score
    score$value <- calculate_colors(try_word, true_word$word)

    # update counter
    attempts$countervalue <- attempts$countervalue + 1

    # update data table
    new_row <- as.list(toupper(names(score$value))) %>%
      tibble::as_tibble(.name_repair = 'unique')

    RV$data[attempts$countervalue,] <- new_row

    print(try_word)
    print(true_word$word)
    print(RV$data)

  })

  output$numberAttempts <- renderText({
    attempts$countervalue
  })

  output$table <- DT::renderDataTable({

    if (attempts$countervalue == 0) {
      RV$data %>%
        datatable(rownames = TRUE, colnames = rep('', length(RV$data)),
                  # https://stackoverflow.com/questions/35624413/remove-search-option-but-leave-search-columns-option
                  options = list(dom = 't')) %>%
        formatStyle(target = 'cell',
                    columns = names(RV$data),
                    color = 'white',
                    backgroundColor = 'gray',
                    border = '1px solid white' )

    } else {

        RV$data %>%
        datatable(rownames = TRUE, colnames = rep('', length(RV$data)),
                  # https://stackoverflow.com/questions/35624413/remove-search-option-but-leave-search-columns-option
                  options = list(dom = 't')) %>%
          formatStyle(target = 'cell',
                      columns = names(RV$data),
                      color = 'white',
                      backgroundColor = styleEqual(toupper(names(score$value)),
                                                   score$value),
                      border = '1px solid white' )
    }
  })
}


shinyApp(ui = ui, server = server)
