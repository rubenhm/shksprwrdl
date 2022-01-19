source('global.R')

ui <- fluidPage(
  title = 'shksprwordl',
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  titlePanel("Shksprwordl: A wordle clone with Shakespeare's words"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column = 4,
        hr(),
        p("Shksprwordl is a clone of the wordle game",br(),
          "that uses words from Shakespeare's plays.",br(),
          "Words with apostrophes are valid,", br(),
          "but it doesn't count toward the letter count,", br(),
          "e.g., ",strong("whoe'er"), "counts as 6 characters.", br(),
          "The game uses the same color scoring as wordle:",br(),
          "green denotes characters in the correct position,",br(),
          "yellow denotes characters in the word but in the wrong position"),
        wellPanel(
          id = "leftPanel",
          selectInput("wordSize", "Word length",allowed_sizes),
          selectInput("trySize", "Number of attempts",attempts_sizes),
          actionButton(inputId = "newgameButton",
                       label = "New game", class = "btn btn-success action-button")
        ),
        wellPanel(
          tags$br(),
          tags$button("Restart", id = "restart", type = "button", class = "btn btn-danger action-button", onclick = "history.go(0)"),
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
                   label = "Submit word", class = "btn btn-success action-button"),
             hr(),
             p("Number of attempts:"),
             textOutput('numberAttempts'),
             shinyjs::hidden(
                div(id = "gameover", title = "Result", width = '200px',
                  span(p('GAME OVER'), style = 'color:red; fontSize:30px;'),
                  hr(),
                  p("The solution is:"),
                  textOutput('solution')
                )
              )
             # ,
             #  shinyjs::hidden(
             #    div(id = "youwon", title = "Result", width = '200px',
             #        span(p('Congratulations, you won!'), style = 'color:red; fontSize:30px;'),
             #        hr(),
             #        p("The solution is:"),
             #        textOutput('solution')
             #    )
             #  )
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

  ReDT <- reactiveValues(dt = NULL)

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

               # Remove '
               true_word$word <- stringr::str_replace(string = true_word$word,
                                                      pattern = "'",
                                                      replacement = "")

               print(true_word$word)

               # Initialize results matrix
               ReDT$dt <- RV$data %>%
                 datatable(rownames = TRUE, colnames = rep('', length(RV$data)),
                           class = 'cell-border',
                           # https://stackoverflow.com/questions/35624413/remove-search-option-but-leave-search-columns-option
                           options = list(dom = 't',
                                          pageLength = as.numeric(input$trySize))) %>%
                 formatStyle(target = 'cell',
                             columns = names(RV$data),
                             color = 'white',
                             backgroundColor = '#615F5B',
                             border = '1px solid white' )

              })



  # update the data
  observeEvent(input$submit, {


    # collect try word
    try_word <- stringr::str_replace(string = input$tryword,
                                     pattern = "'",
                                     replacement = "")

    # update counter
    attempts$countervalue <- attempts$countervalue + 1

    # Verify input
    n_cols <- input$wordSize %>% as.numeric()
    short_list <- plays_tokens %>%
      dplyr::filter(nchar == n_cols)

    if (nchar(try_word) != as.numeric(input$wordSize) | !(try_word %in% short_list$word)) {

      if (nchar(try_word) != as.numeric(input$wordSize)) {
        message_nchar <- paste0('Error: try word should have ',as.numeric(input$wordSize),' characters.')
        print(message_nchar)
      } else {
        message_nchar <- ''
      }
      if (!(try_word %in% short_list$word)) {
        message_dict <- paste0('Error: word ', try_word, ' is not in dictionary.')
        print(message_dict)
      } else {
        message_dict <- ''
      }
      showModal(modalDialog(
        div(tags$b("Invalid input:"),
            tags$br(),
            tags$b(message_nchar),
            tags$br(),
            tags$b(message_dict),
            tags$br(),
            tags$b("Try again."),
            style = "color: red;"
            )        ,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))
      # reset counter
      attempts$countervalue <- attempts$countervalue - 1
    } else {
      # update data table
      new_row <- as.list(toupper(unlist(strsplit(try_word, split = '')))) %>%
        tibble::as_tibble(.name_repair = 'unique')

      RV$data[attempts$countervalue,] <- new_row

      print(try_word)
      print(true_word$word)
      print(RV$data)

      # Update datatable
      # browser()
      ReDT$dt <- RV$data %>%
        datatable(rownames = TRUE, colnames = rep('', length(RV$data)),
                  class = 'cell-border',
                  # https://stackoverflow.com/questions/35624413/remove-search-option-but-leave-search-columns-option
                  options = list(dom = 't',
                                 pageLength = as.numeric(input$trySize))
        ) %>%
        formatStyle(target = 'cell',
                    columns = names(RV$data),
                    color = 'white',
                    fontWeight = 'bold',
                    backgroundColor = '#615F5B',
                    border = '1px solid white' )

      #browser()
      # Update columns

      # Score the entire data table
      score_colors <- score_df(RV$data, true_word$word)

      for (i in 1:length(RV$data)) {

        # Style current column
        ReDT$dt <- ReDT$dt %>%
          formatStyle(target = 'cell',
                      columns = names(RV$data)[i],
                      color = 'white',
                      fontWeight = 'bold',
                      backgroundColor = styleEqual(RV$data[[i]], score_colors[[i]]),
                      border = '1px solid white' )
      }
    }

    # Determine if game is over
    if ((attempts$countervalue == as.numeric(input$trySize) &
        try_word != true_word$word) |
        (attempts$countervalue <= as.numeric(input$trySize) &
         try_word == true_word$word)) {
      # Number of attempts reached
      shinyjs::show(id = 'gameover')
      shinyjs::disable('submit')
    }

  })

  output$numberAttempts <- renderText({
    attempts$countervalue
  })

  output$table <- DT::renderDataTable({
    ReDT$dt
  })

  output$solution <- renderText(({
    toupper(true_word$word)
  }))


}


shinyApp(ui = ui, server = server)
