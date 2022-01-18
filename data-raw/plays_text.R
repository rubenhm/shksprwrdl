## code to prepare `plays_text` dataset goes here

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(tibble)

# Download xlm files form http://www.ibiblio.org/xml/examples/shakespeare/

url <- 'http://www.ibiblio.org/xml/examples/shakespeare/'


# Read index page
page0 <- rvest::read_html(url)

# Extract url to individual plays
links <- page0 %>%
  html_nodes('li>a') %>%
  html_attr('href')

# Form urls of individual plays
urls <- paste0(url,  links)

# Download plays and parse to data frame


parse_play <- function(url) {
  # Plays are xml files with up to four levels of nesting
  # play>act>scene>speech>line>text
  # browser()

  # Initialize empty tibble
  play_df <- tibble::tibble(
    play    = character(0),
    act     = character(0),
    scene   = character(0),
    speaker = character(0),
    line    =  character(0)
  )

  # Read raw xml
  play_xml <- read_xml(url)

  # Find all children of play
  play <- xml_children(play_xml)

  # play title
  play_title <- play[[which(xml_name(play) == 'TITLE')]] %>% xml_text()


  # Find acts
  acts <- which(xml_name(play) == 'ACT')


  # Recurse through acts
  for (i in acts) {

    # Find children of act i
    act <- xml_children(play[[i]])

    # act title
    act_title <- act[[which(xml_name(act) == "TITLE")]]  %>% xml_text()

    # Determine scenes indexes
    scenes <- which(xml_name(act) == "SCENE" | xml_name(act) == "EPILOGUE")

    # Examine scenes
    for (j in scenes) {

      # find children of scene j
      scene <- xml_children(act[[j]])

      # scene title
      scene_title <- scene[[which(xml_name(scene) == 'TITLE')]] %>% xml_text()

      # Determine speech indexes
      speeches <- which(xml_name(scene) == 'SPEECH')

      # Examine speeches
      for (k in speeches) {

        # Find children of speech k
        speech <- xml_children(scene[[k]])

        # print(paste0('i= ', i, ', j= ', j, ', k= ', k))

        # speaker name
        speaker_name <- sapply(which(xml_name(speech) == 'SPEAKER'),
                               function(x) {
                                 speech[[x]] %>% xml_text()
                               }) %>% paste(collapse = ", ")

        # Determine lines indexes
        lines <- which(xml_name(speech) == 'LINE')

        # Examine lines
        for (l in lines) {

          # print(paste0('i= ', i, ', j= ', j, ', k= ', k, ', l= ', l))

          # line text
          line_text <- speech[[l]] %>% xml_text()

          # Append row to tibble
          play_df <- play_df %>% tibble::add_row(
            play     = play_title,
            act      = act_title,
            scene    = scene_title,
            speaker  = speaker_name,
            line     = line_text
          )

        }
      }

    }
  }
  return( play_df )
}

df_plays <- purrr::map(urls,
                          function(x) {
                            dt <- tryCatch(
                              {
                                parse_play(x)
                              },
                              error = function(e){
                                message(paste0('Error with play: ', x))
                                message('Original error:')
                                message(e)
                                return(NA)
                              },
                              warning = function(w) {
                                message(psate0('Warning with play: ', x))
                                message('Original warning:')
                                message(w)
                                return(NULL)
                              },
                              finally = {
                                message(paste0('Processed play: ', x))
                              }
                            )
                          })


plays_text <- bind_rows(df_plays)

usethis::use_data(plays_text, overwrite = TRUE)

