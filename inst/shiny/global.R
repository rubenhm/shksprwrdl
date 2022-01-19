library(shiny)
library(shinyjs)
library(dplyr)
library(DT)


data('plays_tokens')

#source('../../R/master_mind.R')

allowed_sizes <- c(5, 6, 7)
attempts_sizes <- c(6, 12, 18)

