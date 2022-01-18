library(shiny)
library(shinyjs)
library(shinyMatrix)
library(DT)

load('../../data/plays_tokens.rda')

source('../../R/master_mind.R')

allowed_sizes <- c(5, 6, 7)
attempts_sizes <- c(6, 12, 18)
