# load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gt)

# load data --------------------------------------------------------------------

top_words_nested <- read_rds("data/top-words-nexted.rds")
top_articles <- read_rds("data/top-articles.rds")

# ui ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "headlines.css")
  ),

  br(),

  sidebarLayout(
    sidebarPanel(
      style = "background-image: url('2020.png'); background-repeat: no-repeat; background-size: 80%; background-position: center; text-align: justify; position: fixed; width: 30%;",
      uiOutput("links", inline = TRUE),
      tags$img(height = 16, width = 16, src = "icons8-poo.svg")
    ),
    mainPanel(
      gt_output("articles_table")
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  ui_elems <- pmap(
    top_words_nested,
    function(month, words) {
      links <- imap(
        words, ~ list(tags$div(class = month, actionLink(paste0(month, "_", .y), .x), " · "))
      )
      c(list(tags$div(class = "month", tags$b(toupper(as.character(month))))), " · ", links)
    }
  ) %>% unlist(recursive = FALSE)

  output$links <- renderUI(ui_elems)

  pwalk(
    top_words_nested,
    function(month, words) {
      iwalk(
        words, ~ observeEvent(
          input[[paste0(month, "_", .y)]],
          {
            output$articles_table <- render_gt({
              cat("month = ", month, "\n")
              cat("month name = ", month.name[month], "\n")
              cat(".x = ", .x, "\n")
              cat(".y = ", .y, "\n")
              top_articles %>%
                filter(
                  month == month.name[month],
                  word == .x
                ) %>%
                distinct(web_url, .keep_all = TRUE) %>%
                rowwise() %>%
                mutate(
                  article = glue::glue('<a href={web_url} target = "blank">{headline}</a>'),
                  article = map(as.character(article), gt::html)
                ) %>%
                ungroup() %>%
                select(-web_url, -headline, -month, -word) %>%
                group_by(date) %>%
                gt() %>%
                cols_merge(
                  columns = vars(article, abstract),
                  pattern = "{1}<br>{2}"
                ) %>%
                cols_align(align = "left") %>%
                tab_style(
                  style = cell_text(color = "black"),
                  locations = cells_body(
                    columns = vars(article)
                  )
                ) %>%
                cols_label(
                  article = paste0('When "', .x, '" first topped New York Times headlines...')
                ) %>%
                tab_options(
                  column_labels.font.size = "18px",
                  column_labels.font.weight = "bold"
                )
            })
          },
          ignoreInit = TRUE
        )
      )
    }
  )
}

# create shiny app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
