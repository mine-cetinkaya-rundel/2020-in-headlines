# load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gt)

# load data --------------------------------------------------------------------

top_words_nested <- read_rds("data/top-words-nested.rds")
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
                tab_style(
                  style = cell_text(color = "#838383", style = "italic", size = "18px", weight = "bold"),
                  locations = cells_column_labels("article")
                ) %>%
                cols_label(
                  article = gt::html(paste0('When <span style="color:#D5AB39;font-style:normal">', .x, "</span> first topped New York Times headlines..."))
                ) %>%
                tab_style(
                  style = cell_text(color = "#B5B5B5"),
                  locations = cells_row_groups()
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
