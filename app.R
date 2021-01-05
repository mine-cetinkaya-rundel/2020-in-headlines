# load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gt)
library(waiter)

# loading ----------------------------------------------------------------------

loading_screen <- tagList(
  img(src = "loading.gif", height = "400px")
)

# load data --------------------------------------------------------------------

top_words_nested <- read_rds("data/top-words-nested.rds")
top_articles <- read_rds("data/top-articles.rds")

# ui ---------------------------------------------------------------------------

ui <- fluidPage(
  use_waiter(),
  waiter_show_on_load(html = loading_screen, color = "white"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "headlines.css"),
    tags$link(rel = "shortcut icon", href = "icons8-poo.ico")
  ),

  br(),

  titlePanel(title = NULL, windowTitle = "2020 in Headlines"),
  sidebarLayout(
    sidebarPanel(
      style = "text-align: justify; background-color: #f8f8f8",
      div(
        style = "margin-bottom: 10px; background-image: url('2020.png'); background-repeat: no-repeat; background-size: 80%; background-position: center;",
        uiOutput("links", inline = TRUE),
        tags$img(height = 16, width = 16, src = "icons8-poo.svg"),
      ),
      hr(),
      div(
        style = "margin-top: 10px; font-size:14px; font-family: Arial;",
        HTML(
          paste0(
            "Created by ",
            a(href = "https://twitter.com/iowio", target = "_blank", "Müge Çetinkaya"),
            " and ",
            a(href = "https://twitter.com/minebocek", target = "_blank", "Mine Çetinkaya-Rundel"),
            ". Thanks to The New York Times for making their data easily accessible! Source code available ",
            a(href = "https://github.com/mine-cetinkaya-rundel/2020-in-headlines", target = "_blank", "here"),
            " and blog post ",
            a(href = "https://mugecetinkaya.medium.com/this-is-how-you-create-a-one-year-long-newspaper-8a3088e2b050", target = "_blank", "here"),
            "."
          )
        )
      ),
      div(
        style = "margin-top: 10px; font-size:14px; font-family: Arial;",
        "If you're on mobile, this app might look as shitty as 2020. Sorry, but this is certainly not the worst thing that happened in 2020."
      )
    ),
    mainPanel(
      div(
        style = "text-align: justify; margin-bottom: 10px; font-size:14px; font-family: Arial;",
        "Words that topped The New York Times headlines, organized by month of first appearance. Select to see what they were about."
      ),
      hr(),
      div(
        style = "margin-bottom: 10px;",
        gt_output("articles_table")
      )
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {

  # for the sidebar panel
  action_links <- pmap(
    top_words_nested,
    function(month, words) {
      links <- imap(
        words, ~ list(tags$div(class = month, actionLink(paste0(month, "_", .y), .x), " · "))
      )
      c(list(tags$div(class = "month", tags$b(toupper(as.character(month))))), " · ", links)
    }
  ) %>% unlist(recursive = FALSE)

  output$links <- renderUI(action_links)

  # for the main panel
  pwalk(
    top_words_nested,
    function(month, words) {
      iwalk(
        words, ~ observeEvent(
          input[[paste0(month, "_", .y)]],
          {
            output$articles_table <- render_gt(
              {
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
                    article = gt::html(paste0('When <span style="color:#D5AB39;font-style:normal">', .x, "</span> first topped The New York Times headlines..."))
                  ) %>%
                  tab_style(
                    style = cell_text(color = "#B5B5B5"),
                    locations = cells_row_groups()
                  )
              },
              height = px(700)
            )
          },
          ignoreInit = TRUE
        )
      )
    }
  )

  waiter_hide()
}

# create shiny app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
