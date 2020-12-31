rsconnect::deployApp(
  appName = "2020-in-headlines",
  appTitle = "2020 in headlines",
  account = "minecr",
  forceUpdate = TRUE,
  launch.browser = TRUE,
  appFiles = c("app.R",
               "data/top-articles.rds",
               "data/top-words-nested.rds",
               "www/2020.png",
               "www/headlines.css",
               "www/icons8-poo.svg",
               "www/loading.gif")
)

# instance size - largest possible
# max worker - 2
# max connections - 20
# start count - 25