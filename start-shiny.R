# Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn

# Script that starts the shiny webserver
# Parameters are supplied using environment variables
assign(".lib.loc", Sys.getenv("R_LIB_PATHS"), envir = environment(.libPaths))

trace(utils::download.file, tracer = quote(print(list(url = url, destfile = destfile))), print = FALSE)
options(download.file.method = "libcurl")

shiny::runApp(
  Sys.getenv("RE_SHINY_PATH"),
  host = "127.0.0.1",
  launch.browser = FALSE,
  port = as.integer(Sys.getenv("RE_SHINY_PORT"))
)
