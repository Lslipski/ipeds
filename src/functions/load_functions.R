# load_functions.R

load_functions <- function() {
  file.sources = list.files(path = here::here("src/functions"),
                            pattern = "*.R",
                            full.names = TRUE)
  sapply(file.sources, source)
  
  return("All local functions loaded.")
}