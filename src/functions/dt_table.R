dt_table <- function(df,
                     filter = TRUE,
                     search = TRUE,
                     page = TRUE,
                     row_nums = TRUE) {
  if(filter) {
    df %>% 
      DT::datatable(extensions = 'Buttons',
                    filter = c('top'),
                    rownames = row_nums,
                    options = list(
                      paging = page,
                      searching = search,
                      fixedColumns = TRUE,
                      autoWidth = TRUE,
                      ordering = TRUE,
                      dom = 'ftBlp',
                      buttons = c('copy', 'csv')
                    ),
                    class = "display")
  } else {
    df %>% 
      DT::datatable(extensions = 'Buttons',
                    rownames = row_nums,
                    options = list(
                      paging = page,
                      searching = search,
                      fixedColumns = TRUE,
                      autoWidth = TRUE,
                      ordering = TRUE,
                      dom = 'ftBlp',
                      buttons = c('copy', 'csv')
                    ),
                    class = "display")
  }
}

# vars is a character vector of variable names to be formatted with commas
# digits is how many past the decimal place to include. Default is rounded to whole number
add_commas <- function(df,
                       vars,
                       digits = 0) {
  df %>% 
    formatCurrency(vars,
                   currency = "", 
                   digits = digits,
                   interval = 3, 
                   mark = ",")
}