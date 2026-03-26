ratio_table_formatting <- function(df,
                                   search = FALSE,
                                   page = FALSE,
                                   ratio = "") {

  if (ratio == "primary_reserve") {
    row_names = c("Expendable Net Assets", 
                  "Total Expenses",
                  "Primary Reserve Ratio")
  }
  else if (ratio == "viability") {
    row_names = c("Expendable Net Assets",
                  "Plant Related Debt",
                  "Viability Ratio")
  }
  else if (ratio == "return_on_net_assets") {
    row_names = c("Change in Net Assets",
                  "Total Net Assets",
                  "Return on Net Assets Ratio")
  }
  else if (ratio == "net_operating_revenues") {
    row_names = c("Excess (deficiency) of unrestricted operating revenues over unrestricted operating expenses",
                  "Total unrestricted operating revenue",
                  "Net Operating Revenues Ratio")
  }
  else {stop("ratio parameter must be one of: primary_reserve, viability, return_on_net_assets, or net_operating_revenues")}
  
  
  df %>% 
    select(-1) %>% 
     DT::datatable(extensions = 'Buttons',
                   rownames = row_names,
                   options = list(
                     paging = page,
                     searching = search,
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     ordering = TRUE,
                     dom = 'ftBlp',
                     buttons = c('copy', 'csv')
                   ),
                   class = "compact",
                   selection = list(mode = "single",
                                    selected = 3,
                                    target = "row")) %>% 
     formatCurrency(.,
                    columns = colnames(df)[grep("2", x = colnames(df))],
                    rows = c(1,2)) %>% 
     formatStyle(.,
                 columns = 0,
                 fontWeight = "bold")
}
