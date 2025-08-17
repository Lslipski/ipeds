library(shiny)
library(tidyverse)
library(janitor)
library(bslib)
library(gt)
source(here::here('src/functions/load_functions.R'))
load_functions()


# Load data --------------------------------------------------------------------

years = c("2017",
          "2018",
          "2019",
          "2020",
          "2021",
          "2022", 
          "2023")


df_f1a <- load_public_financial(years)
df_directory <- load_directory(years) %>% 
  mutate_if(is.character, utf8::utf8_encode)



# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  
  
  sidebar = sidebar(
    
    # Choose University
    selectInput(inputId = "this_college",
                label = "University",
                choices = df_directory %>% 
                  distinct(instnm),
                multiple = FALSE),
    
    # Choose Years
    checkboxGroupInput(inputId = "this_year", 
                label = "Year",
                choices = years,
                selected = years),
  ),
  
  card(
    card_header(
      "Primary Reserve Ratio"
    ),
    gt_output('tbl_primary_reserve')
  ),
  
  card(
    card_header(
      "Net Asset Ratio"
    ),
    gt_output('tbl_net_assets')
  ),

  card(
    card_header(
      "Net Operating Revenue Ratio"
    ),
    gt_output('tbl_net_op_rev')
  ),

  card(
    card_header(
      "Viability Ratio"
    ),
    gt_output('tbl_viability')
  )
  
  
)



# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
# create reactive data frame
  df <- reactive({
    req(input$this_college)
    req(input$this_year)
    df_f1a %>% 
      dplyr::left_join(df_directory,
                       by = c("unitid" = "unitid",
                              "year" = "year")) %>% 
      dplyr::filter(instnm == input$this_college,
                    year %in% input$this_year) %>% 
      dplyr::select(year,
                    unitid,
                    instnm,
                    expendable_net_assets =f1a17,
                    total_expenses = f1d02,
                    change_in_net_assets = f1d03,
                    total_net_asssets = f1a14,
                    op_inc_b09 = f1b09,
                    op_inc_c110 = f1c101,
                    revenue = f1b25,
                    expendable_net_assets = f1a17,
                    long_term_debt = f1a10) %>%
      dplyr::mutate(primary_reserve_ratio = round(expendable_net_assets / total_expenses, 2),
                    net_assets_ratio = round(change_in_net_assets / total_net_asssets, 2),
                    operating_income = op_inc_b09 - op_inc_c110,
                    net_operating_revenue_ratio = round(operating_income / revenue, 2),
                    viability_ratio = round(expendable_net_assets / long_term_debt, 2)) %>%
      dplyr::select(-unitid,
                    -instnm)
    
  })
  
  output$tbl_primary_reserve <- render_gt({
    df() %>% 
      dplyr::select(year,
                    expendable_net_assets,
                    total_expenses,
                    primary_reserve_ratio) %>% 
      tidyr::pivot_longer(names_to = "column",
                          values_to = "values",
                          cols = -c("year"),
                          values_transform = list(values = as.character)) %>% 
      dplyr::mutate(values = as.numeric(values)) %>% 
      dplyr::arrange(desc(year)) %>% 
      tidyr::pivot_wider(names_from = "year",
                  values_from = "values") %>% 
      gt(
        rowname_col = "column"
      ) %>% 
      tab_header(title = input$this_college) %>% 
      tab_spanner(
        label = "Year",
        columns = sort(input$this_year, decreasing = T)
      ) %>%
      fmt_number(
        columns = starts_with("2"),
        rows = c("expendable_net_assets",
                 "total_expenses"),
        decimals = 0,
        use_seps = TRUE
      ) %>% 
      data_color(
        columns = everything(),
        rows = "primary_reserve_ratio",
        palette = "plasma"
      )
  })
  
  
  output$tbl_net_assets <- render_gt({
    df() %>% 
      dplyr::select(year,
                    change_in_net_assets,
                    total_net_asssets,
                    net_assets_ratio) %>% 
      tidyr::pivot_longer(names_to = "column",
                          values_to = "values",
                          cols = -c("year"),
                          values_transform = list(values = as.character)) %>% 
      dplyr::mutate(values = as.numeric(values)) %>% 
      dplyr::arrange(desc(year)) %>% 
      tidyr::pivot_wider(names_from = "year",
                         values_from = "values") %>% 
      gt(
        rowname_col = "column"
      ) %>% 
      tab_header(title = input$this_college) %>% 
      tab_spanner(
        label = "Year",
        columns = sort(input$this_year, decreasing = T)
      ) %>%
      fmt_number(
        columns = starts_with("2"),
        rows = c("change_in_net_assets",
                 "total_net_asssets"),
        decimals = 0,
        use_seps = TRUE
      ) %>% 
      data_color(
        columns = everything(),
        rows = "net_assets_ratio",
        palette = "plasma"
      )
  })
  
  
  output$tbl_net_op_rev<- render_gt({
    df() %>% 
      dplyr::select(year,
                    operating_income,
                    revenue,
                    net_operating_revenue_ratio) %>% 
      tidyr::pivot_longer(names_to = "column",
                          values_to = "values",
                          cols = -c("year"),
                          values_transform = list(values = as.character)) %>% 
      dplyr::mutate(values = as.numeric(values)) %>% 
      dplyr::arrange(desc(year)) %>% 
      tidyr::pivot_wider(names_from = "year",
                         values_from = "values") %>% 
      gt(
        rowname_col = "column"
      ) %>% 
      tab_header(title = input$this_college) %>% 
      tab_spanner(
        label = "Year",
        columns = sort(input$this_year, decreasing = T)
      ) %>%
      fmt_number(
        columns = starts_with("2"),
        rows = c("operating_income",
                 "revenue"),
        decimals = 0,
        use_seps = TRUE
      ) %>% 
      data_color(
        columns = everything(),
        method = "numeric",
        rows = "net_operating_revenue_ratio",
        palette = "plasma"
      )
  })
  
  output$tbl_viability <- render_gt({
    df() %>% 
      dplyr::select(year,
                    expendable_net_assets,
                    long_term_debt,
                    viability_ratio) %>% 
      tidyr::pivot_longer(names_to = "column",
                          values_to = "values",
                          cols = -c("year"),
                          values_transform = list(values = as.character)) %>% 
      dplyr::mutate(values = as.numeric(values)) %>% 
      dplyr::arrange(desc(year)) %>% 
      tidyr::pivot_wider(names_from = "year",
                         values_from = "values") %>% 
      gt(
        rowname_col = "column"
      ) %>% 
      tab_header(title = input$this_college) %>% 
      tab_spanner(
        label = "Year",
        columns = sort(input$this_year, decreasing = T)
      ) %>%
      fmt_number(
        columns = starts_with("2"),
        rows = c("expendable_net_assets",
                 "long_term_debt"),
        decimals = 0,
        use_seps = TRUE
      ) %>% 
      data_color(
        columns = everything(),
        method = "numeric",
        rows = "viability_ratio",
        palette = "plasma"
      )
  })
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)






























