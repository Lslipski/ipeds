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

min_card_height = 350

# Links
link_scr <- tags$a("Strategic Corporate Research", href = "https://strategiccorporateresearch.org/", target = "_blank")
link_ipeds <- tags$a("IPEDS", href = "https://nces.ed.gov/ipeds", target = "_blank")

# Header
my_title <- "IPEDS Data"

# data
df_f1a <- load_public_financial(years)
df_f2 <- load_nfp_financial(years)
df_f3 <- load_private_financial(years)
df_directory <- load_directory(years) %>% 
  mutate_if(is.character, utf8::utf8_encode)



# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  
  title = my_title,
  
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
  

    bslib::navset_tab(
      nav_panel(
        title = "Public",
        card(
          min_height = min_card_height,
          card_header(
            "Primary Reserve Ratio"
          ),
          card_body("The primary reserve ratio divides the expendable net assets by the total expenses of the university and 
                    gives an estimate of..."),
          gt_output('tbl_primary_reserve')
        ),
        
        card(
          min_height = min_card_height,
          card_header(
            "Net Asset Ratio"
          ),
          card_body("The net asset ratio divides the change in net assets by the total net assets of the university and 
                    gives an estimate of..."),
          gt_output('tbl_net_assets')
        ),
      
        card(
          min_height = min_card_height,
          card_header(
            "Net Operating Revenue Ratio"
          ),
          card_body("The net operating revenue ratio divides the operating income by the revenue of the university and 
                    gives an estimate of..."),
          gt_output('tbl_net_op_rev')
        ),
      
        card(
          min_height = min_card_height,
          card_header(
            "Viability Ratio"
          ),
          card_body("The viability ratio divides the expendable net assets by the long term debt of the university and 
                    gives an estimate of..."),
          gt_output('tbl_viability')
        )
    ), # close nav_panel public institutions
    
    nav_panel(
      title = "Private",
      p("Under Construction")
    ), # close nave_panel private institutions
    
    nav_panel(
      title = "Not-for-Profit",
      card(
        min_height = min_card_height,
        card_header(
          "Primary Reserve Ratio"
        ),
        card_body("The primary reserve ratio divides the unrestricted net assets by the total expenses of the university and 
                    gives an estimate of..."),
        gt_output('tbl_nfp_primary_reserve')
      ),
      card(
        min_height = min_card_height,
        card_header(
          "Net Income Ratio"
        ),
        card_body("The net income ratio divides the change in net assets by the total revenues and investment return of the university and 
                    gives an estimate of..."),
        gt_output('tbl_nfp_net_income')
      ),
      card(
        min_height = min_card_height,
        card_header(
          "Net Operating Revenue Ratio"
        ),
        card_body("The net operating revenue ratio divides the net assets, end of year by the total assets of the university and 
                    gives an estimate of..."),
        gt_output('tbl_nfp_net_op_rev')
      ),
      card(
        min_height = min_card_height,
        card_header(
          "Viability Ratio"
        ),
        card_body("The viability ratio divides the unrestricted net assets by the debt related to property, plant, and equipment of the university and 
                    gives an estimate of..."),
        gt_output('tbl_nfp_viability')
      )
      
      
    ), # close nave_panel not for profit institutions
    
    nav_spacer(), # push link menu to right side
    
    nav_menu(
      title = "Links",
      nav_item(link_scr),
      nav_item(link_ipeds)
    ) # close nav menu
    
  ) # close navset_tab
) # close page_sidebar



# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
# PUBLIC UNIVERSITIES --------------------------------------------------------------------
  
# create reactive data frame for public universities
  df_pub <- reactive({
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
    df_pub() %>% 
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
      fmt_currency(columns = everything(),
                   rows = c("expendable_net_assets",
                            "total_expenses"),
                   decimals = 0) %>% 
      data_color(
        columns = everything(),
        rows = "primary_reserve_ratio",
        palette = "#007FFF"
      )
  })
  
  
  output$tbl_net_assets <- render_gt({
    df_pub() %>% 
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
      fmt_currency(columns = everything(),
                   rows = c("change_in_net_assets",
                            "total_net_asssets"),
                   decimals = 0) %>% 
      data_color(
        columns = everything(),
        rows = "net_assets_ratio",
        palette = "#99EDFF"
      )
  })
  
  
  output$tbl_net_op_rev<- render_gt({
    df_pub() %>% 
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
      fmt_currency(columns = everything(),
                   rows = c("operating_income",
                            "revenue"),
                   decimals = 0) %>% 
      data_color(
        columns = everything(),
        method = "numeric",
        rows = "net_operating_revenue_ratio",
        palette = "#FFEE99"
      )
  })
  
  output$tbl_viability <- render_gt({
    df_pub() %>% 
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
      fmt_currency(columns = everything(),
                   rows = c("expendable_net_assets",
                            "long_term_debt"),
                   decimals = 0) %>% 
      data_color(
        columns = everything(),
        method = "numeric",
        rows = "viability_ratio",
        palette = "#FF7F00"
      )
  })
  
  
# PRIVATE FOR PROFIT UNIVERSITIES --------------------------------------------------------------------
  # create reactive data frame for public universities
  # df_priv <- reactive({
  #   req(input$this_college)
  #   req(input$this_year)
  #   df_f3 %>% 
  #     dplyr::left_join(df_directory,
  #                      by = c("unitid" = "unitid",
  #                             "year" = "year")) %>% 
  #     dplyr::filter(instnm == input$this_college,
  #                   year %in% input$this_year) %>% 
  #     dplyr::select(year,
  #                   unitid,
  #                   instnm,
  #                   unrestricted_net_assets = f3a04) %>%
  #     dplyr::mutate(primary_reserve_ratio = round(expendable_net_assets / total_expenses, 2),
  #                   net_assets_ratio = round(change_in_net_assets / total_net_asssets, 2),
  #                   operating_income = op_inc_b09 - op_inc_c110,
  #                   net_operating_revenue_ratio = round(operating_income / revenue, 2),
  #                   viability_ratio = round(expendable_net_assets / long_term_debt, 2)) %>%
  #     dplyr::select(-unitid,
  #                   -instnm)
  #   
  # })
  # 
  # output$tbl_priv_primary_reserve <- render_gt({
  #   df_pub() %>% 
  #     dplyr::select(year,
  #                   expendable_net_assets,
  #                   total_expenses,
  #                   primary_reserve_ratio) %>% 
  #     tidyr::pivot_longer(names_to = "column",
  #                         values_to = "values",
  #                         cols = -c("year"),
  #                         values_transform = list(values = as.character)) %>% 
  #     dplyr::mutate(values = as.numeric(values)) %>% 
  #     dplyr::arrange(desc(year)) %>% 
  #     tidyr::pivot_wider(names_from = "year",
  #                        values_from = "values") %>% 
  #     gt(
  #       rowname_col = "column"
  #     ) %>% 
  #     tab_header(title = input$this_college) %>% 
  #     tab_spanner(
  #       label = "Year",
  #       columns = sort(input$this_year, decreasing = T)
  #     ) %>%
  #     fmt_number(
  #       columns = starts_with("2"),
  #       rows = c("expendable_net_assets",
  #                "total_expenses"),
  #       decimals = 0,
  #       use_seps = TRUE
  #     ) %>% 
  #     fmt_currency(columns = everything(),
  #                  rows = c("expendable_net_assets",
  #                           "total_expenses"),
  #                  decimals = 0) %>% 
  #     data_color(
  #       columns = everything(),
  #       rows = "primary_reserve_ratio",
  #       palette = "#007FFF"
  #     )
  # })
  
  
  
  
  
  
  # PRIVATE NOT-FOR-PROFIT UNIVERSITIES --------------------------------------------------------------------
  
  # create reactive data frame for public universities
  df_nfp <- reactive({
    req(input$this_college)
    req(input$this_year)
    df_f2 %>% 
      dplyr::left_join(df_directory,
                       by = c("unitid" = "unitid",
                              "year" = "year")) %>% 
      dplyr::filter(instnm == input$this_college,
                    year %in% input$this_year) %>% 
      dplyr::select(year,
                    unitid,
                    instnm,
                    unrestricted_net_assets =f2a04,
                    total_expenses = f2b02,
                    change_in_net_assets = f2b04,
                    total_revenues_and_investment_return = f2b01,
                    net_assets_end_of_year = f2b07,
                    total_assets = f2a02,
                    debt_property_plant_equipment = f2a03a) %>%
      dplyr::mutate(primary_reserve_ratio = round(unrestricted_net_assets / total_expenses, 2),
                    net_income_ratio = round(change_in_net_assets / total_revenues_and_investment_return, 2),
                    net_operating_revenue_ratio = round(net_assets_end_of_year / total_assets, 2),
                    viability_ratio = round(unrestricted_net_assets / debt_property_plant_equipment, 2)) %>%
      dplyr::select(-unitid,
                    -instnm)
    
  })
  
  
  output$tbl_nfp_primary_reserve <- render_gt({
    df_nfp() %>% 
      dplyr::select(year,
                    unrestricted_net_assets,
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
      gt(data = .,
         rowname_col = "column"
      ) %>% 
      tab_header(title = input$this_college) %>% 
      tab_spanner(
        label = "Year",
        columns = sort(input$this_year, decreasing = T)
      ) %>%
      fmt_number(
        columns = starts_with("2"),
        rows = c("unrestricted_net_assets",
                 "total_expenses"),
        decimals = 0,
        use_seps = TRUE
      ) %>%
      fmt_currency(columns = everything(),
                   rows = c("unrestricted_net_assets",
                            "total_expenses"),
                   decimals = 0) %>%
      data_color(
        columns = everything(),
        rows = "primary_reserve_ratio",
        palette = "#A50021"
      )
  })
  
  
  output$tbl_nfp_net_income <- render_gt({
    df_nfp() %>% 
      dplyr::select(year,
                    change_in_net_assets,
                    total_revenues_and_investment_return,
                    net_income_ratio) %>% 
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
                 "total_revenues_and_investment_return"),
        decimals = 0,
        use_seps = TRUE
      ) %>% 
      fmt_currency(columns = everything(),
                   rows = c("change_in_net_assets",
                            "total_revenues_and_investment_return"),
                   decimals = 0) %>% 
      data_color(
        columns = everything(),
        rows = "net_income_ratio",
        palette = "#F76D5E"
      )
  })
  
  
  
  output$tbl_nfp_net_op_rev<- render_gt({
    df_nfp() %>% 
      dplyr::select(year,
                    net_assets_end_of_year,
                    total_assets,
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
        rows = c("net_assets_end_of_year",
                 "total_assets"),
        decimals = 0,
        use_seps = TRUE
      ) %>% 
      fmt_currency(columns = everything(),
                   rows = c("net_assets_end_of_year",
                            "total_assets"),
                   decimals = 0) %>% 
      data_color(
        columns = everything(),
        method = "numeric",
        rows = "net_operating_revenue_ratio",
        palette = "#FFE099"
      )
  })
  
  
  output$tbl_nfp_viability <- render_gt({
    df_nfp() %>% 
      dplyr::select(year,
                    unrestricted_net_assets,
                    debt_property_plant_equipment,
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
        rows = c("unrestricted_net_assets",
                 "debt_property_plant_equipment"),
        decimals = 0,
        use_seps = TRUE
      ) %>% 
      fmt_currency(columns = everything(),
                   rows = c("unrestricted_net_assets",
                            "debt_property_plant_equipment"),
                   decimals = 0) %>% 
      data_color(
        columns = everything(),
        method = "numeric",
        rows = "viability_ratio",
        palette = "#FF7F00"
      )
  })
  
  
  
  
  
  
  
} # close server






# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)






























