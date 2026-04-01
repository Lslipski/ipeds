library(shiny)
library(tidyverse)
library(janitor)
library(bslib)
library(gt)
library(DT)
source(here::here('src/functions/load_functions.R'))
load_functions()


# Setup --------------------------------------------------------------------

years = c("2020",
          "2021",
          "2022", 
          "2023")

institution_types = c("Public",
                      #"Private",
                      "Not-for-Profit")

# data
df_f1a <- load_public_financial(years)
df_f2 <- load_nfp_financial(years)
#df_f3 <- load_private_financial(years)
df_directory <- load_directory(years) %>% 
  mutate_if(is.character, utf8::utf8_encode)
df_institutions <- load_institution_types(years)


# Links
link_scr <- tags$a("Strategic Corporate Research", href = "https://strategiccorporateresearch.org/", target = "_blank")
link_ipeds <- tags$a("IPEDS", href = "https://nces.ed.gov/ipeds", target = "_blank")

# Header
my_title <- "IPEDS Data"



# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  
  title = my_title,
  
  sidebar = sidebar(
    
    selectInput(inputId = "this_type",
                label = "Institution Type",
                choices = institution_types,
                selected = "Public",
                multiple = FALSE),
    
    # Choose University
    selectInput(inputId = "this_college",
                label = "University",
                choices = df_institutions[[1]],
                multiple = FALSE),
    
    # Choose Years
    checkboxGroupInput(inputId = "this_year", 
                label = "Year",
                choices = years,
                selected = years),
  ),
  

    bslib::navset_tab(
      nav_panel(
        title = "Key Ratios",
        
        # card(
        #   min_height = min_card_height,
        #   dataTableOutput("checker")
        # ),
        # 
        # Primary Reserve Ratio
        card(
          max_height = 450,
          card_header(
            "Primary Reserve Ratio"
          ),
          card_body(markdown("The primary reserve ratio answers the question: Are resources sufficient and flexible enough to support the mission?"),
          dataTableOutput("tbl_primary_reserve"),
          markdown("What is a good ratio? **0.4 or higher**
                             
                  What is a bad ratio? **0.15 or lower**"))
        ),
        
        # card(
        #   min_height = min_card_height,
        #   card_header(
        #     "Net Asset Ratio"
        #   ),
        #   card_body("The net asset ratio divides the change in net assets by the total net assets of the university and 
        #             gives an estimate of..."),
        #   gt_output('tbl_net_assets')
        # ),
        # 
        
        card(
          max_height = 490,
          card_header(
            "Viability Ratio"
          ),
          card_body(markdown("The viability ratio answers the questions: 
          
          1. Are debt resources managed strategically to advance the mission?
          
          2. Can the institution use its expendable net assets to cover debt?"),
          dataTableOutput('tbl_viability'),
          markdown("A good ratio depends on the firm's goals.  1:1 is nice, but less than that can be fine too if the firm's goals are to..."))
        ),
        
        card(
          max_height = 500,
          card_header(
            "Net Operating Revenues Ratio"
          ),
          card_body(markdown("The Net Operating Revenues Ratio answers the question: Do operating results indicate the institution is living within available resources?"),
          dataTableOutput('tbl_net_op_rev'),
          markdown("A positive ratio means there is an operating surplus for the year. A negative ratio means a loss for the year.
          Generally, the bigger the surplus, the better. However, too big of a surplus might indicate that the institution is not spending enough on mission-critical investments.
          An average of 2-4% over a period of several years for most institutions is good. Your goal should be to find out why there is a surplus or deficit. 
          The institution should aim for long-term equilibrium."))
        )
      ), # close nav_panel key ratios
    
    nav_panel(
      title = "Institution Lookup",
      card(
        card_body("Use this table to learn what type of institution you're trying to find key ratios for.
                  Once you've found your institution type, filter to that type in the 'Key Ratios' tab."),
        gt_output('tbl_lookup')
      )
    ), # close nave_panel Institution Lookup
    
    nav_spacer(), # push link menu to right side
    
    nav_menu(
      title = "External Links",
      nav_item(link_scr),
      nav_item(link_ipeds)
    ) # close nav menu
    
  ) # close navset_tab
) # close page_sidebar



# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Colors
  primary_reserve_color = "r"
  
  
  # REACTION SELECTION ---------------------------------------------------------------------  
  # Make drop-down choice of `this_college` dependent upon user input of `this_type`
  shiny::observeEvent(input$this_type, {
    shiny::updateSelectInput(session,
                             "this_college",
                             choices = df_institutions %>% 
                               dplyr::filter(type == input$this_type))
  })
  
  
  # STATIC TABLES --------------------------------------------------------------------- 
  output$tbl_lookup <- render_gt(df_institutions %>%
                                   rename(`Institution Name` = instnm,
                                          `Institution Type` = type) %>% 
                                   gt::gt() %>%
                                   gt::opt_interactive(
                                     use_search = TRUE,
                                     use_filters = TRUE,
                                     use_resizers = TRUE,
                                     use_highlight = TRUE,
                                     use_compact_mode = TRUE,
                                     use_text_wrapping = FALSE,
                                     use_page_size_select = TRUE
                                     ))
  
  
  # FINANCIAL TABLE --------------------------------------------------------------------
    
  # create reactive data frame for public universities
    df_financial <- reactive({
      req(input$this_college)
      req(input$this_year)
      req(input$this_type)
      
      if (input$this_type == "Public") {
        df_f1a %>% 
          dplyr::left_join(df_directory,
                           by = c("unitid" = "unitid",
                                  "year" = "year")) %>% 
          dplyr::filter(instnm == input$this_college,
                        year %in% input$this_year) %>% 
          dplyr::select(year,
                        unitid,
                        instnm,
                        expendable_net_assets = f1n05,
                        total_expenses = f1d02,
                        plant_related_debt = f1d03,
                        excess_unrest_op_rev = f1n01,
                        total_unrestricted_op_rev = f1n02
                        ) %>%
          dplyr::mutate(primary_reserve_ratio = round(expendable_net_assets / total_expenses, 2),
                        viability_ratio = round(expendable_net_assets / plant_related_debt, 2),
                        net_operating_rev_ratio = round(excess_unrest_op_rev / total_unrestricted_op_rev, 2)) %>%
          dplyr::select(-unitid,
                        -instnm)}
      else if (input$this_type == "Private") {
        df_f3 %>% 
              dplyr::left_join(df_directory,
                               by = c("unitid" = "unitid",
                                      "year" = "year")) %>%
              dplyr::filter(instnm == input$this_college,
                            year %in% input$this_year) %>%
              dplyr::select(year,
                            unitid,
                            instnm,
                            unrestricted_net_assets = f3a04) %>%
              dplyr::mutate(primary_reserve_ratio = round(expendable_net_assets / total_expenses, 2),
                            net_assets_ratio = round(change_in_net_assets / total_net_asssets, 2),
                            operating_income = op_inc_b09 - op_inc_c110,
                            net_operating_revenue_ratio = round(operating_income / revenue, 2),
                            viability_ratio = round(expendable_net_assets / long_term_debt, 2)) %>%
              dplyr::select(-unitid,
                            -instnm)
      }
      else if (input$this_type == "Not-for-Profit") {
        df_f2 %>% 
          dplyr::left_join(df_directory,
                           by = c("unitid" = "unitid",
                                  "year" = "year")) %>% 
          dplyr::filter(instnm == input$this_college,
                        year %in% input$this_year) %>% 
          dplyr::select(year,
                        unitid,
                        instnm,
                        expendable_net_assets = f2i05,
                        total_expenses = f2b02,
                        plant_related_debt = f2i06,
                        change_in_net_assets = f2i03,
                        total_net_assets = f2i04,
                        excess_unrest_op_rev = f2i02 - f2e134,
                        total_unrestricted_op_rev = f2i02) %>%
          dplyr::mutate(primary_reserve_ratio = round(expendable_net_assets / total_expenses, 2),
                        viability_ratio = round(expendable_net_assets / plant_related_debt, 2),
                        return_on_net_assets_ratio = round(change_in_net_assets / total_net_assets, 2),
                        net_operating_rev_ratio = round(excess_unrest_op_rev / total_unrestricted_op_rev), 2) %>%
          dplyr::select(-unitid,
                        -instnm)
      }
      
    })
    
    
    # PRIMARY RESERVE TABLE --------------------------------------------------------------------
    output$tbl_primary_reserve <- renderDataTable({
      if (input$this_type == "Public") {
        df_financial() %>% 
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
          ratio_table_formatting(ratio = "primary_reserve")
        }
      else if (input$this_type == "Private") {
        df_financial() %>% 
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
          ratio_table_formatting(ratio = "primary_reserve")
      }
      else if (input$this_type == "Not-for-Profit") {
        df_financial() %>% 
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
          ratio_table_formatting(ratio = "primary_reserve")}
    }) # close render DT

    
    
      # VIABILITY TABLE --------------------------------------------------------------------
      output$tbl_viability <- renderDataTable({
        if (input$this_type == "Public") {
          df_financial() %>%
            dplyr::select(year,
                          expendable_net_assets,
                          plant_related_debt,
                          viability_ratio) %>%
            tidyr::pivot_longer(names_to = "column",
                                values_to = "values",
                                cols = -c("year"),
                                values_transform = list(values = as.character)) %>%
            dplyr::mutate(values = as.numeric(values)) %>%
            dplyr::arrange(desc(year)) %>%
            tidyr::pivot_wider(names_from = "year",
                               values_from = "values") %>% 
            ratio_table_formatting(ratio = "viability")}
        else if (input$this_type == "Private") {

        }
        else if (input$this_type == "Not-for-Profit") {
          df_financial() %>%
            dplyr::select(year,
                          expendable_net_assets,
                          plant_related_debt,
                          viability_ratio) %>%
            tidyr::pivot_longer(names_to = "column",
                                values_to = "values",
                                cols = -c("year"),
                                values_transform = list(values = as.character)) %>%
            dplyr::mutate(values = as.numeric(values)) %>%
            dplyr::arrange(desc(year)) %>%
            tidyr::pivot_wider(names_from = "year",
                               values_from = "values") %>% 
            ratio_table_formatting(ratio = "viability")
      }
    }) # close render DT
    
  
    # NET OPERATING REVENUES TABLE --------------------------------------------------------------------
    output$tbl_net_op_rev<- renderDataTable({
      if (input$this_type == "Public") {
        df_financial() %>%
          dplyr::select(year,
                        excess_unrest_op_rev,
                        total_unrestricted_op_rev,
                        net_operating_rev_ratio) %>%
          tidyr::pivot_longer(names_to = "column",
                              values_to = "values",
                              cols = -c("year"),
                              values_transform = list(values = as.character)) %>%
          dplyr::mutate(values = as.numeric(values)) %>%
          dplyr::arrange(desc(year)) %>%
          tidyr::pivot_wider(names_from = "year",
                             values_from = "values") %>%
          ratio_table_formatting(ratio = "net_operating_revenues")}
      else if (input$this_type == "Private") {

      }
      else if (input$this_type == "Not-for-Profit") {
        df_financial() %>%
          dplyr::select(year,
                        excess_unrest_op_rev,
                        total_unrestricted_op_rev,
                        net_operating_rev_ratio) %>%
          tidyr::pivot_longer(names_to = "column",
                              values_to = "values",
                              cols = -c("year"),
                              values_transform = list(values = as.character)) %>%
          dplyr::mutate(values = as.numeric(values)) %>%
          dplyr::arrange(desc(year)) %>%
          tidyr::pivot_wider(names_from = "year",
                             values_from = "values") %>%
          ratio_table_formatting(ratio = "net_operating_revenues")}
    })

  
  
  
} # close server






# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)






























