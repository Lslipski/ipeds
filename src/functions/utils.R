# Load Financial Tables for Public Institutions
load_public_financial <- function(years) {
  
  load_csv = function(this_year) {
    
    df_f1a <- read_csv(paste0(here::here("data/"),
                              "f1a_",
                              this_year,
                              ".csv")) %>% 
      janitor::clean_names() %>% 
      mutate(year = this_year)
  } 
  
  df = purrr::map(.x = years,
                  ~ load_csv(.x)) %>% 
    bind_rows()
  
  return(df)
}


# Load Financial Tables for Private Universities
load_private_financial <- function(years) {
  
  load_csv = function(this_year) {
    
    df_f1a <- read_csv(paste0(here::here("data/"),
                              "f3_",
                              this_year,
                              ".csv")) %>% 
      janitor::clean_names() %>% 
      mutate(year = this_year)
  } 
  
  df = purrr::map(.x = years,
                  ~ load_csv(.x)) %>% 
    bind_rows()
  
  return(df)
}


# Load Financial Tables for Private not-for-profit institutions or Public institutions using FASB
load_nfp_financial <- function(years) {
  
  load_csv = function(this_year) {
    
    df_f1a <- read_csv(paste0(here::here("data/"),
                              "f2_",
                              this_year,
                              ".csv")) %>% 
      janitor::clean_names() %>% 
      mutate(year = this_year)
  } 
  
  df = purrr::map(.x = years,
                  ~ load_csv(.x)) %>% 
    bind_rows()
  
  return(df)
}


# Load Directory Tables
load_directory <- function(years) {
  
  load_csv = function(this_year) {
    
    df_f1a <- read_csv(paste0(here::here("data/"),
                              "directory_",
                              this_year,
                              ".csv")) %>% 
      janitor::clean_names() %>% 
      distinct(unitid, instnm) %>% 
      mutate(year = this_year)
  } 
  
  df = purrr::map(.x = years,
                  ~ load_csv(.x)) %>% 
    bind_rows()
  
  return(df)
}


# Load Institution Names and their Associated Types

load_institution_types <- function(years) {
  
  df_directory = load_directory(years)
  
  dplyr::bind_rows(
    
    # PUBLIC
    df_directory %>% 
      dplyr::select(instnm,
                    unitid,
                    year) %>% 
      # limit to those that exist in PUBLIC financials df
      dplyr::inner_join(load_public_financial(years) %>% 
                          select(unitid,
                                 year),
                        by = c("unitid", "year")) %>% 
      dplyr::distinct(instnm) %>% 
      dplyr::mutate(type = "Public"),
  
  # PRIVATE
  df_directory %>% 
    dplyr::select(instnm,
                  unitid,
                  year) %>% 
    # limit to those that exist in PUBLIC financials df
    dplyr::inner_join(load_private_financial(years) %>% 
                        select(unitid,
                               year),
                      by = c("unitid", "year")) %>% 
    dplyr::distinct(instnm) %>% 
    dplyr::mutate(type = "Private"),
  
  # PRIVATE
  df_directory %>% 
    dplyr::select(instnm,
                  unitid,
                  year) %>% 
    # limit to those that exist in PUBLIC financials df
    dplyr::inner_join(load_nfp_financial(years) %>% 
                        select(unitid,
                               year),
                      by = c("unitid", "year")) %>% 
    dplyr::distinct(instnm) %>% 
    dplyr::mutate(type = "Not-for-Profit")
  ) # close bind_cols
  
} # close load_institution_types




