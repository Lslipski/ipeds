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
      mutate(year = this_year)
  } 
  
  df = purrr::map(.x = years,
                  ~ load_csv(.x)) %>% 
    bind_rows()
  
  return(df)
}







