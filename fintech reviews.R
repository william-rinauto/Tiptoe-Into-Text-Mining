library(lubridate)
library(rvest)
library(future)
library(future.apply)
library(tidyverse)
library(DBI)
library(RSQLite)



setwd('C:/users/willi/desktop/projects/BHG Comments/trustlink multiple companies')
#trustlink function

#Pulls scrapers that were present in local system (I think one of these may be the same as trustpilot scrape that I have in github:

# source('../functions/trustpilot/trustpilot functions.R')
# source('sqlite_connection.R')

fintech_url_list <- list(sofi = 'https://www.trustpilot.com/review/sofi.com',
                         ondeck = 'https://www.trustpilot.com/review/ondeck.com',
                         lendingclub = 'https://www.trustpilot.com/review/lendingclub.com',
                         lendingree = 'https://www.trustpilot.com/review/www.lendingtree.com',
                         kabbage = 'https://www.trustpilot.com/review/www.kabbage.com',
                         prosper = 'https://www.trustpilot.com/review/www.prosper.com',
                         macrus = 'https://www.trustpilot.com/review/www.marcus.co.uk',
                         chase = 'https://www.trustpilot.com/review/chase.com',
                         credibility_capital = 'https://www.trustpilot.com/review/credibilitycapital.com',
                         funding_circle = 'https://www.trustpilot.com/review/fundingcircle.com',
                         bhg = 'https://www.trustpilot.com/review/bankershealthcaregroup.com'
                         )

fintech_url_list <- tibble(
  company = names(fintech_url_list),
  trustpilot_url = unname(unlist(fintech_url_list))
)

plan('multisession',workers = 16)

company_reviews <- future_lapply(1:nrow(fintech_url_list), function(c) { 
  company_name <- fintech_url_list[[c,1]]
  company_url <- fintech_url_list[[c,2]]
  
  # loop_max <- page_count_trustpilot(company_url)
  loop_max <- 20
  print(company_name)
  
  reviews <- lapply(1:loop_max, function(p) {
    if(p == 62) {
      tibble(
        review = NA,
        date_published = NA,
        stars = NA,
        brief_summary = NA,
        page_number = 62,
        run_date = today()
      )
    } else {
        
      print(p)
      if(p ==1) {
        wp <- company_url
      } else {
        wp <- paste0(company_url, "?page=",p)
      }
      
      get_trustpilot_reviews(wp,page_number = p)
    }
  }) %>%
    bind_rows() %>%
    mutate(company = company_name,
           base_url = company_url,
           pages_scraped  = loop_max)
  
  print(class(reviews))
  
  return(reviews)
})

all_companies <- bind_rows(company_reviews) %>%
  select(-page_number) %>%
  distinct(.keep_all = T)

dbWriteTable(sqlite_connection(),  paste0('reviews.',str_replace_all(now(),":|- ",".")),all_companies)

