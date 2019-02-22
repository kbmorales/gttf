###
### Skeleton of r scraping
###


# Setup -------------------------------------------------------------------

options(stringsAsFactors = FALSE)

library(rvest)
library(stringr)
library(httr)
library(rebus)
library(dplyr)
library(here)

# Load cops db
load(here("data/tidy_data",
          "mdcs_cops_df.rda"))


# Scrape setup of case numbers --------------------


# MD Case search website:
mdcs_url <- "http://casesearch.courts.state.md.us/casesearch/"

dat.list = list()

## Setup for scrape

# Set a session up
mdcs_session <- html_session(mdcs_url)
# Read the disclaimer form on the main MDCS webpage
mdcs_form <- html_form(mdcs_session)[[1]]
# Agree to disclaimer
set_values(mdcs_form, disclaimer = "Y") 
# Submit disclaimer
mdcs_query <- submit_form(session = mdcs_session,
            form = mdcs_form)

for(i in seq_along(mdcs_cops_df$case_num)) {
  # Time benchmark
  t0 <- Sys.time()
  response <- GET(mdcs_query$url)
  t1 <- Sys.time()
  # Scrape code
  mdcs_query_form <- html_form(mdcs_query)[[4]] %>%
    set_values(locationCode = "0",
               caseId = mdcs_cops_df$case_num[i]) 
  mdcs_search <- submit_form(session = mdcs_query,
                             form = mdcs_query_form) %>%
    read_html()
  case_num = mdcs_cops_df$case_num[i]
  all_dat = html_nodes(mdcs_search, ".FirstColumnPrompt , h5 , .Prompt , .Value") %>% 
    html_text()
  dat.list[[i]] = data.frame(case_num, all_dat)
  # Delay research to prevent server overload
  response_delay <- as.numeric(t1-t0)
  Sys.sleep(10*response_delay) # sleep 10 times longer than response_delay
}

rm(i)

# Compile into single dataframe
mdcs_all_data = do.call(rbind,dat.list)
save(mdcs_all_data,
     file = here("data/raw_data",
                 "mdcs_case_data.rda")
)


# URL Scrape Attempt Work -------------------------------------------------


url_stem <- "http://casesearch.courts.state.md.us/casesearch/inquiry-results.jsp?"

# URL structure for queries

# d-16544- # ???? some sort of sorting thingy
# p=1 # page of the results
# &lastName=HERSL # last name of the person to search
# &filingDate= # Leave blank
# &filingEnd=3%2F31%2F2016 # 1-digit month %2F 1 digit day? %2F 4 digit year
# &partyType= # leave blank
# &courtSystem=B # ?? all?
# &firstName=DANIEL # first name of the person to search
# &site=00 # didn't specify, assume 00 is all
# &filingStart=1%2F1%2F2014 # 1-digit month %2F 1 digit day? %2F 4 digit year
# &action=Search # We're searching!
# &company=N # No company name
# &middleName= # Leave blank
# &exactMatchLn=Y # Exact match!
# &countyName= # Leave blank

# Variables to provide

page_num <- 1 # scrape from element of search results

# Set up start date: ex: January 1, 2014
start_date <- str_c(1, "%2F", 1, "%2F", 2014)

# Set up end date: ex: March 31, 2016
end_date <- str_c(3, "%2F", 31, "%2F", 2016)

urls <- str_c(url_stem, 
              "d-16544-p=", 
              page_num, 
              "&lastName=",
              cops$last_name,
              "&filingDate=&filingEnd=",
              end_date,
              "&partyType=&courtSystem=B&firstName=",
              cops$first_name,
              "&site=00&filingStart=",
              start_date,
              "&action=Search&company=N&middleName=&exactMatchLn=Y&countyName="
)

