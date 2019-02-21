###
### Skeleton of r scraping
###


# Setup -------------------------------------------------------------------


library(rvest)
library(stringr)
library(httr)
library(rebus)
library(dplyr)
library(here)

# Load cops db
load(here("data/tidy_data",
          "cops_names.rda"))


# Establishes URL system for scraping from index sites --------------------


# MD Case search website:

mdcs_url <- "http://casesearch.courts.state.md.us/casesearch/"

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

dat.list = list()

## Test setup

# Set a session up
mdcs_session <- html_session(mdcs_url)
# Read the disclaimer form on the main MDCS webpage
mdcs_form <- html_form(mdcs_session)[[1]]
# Agree to disclaimer
set_values(mdcs_form, disclaimer = "Y") 
# Submit disclaimer
mdcs_query <- submit_form(session = mdcs_session,
            form = mdcs_form)
mdcs_query_co_form <- html_form(mdcs_query)[[2]] %>%
  set_values(company = "N") 
mdcs_query_name_form <- html_form(mdcs_query)[[3]] %>%
  set_values(lastName = "HERSL",
             firstName = "DANIEL",
             middleName = "",
             exactMatchLn = "Y") 

mdcs_search <- submit_form(session = mdcs_query,
                           form = mdcs_query_name_form)



for(i in seq_along(urls)) {
  Sys.sleep(runif(1,0,1))
  # webpage <- html_session(mdcs_url) %>% 
  #   jump_to(urls[i]) %>%
  #   read_html()
  case_num = html_nodes(webpage, '#row tbody td:nth-child(1)') %>% html_text()
  name = html_nodes(webpage, '#row td:nth-child(2)') %>% html_text()
  party_type = html_nodes(webpage, '#row td:nth-child(4)') %>% html_text()
  court = html_nodes(webpage, '#row td:nth-child(5)') %>% html_text()
  case_type = html_nodes(webpage, '#row td:nth-child(6)') %>% html_text()
  filing_date = html_nodes(webpage, '#row td:nth-child(8)') %>% html_text()
  dat.list[[i]] = data.frame(case_num, name, party_type, court, case_type, filing_date)
}

rm(i)

# Compile into single dataframe, identifies duplicates, saves data
all_data = do.call(rbind,dat.list)
duplicate <- duplicated(all_data[,7])
all_data[,"duplicate"] <- duplicate
save(all_data, file = "data/siteindex_data.Rda")

# Delete duplicate rows -- deletes after first appearance, so first categories will be "overrepresented" (if that matters)
all_data <- subset(all_data, !duplicated(all_data[,7]))
all_data <- all_data[,-ncol(all_data)]
save(all_data, file = "data/siteindex_data_nodup.Rda")

# Grab additional information from each video
all_data$url <- as.character(all_data$url)

