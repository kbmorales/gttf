###
### Skeleton of r scraping
###


# Setup -------------------------------------------------------------------


library(rvest)
library(stringr)
library(httr)
library(rebus)
library(dplyr)


# Establishes URL system for scraping from index sites --------------------


# MD Case search website: http://casesearch.courts.state.md.us/casesearch/processDisclaimer.jis?disclaimer=Y

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

for(i in seq_along(urls)) {
  Sys.sleep(runif(1,0,1))
  webpage <- read_html(urls[i])
  titles = html_nodes(webpage, '#categoryListContent .index-title') %>% html_text()
  vid_length = html_nodes(webpage, '#categoryListContent .index-length') %>% html_text()
  views = html_nodes(webpage, '#categoryListContent .index-views') %>% html_text() %>% str_replace_all("[,]", "") %>% as.numeric()
  rating = html_nodes(webpage, '#categoryListContent .index-rating') %>% html_text() %>% str_replace("[%]", "") %>% as.numeric()
  url = html_nodes(webpage, '.index-title a') %>% html_attr("href")
  viewkey = str_replace(url, pattern = trimpatvk, "")
  dat.list[[i]] = data.frame(category = category_names[i],titles,vid_length,views,rating, url, viewkey)
}

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

