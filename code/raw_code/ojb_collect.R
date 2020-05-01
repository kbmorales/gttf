### kbmorales
### kbmorales@protonmail.com
### Set up DBs from CaseHarvester


# Setup -------------------------------------------------------------------
library(tidyverse)
library(dbplyr)

options(dplyr.print_max = Inf)

# Con
source(here::here("auths",
                  "connect_ojb.R")
       )

# Cops
load(here::here("data",
                "cops_names.rda"))
## Keep just indicted for now
cops <- cops %>% 
  filter(status == "indicted")

# Connect -------------------------------------------------------------
### Cases - master
cases <- tbl(con, 
             in_schema("public", "cases"))
cases %>% glimpse()

## Interesting cols
# case_number
# court
# case_type
# filing_date
# status

### cc
# Circuit court civil

### dscivil
# District Court Civil

### dscr 
# District court criminal
dscr <- tbl(con, 
            in_schema("public", "dscr"))

## Sub tables
dscr_rel <- tbl(con, 
                in_schema("public", 
                          "dscr_related_persons"))

dscr_rel %>% glimpse()

### dsk8javascript:;
# DSK8 is Circuit Court Criminal Cases

dsk8 <- tbl(con, 
            in_schema("public", "dsk8"))

dsk8 %>% glimpse()

## Sub tables
dsk8_rel <- tbl(con, in_schema("public",
                               "dsk8_related_persons"))

dsk8_rel %>% glimpse()



# Filter ---------------------------------------------------------------------

### Create last name filter
last_name_pat <- paste(paste0("^", cops$last_name), 
                       sep=" ",
                       collapse = "|")

### dsk8

# Look at connection values
con_desc <- dsk8_rel %>% 
  count(connection) %>% 
  arrange(desc(n)) %>% 
  collect()

dsk8_rel_filt <- dsk8_rel %>% 
  filter(str_detect(connection,
                    "POLICE") |
           is.na(connection) |
           connection == "OTHER") %>% 
  mutate(name = toupper(name)) %>% 
  filter(str_detect(name,
                    local(last_name_pat)))
  # Remove middle initials
  # mutate(name = (name,
  #                          " [A-Z]$"))
  ## Need to double check name filters
  # filter(name %in% local(cops$name))

dsk8_casenum <- dsk8_rel_filt %>% 
  select(case_number) %>% 
  collect() %>% 
  pull(case_number) %>% 
  unique()

length(dsk8_casenum)

dsk8_filt <- dsk8 %>% 
  filter(case_number %in% local(dsk8_casenum))

### dscr
# Look at connection values
con_desc <- dscr_rel %>% 
  count(connection) %>% 
  arrange(desc(n)) %>% 
  collect()

dscr_rel_filt <- dscr_rel %>% 
  filter(str_detect(connection,
                    "POLICE") |
           connection == "COMPLAINANT") %>% 
  mutate(name = toupper(name)) %>% 
  filter(str_detect(name,
                    local(last_name_pat)))

dscr_casenum <- dscr_rel_filt %>% 
  select(case_number) %>% 
  collect() %>% 
  pull(case_number) %>% 
  unique()

length(dscr_casenum)

dscr_filt <- dscr %>% 
  filter(case_number %in% local(dscr_casenum))

### Master cases table
casenums <- c(dsk8_casenum, dscr_casenum)

length(casenums)

cases_filt <- cases %>% 
  filter(case_number %in% local(casenums)) %>% 
  arrange(filing_date)

# EDA ---------------------------------------------------------------------

# dsk8_rel %>% 
#   filter(str_detect(connection,
#                     "POLICE") |
#            is.na(connection) |
#            connection == "OTHER") %>% 
#   mutate(name = toupper(name)) %>% 
#   count(name) %>% 
#   arrange(desc(n)) %>% 
#   collect() %>% 
#   View()

cases_filt %>% 
  mutate(year = year(filing_date)) %>% 
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

## Filtering to 2008-- GTTF founded in 2007
cases_filt %>% 
  mutate(year = year(filing_date)) %>% 
  filter(year >= 2008) %>% 
  # count()
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()


