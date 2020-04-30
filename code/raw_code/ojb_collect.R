### kbmorales
### kbmorales@protonmail.com
### Set up DBs from CaseHarvester


# Setup -------------------------------------------------------------------
library(tidyverse)
library(dbplyr)

# Con
source(here::here("auths",
                  "connect_ojb.R")
       )

# Connect -------------------------------------------------------------
### Cases - master
cases <- tbl(con, in_schema("public", "cases"))
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

### dsk8javascript:;
# DSK8 is Circuit Court Criminal Cases

dsk8 <- tbl(con, 
            in_schema("public", "dsk8"))

## Sub tables
dsk8_rel <- tbl(con, in_schema("public",
                               "dsk8_related_persons"))


# Filter ---------------------------------------------------------------------

# Need dates relevant to parties of interest


# dsk8 %>% 
#   filter(filing_date >= "2008-01-01",
#          filing_date <= "2018-01-01") %>%
#   count()
# 
# # dsk8 related persons seems to be where we want to start
# 
# dsk8_ojb %>% 
#   filter(filing_date >= "2008-01-01",
#          filing_date <= "2018-01-01") %>%
#   count()
# 
# dsk8_rel_persons %>% glimpse()
# 
