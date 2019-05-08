# Messing around with OJB data

source(here::here("data/raw_data",
                  "connect_ojb.R"))

# Take a peek at cases
cases_ojb <- tbl(con, in_schema("public", "cases"))

dsk8_ojb <- tbl(con, in_schema("public", "dsk8"))

dsk8_rel_persons <- tbl(con, in_schema("public", "dsk8_related_persons"))

cases_ojb %>% glimpse()

# DSK8 is Circuit Court Criminal Cases

dsk8_ojb %>% 
  filter(filing_date >= "2008-01-01",
         filing_date <= "2018-01-01") %>%
  count()

# dsk8 related persons seems to be where we want to start

dsk8_ojb %>% 
  filter(filing_date >= "2008-01-01",
         filing_date <= "2018-01-01") %>%
  count()

dsk8_rel_persons %>% glimpse()

