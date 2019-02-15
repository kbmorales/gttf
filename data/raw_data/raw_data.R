library(googlesheets)
library(dplyr)

token <- gs_auth("googlesheets_token.rds")
gd_token()
saveRDS(token, file = "googlesheets_token.rds")



gs1 <- gs_url("https://docs.google.com/spreadsheets/d/1w-MVRhfk4obUF0C_YNXVUZ0UuqEz2Ov12ygmFaP58LE/edit#gid=0",
              visibility = "private")
 gs_read("https://docs.google.com/spreadsheets/d/1w-MVRhfk4obUF0C_YNXVUZ0UuqEz2Ov12ygmFaP58LE/edit#gid=0", 1)
 