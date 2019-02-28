###
### Clean MDCS Scraped Data
###



# Setup -------------------------------------------------------------------


library(rvest)
library(stringr)
library(httr)
library(rebus)
library(dplyr)
library(here)
library(readr)
library(stringr)

# Load cops db
load(here("data/tidy_data",
          "mdcs_cops_df.rda"))

# Load in scraped dataset
load(file = here("data/raw_data",
                 "mdcs_case_data.rda"))

# MDCS Demographic Data -----------------------------------------------


# Filter for case numbers in mdcs_cops_df
mdcs_all_data <- mdcs_all_data %>% semi_join(mdcs_cops_df)

# Create demo dataset
mdcs_demo_df <- c()

for(i in 1:length(unique(mdcs_all_data$case_num))) {
  # Break down if circuit or district court:
  mdcs_demo_row <- 
    # If a District Court Case
    # if(str_detect(mdcs_cops_df[mdcs_cops_df$case_num == unique(mdcs_all_data$case_num)[i], 5], "District")) {
      mdcs_all_data %>%
        filter(case_num == unique(mdcs_all_data$case_num)[i]) %>%
        mutate(court_system = str_replace_all(trimws(all_dat[str_which(all_dat, "Court System:")+1]
                                                     ),
                                              "\\s*-?\n\\s+|\\s{2}", " "),
               status_date = NA,
               tracking_num = if("Tracking Number:" %in% all_dat){
                 all_dat[str_which(all_dat, "Tracking Number:")+1]
                 } else {
                   "Unknown"
                   },
               district_code = all_dat[str_which(all_dat, "District Code:")+1],
               location_code = all_dat[str_which(all_dat, "Location Code:")+1],
               doc_type = all_dat[str_which(all_dat, "Document Type:")+1],
               case_disposition = if("Case Disposition:" %in% all_dat){
          all_dat[str_which(all_dat, "Case Disposition:")+1]
        } else {
          "Unknown"
        },
        complaint_num = NA,
        district_case_num = NA,
        defendant_name = all_dat[str_which(all_dat, "Defendant Name:")+1],
        defendant_race = if("Race:" %in% all_dat){
          all_dat[str_which(all_dat, "Race:")+1]
        } else {
          "Unknown"
        },
        defendant_sex = all_dat[str_which(all_dat, "Sex:")+1],
        # defendant_height = sum(as.numeric(str_split(all_dat[str_which(all_dat, "Height:")+1],
        #                                          "",
        #                                          n = 2,
        #                                          simplify = TRUE)
        #                                 ) * c(12,1)
        #                      ),
        # defendant_weight = as.numeric(all_dat[str_which(all_dat, "Weight:")+1]),
        defendant_dob = as.Date(all_dat[str_which(all_dat, "DOB:")+1],
                                format = "%m/%d/%Y"),
        # Multiple of each--indexing from different strings:
        defendant_address = if(sum(str_detect(all_dat, "^APT \\w+")) > 0){
          all_dat[str_which(all_dat, "Address:")[1] +1]
        } else {
          all_dat[str_which(all_dat, "City:")[1] -1]
        },
        defendant_city = all_dat[str_which(all_dat, "City:")[1] +1],
        defendant_state = all_dat[str_which(all_dat, "City:")[1] +3],
        defendant_zip = all_dat[str_which(all_dat, "City:")[1] +5]) %>%
        filter(row_number() == 1)
    # NO LONGER NEED CIRCUIT COURT CASES
    # } else {
    #   # For Circuit court cases
    #   mdcs_all_data %>%
    #     filter(case_num == unique(mdcs_all_data$case_num)[i]) %>%
    #     mutate(court_system = str_replace_all(trimws(all_dat[str_which(all_dat, "Court System:")+1]
    #     ),
    #     "\\s*-?\n\\s+|\\s{2}", " "),
    #     status_date = if("Status Date:" %in% all_dat){
    #       all_dat[str_which(all_dat, "Status Date:")+1]}
    #     else {
    #       "Unknown"
    #     },
    #     tracking_num = if("Tracking Number:" %in% all_dat){
    #       all_dat[str_which(all_dat, "Tracking Number:")+1]
    #     } else {
    #       "Unknown"
    #     },
    #     district_code = NA,
    #     location_code = NA,
    #     doc_type = NA,
    #     case_disposition = NA,
    #     complaint_num = if("Complaint No:" %in% all_dat) {
    #       all_dat[str_which(all_dat, "Complaint No:")+1]
    #     } else {
    #       "Unknown"
    #     },
    #     district_case_num = if("District Case No:" %in% all_dat){
    #       all_dat[str_which(all_dat, "District Case No:")+1]
    #     } else {
    #       "Unknown"
    #     },
    #     defendant_name = all_dat[str_which(all_dat, "Defendant Name:")+1],
    #     defendant_race = if("Race:" %in% all_dat){
    #       all_dat[str_which(all_dat, "Race:")+1]
    #     } else {
    #       "Unknown"
    #     },
    #     defendant_sex = all_dat[str_which(all_dat, "Sex:")+1],
    #     # defendant_height = sum(as.numeric(str_split(all_dat[str_which(all_dat, "Height:")+1],
    #     #                                             "",
    #     #                                             n = 2,
    #     #                                             simplify = TRUE)
    #     # ) * c(12,1)
    #     # ),
    #     # defendant_weight = as.numeric(all_dat[str_which(all_dat, "Weight:")+1]),
    #     defendant_dob = if("DOB:" %in% all_dat){
    #       as.Date(all_dat[str_which(all_dat, "DOB:")+1],
    #                             format = "%m/%d/%Y")
    #     } else {
    #       NA
    #     },
    #     # Multiple of each--indexing from different strings:
    #     defendant_address = if(sum(str_detect(all_dat, "^APT \\w+")) > 0){
    #       all_dat[str_which(all_dat, "Address:")[1] +1]
    #       } else {
    #         all_dat[str_which(all_dat, "City:")[1] -1]
    #         },
    #     defendant_city = all_dat[str_which(all_dat, "City:")[1] +1],
    #     defendant_state = all_dat[str_which(all_dat, "City:")[1] +3],
    #     defendant_zip = all_dat[str_which(all_dat, "City:")[1] +5]) %>%
    #     filter(row_number() == 1)
    # }
  mdcs_demo_df <- bind_rows(mdcs_demo_df, mdcs_demo_row)
}

rm(i)

# Remove initial all_dat column from demo data
mdcs_demo_df <- mdcs_demo_df %>% select(-all_dat)

# Make all zips 5-digit
mdcs_demo_df <- mdcs_demo_df %>%
  mutate(defendant_zip = str_trunc(defendant_zip,
                                   width = 5,
                                   side = "right",
                                   ellipsis = "")
  )

# Save Demographics dataset
save(mdcs_demo_df,
     file = here("data/tidy_data",
                 "mdcs_demo_data.rda")
)


# Join demo dataset to cops dataset
mdcs_df <- mdcs_cops_df %>% left_join(mdcs_demo_df)

# Save MDCS cops + demo dataset
save(mdcs_df,
     file = here("data/tidy_data",
                 "mdcs_data.rda")
)


# Charges dataset ---------------------------------------------------------


# Create charges dataset
mdcs_charges_df <- c()

for(i in 1:length(unique(mdcs_all_data$case_num))) {
   case <- mdcs_all_data %>% 
     filter(case_num == unique(mdcs_all_data$case_num)[i])
   # If a District Court Case
   case_charge <-
     # if(str_detect(mdcs_cops_df[mdcs_cops_df$case_num == unique(mdcs_all_data$case_num)[i], 5], "District")) 
     # {
       tibble(
         case_num = unique(mdcs_all_data$case_num)[i],
         case_court = "District",
         charge_num = case[str_which(case$all_dat, "Charge No:") +1, 2],
         charge_desc = case[str_which(case$all_dat, "Charge No:") +3, 2],
         charge_statute = case[str_which(case$all_dat, "Statute:") +1, 2],
         charge_amend_date = case[str_which(case$all_dat, "Amended Date:") +1, 2],
         charge_cjis_code = case[str_which(case$all_dat, "CJIS Code:") +1, 2],
         charge_mo_pll = case[str_which(case$all_dat, "MO/PLL:") +1, 2],
         charge_prob_cause = case[str_which(case$all_dat, "Probable Cause:") +1, 2],
         charge_inc_date_from = as.Date(case[str_which(case$all_dat, "Incident Date From:") +1, 2],
                                        format = "%m/%d/%Y"
                                        ),
         charge_inc_date_to = as.Date(case[str_which(case$all_dat, "To:") +1, 2],
                                      format = "%m/%d/%Y"
                                      ),
         charge_victim_age = case[str_which(case$all_dat, "Victim Age:") +1, 2],
         charge_plea = case[str_which(case$all_dat, "Plea:") +1, 2],
         charge_disposition = if("^Disposition:" %in% case$all_dat){
           case[str_which(case$all_dat, "^Disposition:") +1, 2]
         } else {
           NA
         },
         charge_disp_date = if("Disposition Date:" %in% case$all_dat){
           as.Date(case[str_which(case$all_dat, "Disposition Date:") +1, 2],
                   format = "%m/%d/%Y"
           )
         } else {
           NA
         },
         charge_fine = as.numeric(
           str_remove(
             case[str_which(case$all_dat, "Fine:") +1, 2][c(TRUE, FALSE)]
             , "\\$")
           ),
         charge_court_costs = as.numeric(
           str_remove(
             case[str_which(case$all_dat, "Court Costs:") +1, 2][c(TRUE, FALSE)]
             , "\\$")
         ),
         charge_cicf = as.numeric(
           str_remove(
             case[str_which(case$all_dat, "CICF:") +1, 2][c(TRUE, FALSE)]
             , "\\$")
         ),
         charge_susp_fine = as.numeric(
           str_remove(
             case[str_which(case$all_dat, "Fine:") +1, 2][c(FALSE, TRUE)]
             , "\\$")
         ),
         charge_susp_court_costs = as.numeric(
           str_remove(
             case[str_which(case$all_dat, "Court Costs:") +1, 2][c(FALSE, TRUE)]
             , "\\$")
         ),
         charge_susp_cicf = as.numeric(
           str_remove(
             case[str_which(case$all_dat, "CICF:") +1, 2][c(FALSE, TRUE)]
             , "\\$")
         ),
         charge_pbj_end_date = case[str_which(case$all_dat, "PBJ EndDate:") +1, 2],
         charge_prob_end_date = case[str_which(case$all_dat, "Probation End Date:") +1, 2],
         charge_rest_amt = as.numeric(
           str_remove(
             case[str_which(case$all_dat, "Restitution Amount:") +1, 2]
             , "\\$")
           ),
         # Does total jail time?
         charge_jailtime = as.numeric(
           case[str_which(case$all_dat, "Yrs:") +1, 2][c(TRUE, FALSE)]
           ) + 
           as.numeric(
             case[str_which(case$all_dat, "Mos:") +1, 2][c(TRUE, FALSE)]
           )/12 +
           as.numeric(
             case[str_which(case$all_dat, "Days:") +1, 2][c(TRUE, FALSE)]
           )/365,
         charge_susp_jailtime = as.numeric(
           case[str_which(case$all_dat, "Yrs:") +1, 2][c(FALSE, TRUE)]
         ) + 
           as.numeric(
             case[str_which(case$all_dat, "Mos:") +1, 2][c(FALSE, TRUE)]
           )/12 +
           as.numeric(
             case[str_which(case$all_dat, "Days:") +1, 2][c(FALSE, TRUE)]
           )/365,
         charge_credit_timeserved = as.numeric(
           case[str_which(case$all_dat, "Credit Time Served:") +1, 2]
         )
       )
   #     } 
   # else { 
   #   # For Circuit court cases
   #   case_charge <- tibble(
   #     case_num = unique(mdcs_all_data$case_num)[i],
   #     case_court = "Circuit",
   #     charge_num = case[str_which(case$all_dat, "Charge No:") +1, 2],
   #     charge_desc = case[str_which(case$all_dat, "Description:") +1, 2],
   #     charge_statute = NA,
   #     charge_amend_date = NA,
   #     charge_cjis_code = if("CJIS" %in% case$all_dat){
   #       case[str_which(case$all_dat, "CJIS/Traffic Code:") +1, 2]
   #       }
   #     else {
   #       NA
   #     },
   #     charge_mo_pll = NA,
   #     charge_prob_cause = NA,
   #     charge_inc_date_from = NA,
   #     charge_inc_date_to = NA,
   #     charge_victim_age = NA,
   #     charge_disposition = if("Disposition:" %in% case$all_dat){
   #       case[str_which(case$all_dat, "Disposition:") +1, 2]
   #     } else {
   #         NA
   #       },
   #     charge_disp_date = if("Disposition Date:" %in% case$all_dat){
   #       as.Date(case[str_which(case$all_dat, "Disposition Date:") +1, 2],
   #               format = "%m/%d/%Y"
   #               )
   #       } else {
   #         NA
   #         }
   #   )
   # }
   mdcs_charges_df <- bind_rows(mdcs_charges_df, case_charge)
}

# Manual checking of cases:

case <- mdcs_all_data %>% filter(case_num == unique(mdcs_all_data$case_num)[i])
View(case)
# weird_cases[[x]] <- test_data
# i <- i + 1

rm(i)


