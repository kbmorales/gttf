###
### Clean MDCS Scraped Data
###



# Setup -------------------------------------------------------------------
library(stringr)
library(httr)
library(rebus)
library(dplyr)
library(readr)
library(tidyr)
library(readr)

# Load cops db
load(here::here("data/tidy_data",
                "mdcs_cops_df.rda"))

# Load in scraped dataset
load(file = here::here("data/raw_data",
                       "mdcs_case_data.rda"))

# Filter for case numbers in mdcs_cops_df (should be district cases)
mdcs_dist_data <- mdcs_all_data %>% semi_join(mdcs_cops_df)

# Filter for circuit court case numbers in mdcs_circ_cops_df
mdcs_circ_data <- mdcs_all_data %>% semi_join(mdcs_circ_cops_df) %>%
  rename(circ_case_num = case_num)


# MDCS Demographic Data -----------------------------------------------


# Create demo dataset
mdcs_demo_df <- c()

for(i in 1:length(unique(mdcs_dist_data$case_num))) {
  # Break down if circuit or district court:
  mdcs_demo_row <- 
    # If a District Court Case
    # if(str_detect(mdcs_cops_df[mdcs_cops_df$case_num == unique(mdcs_dist_data$case_num)[i], 5], "District")) {
      mdcs_dist_data %>%
        filter(case_num == unique(mdcs_dist_data$case_num)[i]) %>%
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
    #   mdcs_dist_data %>%
    #     filter(case_num == unique(mdcs_dist_data$case_num)[i]) %>%
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
     file = here::here("data/tidy_data",
                 "mdcs_data.rda")
)


# Charges dataset ---------------------------------------------------------


# Create charges dataset
mdcs_charges_df <- c()

for(i in 1:length(unique(mdcs_dist_data$case_num))) {
   case <- mdcs_dist_data %>% 
     filter(case_num == unique(mdcs_dist_data$case_num)[i])
   # If a District Court Case
   rows <- sum(str_detect(case$all_dat, "Charge No:")) 
   case_charge <-
     # if(str_detect(mdcs_cops_df[mdcs_cops_df$case_num == unique(mdcs_dist_data$case_num)[i], 5], "District")) 
     # {
     tibble(
         case_num = unique(mdcs_dist_data$case_num)[i],
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
         charge_plea = c(case[str_which(case$all_dat, "Plea:") +1, 2],
                         rep(NA, rows - sum(str_detect(case$all_dat, "Plea:")
                                            )
                             )
                         ),
         charge_disposition = c(case[str_which(case$all_dat, "^Disposition:") +1, 2],
                                rep(NA, rows - sum(str_detect(case$all_dat, "^Disposition:")
                                                   )
                                    )
                                ),
         charge_disp_date = c(as.Date(case[str_which(case$all_dat, "Disposition Date:") +1, 2],
                     format = "%m/%d/%Y"),
                     rep(NA, rows - sum(str_detect(case$all_dat, "Disposition Date")
                                        )
                         )
                     ),
         charge_fine = if("Fine:" %in% case$all_dat)
           {
           c(as.numeric(
           str_remove(case[str_which(case$all_dat, "Fine:") +1, 2][c(TRUE, FALSE)], "\\$")
           ),
           rep(NA, rows - sum(str_detect(case$all_dat, "Fine:")[c(TRUE, FALSE)]))
           )
           } else {
             NA
           },
         charge_court_costs = if("Court Costs:" %in% case$all_dat)
           {
           c(as.numeric(
           str_remove(case[str_which(case$all_dat, "Court Costs:") +1, 2][c(TRUE, FALSE)], "\\$")
           ),
           rep(NA, rows - sum(str_detect(case$all_dat, "Court Costs:")[c(TRUE, FALSE)]))
           )
           } else {
             NA
           },
         charge_cicf = if("CICF:" %in% case$all_dat)
           {
           c(as.numeric(
           str_remove(
             case[str_which(case$all_dat, "CICF:") +1, 2][c(TRUE, FALSE)]
             , "\\$")
           ),
           rep(NA, rows - sum(str_detect(case$all_dat, "CICF:")[c(TRUE, FALSE)]))
         )
           } else {
             NA
           },
         charge_susp_fine = if("Fine:" %in% case$all_dat)
           {
           c(as.numeric(
           str_remove(
             case[str_which(case$all_dat, "Fine:") +1, 2][c(FALSE, TRUE)], "\\$")
           ),
         rep(NA, rows - sum(str_detect(case$all_dat, "Fine:")[c(FALSE, TRUE)]
                            )
             )
         )
           } else {
             NA
           },
         charge_susp_court_costs = if("Court Costs:" %in% case$all_dat)
           {
           c(as.numeric(
           str_remove(
             case[str_which(case$all_dat, "Court Costs:") +1, 2][c(FALSE, TRUE)], "\\$")
         ),
         rep(NA, rows - sum(str_detect(case$all_dat, "Court Costs:")[c(FALSE, TRUE)]
                            )
             )
         )
          } else {
            NA
          },
         charge_susp_cicf = if("CICF:" %in% case$all_dat)
           {
           c(as.numeric(
           str_remove(
             case[str_which(case$all_dat, "CICF:") +1, 2][c(FALSE, TRUE)], "\\$")
         ),
         rep(NA, rows - sum(str_detect(case$all_dat, "CICF:")[c(FALSE, TRUE)]
                            )
             )
         )
           } else {
             NA
           },
         charge_pbj_end_date = c(case[str_which(case$all_dat, "PBJ EndDate:") +1, 2],
                                 rep(NA, rows - sum(str_detect(case$all_dat, "PBJ EndDate:")
                                                    )
                                     )
                                 ),
         charge_prob_end_date = c(case[str_which(case$all_dat, "Probation End Date:") +1, 2],
                                  rep(NA, rows - sum(str_detect(case$all_dat, "Probation End Date:")
                                                     )
                                      )
                                  ),
         charge_rest_amt = c(as.numeric(
           str_remove(
               case[str_which(case$all_dat, "Restitution Amount:") +1, 2], "\\$")
           ),
           rep(NA, rows - sum(str_detect(case$all_dat, "Restitution Amount:")
                              )
               )
           ),
         charge_jailtime = if("Jail Term:" %in% case$all_dat)
           {
           c(as.numeric(replace_na(case[str_which(case$all_dat, "Yrs:") +1, 2][c(TRUE, FALSE)], 0)) + 
             as.numeric(replace_na(case[str_which(case$all_dat, "Mos:") +1, 2][c(TRUE, FALSE)], 0))/12 +
             as.numeric(replace_na(case[str_which(case$all_dat, "Days:") +1, 2][c(TRUE, FALSE)], 0))/365,
           rep(NA, rows - sum(str_detect(case$all_dat, "Jail Term:")
                              )
               )
           )
           } else {
             NA
           },
         charge_susp_jailtime = if("Suspended Term:" %in% case$all_dat)
           {
           c(as.numeric(replace_na(case[str_which(case$all_dat, "Yrs:") +1, 2][c(FALSE, TRUE)], 0)) + 
             as.numeric(replace_na(case[str_which(case$all_dat, "Mos:") +1, 2][c(FALSE, TRUE)], 0))/12 +
             as.numeric(replace_na(case[str_which(case$all_dat, "Days:") +1, 2][c(FALSE, TRUE)], 0))/365,
           rep(NA, rows - sum(str_detect(case$all_dat, "Suspended Term:")
                              )
               )
         )
           } else {
             NA
           },
         charge_credit_timeserved = c(as.numeric(
             case[str_which(case$all_dat, "Credit Time Served:") +1, 2]),
             rep(NA, rows - sum(str_detect(case$all_dat, "Credit Time Served:")
                                )
                 )
             )
         )
   mdcs_charges_df <- bind_rows(mdcs_charges_df, case_charge)
}

# Save charges dataset
save(mdcs_charges_df,
     file = here::here("data/tidy_data",
                       "mdcs_charges_data.rda")
)


# MDCS Charges for Circuit ------------------------------------------------


# Find court case connections between district and circuit court cases
connect_cases <- mdcs_dist_data[str_detect(mdcs_dist_data$all_dat, 
                                           "CIRCUIT COURT CASE"),]
connect_cases <- connect_cases %>% 
  mutate(circ_case_num = str_extract(connect_cases$all_dat, 
                                     "\\d+[/,-]*\\d*")) %>%
  select(-all_dat) %>%
  rename(dist_case_num = case_num) %>%
  filter(!is.na(circ_case_num))

# Split into two--1:1 connections vs. 1:2+ connections
mult_connect_cases <- connect_cases %>%
  filter(str_detect(connect_cases$circ_case_num, "\\d+[/,-]\\d*"))

single_connect_cases <- connect_cases %>%
  filter(!str_detect(connect_cases$circ_case_num, "\\d+[/,-]\\d*"))

# export multi-connect cases for manual workup
write_csv(mult_connect_cases,
          path = here::here("data/raw_data",
                            "mult_connect_cases.csv"))

# I manually added rows for multiple cases, and did some sporadic confirmation
# on MDCS to see when things didn't add up

# Overwrite mult_connect_cases
mult_connect_cases <- read_csv(here::here("data/raw_data",
                    "mult_connect_cases_2.csv"),
                    col_types = "cc")

# Combine mult and single connect cases together again
connect_cases <- bind_rows(single_connect_cases, mult_connect_cases) %>%
  arrange(dist_case_num)

nrow(connect_cases %>% anti_join(mdcs_circ_cops_df, 
                            by = c("circ_case_num" = "case_num"))
)

# 216 rows don't match between the originally scraped circuit court dataset 
# and the new connects.
# May need to scrape additional cases, but for now proceeding

mdcs_circ_data_filt <- mdcs_circ_data %>% semi_join(connect_cases)

sum(
  as.numeric(
    mdcs_circ_data_filt$all_dat[str_which(mdcs_circ_data_filt$all_dat, "Yrs:") + 1]),
  na.rm = TRUE)
# 7749 years total so far


# MDCS Circuit Case Charges -----------------------------------------------


# Testing
case <- mdcs_circ_data_filt %>% 
  filter(circ_case_num == unique(mdcs_circ_data_filt$circ_case_num)[i])

mdcs_circ_charges_df <- c()

for(i in 1:length(unique(mdcs_circ_data_filt$circ_case_num))) {
  case <- mdcs_circ_data_filt %>% 
    filter(circ_case_num == unique(mdcs_circ_data_filt$circ_case_num)[i])
  rows <- sum(str_detect(case$all_dat, "Charge No:")) 
  case_charge <-
    tibble(
    # For Circuit court cases
      case_num = unique(mdcs_dist_data$case_num)[i],
      case_court = "Circuit",
      charge_num = case[str_which(case$all_dat, "Charge No:") +1, 2],
      charge_desc = case[str_which(case$all_dat, "Description:") +1, 2],
      charge_statute = NA,
      # Taking Filing Date as Amend Date for Circuit Court cases
      charge_amend_date = case[str_which(case$all_dat, "Filing Date:") +1, 2],
      charge_cjis_code = if("CJIS" %in% case$all_dat)
        {case[str_which(case$all_dat, "CJIS/Traffic Code:") +1, 2]}
      else 
        {NA},
      charge_mo_pll = NA,
      charge_prob_cause = NA,
      # Taking Incident Date as Incident Date From for CC cases
      charge_inc_date_from = if("Incident Date:" %in% case$all_dat)
        {as.Date(case[str_which(case$all_dat, "Incident Date:") +1, 2],
                format = "%m/%d/%Y")}
      else
        {NA},
      charge_inc_date_to = NA,
      charge_victim_age = NA,
      charge_plea = c(
        case[str_which(case$all_dat, "Plea:") +1, 2],
        rep(NA, rows - sum(str_detect(case$all_dat, "Plea:")
                           )
            )
        ),
      # Ignoring Plea Date for now
      charge_disposition = c(
        case[str_which(case$all_dat, "^Disposition:") +1, 2],
        rep(NA, rows - sum(str_detect(case$all_dat, "^Disposition:")
                           )
            )
        ),
      charge_disp_date = c(
        as.Date(case[str_which(case$all_dat, "Disposition Date:") +1, 2],
                format = "%m/%d/%Y"),
        rep(NA, rows - sum(str_detect(case$all_dat, "Disposition Date")
                           )
            )
        ),
      charge_fine = NA,
      charge_court_costs = NA,
      charge_cicf = NA,
      charge_susp_fine = NA,
      charge_susp_court_costs = NA,
      charge_susp_cicf = NA,
      charge_pbj_end_date = NA,
      charge_prob_end_date = NA,
      charge_rest_amt = NA,
      charge_jailtime = if("Sentence Time:" %in% case$all_dat)
      {c(
        as.numeric(replace_na(
          case[str_which(case$all_dat, "Sentence Time:") +2, 2], 0)) + 
          as.numeric(replace_na(
            case[str_which(case$all_dat, "Sentence Time:") +4, 2], 0))/12 +
          as.numeric(replace_na(
            case[str_which(case$all_dat, "Sentence Time:") +6, 2], 0))/365,
        rep(NA, rows - sum(str_detect(case$all_dat, "Sentence Time:")
                           )
            )
        )} 
      else 
        {NA},
      charge_susp_jailtime = if("Suspended Time:" %in% case$all_dat)
      {c(
        as.numeric(replace_na(
          case[str_which(case$all_dat, "Suspended Time:") +2, 2], 0)) + 
          as.numeric(replace_na(
            case[str_which(case$all_dat, "Suspended Time:") +4, 2], 0))/12 +
          as.numeric(replace_na(
            case[str_which(case$all_dat, "Suspended Time:") +6, 2], 0))/365,
        rep(NA, rows - sum(str_detect(case$all_dat, "Suspended Time:")
        )
        )
      )} 
      else 
      {NA},
      charge_credit_timeserved = NA,
      charge_prob_time = if("Probation Time:" %in% case$all_dat)
      {c(
        as.numeric(replace_na(
          case[str_which(case$all_dat, "Probation Time:") +2, 2], 0)) + 
          as.numeric(replace_na(
            case[str_which(case$all_dat, "Probation Time:") +4, 2], 0))/12 +
          as.numeric(replace_na(
            case[str_which(case$all_dat, "Probation Time:") +6, 2], 0))/365,
        rep(NA, rows - sum(str_detect(case$all_dat, "Probation Time:")
                           )
            )
      )} 
      else 
      {NA}
    )
  mdcs_circ_charges_df <- bind_rows(mdcs_circ_charges_df, case_charge)
}


# MDCS Charges Workup -----------------------------------------------------


# Add probation time to district court data
mdcs_charges_df <- mdcs_charges_df %>% mutate(charge_prob_time = NA)

# Combine MDCS district and circuit charges into same dataset
mdcs_charges_df <- bind_rows(mdcs_charges_df, mdcs_circ_charges_df)

# Check out charges
View(mdcs_charges_df %>% 
       group_by(charge_statute) %>%
       count(charge_desc) %>% 
       arrange(desc(n)))

# New assignment for charges
mdcs_charges_df <- mdcs_charges_df %>%
  mutate(
    charge_desc_2 = case_when(
      charge_statute == "CR.5.601.(a)(1)" & 
        !str_detect(charge_desc, "NOT") | 
        str_detect(charge_desc, "POSSESSION - MARI") |
        str_detect(charge_desc, "CDS: POSS[-\\s]MARI[HJ]UANA") |
        str_detect(charge_desc, "CON-CDS:POSS-MARIHUANA") ~ 
        "Marijuana Possession",
      charge_statute == "CR.5.601.(a)(1)" & 
        str_detect(charge_desc, "NOT|POSSESS-NOT") |
        str_detect(charge_desc, "POSSESS-NOT") |
        str_detect(charge_desc, "CDS-UNLAWFUL POSSESSION ETC") |
        str_detect(charge_desc, "CDS POSSESSION-CON") |
        str_detect(charge_desc, "CDS:POSSESSION") |
        str_detect(charge_desc, "CDS POSSESS - LG AMT") ~ 
        "Non-Marijuana Possession",
      str_detect(charge_statute, "CR.5.602") | 
        str_detect(charge_desc, "DIST") |
        str_detect(charge_desc, "NARC POSS W/") |
        str_detect(charge_desc, "CDS[-\\s]POSS W/I") |
        str_detect(charge_desc, "CDS-POSS WI MANUF/DIS/DISP-NARC-CON")	~ 
        "CDS Distribituion / Manufacture",
      str_detect(charge_desc, "PARA") ~ "CDS Paraphernalia Possession",
      str_detect(charge_desc, "FIREARM|RFL|PISTOL|RIFLE|GUN|AMMO") ~ 
        "Firearms-related",
      str_detect(charge_desc, "ASSAULT") |
        str_detect(charge_desc, "DANGEROUS WEAPON-INT/INJURE") ~
        "Assault",
      str_detect(charge_desc, "ARREST") ~ "Resisting Arrest",
      str_detect(charge_desc, "TRESPASS") ~ "Trespassing",
      str_detect(charge_desc, "THEFT|ROBB|BURG|	CARJACKING") ~ 
        "Theft / Burglary / Robbery",
      str_detect(charge_desc, "DISORDERLY CONDUCT") ~ "Disorderly Conduct",
      str_detect(charge_desc, "VIOLATION OF PROB") ~ "Probation Violation",
      TRUE ~ "Other"
    )
  ) 

# Check out charges again
View(mdcs_charges_df %>% 
       group_by(charge_desc_2) %>%
       count(charge_desc) %>% 
       arrange(desc(n)))


# Network dataset ---------------------------------------------------------


# Create charges dataset
mdcs_network_df <- c()

for(i in 1:length(unique(mdcs_all_data$case_num))) {
  case <- mdcs_all_data %>% 
    filter(case_num == unique(mdcs_all_data$case_num)[i])
  # If a District Court Case
  rows <- sum(str_detect(case$all_dat, "^Name:")) 
  case_network <-
    # if(str_detect(mdcs_cops_df[mdcs_cops_df$case_num == unique(mdcs_all_data$case_num)[i], 5], "District")) 
    # {
    tibble(
      case_num = unique(mdcs_all_data$case_num)[i],
      defendant_name = case[str_which(case$all_dat, "Defendant Name:")+1, 2],
      connection_name = case[str_which(case$all_dat, "^Name:")+1, 2],
      connection_type = case[str_which(case$all_dat, "Connection:")+1, 2]
    )
  mdcs_network_df <- bind_rows(mdcs_network_df, case_network)
}


# Network Workup ----------------------------------------------------------


# Remove blank rows
mdcs_network_df <- mdcs_network_df %>% filter(connection_name != "")

# Clean up GTTF officer names (at least for now)
mdcs_network_df <- mdcs_network_df %>% 
  mutate(
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "ALLERS"), "ALLERS, THOMAS"),
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "GONDO"), "GONDO, MOMODU"),
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "HENDRIX"), "HENDRIX, EVODIO"),
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "HERSL"), "HERSL, DANIEL"),
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "JENKINS, W"), "JENKINS, WAYNE"),
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "RAYAM"), "RAYAM, JEMELL"),
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "TAYLOR, [MO]"), "TAYLOR, MARCUS"),
    connection_name = replace(
      connection_name, str_detect(mdcs_network_df$connection_name, "^WARD, M"), "WARD, MAURICE")
    ) 

# Save dataset
save(mdcs_network_df,
     file = here::here("data/tidy_data",
                       "mdcs_network_data.rda")
)