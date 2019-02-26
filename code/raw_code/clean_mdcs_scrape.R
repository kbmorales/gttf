###
### Clean MDCS Scraped Data
###

# MDCS Demographic Data -----------------------------------------------


# Load in scraped dataset
load(file = "data/raw_data/mdcs_case_data.rda")

# Filter for case numbers in mdcs_cops_df
mdcs_all_data <- mdcs_all_data %>% semi_join(mdcs_cops_df)

# Create demo dataset
mdcs_demo_df <- c()

for(i in 1:length(unique(mdcs_all_data$case_num))) {
  # Break down if circuit or district court:
  mdcs_demo_row <- 
    # If a District Court Case
    if(str_detect(mdcs_cops_df[mdcs_cops_df$case_num == unique(mdcs_all_data$case_num)[i], 5], "District")) {
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
    } else {
      # For Circuit court cases
      mdcs_all_data %>%
        filter(case_num == unique(mdcs_all_data$case_num)[i]) %>%
        mutate(court_system = str_replace_all(trimws(all_dat[str_which(all_dat, "Court System:")+1]
        ),
        "\\s*-?\n\\s+|\\s{2}", " "),
        status_date = if("Status Date:" %in% all_dat){
          all_dat[str_which(all_dat, "Status Date:")+1]}
        else {
          "Unknown"
        },
        tracking_num = if("Tracking Number:" %in% all_dat){
          all_dat[str_which(all_dat, "Tracking Number:")+1]
        } else {
          "Unknown"
        },
        district_code = NA,
        location_code = NA,
        doc_type = NA,
        case_disposition = NA,
        complaint_num = if("Complaint No:" %in% all_dat) {
          all_dat[str_which(all_dat, "Complaint No:")+1]
        } else {
          "Unknown"
        },
        district_case_num = if("District Case No:" %in% all_dat){
          all_dat[str_which(all_dat, "District Case No:")+1]
        } else {
          "Unknown"
        },
        defendant_name = all_dat[str_which(all_dat, "Defendant Name:")+1],
        defendant_race = if("Race:" %in% all_dat){
          all_dat[str_which(all_dat, "Race:")+1]
        } else {
          "Unknown"
        },
        defendant_sex = all_dat[str_which(all_dat, "Sex:")+1],
        # defendant_height = sum(as.numeric(str_split(all_dat[str_which(all_dat, "Height:")+1],
        #                                             "",
        #                                             n = 2,
        #                                             simplify = TRUE)
        # ) * c(12,1)
        # ),
        # defendant_weight = as.numeric(all_dat[str_which(all_dat, "Weight:")+1]),
        defendant_dob = if("DOB:" %in% all_dat){
          as.Date(all_dat[str_which(all_dat, "DOB:")+1],
                                format = "%m/%d/%Y")
        } else {
          NA
        },
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
    }
  mdcs_demo_df <- bind_rows(mdcs_demo_df, mdcs_demo_row)
}

rm(i)

# A lot of these had to be removed manually with the folowing code as the loop broke,
# added to weird_cases. Most are torts but:

# test_data <- mdcs_all_data %>% filter(case_num == unique(mdcs_all_data$case_num)[i])
# weird_cases[[x]] <- test_data
# i <- i + 1

mdcs_demo_df <- mdcs_demo_df %>% select(-all_dat)

save(mdcs_demo_df,
     file = "data/tidy_data/mdcs_demo_data.rda")

# Charges dataset ---------------------------------------------------------

# Need to create separate datasets for charges

# charge_num = all_dat[str_which(all_dat, "Charge No:")+1],
# charge_desc = all_dat[str_which(all_dat, "Description:")+1],
# charge_desc = all_dat[str_which(all_dat, "Description:")+1]
