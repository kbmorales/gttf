# loading .csv file into the environment
arrest_data <- read_csv("data/raw_data/arrests_data/BPD_Arrests.csv")

# how the columns were saved after importing the data set
# spec(arrest_data)
# cols(
#   Arrest = col_double(),
#   Age = col_double(),
#   Sex = col_character(),
#   Race = col_character(),
#   ArrestDate = col_character(),
#   ArrestTime = col_time(format = ""),
#   ArrestLocation = col_character(),
#   IncidentOffense = col_character(),
#   IncidentLocation = col_character(),
#   Charge = col_character(),
#   ChargeDescription = col_character(),
#   District = col_character(),
#   Post = col_double(),
#   Neighborhood = col_character(),
#   Longitude = col_double(),
#   Latitude = col_double(),
#   `Location 1` = col_character(),
#   `2010 Census Neighborhoods` = col_double(),
#   `2010 Census Wards Precincts` = col_double(),
#   `Zip Codes` = col_double()
# )

# explore and get a sense of the data!
# from first glance this can easily be joined to our existing datasets.
# but we will still explore this data

# before we mess with the data let's clean colnames
# can't really play with data with spaces in colnames

arrest_data %>%
  make.names()


# arrest col
