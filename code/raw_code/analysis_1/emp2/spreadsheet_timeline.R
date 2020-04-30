library(googlesheets)
library(dplyr)


# SUB README SECTION ------------------------------------------------------
# creating spreadsheet with googlesheets::

# CREATING A DF -----------------------------------------------------------
# per client request will create small googlesheet
# doc and provide: cop name, hire date, first haneous act, 
# and recruitment into the gttf. 

copsdf <- data.frame(Cop.Name = c("Gladstone, Keith Allen Sgt.",
                                           "Gladstone, Keith Allen Sgt.",
                                           "Allers, Thomas Sgt.",
                                           "Allers, Thomas Sgt.",
                                           "Allers, Thomas Sgt.",
                                           "Jenkins, Wayne Earl Sgt.",
                                           "Hersl, Thomas Daniel Dt.",
                                           "Taylor, Marcus Roosevelt Dt.",
                                           "Gondo, Momodu Bondeva Kenton Dt.",
                                           "Rayam, Jemell Lamar Dt.",
                                           "Ward, Maurice Kilpatrick Dt.",
                                           "Hendrix, Evodio Calles Dt."
                                           ),
                 Hire.Date = c("11-20-1992",
                                               "12-09-2013",
                                               "07-22-1996",
                                               "07-22-1996",
                                               "07-22-1996",
                                               "02/20/2003",
                                               "09-07-1999",
                                               "05-18-2009",
                                               "11-29-2005",
                                               "07-12-2005",
                                               "10-08-2003",
                                               "04-02-2009"
                                                         ),
                 District.or.Division = c("North Western",
                                                       "Western ~ SES",
                                                       "Southern",
                                                       "Southern",
                                                       "Southern",
                                                       "Western",
                                                       "Eastern",
                                                       "SES",
                                                       "NA",
                                                       "NA",
                                                       "SES",
                                                       "Western"
                                                       ),
                 First.Haneous.Act = c("04-28-2010",
                                                       "NA",
                                                       "01-01-2014",
                                                       "01-01-2015",
                                                       "01-01-2016",
                                                       "NA",
                                                       "NA",
                                                       "NA",
                                                       "NA",
                                                       "NA",
                                                       "NA",
                                                       "NA"
                                                       ),
                 Description = c("Allegedly supplied drugs to Wayne Jenkins in Umar Burley's car.",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA",
                                              "NA"
                                              ),
                 GTTF.Start.Date = c("NA",
                                                "NA",
                                                "06-14-2013",
                                                "06-14-2013",
                                                "06-14-2013",
                                                "06-13-2016",
                                                "04-28-2016",
                                                "06-13-2016",
                                                "01-01-2010",
                                                "01-01-2010",
                                                "06-13-2016",
                                                "06-13-2016"
                                                ),
                 Fire.or.Retire.Date = c("12-25-2012",
                                                    "05-01-2017",
                                                    "08-30-2017",
                                                    "08-30-2017",
                                                    "08-30-2017",
                                                    "03-01-2017",
                                                    "03-01-2017",
                                                    "03-01-2017",
                                                    "02-12-2019",
                                                    "03-01-2017",
                                                    "03-01-2017",
                                                    "03-01-2017"
                                                    ))
                 
 








# MAKING GOOGLESHEET.  ----------------------------------------------------
options(httr_oob_default = TRUE)

# creates new googlesheet
cops_sheet <- gs_new("GTTF Data Timeline")

# creates new ws AND adds data
cops_sheet <- gs_edit_cells(cops_sheet,
                            input = copsdf,
                            trim = TRUE)
# check out progress
gs_read(cops_sheet)


