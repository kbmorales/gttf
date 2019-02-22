# Database drafts
library(tibble)
library(here)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

cops <- tibble(
  last_name = c("Allers", "Gondo", "Hendrix", "Jenkins", "Rayam", "Ward", "Hersl", "Taylor", "Clewell"),
  first_name = c("Thomas", "Momodu", "Evodio", "Wayne", "Jemell", "Maurice", "Daniel", "Marcus", "John")
)


save(cops, 
     file = "data/tidy_data/cops_names.rda")

save(mdcs_cops_df,
     file = "data/tidy_data/mdcs_cops_df.rda")

# Start Date - 01/01/2008
# End Date - 03/01/2017
# reading in case numbers as .csv files
# if case search is over 500 separate .csv into years
# load into /raw_data
# MIDDLE INITIAL FOR GONDO - K
# MIDDLE INITIAL FOR HENDRIX - C
# MIDDLE INITIAL FOR JENKINS - E for Earl 
# MIDDLE INITIAL FOR Ward - K; is a jr

######################## compiling .csv files into 1 with Ken's code
# filter out Clewell!!!!! 

# Allers, Thomas ----------------------------------------------------------


mdcs_allers <- c()

for(i in 2008:2017){
  mdcs_allers <- bind_rows(mdcs_allers,
                          read_csv(here("data/raw_data",
                                        str_c("Allers_", i, ".csv")),
                                   col_names = FALSE))
}

save(mdcs_allers, 
     file = here("data/tidy_data",
                 "mdcs_allers.rda"))

# Gondo, Momodu -----------------------------------------------------------

mdcs_gondo <- c()

for(i in 2008:2017){
  mdcs_gondo <- bind_rows(mdcs_gondo,
                           read_csv(here("data/raw_data",
                                         str_c("Gondo_", i, ".csv")),
                                    col_names = FALSE))
}

save(mdcs_gondo, 
     file = here("data/tidy_data",
                 "mdcs_gondo.rda"))


# Hendrix, Evodio ---------------------------------------------------------

mdcs_hendrix <- c()

for(i in 2008:2017){
  mdcs_hendrix <- bind_rows(mdcs_hendrix,
                           read_csv(here("data/raw_data",
                                         str_c("Hendrix_", i, ".csv")),
                                    col_names = FALSE))
}

save(mdcs_hendrix, 
     file = here("data/tidy_data",
                 "mdcs_hendrix.rda"))


# Jenkins, Wayne ----------------------------------------------------------

mdcs_jenkins <- c()

for(i in 2008:2017){
  mdcs_jenkins <- bind_rows(mdcs_jenkins,
                           read_csv(here("data/raw_data",
                                         str_c("Jenkins_", i, ".csv")),
                                    col_names = FALSE))
}

save(mdcs_jenkins, 
     file = here("data/tidy_data",
                 "mdcs_jenkins.rda"))






# MDCS mark up ------------------------------------------------------------
mdcs_cops_df <- bind_rows(mdcs_allers, 
          mdcs_clewell, 
          mdcs_gondo, 
          mdcs_hendrix, 
          mdcs_hersl, 
          mdcs_jenkins, 
          mdcs_rayam, 
          mdcs_taylor,
          mdcs_ward) %>%
  mutate(X8 = as.Date(X8, format = "%m/%d/%Y")) %>%
  rename(case_num = X1,
         name = X2,
         dob = X3,
         party_type = X4,
         court = X5,
         case_type = X6,
         status = X7,
         
         date = X8,
         caption = X9) %>%
  # different cases to not lose data
  mutate(case_type_2 = case_when(
    case_type %in% c("CR", "Criminal") ~ "Criminal",
    case_type == "Appeal" ~ "Appeal",
    TRUE ~ "Other"
  )) %>%
  # Filter out bad Taylor names
  filter(name != "Taylor, Marcus Randolph" &
           name != "Taylor, Marcus Rezan" &
           name != "Taylor, Marcus Rezan Jr" &
           name != "Allers, Thomas T" &
           name != "Jenkins, Wayne A" &
           name != "Jenkins, Wayne Anthony" &
           name != "Jenkins, Wayne Edward II" &
           name != "Jenkins, Wayne Jarrell" &
           name != "Jenkins, Wayne L" &
           name != "Jenkins, Wayne Lee" &
           name != "Jenkins, Wayne Lee Jr" &
           name != "Jenkins, Wayne M II" &
           name != "Jenkins, Wayne Maurice" &
           name != "Jenkins, Wayne Maurice II" &
           name != "Jenkins, Wayne T" &
           name != "Ward, Maurice A" &
           name != "Ward, Maurice A Jr" &
           name != "Ward, Maurice A Jr." &
           name != "Ward, Maurice A Sr" &
           name != "Ward, Maurice A. Jr" &
           name != "Ward, Maurice Alexander Jr" &
           name != "Ward, Maurice D" &
           name != "Ward, Maurice Devon" &
           name != "Ward, Maurice Douglas" &
           name != "Ward, Maurice Ethan" &
           name != "Ward, Maurice L" &
           name != "Ward, Maurice Lanier" &
           name != "Ward, Maurice R" &
           name != "Ward, Maurice Reginald" &
           name != "Ward, Maurice S" &
           name != "Ward, Maurice Xavier" 
           ) %>%
  # filter(name != "")
  mutate(gttf_cop = case_when(
    str_detect(name, "Clewell") ~ "Clewell",
    str_detect(name, "Hersl") ~ "Hersl",
    str_detect(name, "Rayam") ~ "Rayam",
    str_detect(name, "Taylor") ~ "Taylor",
    str_detect(name, "Gondo") ~ "Gondo",
    str_detect(name, "Allers") ~ "Allers",
    str_detect(name, "Jenkins") ~ "Jenkins",
    str_detect(name, "Ward") ~ "Ward",
    str_detect(name, "Hendrix") ~ "Hendrix"
    ))



# Let's create plots ------------------------------------------------------
# distinctive cases
mdcs_cops_df %>%
  group_by(gttf_cop) %>%
  summarise(case_count = n_distinct(case_num)) %>%
  arrange(case_count)

# cops_by_case <- 
  mdcs_cops_df %>%
  group_by(case_num) %>%
  summarise(gttf_cop = n()) %>%
  arrange(desc(gttf_cop))
  
  mdcs_cops_df %>%
    group_by(case_num) %>%
    summarise(cop_count = n_distinct(gttf_cop)) %>%
    arrange(desc(cop_count)) %>%
    filter(cop_count >= 2) %>%
    mutate(case_type_2 = case_when(
      case_type %in% c("CR", "Criminal") ~ "Criminal",
      case_type == "Appeal" ~ "Appeal",
      TRUE ~ "Other"
    ))

  # # A tibble: 1,368 x 2
  # case_num   cop_count
  # <chr>          <int>
  # 1 3B02329813         7
  # 2 116195014          6
  # 3 116242002          6
  # 4 3B02327881         6
  # 5 416203007          6
  # 6 816281001          6
  # 7 816348010          6
  # 8 0B02329355         5
  # 9 0B02334234         5
  # 10 116133006         5

# distinctive case numbers ~ n_distinct(case_num)
# gttf_cop     case_count
# <chr>            <int>
# 1 Allers         1147
# 2 Clewell        1461
# 3 Gondo           689
# 4 Hendrix         791
# 5 Hersl          2413
# 6 Jenkins         987
# 7 Rayam           770
# 8 Taylor          516
# 9 Ward           1660

# total case numbers ~ n()
# A tibble: 9 x 2
# gttf_cop     case_count
# <chr>            <int>
# 1 Allers         1415
# 2 Clewell        1695
# 3 Gondo           697
# 4 Hendrix         910
# 5 Hersl          2561
# 6 Jenkins        1078
# 7 Rayam           848
# 8 Taylor          545
# 9 Ward           1943


# graphing plots ----------------------------------------------------------
# trying to create a graph that lists the date[year] the case
# was filed, who filed, and status of case
  # proportion plot OR filter out closed cases
mdcs_cops_df %>%
  filter(case_type_2 == "Criminal") %>%
ggplot(aes(x = date)) +
  geom_histogram(aes(fill = gttf_cop),
           # position = "dodge"
           ) +
  theme(text = element_text(size=15)) +
  theme(legend.position="bottom") +
  ggtitle("Criminal Case Records Status") +
  # labs(x = "Status",
  #      y = "Date",
  #      fill = "Cop Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  # + 
  # geom_point(shape=21,color="grey",size=2) + 
  # geom_smooth(aes(color = gttf_cop),
  #             size = 1.5,
  #             linetype = "longdash",
  #             alpha = 0.5,
  #             method = "lm",
  #             fullrange = TRUE,
  #             se = FALSE,
  #             show.legend = FALSE)
  # 

  # REFERENCE: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Scatterplot
  
  
# eh, it's not what im looking for but this is just up
 theme_set(theme_minimal())

 mdcs_cops_df %>%
   filter(case_type_2 == "Criminal") %>%
   ggplot(aes(x = date, y = status)) +
   geom_point(aes(col = gttf_cop)) +
   theme(text = element_text(size=15)) +
   theme(legend.position="bottom") +
   ggtitle("Criminal Case Records Status") +
   theme(axis.text.x = element_text(angle = 45, hjust =1))
 
# correlation matix? -- correlogram????
library(ggcorrplot)
 
 # creating the matrix
 # cannot use this plot because data is not numeric :/
 
 # diverging bars!
 # correlation in number of arrests before and after initial investigation? aprox 2015
 
 # setting up data -- remember to use sample_cop DF!!
 # i want to see correlation of case counts dependent on the start of investigation of 2015
 # creaates new column in DF and labels a row before or after based on information provided.
 # this column will come in handy when plotting the graph!
 sample_cop$tl_z <- ifelse(mdcs_cops_df$date < "2014-12-31", "before", "after")

 # count of the number of times the case_num pops up
   ncase <- sample_cop %>% count(case_num)
   # join two datasets
   # this creates the column  `n` to be the amount of time the case appears within the data set!
   sample_cop <- left_join(sample_cop, ncase)
 
 sample_cop$tl_z <- factor(sample_cop$tl_z, levels = c("before", "after"))
 
  # puts DF in order of col$tl_z
  sample_cop <- sample_cop[order(sample_cop$tl_z), ]

# this is supposed to keep things ordered?? this retains order for the plot
  # sample_cop$gttf_cop <- factor(n_distinct(sample_cop$gttf_cop),
  #                               levels = sample_cop$gttf_cop)
  # commenting out because this doesn't work for me
  # trying out different method below
  # sample_cop$gttf_cop <- factor(unique(sample_cop$gttf_cop),
  #                               levels = unique(sample_cop$gttf_cop))
  
  ############ CODE WORKS BUT NOT NECCESSARY OOOORRR EASY LOL
  # for(i in 1:nrow(sample_cop)){
  #   factor(unique(sample_cop$gttf_cop),
  #          levels = unique(sample_cop$gttf_cop))
  # }

  # sample_cop$gttf_cop <- factor(unique(sample_cop$gttf_cop),
  #                               levels = unique(sample_cop$gttf_cop))

  # no bueno :(
  # will as.factor give same result?

  
ggplot(sample_cop, aes(x = gttf_cop,
                       y = date)
       ) +
    geom_point(aes(color = tl_z)) +
    scale_color_manual(name = "Before and After Investigation Starts",
                    labels = c("After", "Before"),
                    values = c("#2282d6", "#960907"))+
  coord_flip()
    
  
  
    geom_bar(stat = 'identity',
             aes(fill = tl_z),
             width = 4) +
    scale_fill_manual(name = "Before and After Investigation Starts",
                      labels = c("Before", "After"),
                      values = c("before" = "#2282d6", "after" = "#960907")
                      ) +
    scale_y_date(labels = date_format("%Y")) +
    theme(axis.text.x = element_text(angle = 45, hjust =1)) +
    coord_flip()
  
    # maybe a population pyramid is what I am looking for
    # will show: names of cops, n(cases filed), and whether it was filed before or after
    # the investigation starts. 
    
    # creates variables that makes the x-axis breaks
    brks <- seq(-2600, 2600, 200)
    
    # creates variable that pastes label together
    # example
    lbls = paste0(as.character(c(seq(2600, 0, -200), seq(200, 2600, 200))), " cases")
    
    # 
    # sample_set = sample_cop %>%
    #   group_by(gttf_cop, tl_z) %>%
    #   summarise(case_count = n_distinct(case_num)) %>%
    #   filter(gttf_cop != "Clewell") %>%
    #   filter(tl_z != "NA")
      

      ggplot(sample_set, aes(x = gttf_cop,
                 y = case_count,
                 fill = tl_z)) +
      # geom_col( stat = "identity",
               # width = 1
               # ) +
      geom_col(data = subset(sample_set, tl_z == "after")) +
      geom_col(data = subset(sample_set, tl_z == "before"), aes(y=case_count*(-1))) +
      scale_y_continuous(breaks = brks,
                         labels = lbls) +
      coord_flip() +
      labs(title = "Case Count",
           subtitle = "based on the year 2015, which investigation starts") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5),
            axis.ticks = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust =1)) +
      scale_fill_brewer(palette = "Set2")
      
    
    
    
  