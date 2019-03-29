# doesn't work for me -----------------------------------------------------
library(dplyr)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)


# Spread and gather are complements
df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
df %>% spread(x, y) %>% gather(x, y, a:b, na.rm = TRUE)

mdcs_cops_df %>% spread(case_num, gttf_cop)

# Use 'convert = TRUE' to produce variables of mixed type
df <- data.frame(row = rep(c(1, 51), each = 3),
                 var = c("Sepal.Length", "Species", "Species_num"),
                 value = c(5.1, "setosa", 1, 7.0, "versicolor", 2))
df %>% spread(var, value) %>% str
df %>% spread(var, value, convert = TRUE) %>% str



# overwhelmed -------------------------------------------------------------

# reference
# https://datascienceplus.com/spread-gather-separate-and-unite-variables-and-datasets-in-r/

sample_cop = mdcs_cops_df # %>% gather(name, case_type, -case_num)

sample_cop %>%
  group_by(case_num) %>%
  spread(gttf_cop, case_num)

mdcs_cops_df %>%
  group_by(case_num) %>%
  summarise(cop_count = n_distinct(gttf_cop),
            names_involved = print(unique(gttf_cop))) %>%
  arrange(desc(cop_count))

sample_cop$case_num <- rownames(sample_cop)

(x <- c(sort(sample(1:20, 9)), NA))
(y <- c(sort(sample(3:23, 7)), NA))
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
setequal(x, y)

# Ken's code

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


mutate( = case_when(
  case_type %in% c("CR", "Criminal") ~ "Criminal",
  case_type == "Appeal" ~ "Appeal",
  TRUE ~ "Other"
))


# using built in dataset --------------------------------------------------

# get first observation for each Species in iris data -- base R
mini_iris <- iris[c(1, 51, 101), ]

# gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
gather(mini_iris, key = flower_att, value = measurement,
       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

# same result but less verbose
gather(mini_iris, key = flower_att, value = measurement, -Species)

# lets try with the cops df
sample_cop <- mdcs_cops_df[c(1, 1510, 10001), ]

# dplyr version -----------------------------------------------------------
# repeat iris example using dplyr and the pipe operator
library(dplyr)
mini_iris <-
  iris %>%
  group_by(Species) %>%
  slice(1)
mini_iris %>% gather(key = flower_att, value = measurement, -Species)