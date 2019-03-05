
# practice ----------------------------------------------------------------

fruits <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)

str_split(fruits, " and ")
str_split(fruits, " and ", simplify = TRUE)
# Specify n to restrict the number of possible matches
str_split(fruits, " and ", n = 3)
str_split(fruits, " and ", n = 2)
# If n greater than number of pieces, no padding occurs
str_split(fruits, " and ", n = 5)
# Use fixed to return a character matrix
str_split_fixed(fruits, " and ", 3)
str_split_fixed(fruits, " and ", 4)



# bringing sets together with diff values ---------------------------------
mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]

intersect(first, second)
union(first, second)
setdiff(first, second)
setdiff(second, first)

union_all(first, second)
setequal(mtcars, mtcars[32:1, ])

# Handling of duplicates:
a <- data.frame(column = c(1:10, 10))
b <- data.frame(column = c(1:5, 5))

# intersection is 1 to 5, duplicates removed (5)
intersect(a, b)

# union is 1 to 10, duplicates removed (5 and 10)
union(a, b)

# set difference, duplicates removed (10)
setdiff(a, b)

# union all does not remove duplicates
union_all(a, b)

# work_up -----------------------------------------------------------------
no_geo = no_geo_case
# taking away so I can get unique values
# have `no_geo_case`to ref for `$case_num`
no_geo = no_geo %>%
  select(-case_num)
# takes rowname col into actual rownames
no_geo = column_to_rownames(no_geo)
# only gives unique addresses and keeps rownames
no_geo = unique(no_geo)
# add rowname col back after only getting unique addys
# rownames basically = obs_num
# use this to join again
# no_geo = rownames_to_column(no_geo)

problem_rows = as_tibble(str_split_fixed(no_geo$full_address,
                ", ",
                n = 5))
problem_rows = paste0(str_c(problem_rows$V1,
                            ", ",
                            problem_rows$V3,
                            ", ",
                            problem_rows$V5
                            ))
# converts from vector to df or tibble
problem_rows = enframe(problem_rows)

# renaming for future cleaning
colnames(problem_rows)[1] <- "rowname"
colnames(problem_rows)[2] <- "full_address"

# adds correct obs_num to tbl_df/tbl/data.frame
problem_rows = problem_rows %>%
  mutate(rowname = rownames(no_geo))
# sets appropriate "i" for while loop below. 
problem_rows = column_to_rownames(problem_rows)

# wrote this line of code in case 
# I needed the distinct values, but unsure about whether the 
# rownames will remain!!!!
# problem_rows_unique = unique(problem_rows$full_address)
# rownames will not remain!

# creating var
new_geo <- c()

i = 1
while(i <= length(problem_rows$full_address)) {
  no_geo_row <- geocode_OSM(problem_rows$full_address[i])
  if(is.null(no_geo_row)){
    i <- i + 1
    next;
  }
  new_geo_row <- tibble(obs_num = i, 
                        full_address = no_geo_row[[1]],
                        # up and down
                        lon = no_geo_row[[2]][1],
                        # left to right
                        lat = no_geo_row[[2]][2]
                        )
  # creating geo_bmore sp object
  new_geo <- bind_rows(new_geo, new_geo_row)
  i <- i + 1
}

problem_rows$full_address[i]
i <- i+1

save(new_geo,
     file = here::here("data/tidy_data",
                       "new_geo.rda"))
