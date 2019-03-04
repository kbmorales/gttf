
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


# work_up -----------------------------------------------------------------
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


