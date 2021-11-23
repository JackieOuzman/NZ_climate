library(tidyverse)
library(readxl)

################################################################################################

###################          example dataset                           #########################

################################################################################################

martix_1 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix1_small.csv")
martix_2 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix2_small.csv")
martix_3 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix3_small.csv")


print(martix_1)
matrix_1_long <- pivot_longer(martix_1,)

matrix_1_long <-
  martix_1 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "X"
  )
print(matrix_2_long)

matrix_2_long <-
  martix_2 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "Y"
  )

matrix_3_long <-
  martix_3 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "value"
  )


# join them togther

coord <- left_join(matrix_1_long, matrix_2_long)
coord_value <- left_join(coord, matrix_3_long)
coord_value


################################################################################################

###################          real dataset                           #########################

################################################################################################

martix_1 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix1.csv")
martix_2 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix2.csv")
martix_3 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix3.csv")





matrix_1_long <-
  martix_1 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "X"
  )


matrix_2_long <-
  martix_2 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "Y"
  )

matrix_3_long <-
  martix_3 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "value"
  )


# join them togther

coord <- left_join(matrix_1_long, matrix_2_long)
coord_value <- left_join(coord, matrix_3_long)
coord_value


