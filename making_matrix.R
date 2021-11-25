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

write.csv(coord_value, "V:/Marlborough regional/climate/jax_working/coord_value.csv")

## it does not look quite right ? I am not sure about this flip comment?

##################################################################################################################################



martix_1 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix1_raw.csv")
martix_2 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix2_raw.csv")
martix_3 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix3_raw.csv")

names(martix_2)

martix_2 <- martix_2 %>% dplyr::select(-"X")

str(martix_1)

matrix_1_long <-
  martix_1 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "X"
  )


matrix_2_long <-
  martix_2 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "Y"
  )

str(martix_3)
matrix_3_long <-
  martix_3 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "value"
  )


# join them togther

coord <- cbind(matrix_1_long, matrix_2_long)
coord_value <- cbind(coord, matrix_3_long)
names(coord_value)

coord_value <- coord_value %>% dplyr::select("X", "Y", "value")

coord_value <- coord_value %>% mutate(id = row_number())

write.csv(coord_value, "V:/Marlborough regional/climate/jax_working/coord_value_raw.csv")
### still not right!!!####



##################################################################################################################################



martix_1 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix1_raw.csv")
martix_2 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix2_raw.csv")
martix_3 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix3_flipV.csv")

names(martix_2)

martix_2 <- martix_2 %>% dplyr::select(-"X")

str(martix_1)

matrix_1_long <-
  martix_1 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "X"
  )


matrix_2_long <-
  martix_2 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "Y"
  )

str(martix_3)
matrix_3_long <-
  martix_3 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "value"
  )


# join them togther

coord <- cbind(matrix_1_long, matrix_2_long)
coord_value <- cbind(coord, matrix_3_long)
names(coord_value)

coord_value <- coord_value %>% dplyr::select("X", "Y", "value")

coord_value <- coord_value %>% mutate(id = row_number())

write.csv(coord_value, "V:/Marlborough regional/climate/jax_working/coord_value_Vflip.csv")


##################################################################################################################################

### Flip Horizontal ###

martix_1 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix1_Flip_H.csv")
martix_2 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix2_Flip_H.csv")
martix_3 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix3_Flip_H.csv")

names(martix_2)

martix_2 <- martix_2 %>% dplyr::select(-"X")

str(martix_1)

matrix_1_long <-
  martix_1 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "X"
  )


matrix_2_long <-
  martix_2 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "Y"
  )

str(martix_3)
matrix_3_long <-
  martix_3 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "clm_ID",
    values_to = "value"
  )


# join them togther

coord <- cbind(matrix_1_long, matrix_2_long)
coord_value <- cbind(coord, matrix_3_long)
names(coord_value)

coord_value <- coord_value %>% dplyr::select("X", "Y", "value")

coord_value <- coord_value %>% mutate(id = row_number())

write.csv(coord_value, "V:/Marlborough regional/climate/jax_working/coord_value_Hflip.csv")



##################################################################################################################################

### Flip V and Horizontal again ###

martix_1 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix1_Flip_V_H.csv")
martix_2 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix2_Flip_V_H.csv")
martix_3 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix3_Flip_V_H.csv")

names(martix_2)

#martix_2 <- martix_2 %>% dplyr::select(-"X")

str(martix_1)

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

str(martix_3)
matrix_3_long <-
  martix_3 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "value"
  )


# join them togther

coord <- cbind(matrix_1_long, matrix_2_long)
coord_value <- cbind(coord, matrix_3_long)
names(coord_value)

coord_value <- coord_value %>% dplyr::select("X", "Y", "value")

coord_value <- coord_value %>% mutate(id = row_number())

write.csv(coord_value, "V:/Marlborough regional/climate/jax_working/coord_value_V_Hflip.csv")



##################################################################################################################################

### Flip V and Horizontal again ###

martix_1 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix1_Flip_V_H.csv")
martix_2 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix2_Flip_V_H.csv")
martix_3 <- read.csv("V:/Marlborough regional/climate/jax_working/matrix3_Flip_V_H_H.csv")

names(martix_2)

#martix_2 <- martix_2 %>% dplyr::select(-"X")

str(martix_1)

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

str(martix_3)
matrix_3_long <-
  martix_3 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "clm_ID",
    values_to = "value"
  )


# join them togther

coord <- cbind(matrix_1_long, matrix_2_long)
coord_value <- cbind(coord, matrix_3_long)
names(coord_value)

coord_value <- coord_value %>% dplyr::select("X", "Y", "value")

coord_value <- coord_value %>% mutate(id = row_number())

write.csv(coord_value, "V:/Marlborough regional/climate/jax_working/coord_value_V_H_Hflip.csv")



