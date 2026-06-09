###################################################################################################
                            # Step 1: Loading the data
###################################################################################################

# At present, there are XXXXXX main files to be used: 

    # 1~ Voting data with the election results from 2025
    # 2~ Stembureau location file

# We begin by loading the voting data

library(readr)
voting_data <- read_delim("TK2025_Stemmen_Per_Lijst_Per_Stembureau.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)



location_stembureau <- read_csv("Stembureau_Locatie_Nederland_VK_2025.csv")



###################################################################################################
                              # Step 2: Merging the databases     
###################################################################################################


# We can see that the two databases have the following columns in common

# Gemeentecode vs CBS gemeentecode: The format in voting_data is just numbers, location_stembureau has GM0XXX
# StembureauCode vs Nummer stembureau: The format in voting_data is SBXXX, location_stembureau has the number
# StembureauNaam vs Naam stembureau: The format in voting_data is Stembureau NAME (postcode), location_stembureau has just the name

# For the analysis to be possible we need to add the following columns to voting_data

          # 1~ Gebruiksdoel van het gebouw
          # 2~ Straatnaam
          # 3~ Huisnummer
          # 4~ Huisletter
          # 5~ Huisnummertoevoeging
          # 6~ X
          # 7~ y
          # 8~ Latitude
          # 9~ Longitude

# However, before doing this it is important to match the formats. These modifications increase the rate of success
# the linkage package will have as entries will be more similar to one another. Furthermore, the with the columns
# having similar names it will become easier to locate and compare the data.

library(dplyr)
library(stringr)
library(reclin2)

voting_data <- voting_data |>
  mutate(
    StembureauCode = str_remove(StembureauCode, "^SB"),
    StembureauNaam = StembureauNaam |>
      str_remove("^Stembureau\\s*") |>
      str_remove("\\s*\\(postcode:.*\\)$"))

voting_data$StembureauCode <- as.numeric(voting_data$StembureauCode)


location_stembureau <- location_stembureau |>
  mutate(`CBS gemeentecode` = str_remove(`CBS gemeentecode`, "^GM"))

location_stembureau$`CBS gemeentecode` <- as.numeric(location_stembureau$`CBS gemeentecode`)


# We now make sure the colnames match

colnames(location_stembureau)[colnames(location_stembureau) == "CBS gemeentecode"] <- "GemeenteCode"

colnames(location_stembureau)[colnames(location_stembureau) == "Gemeente"] <- "GemeenteNaam"

colnames(location_stembureau)[colnames(location_stembureau) == "Naam stembureau"] <- "StembureauNaam"

colnames(location_stembureau)[colnames(location_stembureau) == "Nummer stembureau"] <- "StembureauCode"

# We now need to carry out record linkage. 

# It is important to mention that a lot of editing will be carried out so that the stembureaus match,
# as such, creating a unique voting booth id will help us to keep track of which stembureaus we're looking at.

# Since the linking needs to be done for each stembureau, a dataframe that has one row per unique stembureau is needed
# We need to get rid of duplicates and any columns that will artificially inflate the unique rows of the stembureaus.


v_df <- voting_data |>
  select(-PartijNaam, -AantalStemmen, -StembureauCode)

voting_unique <- v_df |>
  distinct() |>
  mutate(vote_booth_id = row_number())


# We will need to do join the location data later to the original voting_data, so we assign the voting_booth_id to their
# corresponing stembureau in the original database.

voting_data <- voting_data |>
  left_join(voting_unique |>
              select(GemeenteCode, 
                     GemeenteNaam, 
                     StembureauNaam,
                     Postcode,
                     vote_booth_id),
            by = c("GemeenteCode", "GemeenteNaam", "StembureauNaam", "Postcode"))

write.csv(voting_data,
          "voting_data_with_id.csv")

# The latter isn't necessary for location_unique because here we are only interested in matching each stembureau
# with a possible location. Some locations have multiple stembureaus in them because they might have had multiple
# booths in the same location. After manual exploration, it seems that the only variable that is consistent amongst
# the databases is the GemeenteCode. As such, we set the pair blocking based on that variable.

location_unique <- location_stembureau |>
  distinct(GemeenteCode, GemeenteNaam, Postcode, StembureauNaam, .keep_all = TRUE)

voting_unique <- voting_unique |>
  mutate(StembureauNaam = str_squish(StembureauNaam),
         StembureauNaam = str_remove(StembureauNaam, 
                                     "^Stembureau\\s+"))

location_unique <- location_unique |>
  mutate(StembureauNaam = str_squish(StembureauNaam),
         StembureauNaam = str_remove(StembureauNaam, 
                                     "^Stembureau\\s+"))

pairs <- pair_blocking(voting_unique, location_unique, "GemeenteCode")

# We use the columns mentioned here to compare the databases. We use cmp_jarowinkler instead of identical, otherwise we'd
# get artificially high counts of non-matching pairs.

compare_pairs(pairs,
              on = c("StembureauNaam", "Postcode"),
              default_comparator = cmp_jarowinkler(),
              inplace = TRUE)


# By definition, the weights are defined by the user, so the results of this step can significantly vary according to the selected weights. 
# More weight is added for the stembureaunaam as it is more important that the names match than that of the postcode. 
pairs <- score_simple(pairs,
                      variable = "score",
                      on = c("StembureauNaam", "Postcode"),
                      w1 = c(StembureauNaam = 4, Postcode = 1),
                      w0 = c(StembureauNaam = -2, Postcode = -0.25),
                      wna = 0)

# We constraint the selection of possible pairs to at most one per database. 

pairs <- select_n_to_m(pairs,
                       "score", 
                       variable = "selected", 
                       threshold = 0)


linked <- reclin2::link(pairs,selection = "selected",
                        x = voting_unique,
                        y = location_unique,
                        suffixes = c(".x", ".y"),
                        keep_from_pairs = c(".x", ".y", "score", "selected"))

linked <- linked |>
  select(GemeenteNaam.x,
         GemeenteNaam.y,
         StembureauNaam.x,
         StembureauNaam.y,
         Postcode.x,
         Postcode.y,
         everything())


# We isolate the ones that didn't receive a match 

matched_x <- unique(pairs[selected == TRUE]$.x)

voting_matched <- voting_unique[matched_x,]

voting_not_matched <- voting_unique[-matched_x, ]

# To confirm that things went well and no row is missing of status we compare

nrow(voting_unique) == nrow(voting_matched) + nrow(voting_not_matched)

write.csv(voting_matched,
          "voting_matched.csv")

write.csv(voting_not_matched,
          "voting_not_matched.csv")
# By definition record linkage doesn't always produce correct linkages. In other words, there can be false positives
# Since the most important part for two records to match is the name of the stembureau, 
# we'll first try to fix all the linkages by name, and then by Postcode. After this, the BAG column will be used
# in mix with the BAG package to see if the addresses are correctly assigned.


# After many manual trials, the best way to clean the linked database is to first clean the linked StembureauNaam.x and StembureauNaam.y columns
# in the following way, and afterwards manually correct for 

linked <- linked |>
  mutate(StembureauNaam.x = str_remove(StembureauNaam.x,
                                       "^stb\\.[0-9]{3}:\\s*"))
# Number of rows = 9534
# If we add up the 135 rows from voting_not_matched we end up with the original 9669 rows of the voting_unique data set

linked_correct_initial <- linked |>
  filter(!is.na(StembureauNaam.x),
         !is.na(StembureauNaam.y),
         !is.na(Postcode.x),
         !is.na(Postcode.y),
         StembureauNaam.x == StembureauNaam.y,
         Postcode.x == Postcode.y)

# 7204

linked_not_equal <- linked |>
  filter(is.na(StembureauNaam.x) |
           is.na(StembureauNaam.y) |
           is.na(Postcode.x) |
           is.na(Postcode.y) |
           StembureauNaam.x != StembureauNaam.y |
           Postcode.x != Postcode.y)
# 2330

# 7204 + 2330 + 135 = 9669

write.csv(linked_not_equal, 
          file = "linked_not_equal.csv")

write.csv(linked_correct_initial,
          file = "linked_correct_initial.csv")

write.csv(voting_unique,
          "voting_unique.csv")

write.csv(location_unique,
          "location_unique.csv")







