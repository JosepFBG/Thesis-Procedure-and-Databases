###################################################################################################
#                                   Step 1: Loading the data
###################################################################################################

# We begin by loading the datasets that contain the unique locations, voting booths information, and the 
# correct and incorrect linkages

library(readr)
library(dplyr)
library(stringr)

linked <- read_csv("linked.csv",
                   col_select = -1)

linked_correct_initial <- read_csv("linked_correct_initial.csv",
                                   col_select = -1)

linked_not_equal <- read_csv("linked_not_equal.csv",
                             col_select = -1)

voting_unique <- read_csv("voting_unique.csv",
                          col_select = -1)

location_unique <- read_csv("location_unique.csv",
                            col_select = -1)

voting_not_matched <- read_csv("voting_not_matched.csv",
                               col_select = -1)

# The rows of linked_correct_initial + linked_not_equal + voting_not_matched still add up to 9669
###################################################################################################
                                # Step 2: Cleaning the data
###################################################################################################


# A noticeable pattern is that some StembureauNaams are extremely similar, but weren't linked due to:

        # a) Punctuation
        # b) Spaces between words
        # c) Extra words at the beginning or end in one of the cells that has the name
        # d) Some StembureauNaams from the original voting_data have an address where the name should go

# For the pairing to work we need for the StembureauNaam.y to be the same as that of StembureauNaam.x because StembureauNaam.x, which comes from voting_data
# only has the post code address. Meaning that if we want to match both datasets fully, it is necessary to make these names the same.

# If the names differ by a small difference, but both the postcodes and Gemeente codes match, the  information in that row belongs to the same StembureauNaam.
# Given this, changing the name of StembureauNaam.y can be done easily with a for-loop.

# There is a subset of stembureaus that have the exact same name, but either Postcode.x or Postcode are missing, or both. If the names and GemeenteCode.x and.y are the same
# There is no little reason to believe that the 

# However, even though about half of the mismatches are merely aesthetic and the rows were matched correctly. Some  are more severe and the correct 
# pair of StembureauNaams are in completely different rows. Meaning that if we just change the name of the StembureauNaam.y to coincide with that of StembureauNaam.x
# that row will have the incorrect location information. As such, some changes must be done manually as comparing and matching postcodes, plus searching in that municipality's
# website for election data and confirming the addresses is the only way to correct them.


###################################################################################################
                                    # Step 3: Fixing the Postcode Mismatches
###################################################################################################

linked_not_equal_id <- linked_not_equal |>
  mutate(row_id = row_number())


# We first make subsets of the Stembureaus whose names and GemeenteCodes are exactly the same. This is to minimize the risk of overcorreciton 

linked_name_equal_postcode_x_missing_y_present <- linked_not_equal_id |>
  filter(GemeenteCode.x == GemeenteCode.y,
         !is.na(StembureauNaam.x),
         !is.na(StembureauNaam.y),
         StembureauNaam.x == StembureauNaam.y,
         is.na(Postcode.x),
         !is.na(Postcode.y))

# Number of rows = 333

linked_name_equal_postcode_y_missing_x_present <- linked_not_equal_id |>
  filter(GemeenteCode.x == GemeenteCode.y,
         !is.na(StembureauNaam.x),
         !is.na(StembureauNaam.y),
         StembureauNaam.x == StembureauNaam.y,
         is.na(Postcode.y),
         !is.na(Postcode.x))

# Number of rows = 45

linked_remaining_after_postcode_missing_subsets <- linked_not_equal_id |>
  filter(!row_id %in% c(linked_name_equal_postcode_x_missing_y_present$row_id,
                        linked_name_equal_postcode_y_missing_x_present$row_id))

# Number of rows = 1952

# 333 + 1952 + 45 = 2330. The subsetting still matches the original data

linked_same_name_different_postcodes <- linked_remaining_after_postcode_missing_subsets |>
  filter(linked_remaining_after_postcode_missing_subsets$StembureauNaam.x ==  linked_remaining_after_postcode_missing_subsets$StembureauNaam.y)

# Number of rows 112 (needs to add up to 1952)

linked_remaining_after_same_name_different_postcodes <- linked_remaining_after_postcode_missing_subsets |>
  filter(!row_id %in% linked_same_name_different_postcodes$row_id)
# Number of rows 1840 (needs to add up to 1952)

# 1840 + 112 = 1952

# We now create a CSV of the remaining rows for easier access later on. It is worth noting that there are XY NAs, but they belong
# the the stembureaus that are in the Caribbean islands. Since the reserach focuses on the continental Netherlands only, they will be excluded
# After this subsetting, we are left with 1833 stembureaus that need to be fixed. We'll save the NAs for later exclusion

stembureaus_with_na_coordinates <- linked_remaining_after_same_name_different_postcodes |>
  filter(is.na(X))

# Number of rows = 7

linked_remaining_after_same_name_different_postcodes <- linked_remaining_after_same_name_different_postcodes |>
  filter(!is.na(X))

# Number of rows = 1833

write.csv(linked_remaining_after_same_name_different_postcodes,
          "linked_remaining_after_same_name_different_postcodes.csv")

write.csv(stembureaus_with_na_coordinates,
          "stembureaus_with_na_coordinates.csv")



# After a manual inspection via https://www.pdok.nl/ and comparing the area of the postcodes visualized via Google Maps in each row,
# it was found that the XY coordinates present in the database lead to either locations that have the same name as the StembureauNaam or 
# or to a location that matches the addresses given by additional location sources, such as https://allecijfers.nl/verkiezingsuitslagen/
# In some instances, and only when available, the information was corroborated via the websites of the individual municipalities
# 


linked_name_equal_postcode_x_missing_y_present_fixed <- linked_name_equal_postcode_x_missing_y_present |>
  mutate(Postcode.x = Postcode.y)

linked_name_equal_postcode_y_missing_x_present_fixed <- linked_name_equal_postcode_y_missing_x_present |>
  mutate(Postcode.y = Postcode.x)


# For the data set linked_remaining_same_name_different postcodes, we can simply exclude the NAs since these belong to the Caribbean NL.

linked_same_name_different_postcodes_fixed <- linked_same_name_different_postcodes |>
  filter(!is.na(X))

# Number of rows = 109

linked_same_name_different_postcodes_na_coordinates <- linked_same_name_different_postcodes |>
  filter(is.na(X))

# Number of rows = 3

write.csv(linked_name_equal_postcode_y_missing_x_present_fixed,
          "linked_name_equal_postcode_y_missing_x_present_fixed.csv")

write.csv(linked_name_equal_postcode_x_missing_y_present_fixed,
          "linked_name_equal_postcode_x_missing_y_present_fixed.csv")

write.csv(linked_same_name_different_postcodes_fixed,
          "linked_same_name_different_postcodes_fixed.csv")

write.csv(linked_same_name_different_postcodes_na_coordinates,
          "linked_same_name_different_postcodes_na_coordinates.csv")


# We now proceed to take care of the 1833 linked_remaining_after_same_name_different_postcodes


#####################################################################################################
#                                       First Loop
#####################################################################################################

# A pattern that appears in the mismatches is that the names of Stembureau.x and Stembureau.y are identical, but not equal.
# Thus, even when they share the exact same Postcode.x and Postcode.y, and GemeenteCode.x and GemeenteCode.y
# they are not a match. We can automate some if the fixing by looking at the ones that share thse characteristics,
# and forcing the pairing with the correct columns located in location_unique

linked_not_equal_list <- split(linked_remaining_after_same_name_different_postcodes, linked_remaining_after_same_name_different_postcodes$GemeenteCode.x)

unresolved <- data.frame()

gemeente_codes <- names(linked_not_equal_list)

for (code in gemeente_codes) {
  df <- linked_not_equal_list[[code]]
  
  resolvable <- df |>
    filter(!is.na(Postcode.x),
           !is.na(Postcode.y),
           Postcode.x == Postcode.y,
           GemeenteCode.x == GemeenteCode.y)
  
  unresolvable <- df |>
    filter(is.na(Postcode.x) |
             is.na(Postcode.y) |
             Postcode.x != Postcode.y)
  
  unresolved <- bind_rows(unresolved, unresolvable)
  
  linked_not_equal_list[[code]] <- resolvable |>
    mutate(lookup_StembureauNaam = StembureauNaam.y,
           lookup_Postcode = Postcode.y) |>
    select(vote_booth_id,
           GemeenteNaam.x,
           GemeenteNaam.y,
           GemeenteCode.x,
           GemeenteCode.y,
           StembureauNaam.x,
           Postcode.x,
           lookup_StembureauNaam,
           lookup_Postcode,
           .y,
           .x,
           score,
           selected) |>
    left_join(location_unique,
              by = c("GemeenteCode.x" = "GemeenteCode",
                     "lookup_StembureauNaam" = "StembureauNaam",
                     "lookup_Postcode" = "Postcode")) |>
    mutate(StembureauNaam.y = StembureauNaam.x,
           Postcode.y = Postcode.x) |>
    select(vote_booth_id,
           GemeenteNaam.x,
           GemeenteNaam.y,
           GemeenteCode.x,
           GemeenteCode.y,
           StembureauNaam.x,
           StembureauNaam.y,
           lookup_StembureauNaam,
           Postcode.x,
           Postcode.y,
           lookup_Postcode,
           .y,
           .x,
           score,
           everything(),
           -GemeenteNaam)
}

linked_not_equal_list <- linked_not_equal_list[sapply(linked_not_equal_list, nrow) > 0]

solved_df <- bind_rows(linked_not_equal_list)

rm(list = intersect(c("df",
                      "resolvable",
                      "unresolvable",
                      "code",
                      "gemeente_codes"),
                    ls()))


write.csv(solved_df,
          "solved_df.csv")

write.csv(unresolved,
          "unresolved.csv")
# Rows from solved_df = 987 + rows from unresolved = 846 == 1833

#####################################################################################################
#                                       Second Loop
#####################################################################################################

# Rows to fix = 846 from unresolved

# A second pattern that is quite noticeable is that some StembureauNaam.x's are the address. 
# A helper column is created in which the StraatNaam, Huisnummer, and Huisnummertoevoeging. The loop then
# checks the unresolved rows municipality by municipality, joining each row to possible locations in location_unique
# with the same GemeenteCode. It keeps only the rows where StembureauNaam.x matches the constructed address and
# Postcode.x matches the postcode from location_unique. The matched StembureauNaam and Postcode from location_unique
# are saved as lookup_StembureauNaam and lookup_Postcode, which are then used to join
# back to the full location_unique dataset and retrieve the correct location information. The rows that are successfully
# matched this way are stored in solved_address_df, while the rows that still cannot be matched are stored in
# still_unresolved_address.

unresolved_list <- split(unresolved, unresolved$GemeenteCode.x)

location_unique_address <- location_unique |>
  mutate(unified_address = paste0(Straatnaam,
                                  " ",
                                  Huisnummer,
                                  coalesce(Huisletter, ""),
                                  coalesce(Huisnummertoevoeging, "")) |>
           str_squish() |>
           str_to_lower(),
         candidate_Postcode = str_to_upper(Postcode)) |>
  select(GemeenteCode,
         StembureauNaam,
         candidate_Postcode,
         unified_address)

resolved_address <- list()
still_unresolved_address <- data.frame()

for (code in names(unresolved_list)) {
  df <- unresolved_list[[code]] |>
    mutate(StembureauNaam.x_lower = str_to_lower(str_squish(StembureauNaam.x)),
           Postcode.x_upper = str_to_upper(Postcode.x),
           StembureauNaam.y_original = StembureauNaam.y,
           Postcode.y_original = Postcode.y)
  
  matched_rows <- df |>
    left_join(location_unique_address,
              by = c("GemeenteCode.x" = "GemeenteCode"),
              relationship = "many-to-many") |>
    filter(!is.na(StembureauNaam.x_lower),
           !is.na(unified_address),
           StembureauNaam.x_lower == unified_address,
           !is.na(Postcode.x_upper),
           !is.na(candidate_Postcode),
           Postcode.x_upper == candidate_Postcode) |>
    mutate(lookup_StembureauNaam = StembureauNaam,
           lookup_Postcode = candidate_Postcode) |>
    select(vote_booth_id,
           GemeenteNaam.x,
           GemeenteNaam.y,
           GemeenteCode.x,
           GemeenteCode.y,
           StembureauNaam.x,
           Postcode.x,
           Postcode.y,
           StembureauNaam.y_original,
           Postcode.y_original,
           lookup_StembureauNaam,
           lookup_Postcode,
           .y,
           .x,
           score,
           selected) |>
    distinct(vote_booth_id, .keep_all = TRUE)
  
  unresolved_rows <- df |>
    anti_join(matched_rows |>
                select(vote_booth_id),
              by = "vote_booth_id")
  
  resolved_address[[code]] <- matched_rows |>
    left_join(location_unique,
              by = c("GemeenteCode.x" = "GemeenteCode",
                     "lookup_StembureauNaam" = "StembureauNaam",
                     "lookup_Postcode" = "Postcode")) |>
    mutate(StembureauNaam.y = StembureauNaam.x,
           Postcode.y = Postcode.x) |>
    select(vote_booth_id,
           GemeenteNaam.x,
           GemeenteNaam.y,
           GemeenteCode.x,
           GemeenteCode.y,
           StembureauNaam.x,
           StembureauNaam.y,
           StembureauNaam.y_original,
           lookup_StembureauNaam,
           Postcode.x,
           Postcode.y,
           Postcode.y_original,
           lookup_Postcode,
           .y,
           .x,
           score,
           everything(),
           -GemeenteNaam)
  
  still_unresolved_address <- bind_rows(still_unresolved_address, unresolved_rows)
}

resolved_address <- resolved_address[sapply(resolved_address, nrow) > 0]


solved_address_df <- bind_rows(resolved_address)

rm(list = intersect(c(
                      "location_unique_address",
                      "resolved_address",
                      "df",
                      "matched_rows",
                      "unresolved_rows",
                      "code"),
                    ls()))

write.csv(still_unresolved_address,
          "still_unresolved_address.csv")

write.csv(solved_address_df,
          "solved_address_df.csv")


#####################################################################################################
#                                       Third Loop
#####################################################################################################

# A final pattern in the remaining unresolved rows is that some cases may still be recoverable using only
# Postcode.x and GemeenteCode.x. This loop checks the remaining unresolved rows municipality by municipality.
# For each row, it searches location_unique for records with the same GemeenteCode and the same Postcode.x.
# If exactly one matching location is found, the row is treated as solved: the matching StembureauNaam and
# Postcode from location_unique are saved as lookup_StembureauNaam and lookup_Postcode, and these lookup
# columns are then used to join back to the full location_unique dataset and retrieve the correct location
# information. If no matching location is found, or if multiple locations share the same postcode within the
# same municipality, the row is not solved automatically and is stored in all_problem_rows for manual checking.
# The automatically solved rows are stored in solved_rows.

still_unresolved_list <- split(still_unresolved_address, still_unresolved_address$GemeenteCode.x)

all_problem_rows <- data.frame()

for (code in names(still_unresolved_list)) {
  
  df <- still_unresolved_list[[code]]
  
  df$Postcode.x_original <- df$Postcode.x
  df$Postcode.y_original <- df$Postcode.y
  df$StembureauNaam.y_original <- df$StembureauNaam.y
  
  df$lookup_StembureauNaam <- df$StembureauNaam.y
  df$lookup_Postcode <- df$Postcode.y
  df$match_status <- NA_character_
  
  for (i in 1:nrow(df)) {
    
    current_postcode <- df$Postcode.x[i]
    current_gemeente <- df$GemeenteCode.x[i]
    
    hits <- which(location_unique$Postcode == current_postcode &
                    location_unique$GemeenteCode == current_gemeente)
    
    if (length(hits) == 1) {
      df$lookup_StembureauNaam[i] <- location_unique$StembureauNaam[hits]
      df$lookup_Postcode[i] <- location_unique$Postcode[hits]
      df$match_status[i] <- "unique match found"
      
    } else if (length(hits) == 0) {
      df$lookup_StembureauNaam[i] <- NA
      df$lookup_Postcode[i] <- current_postcode
      df$match_status[i] <- "no match found"
      
    } else {
      df$lookup_StembureauNaam[i] <- NA
      df$lookup_Postcode[i] <- current_postcode
      df$match_status[i] <- "multiple matches found"
    }
  }
  
  df_result <- df |>
    select(vote_booth_id,
           GemeenteNaam.x,
           GemeenteNaam.y,
           GemeenteCode.x,
           GemeenteCode.y,
           StembureauNaam.x,
           Postcode.x,
           Postcode.x_original,
           Postcode.y_original,
           StembureauNaam.y_original,
           lookup_StembureauNaam,
           lookup_Postcode,
           match_status,
           .y,
           .x,
           score,
           selected) |>
    left_join(location_unique,
              by = c("GemeenteCode.x" = "GemeenteCode",
                     "lookup_StembureauNaam" = "StembureauNaam",
                     "lookup_Postcode" = "Postcode")) |>
    mutate(StembureauNaam.y = StembureauNaam.x,
           Postcode.y = Postcode.x) |>
    select(vote_booth_id,
           GemeenteNaam.x,
           GemeenteNaam.y,
           GemeenteCode.x,
           GemeenteCode.y,
           StembureauNaam.x,
           StembureauNaam.y,
           StembureauNaam.y_original,
           lookup_StembureauNaam,
           Postcode.x,
           Postcode.x_original,
           Postcode.y,
           Postcode.y_original,
           lookup_Postcode,
           match_status,
           .y,
           .x,
           score,
           everything(),
           -GemeenteNaam)
  
  still_unresolved_list[[code]] <- df_result
  
  all_problem_rows <- bind_rows(all_problem_rows,
                                subset(df_result,
                                       match_status %in% c("no match found", "multiple matches found")))
}

solved_rows <- data.frame()

for (code in names(still_unresolved_list)) {
  solved_rows <- bind_rows(solved_rows,
                           subset(still_unresolved_list[[code]],
                                  match_status == "unique match found"))
}

rm(list = intersect(c("code",
                      "current_gemeente",
                      "current_postcode",
                      "hits",
                      "i",
                      "df",
                      "df_result"),
                    ls()))

write.csv(solved_rows,
          "solved_rows_loop_3.csv")

write.csv(all_problem_rows,
          "all_problem_rows_loop_3.csv")

#####################################################################################################
#                                       Manual Corrections
#####################################################################################################

# After the automated matching steps, the remaining rows are cases where the linkage could not be solved.
# These blocks are then manually corrected by searching in various sources for their location, and then searching that location in
# location_unique, only to then join them with their correct match.
# For each municipality, StembureauNaam.x values are matched to the corresponding StembureauNaam and Postcode in location_unique using case_when.
# The corrected values are stored in lookup_StembureauNaam and lookup_Postcode, which are then used to join back to location_unique and get
# the full location information. Postcode.x is also updated when the original voting-data postcode is missing or incorrect.
# The original values are preserved in Postcode.x_original, Postcode.y_original, and StembureauNaam.y_original so the
# correction can be checked afterward. Finally, StembureauNaam.y and Postcode.y are overwritten with the corrected
# StembureauNaam.x and Postcode.x values so the final linked row has harmonized names and postcodes. These manually
# corrected rows are then combined into manual_solved_df and later added to the fully solved linkage dataset.


all_problem_rows_list <- split(all_problem_rows, all_problem_rows$GemeenteCode.x)

# Gemeente 85

all_problem_rows_list[["85"]] <- all_problem_rows_list[["85"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Oosterwolde, Gemeentehuis" ~ "Oosterwolde, Gemeentehuis 2",
                                           StembureauNaam.x == "Appelscha, De Schutse" ~ "Appelscha, De Schutse 2",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Oosterwolde, Gemeentehuis" ~ "8431LE",
                                     StembureauNaam.x == "Appelscha, De Schutse" ~ "8426AG",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Oosterwolde, Gemeentehuis" ~ "8431LE",
                                StembureauNaam.x == "Appelscha, De Schutse" ~ "8426AG",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)


# Gemeente 88

all_problem_rows_list[["88"]] <- all_problem_rows_list[["88"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Gemeentehuis" ~ "Gemeentehuis (Raadzaal)",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Gemeentehuis" ~ "9166LX",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Gemeentehuis" ~ "9166LX",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 109

all_problem_rows_list[["109"]] <- all_problem_rows_list[["109"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Dorpshuis Boerhoorn - Dalerveen" ~ "Dorpshuis Boerhoorn Dalerveen",
                                           StembureauNaam.x == "MFC De Brink - Sleen" ~ "MFC De Brink Sleen",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Dorpshuis Boerhoorn - Dalerveen" ~ "7755NR",
                                     StembureauNaam.x == "MFC De Brink - Sleen" ~ "7841CE",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Dorpshuis Boerhoorn - Dalerveen" ~ "7755NR",
                                StembureauNaam.x == "MFC De Brink - Sleen" ~ "7841CE",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 148

# The postocode 7721ZR is there for Rosengaerde because the entrance is through that street, however, the stembureau was placed in the building in 7721CT

# Source: https://dalfsen.archiefweb.eu/#archive

all_problem_rows_list[["148"]] <- all_problem_rows_list[["148"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Rosengaerde" ~ "Rosengaerde, ingang Brandkolkstraat",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Rosengaerde" ~ "7721CT",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Rosengaerde" ~ "7721CT",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 160

# it seems that there are no entries with a postcode 7701PZ in location_unique. According to the source, and .csv
# the locations are on the same street
# #https://www.hardenberg.nl/verkiezingen/uitslagen

all_problem_rows_list[["160"]] <- all_problem_rows_list[["160"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Unit 1 bij de Kiefer" ~ "Unit bij de Kiefer 1e unit",
                                           StembureauNaam.x == "Unit 2 bij de Kiefer" ~ "Unit bij de Kiefer 2e unit",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Unit 1 bij de Kiefer" ~ "7701PW",
                                     StembureauNaam.x == "Unit 2 bij de Kiefer" ~ "7701PW",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Unit 1 bij de Kiefer" ~ "7701PW",
                                StembureauNaam.x == "Unit 2 bij de Kiefer" ~ "7701PW",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 163

# Both postcodes are correct since this is a mobile stembureau and as evidenced by the pdf with the information.
# However, the postcode that was registered was the one where the stembureau began its route, which is Postcode.x 7447PK
# See file Notes_on_which_postcodes_to_change
# THE BAG CODE CORRESPONDS TO THE ORIGINAL POSTCODE.Y ENTRY, Which is the last stop of the mobile stembureau
# Source: https://www.hellendoorn.nl/verkiezingen/uitslag-verkiezingen-tweede-kamer-2025/

all_problem_rows_list[["163"]] <- all_problem_rows_list[["163"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel Stembureau" ~ "Mobiel stembureau",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel Stembureau" ~ "7443AJ",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel Stembureau" ~ "7443AJ",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 175

# The location_unique has the point where the stembureau ended, while voting_unique has the postcode where the stembureau began
# To be clear, Postcode.x is the beginning of the stembureau route and postcode.y DOES NOT appear in the
# documentation of the vote counts. But the name of the street mentioned in the StembureauNaam.x DOES appear in the referenced postocde ending in TT
# THE BAG CODE BELONGS TO THE ENDPOINT
# Source: https://www.ommen.nl/tweede-kamerverkiezingen-2025/uitslagen-kennisgevingen/

all_problem_rows_list[["175"]] <- all_problem_rows_list[["175"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel stembureau (Buurthuis Ommerkanaal en Vilsterij)" ~ "Mobiel stembureau Ommerkanaal",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel stembureau (Buurthuis Ommerkanaal en Vilsterij)" ~ "7731TT",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel stembureau (Buurthuis Ommerkanaal en Vilsterij)" ~ "7731TT",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 183

all_problem_rows_list[["183"]] <- all_problem_rows_list[["183"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Zaal Kampkuiper, Harbrinkhoek" ~ "Zaal Kampkuiper",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Zaal Kampkuiper, Harbrinkhoek" ~ "7664VV",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Zaal Kampkuiper, Harbrinkhoek" ~ "7664VV",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 243

# The documentation seems to have no mention of the postcodes. However, the postcode does lead to a station

# Source https://www.harderwijk.nl/verkiezingen/verkiezingsuitslagen/uitslagen-tweede-kamerverkiezingen-2025

all_problem_rows_list[["243"]] <- all_problem_rows_list[["243"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Station Portakabin op het stationsplein" ~ "Station",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Station Portakabin op het stationsplein" ~ "3844KR",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Station Portakabin op het stationsplein" ~ "3844KR",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 267

all_problem_rows_list[["267"]] <- all_problem_rows_list[["267"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Willem Farelschool" ~ "Willem Farel school",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Willem Farelschool" ~ "3871JM",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Willem Farelschool" ~ "3871JM",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 279

# The names clearly are the same, but the postcodes.x are missing

all_problem_rows_list[["279"]] <- all_problem_rows_list[["279"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Kulturhus De Breehoek 2" ~ "Kulturhus De Breehoek",
                                           StembureauNaam.x == "De Wittenberg school" ~ "Basisschool De Wittenberg",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Kulturhus De Breehoek 2" ~ "3925JP",
                                     StembureauNaam.x == "De Wittenberg school" ~ "3925BW",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Kulturhus De Breehoek 2" ~ "3925JP",
                                StembureauNaam.x == "De Wittenberg school" ~ "3925BW",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 302

# The names clearly are similar enough, but they must have not matched due to the lack of Postcodes

all_problem_rows_list[["302"]] <- all_problem_rows_list[["302"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Chr. Gereformeerde Dorpskerk" ~ "Chr. Gereformeerde Kerk",
                                           StembureauNaam.x == "De Veluwse Heuvel" ~ "WOC De Veluwse Heuvel",
                                           StembureauNaam.x == "Kerkgebouw de Ontmoeting" ~ "Prot. Kerk de Ontmoeting",
                                           StembureauNaam.x == 'WOC ""De Binnenhof""' ~ "WOC de Binnenhof",
                                           StembureauNaam.x == "Gymzaal Vierhouterweg" ~ "Gymzaal Vierhouterweg, Elspeet",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Chr. Gereformeerde Dorpskerk" ~ "8071WN",
                                     StembureauNaam.x == "De Veluwse Heuvel" ~ "8071GM",
                                     StembureauNaam.x == "Kerkgebouw de Ontmoeting" ~ "8072GZ",
                                     StembureauNaam.x == 'WOC ""De Binnenhof""' ~ "8072AW",
                                     StembureauNaam.x == "Gymzaal Vierhouterweg" ~ "8075BJ",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Chr. Gereformeerde Dorpskerk" ~ "8071WN",
                                StembureauNaam.x == "De Veluwse Heuvel" ~ "8071GM",
                                StembureauNaam.x == "Kerkgebouw de Ontmoeting" ~ "8072GZ",
                                StembureauNaam.x == 'WOC ""De Binnenhof""' ~ "8072AW",
                                StembureauNaam.x == "Gymzaal Vierhouterweg" ~ "8075BJ",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 307

# The names are clearly the same but given that there are small differences plus the fact that there is not Postcode.x diminished the linking score

all_problem_rows_list[["307"]] <- all_problem_rows_list[["307"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Bergkerk 1" ~ "Bergkerk #1",
                                           StembureauNaam.x == "Bergkerk 2" ~ "Bergkerk #2",
                                           StembureauNaam.x == "Fonteinkerk 1" ~ "Fonteinkerk #1",
                                           StembureauNaam.x == "Fonteinkerk 2" ~ "Fonteinkerk #2 (bijgebouw)",
                                           StembureauNaam.x == "Gymzaal Dopheide 1" ~ "Gymzaal Dopheide #1",
                                           StembureauNaam.x == "Gymzaal Dopheide 2" ~ "Gymzaal Dopheide #2",
                                           StembureauNaam.x == "Hart van Vathorst 1 (extra toegankelijk)" ~ "Hart van Vathorst #1 (extra toegankelijk)",
                                           StembureauNaam.x == "Hart van Vathorst 2 (extra toegankelijk)" ~ "Hart van Vathorst #2",
                                           StembureauNaam.x == "KAdE Café" ~ "KAdECafé",
                                           StembureauNaam.x == "Kerkelijk Centrum De Hoeksteen 1" ~ "Kerkelijk Centrum De Hoeksteen #1",
                                           StembureauNaam.x == "Kerkelijk Centrum De Hoeksteen 2" ~ "Kerkelijk Centrum De Hoeksteen #2",
                                           StembureauNaam.x == "Kerkgebouw De Bron 1 (extra toegankelijk)" ~ "Kerkgebouw De Bron #1 (extra toegankelijk)",
                                           StembureauNaam.x == "Kerkgebouw De Bron 2 (extra toegankelijk)" ~ "Kerkgebouw De Bron #2",
                                           StembureauNaam.x == "Leger des Heils 1 (extra toegankelijk)" ~ "Leger des Heils #1 (extra toegankelijk)",
                                           StembureauNaam.x == "Leger des Heils 2 (extra toegankelijk)" ~ "Leger des Heils #2",
                                           StembureauNaam.x == "Meander Medisch Centrum (10:00-16:00 uur)" ~ "Meander Medisch Centrum",
                                           StembureauNaam.x == "Orthodoxe Parochie" ~ "Orthodoxe Parochie Amersfoort",
                                           StembureauNaam.x == "Sint Joriskerk 1 (extra toegankelijk)" ~ "Sint Joriskerk #1 (extra toegankelijk)",
                                           StembureauNaam.x == "Sint Joriskerk 3 (extra toegankelijk)" ~ "Sint Joriskerk #3",
                                           StembureauNaam.x == "Sint Joriskerk 2 (extra toegankelijk)" ~ "Sint Joriskerk #2",
                                           StembureauNaam.x == "Sportcomplex Amerena 1" ~ "Sportcomplex Amerena #1",
                                           StembureauNaam.x == "Sportcomplex Amerena 2" ~ "Sportcomplex Amerena #2",
                                           StembureauNaam.x == "Sporthal De Bieshaar 1" ~ "Sporthal De Bieshaar #1",
                                           StembureauNaam.x == "Sporthal De Bieshaar 2" ~ "Sporthal De Bieshaar #2",
                                           StembureauNaam.x == "Sporthal De Brink 1" ~ "Sporthal De Brink #1",
                                           StembureauNaam.x == "Sporthal De Brink 3" ~ "Sporthal De Brink #3",
                                           StembureauNaam.x == "Sporthal De Brink 2" ~ "Sporthal De Brink #2",
                                           StembureauNaam.x == "Sporthal De Dissel 1" ~ "Sporthal De Dissel #1",
                                           StembureauNaam.x == "Sporthal De Dissel 2" ~ "Sporthal De Dissel #2",
                                           StembureauNaam.x == "Sporthal Juliana van Stolberg 1" ~ "Sporthal Juliana van Stolberg #1",
                                           StembureauNaam.x == "Sporthal Juliana van Stolberg 2" ~ "Sporthal Juliana van Stolberg #2",
                                           StembureauNaam.x == "Sporthal Nieuwland 1" ~ "Sporthal Nieuwland #1",
                                           StembureauNaam.x == "Sporthal Nieuwland 4" ~ "Sporthal Nieuwland #4",
                                           StembureauNaam.x == "Sporthal Nieuwland 2" ~ "Sporthal Nieuwland #2",
                                           StembureauNaam.x == "Sporthal NIeuwland 3" ~ "Sporthal Nieuwland #3",
                                           StembureauNaam.x == "Verpleeghuis Birkhoven (10:00-16:00 uur)" ~ "Verpleeghuis Birkhoven",
                                           StembureauNaam.x == "Verpleeghuis de Lichtenberg (mobiel)" ~ "Mobiel Stembureau (3 locaties)",
                                           StembureauNaam.x == "Woon-zorgcentrum De Koperhorst" ~ "Woon- en zorgcentrum De Koperhorst",
                                           StembureauNaam.x == "Woon-zorgcentrum Nijenstede" ~ "Woon- en zorgcentrum Nijenstede",
                                           StembureauNaam.x == "Woon-zorgcentrum Puntenburg" ~ "Woon- en zorgcentrum Puntenburg",
                                           StembureauNaam.x == "Zon en Schild (10:00-16:00 uur)" ~ "Zon en Schild",
                                           StembureauNaam.x == "bijgebouw Mevlana moskee" ~ "bijgebouw Mevlana Moskee",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Bergkerk 1" ~ "3818JC",
                                     StembureauNaam.x == "Bergkerk 2" ~ "3818JC",
                                     StembureauNaam.x == "Fonteinkerk 1" ~ "3817JM",
                                     StembureauNaam.x == "Fonteinkerk 2" ~ "3817JM",
                                     StembureauNaam.x == "Gymzaal Dopheide 1" ~ "3823HL",
                                     StembureauNaam.x == "Gymzaal Dopheide 2" ~ "3823HL",
                                     StembureauNaam.x == "Hart van Vathorst 1 (extra toegankelijk)" ~ "3825LX",
                                     StembureauNaam.x == "Hart van Vathorst 2 (extra toegankelijk)" ~ "3825LX",
                                     StembureauNaam.x == "KAdE Café" ~ "3812EA",
                                     StembureauNaam.x == "Kerkelijk Centrum De Hoeksteen 1" ~ "3813PZ",
                                     StembureauNaam.x == "Kerkelijk Centrum De Hoeksteen 2" ~ "3813PZ",
                                     StembureauNaam.x == "Kerkgebouw De Bron 1 (extra toegankelijk)" ~ "3815GV",
                                     StembureauNaam.x == "Kerkgebouw De Bron 2 (extra toegankelijk)" ~ "3815GV",
                                     StembureauNaam.x == "Leger des Heils 1 (extra toegankelijk)" ~ "3822BV",
                                     StembureauNaam.x == "Leger des Heils 2 (extra toegankelijk)" ~ "3822BV",
                                     StembureauNaam.x == "Meander Medisch Centrum (10:00-16:00 uur)" ~ "3813TZ",
                                     StembureauNaam.x == "Orthodoxe Parochie" ~ "3812ST",
                                     StembureauNaam.x == "Sint Joriskerk 1 (extra toegankelijk)" ~ "3811CJ",
                                     StembureauNaam.x == "Sint Joriskerk 3 (extra toegankelijk)" ~ "3811CJ",
                                     StembureauNaam.x == "Sint Joriskerk 2 (extra toegankelijk)" ~ "3811CJ",
                                     StembureauNaam.x == "Sportcomplex Amerena 1" ~ "3815XT",
                                     StembureauNaam.x == "Sportcomplex Amerena 2" ~ "3815XT",
                                     StembureauNaam.x == "Sporthal De Bieshaar 1" ~ "3828VL",
                                     StembureauNaam.x == "Sporthal De Bieshaar 2" ~ "3828VL",
                                     StembureauNaam.x == "Sporthal De Brink 1" ~ "3825DJ",
                                     StembureauNaam.x == "Sporthal De Brink 3" ~ "3825DJ",
                                     StembureauNaam.x == "Sporthal De Brink 2" ~ "3825DJ",
                                     StembureauNaam.x == "Sporthal De Dissel 1" ~ "3829MD",
                                     StembureauNaam.x == "Sporthal De Dissel 2" ~ "3829MD",
                                     StembureauNaam.x == "Sporthal Juliana van Stolberg 1" ~ "3818DN",
                                     StembureauNaam.x == "Sporthal Juliana van Stolberg 2" ~ "3818DN",
                                     StembureauNaam.x == "Sporthal Nieuwland 1" ~ "3824EJ",
                                     StembureauNaam.x == "Sporthal Nieuwland 4" ~ "3824EJ",
                                     StembureauNaam.x == "Sporthal Nieuwland 2" ~ "3824EJ",
                                     StembureauNaam.x == "Sporthal NIeuwland 3" ~ "3824EJ",
                                     StembureauNaam.x == "Verpleeghuis Birkhoven (10:00-16:00 uur)" ~ "3819BA",
                                     StembureauNaam.x == "Verpleeghuis de Lichtenberg (mobiel)" ~ "3818EH",
                                     StembureauNaam.x == "Woon-zorgcentrum De Koperhorst" ~ "3813KA",
                                     StembureauNaam.x == "Woon-zorgcentrum Nijenstede" ~ "3816BL",
                                     StembureauNaam.x == "Woon-zorgcentrum Puntenburg" ~ "3812CG",
                                     StembureauNaam.x == "Zon en Schild (10:00-16:00 uur)" ~ "3818EW",
                                     StembureauNaam.x == "bijgebouw Mevlana moskee" ~ "3814RB",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Bergkerk 1" ~ "3818JC",
                                StembureauNaam.x == "Bergkerk 2" ~ "3818JC",
                                StembureauNaam.x == "Fonteinkerk 1" ~ "3817JM",
                                StembureauNaam.x == "Fonteinkerk 2" ~ "3817JM",
                                StembureauNaam.x == "Gymzaal Dopheide 1" ~ "3823HL",
                                StembureauNaam.x == "Gymzaal Dopheide 2" ~ "3823HL",
                                StembureauNaam.x == "Hart van Vathorst 1 (extra toegankelijk)" ~ "3825LX",
                                StembureauNaam.x == "Hart van Vathorst 2 (extra toegankelijk)" ~ "3825LX",
                                StembureauNaam.x == "KAdE Café" ~ "3812EA",
                                StembureauNaam.x == "Kerkelijk Centrum De Hoeksteen 1" ~ "3813PZ",
                                StembureauNaam.x == "Kerkelijk Centrum De Hoeksteen 2" ~ "3813PZ",
                                StembureauNaam.x == "Kerkgebouw De Bron 1 (extra toegankelijk)" ~ "3815GV",
                                StembureauNaam.x == "Kerkgebouw De Bron 2 (extra toegankelijk)" ~ "3815GV",
                                StembureauNaam.x == "Leger des Heils 1 (extra toegankelijk)" ~ "3822BV",
                                StembureauNaam.x == "Leger des Heils 2 (extra toegankelijk)" ~ "3822BV",
                                StembureauNaam.x == "Meander Medisch Centrum (10:00-16:00 uur)" ~ "3813TZ",
                                StembureauNaam.x == "Orthodoxe Parochie" ~ "3812ST",
                                StembureauNaam.x == "Sint Joriskerk 1 (extra toegankelijk)" ~ "3811CJ",
                                StembureauNaam.x == "Sint Joriskerk 3 (extra toegankelijk)" ~ "3811CJ",
                                StembureauNaam.x == "Sint Joriskerk 2 (extra toegankelijk)" ~ "3811CJ",
                                StembureauNaam.x == "Sportcomplex Amerena 1" ~ "3815XT",
                                StembureauNaam.x == "Sportcomplex Amerena 2" ~ "3815XT",
                                StembureauNaam.x == "Sporthal De Bieshaar 1" ~ "3828VL",
                                StembureauNaam.x == "Sporthal De Bieshaar 2" ~ "3828VL",
                                StembureauNaam.x == "Sporthal De Brink 1" ~ "3825DJ",
                                StembureauNaam.x == "Sporthal De Brink 3" ~ "3825DJ",
                                StembureauNaam.x == "Sporthal De Brink 2" ~ "3825DJ",
                                StembureauNaam.x == "Sporthal De Dissel 1" ~ "3829MD",
                                StembureauNaam.x == "Sporthal De Dissel 2" ~ "3829MD",
                                StembureauNaam.x == "Sporthal Juliana van Stolberg 1" ~ "3818DN",
                                StembureauNaam.x == "Sporthal Juliana van Stolberg 2" ~ "3818DN",
                                StembureauNaam.x == "Sporthal Nieuwland 1" ~ "3824EJ",
                                StembureauNaam.x == "Sporthal Nieuwland 4" ~ "3824EJ",
                                StembureauNaam.x == "Sporthal Nieuwland 2" ~ "3824EJ",
                                StembureauNaam.x == "Sporthal NIeuwland 3" ~ "3824EJ",
                                StembureauNaam.x == "Verpleeghuis Birkhoven (10:00-16:00 uur)" ~ "3819BA",
                                StembureauNaam.x == "Verpleeghuis de Lichtenberg (mobiel)" ~ "3818EH",
                                StembureauNaam.x == "Woon-zorgcentrum De Koperhorst" ~ "3813KA",
                                StembureauNaam.x == "Woon-zorgcentrum Nijenstede" ~ "3816BL",
                                StembureauNaam.x == "Woon-zorgcentrum Puntenburg" ~ "3812CG",
                                StembureauNaam.x == "Zon en Schild (10:00-16:00 uur)" ~ "3818EW",
                                StembureauNaam.x == "bijgebouw Mevlana moskee" ~ "3814RB",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 308

# The location clearly is the same, but the postcode in voting_unique seems to be incorrect.
# Both postcodes cover an area of around 1 block and border one another.

all_problem_rows_list[["308"]] <- all_problem_rows_list[["308"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Gemeentehuis Baarn (Laanstraat)" ~ "Gemeentehuis (Laanstraat)",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Gemeentehuis Baarn (Laanstraat)" ~ "3743BA",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Gemeentehuis Baarn (Laanstraat)" ~ "3743BA",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 335

# The BAG code leads to an address without Postcode

all_problem_rows_list[["335"]] <- all_problem_rows_list[["335"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Kerkgebouw de Rank" ~ "Kerkgebouw De Rank",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Kerkgebouw de Rank" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Kerkgebouw de Rank" ~ NA,
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 339

# Gebouw Rehoboth is located within the Postcode.y postcode

# Source: https://maps.app.goo.gl/FkKyqThV48UkxFTt5

all_problem_rows_list[["339"]] <- all_problem_rows_list[["339"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "gebouw Rehoboth" ~ "Rehoboth",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "gebouw Rehoboth" ~ "3927BL",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "gebouw Rehoboth" ~ "3927BL",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 342

# Needs later correctionvia Geopackage. Check additional R documentation for this

all_problem_rows_list[["342"]] <- all_problem_rows_list[["342"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == 'Postduiven Vereniging ""De Zwaluw""' ~ "NO INFORMATION",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == 'Postduiven Vereniging ""De Zwaluw""' ~ "NO INFORMATION",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == 'Postduiven Vereniging ""De Zwaluw""' ~ "NO INFORMATION",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)


# Gemeente 344

# The addresses are put as names in StembureauNaam.x. As such, searching for the street in the other entries or in
# location_unique to make the match must be fixed.

all_problem_rows_list[["344"]] <- all_problem_rows_list[["344"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Jagerskade 4" ~ "Buurthuis Stichting Stella Maris",
                                           StembureauNaam.x == "Oesterzwam null nabij station Vleuten" ~ "Tijdelijke unit P+R Vleuten",
                                           StembureauNaam.x == "Stationshal 1e verdieping null" ~ "Centraal Station",
                                           StembureauNaam.x == "Stationshal 9 Zaal 7 Spoorpaviljoen" ~ "Bar Beton",
                                           StembureauNaam.x == "Stationshal 9 Zaal 8 t/m 10 Croesepaviljoen" ~ "Bar Beton",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Jagerskade 4" ~ "3552TL",
                                     StembureauNaam.x == "Oesterzwam null nabij station Vleuten" ~ NA,
                                     StembureauNaam.x == "Stationshal 1e verdieping null" ~ "3511CE",
                                     StembureauNaam.x == "Stationshal 9 Zaal 7 Spoorpaviljoen" ~ "3511CE",
                                     StembureauNaam.x == "Stationshal 9 Zaal 8 t/m 10 Croesepaviljoen" ~ "3511CE",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Jagerskade 4" ~ NA,
                                StembureauNaam.x == "Oesterzwam null nabij station Vleuten" ~ NA,
                                StembureauNaam.x == "Stationshal 1e verdieping null" ~ "3511CE",
                                StembureauNaam.x == "Stationshal 9 Zaal 7 Spoorpaviljoen" ~ "3511CE",
                                StembureauNaam.x == "Stationshal 9 Zaal 8 t/m 10 Croesepaviljoen" ~ "3511CE",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 345

all_problem_rows_list[["345"]] <- all_problem_rows_list[["345"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Drive-Through (Hardeman)" ~ "Drive-Through Hardeman",
                                           StembureauNaam.x == "Het Apostolisch Genootschap" ~ "Gebouw Apostolisch Genootschap",
                                           StembureauNaam.x == "Gebouw de Scouting" ~ "Gebouw De Scouting",
                                           StembureauNaam.x == "I-Centrum (Informatiecentrum)" ~ "I-centrum (Informatiecentrum)",
                                           StembureauNaam.x == "Sporthal Oost (1) Ontmoetingshuis" ~ "Sporthal Oost (Ontmoetingshuis) (1)",
                                           StembureauNaam.x == "Sporthal Oost (2) Ontmoetingshuis" ~ "Sporthal Oost (Ontmoetingshuis) (2)",
                                           StembureauNaam.x == "Sporthal Oost (3) Ontmoetingshuis" ~ "Sporthal Oost (Ontmoetingshuis) (3)",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Drive-Through (Hardeman)" ~ "3903LH",
                                     StembureauNaam.x == "Het Apostolisch Genootschap" ~ "3904JZ",
                                     StembureauNaam.x == "Gebouw de Scouting" ~ "3904MD",
                                     StembureauNaam.x == "I-Centrum (Informatiecentrum)" ~ "3907JA",
                                     StembureauNaam.x == "Sporthal Oost (1) Ontmoetingshuis" ~ "3907NJ",
                                     StembureauNaam.x == "Sporthal Oost (2) Ontmoetingshuis" ~ "3907NJ",
                                     StembureauNaam.x == "Sporthal Oost (3) Ontmoetingshuis" ~ "3907NJ",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Drive-Through (Hardeman)" ~ "3903LH",
                                StembureauNaam.x == "Het Apostolisch Genootschap" ~ "3904JZ",
                                StembureauNaam.x == "Gebouw de Scouting" ~ "3904MD",
                                StembureauNaam.x == "I-Centrum (Informatiecentrum)" ~ "3907JA",
                                StembureauNaam.x == "Sporthal Oost (1) Ontmoetingshuis" ~ "3907NJ",
                                StembureauNaam.x == "Sporthal Oost (2) Ontmoetingshuis" ~ "3907NJ",
                                StembureauNaam.x == "Sporthal Oost (3) Ontmoetingshuis" ~ "3907NJ",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 355

# Flex@Work, Laan van Vollenhove 3233 Postcode.x belongs to a small street that has an entrance to the establishment. Postcode.y has the postcode where most of the business is

all_problem_rows_list[["355"]] <- all_problem_rows_list[["355"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Flex@Work, Laan van Vollenhove 3233" ~ "Flex@Work, laan van Vollenhove 3233",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Flex@Work, Laan van Vollenhove 3233" ~ "3706AR",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Flex@Work, Laan van Vollenhove 3233" ~ "3706AR",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 363

# "Stationsplein null Ingang Metrohal Stationsplein" NO INFORMATION ON ANY WEBSITE
# Some postcodes seem to be incorrect in voting_unique. The BAG code leads to the addresses in the StembureauNaam.x

#https://stembureaus.amsterdam.nl/map

#Leiduinstraat Tolhuisweg Stationplein 

all_problem_rows_list[["363"]] <- all_problem_rows_list[["363"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y,
         StembureauNaam.y_original = StembureauNaam.y,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Stationsplein null Ingang Metrohal Stationsplein" ~ "NO INFORMATION",
                                           StembureauNaam.x == "Westermarkt 20" ~ "Anne Frank Huis",
                                           StembureauNaam.x == "Leidsekade 90" ~ "Theater Bellevue",
                                           StembureauNaam.x == "Waterlandplein 302 Waterlandplein 302" ~ "Huis van de Wijk Waterlandplein",
                                           StembureauNaam.x == "Nydia Ecurystraat 31 null" ~ "Wooncoöperatie De Warren",
                                           StembureauNaam.x == "Rodenrijsstraat 43" ~ "Lola Lieven",
                                           StembureauNaam.x == "Leiduinstraat 11 Leiduinstraat 13" ~ "OBS De Notenkraker (dependance)",
                                           StembureauNaam.x == "Tolhuisweg 3 null Huisje 3" ~ "Tolhuistuin",
                                           StembureauNaam.x == "Stationsplein null" ~ "Centraal Station - GVB Metroplein",
                                           StembureauNaam.x == "Hendrikje Stoffelsstraat 1 null" ~ "Westcord Fashion Hotel Amsterdam",
                                           StembureauNaam.x == "Dam null" ~ "Tent Dam",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Stationsplein null Ingang Metrohal Stationsplein" ~ "NO INFORMATION",
                                     StembureauNaam.x == "Westermarkt 20" ~ "1016DK",
                                     StembureauNaam.x == "Leidsekade 90" ~ "1017PN",
                                     StembureauNaam.x == "Waterlandplein 302 Waterlandplein 302" ~ "1024NB",
                                     StembureauNaam.x == "Nydia Ecurystraat 31 null" ~ "1087VV",
                                     StembureauNaam.x == "Rodenrijsstraat 43" ~ "1062JA",
                                     StembureauNaam.x == "Leiduinstraat 11 Leiduinstraat 13" ~ NA,
                                     StembureauNaam.x == "Tolhuisweg 3 null Huisje 3" ~ NA,
                                     StembureauNaam.x == "Stationsplein null" ~ NA,
                                     StembureauNaam.x == "Hendrikje Stoffelsstraat 1 null" ~ "1058GC",
                                     StembureauNaam.x == "Dam null" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Stationsplein null Ingang Metrohal Stationsplein" ~ "NO INFORMATION",
                                StembureauNaam.x == "Westermarkt 20" ~ "1016DK",
                                StembureauNaam.x == "Leidsekade 90" ~ "1017PN",
                                StembureauNaam.x == "Waterlandplein 302 Waterlandplein 302" ~ "1024NB",
                                StembureauNaam.x == "Nydia Ecurystraat 31 null" ~ "1087VV",
                                StembureauNaam.x == "Rodenrijsstraat 43" ~ "1062JA",
                                StembureauNaam.x == "Leiduinstraat 11 Leiduinstraat 13" ~ NA,
                                StembureauNaam.x == "Tolhuisweg 3 null Huisje 3" ~ NA,
                                StembureauNaam.x == "Stationsplein null" ~ NA,
                                StembureauNaam.x == "Hendrikje Stoffelsstraat 1 null" ~ "1058GC",
                                StembureauNaam.x == "Dam null" ~ NA,
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 375

# Al documentaion points that Postcode.y is the correct one. Upon inspecting the area of the Postocodes
# ithas been noticed that both Poscoode.x and Postcode.y cover different sides of the same street.

all_problem_rows_list[["375"]] <- all_problem_rows_list[["375"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Speeltuin Jeugd & Vreugd" ~ "Speeltuin Jeugd en Vreugd",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Speeltuin Jeugd & Vreugd" ~ "1946TM",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Speeltuin Jeugd & Vreugd" ~ "1946TM",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 383

# Check additional R file for cleanup

all_problem_rows_list[["383"]] <- all_problem_rows_list[["383"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Pastorie RK kerk" ~ "NO INFORMATION",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Pastorie RK kerk" ~ "NO INFORMATION",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Pastorie RK kerk" ~ "NO INFORMATION",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)


# Gemeente 394

# Check R documentation for additional information

all_problem_rows_list[["394"]] <- all_problem_rows_list[["394"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Bennebroekerweg 600" ~ "NO INFORMATION",
                                           StembureauNaam.x == "Marktplein 96" ~ "Marktpleinkerk",
                                           StembureauNaam.x == "Sandestein 40 2" ~ "Sandestein Sportzaal",
                                           StembureauNaam.x == "Sandestein 40 1" ~ "Sandestein Sportzaal",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Bennebroekerweg 600" ~ "NO INFORMATION",
                                     StembureauNaam.x == "Marktplein 96" ~ "2132DC",
                                     StembureauNaam.x == "Sandestein 40 2" ~ "2151KG",
                                     StembureauNaam.x == "Sandestein 40 1" ~ "2151KG",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Bennebroekerweg 600" ~ "NO INFORMATION",
                                StembureauNaam.x == "Marktplein 96" ~ "2132DC",
                                StembureauNaam.x == "Sandestein 40 2" ~ "2151KG",
                                StembureauNaam.x == "Sandestein 40 1" ~ "2151KG",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 417

# Gemeente 417

# The postcodes border one another and they divide the street in two.
# The building is in the two postcodes

all_problem_rows_list[["417"]] <- all_problem_rows_list[["417"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Historische Kring" ~ "Historische Kring Laren",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Historische Kring" ~ "1251KE",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Historische Kring" ~ "1251KE",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 432 

# The postcodes border one another; the building's entrance is in on Postcode.x

all_problem_rows_list[["432"]] <- all_problem_rows_list[["432"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == 'Hockeyclub ""Spire""' ~ "Hockeyclub Spire",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == 'Hockeyclub ""Spire""' ~ "1718MS",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == 'Hockeyclub ""Spire""' ~ "1718MS",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 448

# The postcodes do border one another, but the address is far from the border

# https://archief05.archiefweb.eu/archives/archiefweb/20251103110400/https://www.texel.nl/bestuur-en-organisatie/tweedekamerverkiezingen/verkiezingsuitslagen-tweede-kamerverkiezingen/

# Buureton seems to have two locations and none of the ones in location_unique seem to have the correct addres or postcode


all_problem_rows_list[["448"]] <- all_problem_rows_list[["448"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "De Buureton 2" ~ "NO INFORMATION",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "De Buureton 2" ~ "NO INFORMATION",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "De Buureton 2" ~ "NO INFORMATION",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 453

# The discrepancies in addresses come from the databases recording the places where the entrances seemed to be

all_problem_rows_list[["453"]] <- all_problem_rows_list[["453"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "gem. zaal Roos en Beek appartementencomlex" ~ "Gem. zaal Roos en Beek appartementencomplex",
                                           StembureauNaam.x == "'t Brederode Huys" ~ "t Brederode Huys",
                                           StembureauNaam.x == "Heerenduinzaal" ~ "Heerenduinzaal, ingang Heerenduinweg",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "gem. zaal Roos en Beek appartementencomlex" ~ "2071SH",
                                     StembureauNaam.x == "'t Brederode Huys" ~ "2082EK",
                                     StembureauNaam.x == "Heerenduinzaal"~ "1971JA",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "gem. zaal Roos en Beek appartementencomlex" ~ "2071SH",
                                StembureauNaam.x == "'t Brederode Huys" ~ "2082EK",
                                StembureauNaam.x == "Heerenduinzaal"~ "1971JA",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 489

# The location doesn't seem to have a postcode,but it does have a BAG code. The BAG code corresponds to the address
# But it doesn't have a postcode

all_problem_rows_list[["489"]] <- all_problem_rows_list[["489"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Pop-up stembureau (tent)" ~ "Pop-up stembureau",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Pop-up stembureau (tent)" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Pop-up stembureau (tent)" ~ "2994HP",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 518

# The stembureau is a mobile stembureau that seems to have the postcode of one of the stops linked to it.

# Source: https://uitslagen.denhaag.nl/tweede_kamerverkiezing_2025/kaart

all_problem_rows_list[["518"]] <- all_problem_rows_list[["518"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Dienstbus (portakabin)" ~ "Dienstbus GDH (Portakabin)",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Dienstbus (portakabin)" ~ "2564BZ",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Dienstbus (portakabin)" ~ "2564BZ",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 523

all_problem_rows_list[["523"]] <- all_problem_rows_list[["523"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Raadzaal Trouwzaal" ~ "Raadzaal Gemeentehuis",
                                           StembureauNaam.x == "Gereformeerde Kerk Stationsstraat" ~ "Gereformeerde Kerk, Stationsstraat",
                                           StembureauNaam.x == "Gereformeerde Kerk ingang W. Droststraat" ~ "Gereformeerde Kerk, ingang. W. Droststraat",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Raadzaal Trouwzaal" ~ "3371AS",
                                     StembureauNaam.x == "Gereformeerde Kerk Stationsstraat" ~ "3371AX",
                                     StembureauNaam.x == "Gereformeerde Kerk ingang W. Droststraat" ~ "3372XH",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Raadzaal Trouwzaal" ~ "3371AS",
                                StembureauNaam.x == "Gereformeerde Kerk Stationsstraat" ~ "3371AX",
                                StembureauNaam.x == "Gereformeerde Kerk ingang W. Droststraat" ~ "3372XH",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 627 

# Anne Frank Centrum is in Postcode.y

all_problem_rows_list[["627"]] <- all_problem_rows_list[["627"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Anne Frankcentrum" ~ "Anne Frank Centrum",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Anne Frankcentrum" ~ "2742VS",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Anne Frankcentrum" ~ "2742VS",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 753

# Source: https://www.groeiendbest.nl/politiek/waar-kunt-u-morgen-stemmen-in-best-6.72.669005.244ff27eac

# The postcode.y is correct

all_problem_rows_list[["753"]] <- all_problem_rows_list[["753"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Buurthuis Kadans" ~ "Buurthuis Kadans A",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Buurthuis Kadans" ~ "5684TS",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Buurthuis Kadans" ~ "5684TS",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 766

# If one searches for the official address of the Stembureau on a Maps app, the location changes to that one of location_unique

# https://www.dongen.nl/stembureaus

all_problem_rows_list[["766"]] <- all_problem_rows_list[["766"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Clubgebouw V.V.Dongen" ~ "V.V. Dongen",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Clubgebouw V.V.Dongen" ~ "5103BH",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Clubgebouw V.V.Dongen" ~ "5103BH",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 826

# The postcode.x seems to have the correct information but the data in location_unique is empty

# https://mijnstembureau-oosterhout.nl/uitslagen/verkiezingen/gr/download-opties


all_problem_rows_list[["826"]] <- all_problem_rows_list[["826"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Paulusveste Ingang Sint Paulusweg" ~ "Paulusveste",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Paulusveste Ingang Sint Paulusweg" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Paulusveste Ingang Sint Paulusweg" ~ NA,
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 879

all_problem_rows_list[["879"]] <- all_problem_rows_list[["879"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Wierenbos" ~ "Gemeenschapshuis Wierenbos 2",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Wierenbos" ~ "4884AB",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Wierenbos" ~ "4884AB",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 889

# Mobiel Stembureau was registered in the official website of the gemeente and in location_unique at Postcode.y

# Source: https://www.beesel.nl/verkiezingsuitslag

all_problem_rows_list[["889"]] <- all_problem_rows_list[["889"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "Mobiel Stembureau",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "5953DV",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "5953DV",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 899

# The Postcode.y is incorrect. So is the Postcode.x the official sources for Mobiel Stembureau have other locations
# that are in location_stembureau as the correct place

all_problem_rows_list[["899"]] <- all_problem_rows_list[["899"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel Stembureau" ~ "Mobiel Stembureau Zorgcentrum Huize Louise",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel Stembureau" ~ "6443BA",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel Stembureau" ~ "6443BA",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 935

# Fanfarezaal St. Servatius Biesland is in Postcode.y 
# Gymzaal OBS de Regenboog Postcode.x is the correct postccode, however, the building is part of a school that is the two postcodes. The side of the gym is in Postcode.x Since they border one 
# another, Postcode.y will be used

all_problem_rows_list[["935"]] <- all_problem_rows_list[["935"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Fanfarezaal St Servatius" ~ "Fanfarezaal St. Servatius Biesland",
                                           StembureauNaam.x == "Gymzaal OBS de Regenboog" ~ "Gymzaal, OBS de Regenboog",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Fanfarezaal St Servatius" ~ "6213CG",
                                     StembureauNaam.x == "Gymzaal OBS de Regenboog" ~ "6226DN",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Fanfarezaal St Servatius" ~ "6213CG",
                                StembureauNaam.x == "Gymzaal OBS de Regenboog" ~ "6226DN",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 983

# Mobiel Stembureau accoridng to the official Venlo website is registered to this address: Wal 32, 5944 AW, Arcen
# The postcode is the same as the one for Postcode.x

all_problem_rows_list[["983"]] <- all_problem_rows_list[["983"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel Bijzonder Stembureau Arcen - Venlo" ~ "NO INFORMATION",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel Bijzonder Stembureau Arcen - Venlo" ~ "NO INFORMATION",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel Bijzonder Stembureau Arcen - Venlo" ~ "NO INFORMATION",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 986

# The address is location_unique under a different StembureauNaam

all_problem_rows_list[["986"]] <- all_problem_rows_list[["986"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "MFC de Auw Sjoeël Ubachsberg" ~ "MFC De Auw Sjoeël Ubachsberg",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "MFC de Auw Sjoeël Ubachsberg" ~ "6367HC",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "MFC de Auw Sjoeël Ubachsberg" ~ "6367HC",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)


# The only Mobiel Stemburau registered in the official results from the municiaplity is that of Postcode 6001GS

# Source: https://www.weert.nl/verkiezingentk2025

all_problem_rows_list[["988"]] <- all_problem_rows_list[["988"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "Stadhuis Weert",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "6001GS",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "6001GS",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1598

# The addressesare registered to different locations within location_unique

# Source: https://www.koggenland.nl/tweede-kamer-verkiezingen-2025

all_problem_rows_list[["1598"]] <- all_problem_rows_list[["1598"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Gemeentehuis" ~ "Gemeentehuis De Goorn",
                                           StembureauNaam.x == "Verenigingsgebouw de Mantel" ~ "verenigingsgebouw De Mantel",
                                           StembureauNaam.x == "Cafe de Ridder" ~ "Café De Ridder",
                                           StembureauNaam.x == 'st voor Jeugdrecreatie ""het Gilde""' ~ 'St. voor jeugdrecreatie "Het Gilde"',
                                           StembureauNaam.x == "de Koperblazer" ~ "De Koperblazer",
                                           StembureauNaam.x == "SC handbal en voetbal Dynamo Ursem" ~ "SC Handbal en voetbal Dynamo Ursem",
                                           StembureauNaam.x == "Tennisvereniging de Berk" ~ "Tennisvereniging De Berk",
                                           StembureauNaam.x == "de Pianokamer" ~ "De Pianokamer",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Gemeentehuis" ~ "1648JG",
                                     StembureauNaam.x == "Verenigingsgebouw de Mantel" ~ "1641LZ",
                                     StembureauNaam.x == "Cafe de Ridder" ~ "1647ME",
                                     StembureauNaam.x == 'st voor Jeugdrecreatie ""het Gilde""' ~ "1633GZ",
                                     StembureauNaam.x == "de Koperblazer" ~ "1648KS",
                                     StembureauNaam.x == "SC handbal en voetbal Dynamo Ursem" ~ "1645SG",
                                     StembureauNaam.x == "Tennisvereniging de Berk" ~ "1647CB",
                                     StembureauNaam.x == "de Pianokamer" ~ "1641LW",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Gemeentehuis" ~ "1648JG",
                                StembureauNaam.x == "Verenigingsgebouw de Mantel" ~ "1641LZ",
                                StembureauNaam.x == "Cafe de Ridder" ~ "1647ME",
                                StembureauNaam.x == 'st voor Jeugdrecreatie ""het Gilde""' ~ "1633GZ",
                                StembureauNaam.x == "de Koperblazer" ~ "1648KS",
                                StembureauNaam.x == "SC handbal en voetbal Dynamo Ursem" ~ "1645SG",
                                StembureauNaam.x == "Tennisvereniging de Berk" ~ "1647CB",
                                StembureauNaam.x == "de Pianokamer" ~ "1641LW",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1640

# Meeting Point De Nassaurie 	Nassauplein 10 -12 	6096 AZ 	Grathem this information is nowhere in any of the databases

# Source: https://www.leudal.nl/stemlokalen

all_problem_rows_list[["1640"]] <- all_problem_rows_list[["1640"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Meeting Point De Nassaurie" ~ "NO INFORMATION",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Meeting Point De Nassaurie" ~ "NO INFORMATION",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Meeting Point De Nassaurie" ~ "NO INFORMATION",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1667

# The address is in location_unique

all_problem_rows_list[["1667"]] <- all_problem_rows_list[["1667"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "café 't Drieske" ~ "Café 't Drieske",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "café 't Drieske" ~ "5096BW",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "café 't Drieske" ~ "5096BW",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)


# Gemeente 1695

# Sources: https://allecijfers.nl/stembureaus/gemeente-noord-beveland/

all_problem_rows_list[["1695"]] <- all_problem_rows_list[["1695"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Kamperland" ~ "Dorpshuis Casembroot",
                                           StembureauNaam.x == "Colijnsplaat" ~ "Zorgcentrum Cleijenborch",
                                           StembureauNaam.x == "Wissenkerke" ~ "Dorpshuis Zaal onder de Toren",
                                           StembureauNaam.x == "Geersdijk" ~ "Dorpshuis Het Drenthehuis",
                                           StembureauNaam.x == "Kats" ~ "Dorpshuis de Vriendschap",
                                           StembureauNaam.x == "Kortgene" ~ "Dorpshuis De Stadsweide",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Kamperland" ~ "4493EZ",
                                     StembureauNaam.x == "Colijnsplaat" ~ "4486BW",
                                     StembureauNaam.x == "Wissenkerke" ~ "4491EW",
                                     StembureauNaam.x == "Geersdijk" ~ "4494NS",
                                     StembureauNaam.x == "Kats" ~ "4485AP",
                                     StembureauNaam.x == "Kortgene" ~ "4484DG",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Kamperland" ~ "4493EZ",
                                StembureauNaam.x == "Colijnsplaat" ~ "4486BW",
                                StembureauNaam.x == "Wissenkerke" ~ "4491EW",
                                StembureauNaam.x == "Geersdijk" ~ "4494NS",
                                StembureauNaam.x == "Kats" ~ "4485AP",
                                StembureauNaam.x == "Kortgene" ~ "4484DG",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1699

# Source: https://www.noordenveld.nl/processen-verbaal-tweede-kamer-verkiezing-29-oktober-2025

# Mobiel Stembureau 1 - Ceintuurbaan Zuid 21 , , Roden

all_problem_rows_list[["1699"]] <- all_problem_rows_list[["1699"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel stembureau 1" ~ "Verzorgingscentrum de Hullen",
                                           StembureauNaam.x == "Mobiel stembureau 2" ~ "Coöperatief Servicecentrum de Vijversburg",
                                           StembureauNaam.x == "Mobiel stembureau 3" ~ "Interzorg De Hoprank",
                                           StembureauNaam.x == "'t Dörpshuus NijRoon" ~ "t Dörpshuus NijRoon",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel stembureau 1" ~ "9301HW",
                                     StembureauNaam.x == "Mobiel stembureau 2" ~ "9331EB",
                                     StembureauNaam.x == "Mobiel stembureau 3" ~ "9321CC",
                                     StembureauNaam.x == "'t Dörpshuus NijRoon" ~ "9311PB",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel stembureau 1" ~ "9301HW",
                                StembureauNaam.x == "Mobiel stembureau 2" ~ "9331EB",
                                StembureauNaam.x == "Mobiel stembureau 3" ~ "9321CC",
                                StembureauNaam.x == "'t Dörpshuus NijRoon" ~ "9311PB",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1711

all_problem_rows_list[["1711"]] <- all_problem_rows_list[["1711"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Gemeenschapshuis de Annendaal" ~ "Gemeenschapshuis De Annendaal",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Gemeenschapshuis de Annendaal" ~ "6105AT",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Gemeenschapshuis de Annendaal" ~ "6105AT",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1730

all_problem_rows_list[["1730"]] <- all_problem_rows_list[["1730"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Sportcentrum De Marsch (evenemententent)" ~ "Sportcentrum De Marsch",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Sportcentrum De Marsch (evenemententent)" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Sportcentrum De Marsch (evenemententent)" ~ NA,
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1735

# By seraching the addresses Postcode.y are the correct ones 

# Source: https://www.hofweekblad.nl/nieuws/nieuws/20575/waar-kunnen-we-stemmen-in-hof-van-twente-alle-27-locaties-

all_problem_rows_list[["1735"]] <- all_problem_rows_list[["1735"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Zalencentrum De Bebsel" ~ "Zalencentrum de Bebsel",
                                           StembureauNaam.x == "Kantine sporthal de Mossendam" ~ "Kantine Sporthal de Mossendam",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Zalencentrum De Bebsel"~ "7471EL",
                                     StembureauNaam.x == "Kantine sporthal de Mossendam" ~ "7471SJ",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Zalencentrum De Bebsel"~ "7471EL",
                                StembureauNaam.x == "Kantine sporthal de Mossendam" ~ "7471SJ",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1742

all_problem_rows_list[["1742"]] <- all_problem_rows_list[["1742"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "De Wellehof (ingang Kattenhaarsweg)" ~ "De Wellehof",
                                           StembureauNaam.x == "Carinova Woonzorg De Diessenplas" ~ "Carinova Woonzorg Diessenplas",
                                           StembureauNaam.x == "Gemeentehuis Rijssen (Raadzaal)" ~ "Gemeentehuis",
                                           StembureauNaam.x == "Jacobus Fruytierschool (ingang achterzijde parkeerplaats Jumbo)" ~ "Jacobus Fruytierschool",
                                           StembureauNaam.x == "Kerkelijk Centrum de Ark" ~ "Kerkelijk centrum de Ark",
                                           StembureauNaam.x == "Kerkelijk Centrum Sion" ~ "Kerkelijk centrum Sion",
                                           StembureauNaam.x == "Kulturhus Holten (ingang Kerkstraat)" ~ "Kulturhus Holten",
                                           StembureauNaam.x == "Voormalig obs De Salto (ingang Keizersdijk)" ~ "Voormalige obs De Salto",
                                           StembureauNaam.x == "Willem-Alexanderschool" ~ "Willem Alexanderschool",
                                           StembureauNaam.x == "Pius X college (ingang gymzaal)" ~ "Pius X College Rijssen",
                                           StembureauNaam.x == "Gemeentehuis Rijssen (Brussel)" ~ "Gemeentehuis",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "De Wellehof (ingang Kattenhaarsweg)" ~ "7462GE",
                                     StembureauNaam.x == "Carinova Woonzorg De Diessenplas" ~ "7451DG",
                                     StembureauNaam.x == "Gemeentehuis Rijssen (Raadzaal)" ~ "7461DD",
                                     StembureauNaam.x == "Jacobus Fruytierschool (ingang achterzijde parkeerplaats Jumbo)" ~ "7463CA",
                                     StembureauNaam.x == "Kerkelijk Centrum de Ark" ~ "7463CR",
                                     StembureauNaam.x == "Kerkelijk Centrum Sion" ~ "7462BN",
                                     StembureauNaam.x == "Kulturhus Holten (ingang Kerkstraat)" ~ "7451BL",
                                     StembureauNaam.x == "Voormalig obs De Salto (ingang Keizersdijk)" ~ "7462JA",
                                     StembureauNaam.x == "Willem-Alexanderschool" ~ "7462HN",
                                     StembureauNaam.x == "Pius X college (ingang gymzaal)" ~ "7461CW",
                                     StembureauNaam.x == "Gemeentehuis Rijssen (Brussel)" ~ "7461DD",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "De Wellehof (ingang Kattenhaarsweg)" ~ "7462GE",
                                StembureauNaam.x == "Carinova Woonzorg De Diessenplas" ~ "7451DG",
                                StembureauNaam.x == "Gemeentehuis Rijssen (Raadzaal)" ~ "7461DD",
                                StembureauNaam.x == "Jacobus Fruytierschool (ingang achterzijde parkeerplaats Jumbo)" ~ "7463CA",
                                StembureauNaam.x == "Kerkelijk Centrum de Ark" ~ "7463CR",
                                StembureauNaam.x == "Kerkelijk Centrum Sion" ~ "7462BN",
                                StembureauNaam.x == "Kulturhus Holten (ingang Kerkstraat)" ~ "7451BL",
                                StembureauNaam.x == "Voormalig obs De Salto (ingang Keizersdijk)" ~ "7462JA",
                                StembureauNaam.x == "Willem-Alexanderschool" ~ "7462HN",
                                StembureauNaam.x == "Pius X college (ingang gymzaal)" ~ "7461CW",
                                StembureauNaam.x == "Gemeentehuis Rijssen (Brussel)" ~ "7461DD",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1774

# All the options of Mobiel Stembureau refer to the same stemburau.

all_problem_rows_list[["1774"]] <- all_problem_rows_list[["1774"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "Mobiel Stembureau, St. Jozef",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "7595AP",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Mobiel stembureau" ~ "7595AP",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1891

# All the Postcode.y are correct 

# Source: https://www.dantumadiel.frl/sites/default/files/2025-10/verkiezingen-tweede-kamer-2025-29-oktober-def.pdf

all_problem_rows_list[["1891"]] <- all_problem_rows_list[["1891"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Doopsgezinde kerk Damwâld" ~ "Doopsgezinde Kerk Damwâld",
                                           StembureauNaam.x == "Dorpshuis de Nije Warf Walterswâld" ~ "Dorpshuis De Nije Warf",
                                           StembureauNaam.x == "Nij Tjaerda Damwâld" ~ "Nij Tjaerda",
                                           StembureauNaam.x == "MFC de Beijer Rinsumageest" ~ "MFC De Beijer",
                                           StembureauNaam.x == "Herberg de Trochreed Readtsjerk" ~ "Herberg De Trochreed",
                                           StembureauNaam.x == "Dorpshuis de Pipegeal Broeksterwâld" ~ "Dorpshuis De Pipegael",
                                           StembureauNaam.x == "Kerkelijk Centrum de Mienskip Feanwâlden" ~ "Kerkelijk Centrum De Mienskip",
                                           StembureauNaam.x == "Zorgcentrum Talma Hoeve Feanwâlden" ~ "Zorgcentrum Talma Hoeve",
                                           StembureauNaam.x == "Zorgcentrum Brugchelenkamp de Westereen" ~ "Zorgcentrum Brugchelencamp",
                                           StembureauNaam.x == "Lokaal Christelijk Gereformeerde kerk de Westereen" ~ "Lokaal Chr.Geref. kerk",
                                           StembureauNaam.x == "Lokaal de Ferbiningstjerke de Westereen" ~ "Lokaal De Ferbiningstsjerke",
                                           StembureauNaam.x == "Kantine SC Veenwouden Feanwâlden" ~ "Kantine SC Veenwouden",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Doopsgezinde kerk Damwâld" ~ "9104GM",
                                     StembureauNaam.x == "Dorpshuis de Nije Warf Walterswâld" ~ "9113PA",
                                     StembureauNaam.x == "Nij Tjaerda Damwâld" ~ "9104KA",
                                     StembureauNaam.x == "MFC de Beijer Rinsumageest" ~ "9105KG",
                                     StembureauNaam.x == "Herberg de Trochreed Readtsjerk" ~ "9067DM",
                                     StembureauNaam.x == "Dorpshuis de Pipegeal Broeksterwâld" ~ "9108NE",
                                     StembureauNaam.x == "Kerkelijk Centrum de Mienskip Feanwâlden" ~ "9269SW",
                                     StembureauNaam.x == "Zorgcentrum Talma Hoeve Feanwâlden" ~ "9269VS",
                                     StembureauNaam.x == "Zorgcentrum Brugchelenkamp de Westereen" ~ "9271EP",
                                     StembureauNaam.x == "Lokaal Christelijk Gereformeerde kerk de Westereen" ~ "9271CH",
                                     StembureauNaam.x == "Lokaal de Ferbiningstjerke de Westereen" ~ "9271BP",
                                     StembureauNaam.x == "Kantine SC Veenwouden Feanwâlden" ~ "9269NB",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Doopsgezinde kerk Damwâld" ~ "9104GM",
                                StembureauNaam.x == "Dorpshuis de Nije Warf Walterswâld" ~ "9113PA",
                                StembureauNaam.x == "Nij Tjaerda Damwâld" ~ "9104KA",
                                StembureauNaam.x == "MFC de Beijer Rinsumageest" ~ "9105KG",
                                StembureauNaam.x == "Herberg de Trochreed Readtsjerk" ~ "9067DM",
                                StembureauNaam.x == "Dorpshuis de Pipegeal Broeksterwâld" ~ "9108NE",
                                StembureauNaam.x == "Kerkelijk Centrum de Mienskip Feanwâlden" ~ "9269SW",
                                StembureauNaam.x == "Zorgcentrum Talma Hoeve Feanwâlden" ~ "9269VS",
                                StembureauNaam.x == "Zorgcentrum Brugchelenkamp de Westereen" ~ "9271EP",
                                StembureauNaam.x == "Lokaal Christelijk Gereformeerde kerk de Westereen" ~ "9271CH",
                                StembureauNaam.x == "Lokaal de Ferbiningstjerke de Westereen" ~ "9271BP",
                                StembureauNaam.x == "Kantine SC Veenwouden Feanwâlden" ~ "9269NB",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1903

# The information with the address is in column Extra information. Extract later

all_problem_rows_list[["1903"]] <- all_problem_rows_list[["1903"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Hoeve De Laethof Mesch" ~ "De Laethof",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Hoeve De Laethof Mesch" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Hoeve De Laethof Mesch" ~ NA,
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1924

# The postcodes weren't recorded in Postcode.x but the names are a match

all_problem_rows_list[["1924"]] <- all_problem_rows_list[["1924"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Doopsgezinde Gemeente" ~ "Dorpsgezinde kerk",
                                           StembureauNaam.x == "Jongeren Activiteiten Centrum (JAC)" ~ "JAC",
                                           StembureauNaam.x == "Trefpunt" ~ "'t Trefpunt",
                                           StembureauNaam.x == "De Grutterswei (zaal A)" ~ "De Grutterswei zaal A",
                                           StembureauNaam.x == "De Grutterswei (zaal C)" ~ "De Grutterswei zaal C",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Doopsgezinde Gemeente" ~ "3253AS",
                                     StembureauNaam.x == "Jongeren Activiteiten Centrum (JAC)" ~ "3241LH",
                                     StembureauNaam.x == "Trefpunt" ~ "3243AP",
                                     StembureauNaam.x == "De Grutterswei (zaal A)" ~ "3255BS",
                                     StembureauNaam.x == "De Grutterswei (zaal C)" ~ "3255BS",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Doopsgezinde Gemeente" ~ "3253AS",
                                StembureauNaam.x == "Jongeren Activiteiten Centrum (JAC)" ~ "3241LH",
                                StembureauNaam.x == "Trefpunt" ~ "3243AP",
                                StembureauNaam.x == "De Grutterswei (zaal A)" ~ "3255BS",
                                StembureauNaam.x == "De Grutterswei (zaal C)" ~ "3255BS",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1950

# The Postcode.y_original is the names between .x and.y barely differ. There are two exceptions Wischmei Vlagtwedde and De Sprankel Sellingen
# However, the data is in location_unique

all_problem_rows_list[["1950"]] <- all_problem_rows_list[["1950"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Buurthuis 't Streekje Ter Apel" ~ "Buurthuis 't Streekje",
                                           StembureauNaam.x == "Buurthuis 't Ganzenust Vriescheloo" ~ "Buurthuis 't Ganzenust",
                                           StembureauNaam.x == "Buurthuis De Brug Bellingwolde" ~ "Buurthuis De Brug",
                                           StembureauNaam.x == "Buurthuis Jipsinghuizen Sellingen" ~ "Buurthuis Jipsinghuizen",
                                           StembureauNaam.x == "Buurthuis D'Oale School Ter Apelkanaal" ~ "D'Oale School",
                                           StembureauNaam.x == "Buurthuis De Grensstreek Barnflair Ter Apel" ~ "De Grensstreek Barnflair",
                                           StembureauNaam.x == "De Middenborg Blijham" ~ "De Middenborg",
                                           StembureauNaam.x == "De Turfschuur Bourtange" ~ "De Turfschuur",
                                           StembureauNaam.x == "Buurthuis De Voortgang Wedde" ~ "Dorpshuis De Voortgang",
                                           StembureauNaam.x == "Het MOW Bellingwolde" ~ "Het MOW | museum Westerwolde",
                                           StembureauNaam.x == "MFA De Meet Bellingwolde" ~ "MFA De Meet",
                                           StembureauNaam.x == "MFA Ons Noabershoes Veelerveen" ~ "MFA Ons Noabershoes",
                                           StembureauNaam.x == "MFA De Koningsspil Blijham" ~ "MFA de Koningsspil I Blijham",
                                           StembureauNaam.x == "OBS De Vlinder Ter Apel" ~ "OBS de Vlinder",
                                           StembureauNaam.x == "Steunstee Reiderstee Bellingwolde" ~ "Steunstee Reiderstee",
                                           StembureauNaam.x == "De Sprankel Sellingen" ~ "Verenigingsgebouw De Sprankel",
                                           StembureauNaam.x == "Wischmei Vlagtwedde" ~ "Sportzaal Wischmei 1",
                                           StembureauNaam.x == "Vestingkerkje Oudeschans" ~ "Vestingkerkje",
                                           StembureauNaam.x == "Museum Klooster Ter Apel" ~ "Museum Klooster",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Buurthuis 't Streekje Ter Apel" ~ "9561SC",
                                     StembureauNaam.x == "Buurthuis 't Ganzenust Vriescheloo" ~ "9699SE",
                                     StembureauNaam.x == "Buurthuis De Brug Bellingwolde" ~ "9695CJ",
                                     StembureauNaam.x == "Buurthuis Jipsinghuizen Sellingen" ~ "9551TH",
                                     StembureauNaam.x == "Buurthuis D'Oale School Ter Apelkanaal" ~ "9563RD",
                                     StembureauNaam.x == "Buurthuis De Grensstreek Barnflair Ter Apel" ~ "9561PC",
                                     StembureauNaam.x == "De Middenborg Blijham" ~ "9697RG",
                                     StembureauNaam.x == "De Turfschuur Bourtange" ~ "9545PH",
                                     StembureauNaam.x == "Buurthuis De Voortgang Wedde" ~ "9698AR",
                                     StembureauNaam.x == "Het MOW Bellingwolde" ~ "9695AE",
                                     StembureauNaam.x == "MFA De Meet Bellingwolde" ~ "9695DA",
                                     StembureauNaam.x == "MFA Ons Noabershoes Veelerveen" ~ "9566PL",
                                     StembureauNaam.x == "MFA De Koningsspil Blijham" ~ "9697RZ",
                                     StembureauNaam.x == "OBS De Vlinder Ter Apel" ~ "9561GN",
                                     StembureauNaam.x == "Steunstee Reiderstee Bellingwolde" ~ "9695GD",
                                     StembureauNaam.x == "De Sprankel Sellingen" ~ "9551BJ",
                                     StembureauNaam.x == "Wischmei Vlagtwedde" ~ "9541HE",
                                     StembureauNaam.x == "Vestingkerkje Oudeschans" ~ NA,
                                     StembureauNaam.x == "Museum Klooster Ter Apel" ~ "9561LH",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Buurthuis 't Streekje Ter Apel" ~ "9561SC",
                                StembureauNaam.x == "Buurthuis 't Ganzenust Vriescheloo" ~ "9699SE",
                                StembureauNaam.x == "Buurthuis De Brug Bellingwolde" ~ "9695CJ",
                                StembureauNaam.x == "Buurthuis Jipsinghuizen Sellingen" ~ "9551TH",
                                StembureauNaam.x == "Buurthuis D'Oale School Ter Apelkanaal" ~ "9563RD",
                                StembureauNaam.x == "Buurthuis De Grensstreek Barnflair Ter Apel" ~ "9561PC",
                                StembureauNaam.x == "De Middenborg Blijham" ~ "9697RG",
                                StembureauNaam.x == "De Turfschuur Bourtange" ~ "9545PH",
                                StembureauNaam.x == "Buurthuis De Voortgang Wedde" ~ "9698AR",
                                StembureauNaam.x == "Het MOW Bellingwolde" ~ "9695AE",
                                StembureauNaam.x == "MFA De Meet Bellingwolde" ~ "9695DA",
                                StembureauNaam.x == "MFA Ons Noabershoes Veelerveen" ~ "9566PL",
                                StembureauNaam.x == "MFA De Koningsspil Blijham" ~ "9697RZ",
                                StembureauNaam.x == "OBS De Vlinder Ter Apel" ~ "9561GN",
                                StembureauNaam.x == "Steunstee Reiderstee Bellingwolde" ~ "9695GD",
                                StembureauNaam.x == "De Sprankel Sellingen" ~ "9551BJ",
                                StembureauNaam.x == "Wischmei Vlagtwedde" ~ "9541HE",
                                StembureauNaam.x == "Vestingkerkje Oudeschans" ~ NA,
                                StembureauNaam.x == "Museum Klooster Ter Apel" ~ "9561LH",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1952

all_problem_rows_list[["1952"]] <- all_problem_rows_list[["1952"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Oudeweg 70" ~ "MFC Siddeburen",
                                           StembureauNaam.x == "Herman Gorterweg 21" ~ "De Zijlen, locatie de Gilde",
                                           StembureauNaam.x == "Tak van Poortvlietstraat 361" ~ "De Reensche Compagnie",
                                           StembureauNaam.x == "Hoofdweg 65" ~ "De Graankorrel",
                                           StembureauNaam.x == "Pieter Venemakade 59" ~ "Voormalige J. Albrondaschool",
                                           StembureauNaam.x == "Kleinemeersterstraat 158" ~ "Brandweermuseum Hoogezand-Sappemeer",
                                           StembureauNaam.x == "Viskenijlaan 2" ~ "De Viskenij",
                                           StembureauNaam.x == "Heiligelaan 71A" ~ "Gebouw Muziekvereniging",
                                           StembureauNaam.x == "Gorecht-Oost 157" ~ "Huis van Cultuur en Bestuur",
                                           StembureauNaam.x == "Dorpshuisweg 36" ~ "De Borgstee",
                                           StembureauNaam.x == "Noorderstraat 167" ~ "Koepelkerk",
                                           StembureauNaam.x == "Pluvierstraat 11" ~ "Multi Functioneel Centrum Foxhol",
                                           StembureauNaam.x == "Noorderstraat 27" ~ "Zalencentrum Brandpunt",
                                           StembureauNaam.x == "Oudeweg 89" ~ "Dorpshuis Westerbroek",
                                           StembureauNaam.x == "Hereweg 203" ~ "MFA Meeden",
                                           StembureauNaam.x == "Meerweg 16" ~ "Dorpshuis De Pompel",
                                           StembureauNaam.x == "W.A. Scholtenweg 18" ~ "Dorpscentrum De Broeckhof",
                                           StembureauNaam.x == "Pleiaden 21" ~ "Wijkcentrum Woldwijck",
                                           StembureauNaam.x == "Ruitenweg 39" ~ "Dorpshuis De Ruyten",
                                           StembureauNaam.x == "Hoofdweg 9A" ~ "De Houtstek",
                                           StembureauNaam.x == "Julianaplein 1" ~ "De Menterne",
                                           StembureauNaam.x == "Hoofdweg 81A" ~ "Dorpshuis Mainschoar",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Oudeweg 70" ~ "9628CG",
                                     StembureauNaam.x == "Herman Gorterweg 21" ~ "9649DA",
                                     StembureauNaam.x == "Tak van Poortvlietstraat 361" ~ "9602PJ",
                                     StembureauNaam.x == "Hoofdweg 65" ~ "9617AB",
                                     StembureauNaam.x == "Pieter Venemakade 59" ~ "9605PL",
                                     StembureauNaam.x == "Kleinemeersterstraat 158" ~ "9611JJ",
                                     StembureauNaam.x == "Viskenijlaan 2" ~ "9628AZ",
                                     StembureauNaam.x == "Heiligelaan 71A" ~ "9636CL",
                                     StembureauNaam.x == "Gorecht-Oost 157" ~ "9603AE",
                                     StembureauNaam.x == "Dorpshuisweg 36" ~ "9617BN",
                                     StembureauNaam.x == "Noorderstraat 167" ~ "9611AD",
                                     StembureauNaam.x == "Pluvierstraat 11" ~ "9607RJ",
                                     StembureauNaam.x == "Noorderstraat 27" ~ "9611AA",
                                     StembureauNaam.x == "Oudeweg 89" ~ "9608PK",
                                     StembureauNaam.x == "Hereweg 203" ~ "9651AG",
                                     StembureauNaam.x == "Meerweg 16" ~ "9625PJ",
                                     StembureauNaam.x == "W.A. Scholtenweg 18" ~ "9636BS",
                                     StembureauNaam.x == "Pleiaden 21" ~ "9602KD",
                                     StembureauNaam.x == "Ruitenweg 39" ~ "9619PL",
                                     StembureauNaam.x == "Hoofdweg 9A" ~ "9621AC",
                                     StembureauNaam.x == "Julianaplein 1" ~ "9649BX",
                                     StembureauNaam.x == "Hoofdweg 81A" ~ "9615AB",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Oudeweg 70" ~ "9628CG",
                                StembureauNaam.x == "Herman Gorterweg 21" ~ "9649DA",
                                StembureauNaam.x == "Tak van Poortvlietstraat 361" ~ "9602PJ",
                                StembureauNaam.x == "Hoofdweg 65" ~ "9617AB",
                                StembureauNaam.x == "Pieter Venemakade 59" ~ "9605PL",
                                StembureauNaam.x == "Kleinemeersterstraat 158" ~ "9611JJ",
                                StembureauNaam.x == "Viskenijlaan 2" ~ "9628AZ",
                                StembureauNaam.x == "Heiligelaan 71A" ~ "9636CL",
                                StembureauNaam.x == "Gorecht-Oost 157" ~ "9603AE",
                                StembureauNaam.x == "Dorpshuisweg 36" ~ "9617BN",
                                StembureauNaam.x == "Noorderstraat 167" ~ "9611AD",
                                StembureauNaam.x == "Pluvierstraat 11" ~ "9607RJ",
                                StembureauNaam.x == "Noorderstraat 27" ~ "9611AA",
                                StembureauNaam.x == "Oudeweg 89" ~ "9608PK",
                                StembureauNaam.x == "Hereweg 203" ~ "9651AG",
                                StembureauNaam.x == "Meerweg 16" ~ "9625PJ",
                                StembureauNaam.x == "W.A. Scholtenweg 18" ~ "9636BS",
                                StembureauNaam.x == "Pleiaden 21" ~ "9602KD",
                                StembureauNaam.x == "Ruitenweg 39" ~ "9619PL",
                                StembureauNaam.x == "Hoofdweg 9A" ~ "9621AC",
                                StembureauNaam.x == "Julianaplein 1" ~ "9649BX",
                                StembureauNaam.x == "Hoofdweg 81A" ~ "9615AB",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1955

all_problem_rows_list[["1955"]] <- all_problem_rows_list[["1955"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Cafe-restaurant Uniek" ~ "Café-restaurant Uniek Didam",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Cafe-restaurant Uniek" ~ "6942EB",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Cafe-restaurant Uniek" ~ "6942EB",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1963

# The Postcode.y has the correct address of the Stembureau. The Postcodes border one another.

all_problem_rows_list[["1963"]] <- all_problem_rows_list[["1963"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "CBS De Weerklank" ~ "cbs De Weerklank",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "CBS De Weerklank" ~ "3273AL",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "CBS De Weerklank" ~ "3273AL",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1978

# Postcodes are on Postcode.y 

all_problem_rows_list[["1978"]] <- all_problem_rows_list[["1978"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "De Beemd" ~ "Dorpshuis De Beemd",
                                           StembureauNaam.x == "'t Wingerds Hof" ~ "t Wingerds Hof",
                                           StembureauNaam.x == "Zalen- en partycentrum De Til" ~ "Zalen- en Partycentrum De Til",
                                           StembureauNaam.x == "MFC Noorderhuis" ~ "MFC Het Noorderhuis",
                                           StembureauNaam.x == "Peperhof Arkel" ~ "De Peperhof",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "De Beemd" ~ "2969CD",
                                     StembureauNaam.x == "'t Wingerds Hof" ~ "3366BD",
                                     StembureauNaam.x == "Zalen- en partycentrum De Til" ~ "3381BS",
                                     StembureauNaam.x == "MFC Noorderhuis" ~ "4225RT",
                                     StembureauNaam.x == "Peperhof Arkel" ~ "4241BW",
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "De Beemd" ~ "2969CD",
                                StembureauNaam.x == "'t Wingerds Hof" ~ "3366BD",
                                StembureauNaam.x == "Zalen- en partycentrum De Til" ~ "3381BS",
                                StembureauNaam.x == "MFC Noorderhuis" ~ "4225RT",
                                StembureauNaam.x == "Peperhof Arkel" ~ "4241BW",
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1980

all_problem_rows_list[["1980"]] <- all_problem_rows_list[["1980"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Kantoorunit parkeerplaats NS Station" ~ "Kantoorunit parkeerterrein NS station",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Kantoorunit parkeerplaats NS Station" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Kantoorunit parkeerplaats NS Station" ~ NA,
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

# Gemeente 1992


# IT NEEDS TO BE ADDED THE POSTCODES.X

all_problem_rows_list[["1992"]] <- all_problem_rows_list[["1992"]] |>
  mutate(Postcode.x_original = Postcode.x,
         Postcode.y_original = Postcode.y_original,
         StembureauNaam.y_original = StembureauNaam.y_original,
         lookup_StembureauNaam = case_when(StembureauNaam.x == "Gebouw Vlinderhof" ~ "Gebouw Vlinderhof 4 (2)",
                                           StembureauNaam.x == "Parkzicht I (recreatieruimte)" ~ "Parkzicht I",
                                           TRUE ~ StembureauNaam.y),
         lookup_Postcode = case_when(StembureauNaam.x == "Gebouw Vlinderhof" ~ NA,
                                     StembureauNaam.x == "Parkzicht I (recreatieruimte)" ~ NA,
                                     TRUE ~ Postcode.y),
         Postcode.x = case_when(StembureauNaam.x == "Gebouw Vlinderhof" ~ NA,
                                StembureauNaam.x == "Parkzicht I (recreatieruimte)" ~ NA,
                                TRUE ~ Postcode.x)) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         Postcode.x,
         Postcode.x_original,
         Postcode.y_original,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         lookup_Postcode,
         .y,
         .x,
         score,
         selected) |>
  left_join(location_unique,
            by = c("GemeenteCode.x" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode")) |>
  mutate(StembureauNaam.y = StembureauNaam.x,
         Postcode.y = Postcode.x) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteNaam.y,
         GemeenteCode.x,
         GemeenteCode.y,
         StembureauNaam.x,
         StembureauNaam.y,
         StembureauNaam.y_original,
         lookup_StembureauNaam,
         Postcode.x,
         Postcode.x_original,
         Postcode.y,
         Postcode.y_original,
         lookup_Postcode,
         .y,
         .x,
         score,
         everything(),
         -GemeenteNaam)

manual_corrections_finished <- bind_rows(all_problem_rows_list, .id = "manual_list_code")


manual_corrections_solved <- manual_corrections_finished |>
  filter(!lookup_StembureauNaam %in% c("NO INFORMATION"),
         !lookup_Postcode %in% c("NO INFORMATION"),
         !Postcode.x %in% c("NO INFORMATION"))

manual_corrections_no_information <- manual_corrections_finished |>
  filter(if_any(c(lookup_StembureauNaam,
                  lookup_Postcode,
                  Postcode.x,
                  Postcode.y),
                ~ .x == "NO INFORMATION"))

write.csv(manual_corrections_finished,
          "manual_corrections_finished.csv")

write.csv(manual_corrections_solved,
          "manual_corrections_solved.csv")

write.csv(manual_corrections_no_information,
          "manual_corrections_no_information.csv")

#############################################################################################################
#                               Joining the location data
#############################################################################################################


all_corrected_problem_rows <- bind_rows("postcode_x_missing_y_present" = linked_name_equal_postcode_x_missing_y_present_fixed,
                                        "postcode_y_missing_x_present" = linked_name_equal_postcode_y_missing_x_present_fixed,
                                        "same_name_different_postcodes" = linked_same_name_different_postcodes_fixed,
                                        "first_loop_same_postcode" = solved_df,
                                        "second_loop_address_match" = solved_address_df,
                                        "third_loop_unique_postcode_match" = solved_rows,
                                        "manual_corrections" = manual_corrections_solved,
                                        .id = "correction_source")



missing_from_all_corrected_problem_rows <- linked_not_equal |>
  anti_join(
    all_corrected_problem_rows |>
      select(vote_booth_id) |>
      distinct(),
    by = "vote_booth_id"
  ) |>
  select(vote_booth_id,
         GemeenteNaam.x,
         GemeenteCode.x,
         StembureauNaam.x,
         Postcode.x,
         StembureauNaam.y,
         Postcode.y,
         X,
         Y,
         everything())


linked_correct_plus_corrected_FINAL <- bind_rows(linked_correct_initial,
                                           all_corrected_problem_rows)


linkage_columns_to_add <- linked_correct_plus_corrected_FINAL |>
  mutate(StembureauNaam_final = StembureauNaam.x,
         Postcode_final = Postcode.x,
         StembureauNaam_location_original = coalesce(lookup_StembureauNaam, StembureauNaam.y),
         Postcode_location_original = coalesce(lookup_Postcode, Postcode.y)) |>
  select(vote_booth_id,
         StembureauNaam_final,
         Postcode_final,
         StembureauNaam_location_original,
         Postcode_location_original,
         StembureauNaam.y,
         Postcode.y,
         X,
         Y,
         Latitude,
         Longitude,
         `BAG Nummeraanduiding ID`,
         Straatnaam,
         Huisnummer,
         Huisletter,
         Huisnummertoevoeging)

voting_unique_linked <- voting_unique |>
  left_join(linkage_columns_to_add,
            by = "vote_booth_id")

write.csv(voting_unique_linked,
          "voting_unique_linked_incomplete.csv")

sum(is.na(voting_unique_linked$X))


missing_coordinates_vl <- voting_unique_linked |>
  filter(is.na(voting_unique_linked$X))

##############################################################################################################
#                                 Fixing the 152 remaining Stembureaus
##############################################################################################################

# For most of the addresses, the information was already stored in location_unique. It was only a matter of
# searching for the postcode, or for similarly named stembureaus that shared either the GemeenteCode, GemeenteNaam
# or a mix of all of the previous options. However, some stembureaus' information was not located in the available databases.
# A solution is to download the BAG database that includes every single XY coordinate in the Netherlands and with the supplementary
# information with the exact addresses. We can then use those addresses to search for the XY coordinates in the bag database.

library(tibble)

manual_pairs <- tribble(
  ~vote_booth_id, ~GemeenteCode, ~lookup_StembureauNaam, ~lookup_Postcode,
  337, 74, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  542, 106, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  581, 109, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  582, 109, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  1055, 166, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  1061, 166, "Sporthal KHC 1", "8265VC",
  1066, 166, "De Hoeksteen 1", "8271GB",
  1074, 166, "Sporthal KHC 1","8265VC",
  1075, 166, "Oosterholthoeve 1", "8271PS",
  1078, 166, "Stadhuis 1", "8261DD",
  1079, 166, "De Hoeksteen 1", "8271GB",
  1141, 173, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  1178, 177, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  1348, 200, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  1726, 228, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  1856, 263, "Gemeentehuis", "5331CB",
  1859, 263, "Gelre's End", "5321GG",
  1861, 263, "Rehoboth", "5321TM",
  2055, 279, "Kulturhus De Breehoek", "3925JP",
  2120, 293, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  2169, 297, "Wijkcentrum 'de Grote Aak'", "5301GX",
  2183, 297, "Van der Valk Zaltbommel", "5301LJ",
  2187, 297, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  2396, 308, "NS Station (zowel spoor- als pleinzijde)", "3743KM",
  2403, 308, "NO INFORMATION", "NO INFORMATION",
  2433, 312, "Rijnzaal", "3981GD",
  2435, 312, "R.K. Parochie Odijk", "3984JA",
  2480, 327, "Huis van Leusden", "3831NA",
  2488, 327, "Maxima's Leusden", "3832JS",
  2489, 327, "Ons Gebouw", "3791PN",
  2510, 335, "Sporthal Hofland", "3417TA",
  2514, 335, "Sporthal De Vaart", "3461GA",
  2537, 342, 'Postduiven Vereniging "De Zwaluw"', "3762CR",
  2549, 342, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  2643, 344, "De Olijfboom", "3526VN",
  2653, 344, "OBS De Klimroos", "3544RK",
  2758, 345, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  2830, 355, "Gemeentehuis", "3701HS",
  2846, 355, "Sionskerk", "3705BP",
  2850, 355, "Kerkelijk Centrum Zeist-West, De Clomp 3302","3704KB",
  3061, 363, "Centraal Station - GVB Metroplein", NA,
  3129, 363, "Servicepunt Het Brinkhuis", "1097TM",
  3145, 363, "Hotel Jakarta Amsterdam", "1019SH",
  3205, 363, "NH Amsterdam Zuid", "1082GG",
  3224, 363, "Huis van de Buurt De Boeg", "1055SC",
  3358, 363, "Muziekschool Noord", "1024TT",
  3411, 363, "Buurtkamer Kadoelen", "1035SB",
  3496, 377, "Gemeentehuis", "2051GJ",
  3499, 377, "Woonzorgcentrum de Rijp", "2061SC",
  3517, 383, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  3670, 392, "NO INFORMATION - EXCLUDE", "NO INFORMATION - EXCLUDE",
  3677, 394, "De Caleidoscoop", "2131VN",
  3681, 394, "Wijkcentrum 't Kattegat", "2133DX",
  3682, 394, "Gymzaal Bornholm", "2133DW",
  3697, 394, "Dorpshuis De Ontmoeting", "2158MC",
  3722, 394, "IBS De Lotus", "2135HA",
  3753, 394, "Dorpscentrum Spaarndam", "2064KK",
  3754, 394, "Buurthuis De Stoep", "2065AK",
  3762, 394, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  3763, 394, "Pelgrimskerk Badhoevedorp-Lijnden", "1171DW",
  3766, 394, "Basisschool 't Venne", "2152TB",
  3768, 394, "Gymzaal Braambos", "2134XL",
  3769, 394, "TC Zwaanshoek", "2136AH",
  3806, 399, "Witte Kerk", "1851KS",
  4100, 448, "De Buureton", "1791GA",
  4110, 448, "Gemeentehuis", "1791AT",
  4111, 448, "De Buureton", "1791GA",
  4120, 450, "Gemeentehuis Uitgeest", "1911EG",
  4151, 453, "Sporthal Het Polderhuis", "1991RK",
  4182, 473, "NO INFORMATION - EXCLUDE", "NO INFORMATION - EXCLUDE",
  4341, 489, "Jongerencentrum BLOK 0180", "2992ZB",
  4342, 489, "Jongerencentrum BLOK 0180", "2992ZB",
  4343, 489, "Sporthal Aksent", "2992GK",
  4346, 489, "Carnisse Haven kerk", "2993TA",
  4350, 489, "Sportpark vv Smitshoek", "2993BZ",
  4352, 489, "Sporthal Waterpoort", "2993DL",
  4353, 489, "Sporthal Waterpoort", "2993DL",
  5058, 575, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  5061, 575, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  5069, 575, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  5070, 575, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  5879, 715, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  5915, 716, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  5930, 717, "Verenigingsgebouw Westkapelle Herrijst", "4361AE",
  5932, 717, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  5933, 717, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  6155, 758, "SCC De Pekhoeve", "4851CN",
  6167, 758, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  6168, 758, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  6184, 762, "Hofke van Marijke", "5753BE",
  6185, 762, "Gemeentehuis", "5751BE",
  6187, 762, "S.C.C. Den Draai", "5754AV",
  6320, 779, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  6323, 779, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  6479, 797, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  6494, 797, "M.F.A. Caleidoscoop 1", "5251NG",
  6827, 865, "Tennisvereniging Woburnpark", "5263GN",
  7180, 971, "MFC De Grous", "6171HW",
  7187, 971, "MFC Chaparral", "6129PG",
  7190, 971, "Maaslandcentrum", "6181EA",
  7233, 983, "Bibliotheek Tegelen", "5931NL",
  7249, 983, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  7423, 1525, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  7431, 1581, "Nieuw Salem", "3971CA",
  7442, 1581, "De Twee Marken", "3951CR",
  7447, 1581, "Allemanswaard", "3958KA",
  7452, 1581, "De Binder", "3956ED",
  7456, 1581, "Cultuurhuis Pléiade", "3941HV",
  7460, 1581, "Sporthal Steinheim", "3941MB",
  7501, 1598, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  7526, 1621, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  7534, 1640, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  7571, 1652, "De Eendracht", "5421KC",
  7772, 1696, "Sporthal De Fuik", "1241HD",
  7774, 1696, "Gemeentehuis Wijdemeren", "1231KB",
  7854, 1705, "Sporthal De Brink", "6852DW",
  7855, 1705, "Sporthal De Brink", "6852DW",
  7860, 1705, "Sporthal de Bongerd", "6681CD",
  8087, 1731, "Zalencentrum Het Kompas", "9422AA",
  8155, 1740, "Ons Dorpshuis Kesteren", "4041XE",
  8156, 1740, "Het Baken", "4051AA",
  8171, 1742, "Openbare Basisschool de Holterenk", "7451AW",
  8293, 1859, "Gemeentehuis", "7271AX",
  8301, 1859, "Kulturhus Ruurlo", "7261NL",
  8506, 1896, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  8696, 1916, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  8697, 1916, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  8698, 1916, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  8701, 1916, "Gemeentehuis Leidschendam (2)", "2264BP",
  8716, 1916, "EXTERNAL ADDRESS", "EXTERNAL ADDRESS",
  9549, 1982, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  9592, 1982, "MOBILE STEMBUREAU - EXCLUDE", "MOBILE STEMBUREAU - EXCLUDE",
  9593, 1991, "Gemeentehuis Maashorst", "5401EJ",
  9596, 1991, "Gymzaal Hoenderbos", "5406CG",
  9599, 1991, "Ontmoetingsplein Mellepark", "5403XE",
  9608, 1991, "Gemeenschapshuis De Schakel", "5408XA",
  9612, 1991, 'Dorpshuis "De Phoenix"', "5374BG",
  9614, 1991, "Dorpshuis Zeeland", "5411BA")


missing_coordinates_vl_fixed <- missing_coordinates_vl |>
  left_join(manual_pairs,
            by = c("vote_booth_id", "GemeenteCode")) |>
  left_join(location_unique,
            by = c("GemeenteCode" = "GemeenteCode",
                   "lookup_StembureauNaam" = "StembureauNaam",
                   "lookup_Postcode" = "Postcode"),
            suffix = c("", "_location"))

# List of stembureauswhose information was found online:

# 542 106 (Assen, Peelerhof): Walakker 3, 9407BT Assen Source: https://technischbeheerassen.stackstorage.com/s/TrAgEUIPvzmCfCwp/en
# 1055 166 (Kampen, Amandelboom): Wederiklaan 2,8265DC Kampen Source:https://www.kampen.nl/file/kampen11de-amandelboomgr261etellingpdf
# 1178 177 (Raalte, Sporthal Hoogerheyne (Heino)) Brinkweg 50, 8141NH Heino Source: https://allecijfers.nl/stembureau/sporthal-hoogerheijne-heino/
# 1348 200 (Apeldoorn, Postduivenvereniging ""Holthuizen"") Hoenderparkweg 14, 7335GT Source: https://www.apeldoorn.nl/fl-uitslag-gr-2026
# 1726 228 (Ede, Theehuis De Roek) Roeklaan 24,  6733SR Source: https://www.ede.nl/bestuur-en-organisatie/gemeenteraadsverkiezingen-2026/stemlokalen-gemeenteraadsverkiezingen-2026
# 2120 293 (Westervoort, Huize Vredenburg) Klapstraat 112 6931CM Source: https://allecijfers.nl/stembureau/huize-vredenburg-westervoort/
# 2396 AND 2403, 308 SOURCE: https://www.baarn.nl/uitslagen-tweede-kamerverkiezing-gemeente-baarn
# 2549, 342 (Soest,  Sporthal De Bunt) Oude Utrechtseweg 10 3768CC Soest Source: https://www.soest.nl/fileadmin/documenten/Gemeenteraadsverkiezingen/Stembureau_20_Sporthal_De_Bunt.pdf
# 2758, 345, (Veenendaal, Gymzaal Trommelaar) Trommelaar 63 3905AV Veenendaal, Source: https://www.veenendaal.nl/fileadmin/files/Veenendaal/Tweede_Kamerverkiezing_2025/Publicaties/De_processen-verbaal_van_de_stembureaus___model_N_10-2_/Veenendaal_203_GymzaalTrommelaar_TK25_eerste_telling.pdf
# 3517, 383 (Castricum, Pastorie RK Kerk) Dorpstraat 113, 1901EK. Source: https://www.castricum.nl/fileadmin/Castricum/documenten-per-onderwerp/verkiezingen/TKV-procesverbalen/Castricum_5_Pastorie_RK_Kerk_TK25.pdf
# 3670 392, Haarlem, source: https://haarlem.verkiezinginbeeld.nl/
# 3762 394 (Haarlemmermeer Bennebroekerweg 600 2135AA)  Source: https://www.rodi.nl/haarlemmermeer/nieuws/465314/dit-zijn-alle-stemlocaties-in-haarlemmermeer-per-regio
# 5058 575, (Noordwijk, Stayokay Noordwijk) langevelderlaan 45 2204BC. Source: https://www.denoordwijker.nl/nieuws/algemeen/103018/hier-kun-je-in-noordwijk-stemmen
# 5061 575, (Noordwijk, Puyckendam) Pilarenlaan 4, p4, 2211NA Source: https://www.denoordwijker.nl/nieuws/algemeen/103018/hier-kun-je-in-noordwijk-stemmen
# 5879 715, (Terneuzen,	Geref. Kerk De Levensbron) Sloelaan 40 4535EG. Source: https://www.terneuzen.nl/app/uploads/2026/03/Terneuzen_11_Geref.-Kerk-De-Levensbron_GR26.pdf
# 6320 779, (Geertruidenberg, Mauritsstaete) Stadsweg 9 a 01, 4931HV. Source: https://www.geertruidenberg.nl/sites/default/files/Geertruidenberg_9_Mauritsstaete_GR26.pdf
# 6323, 779 (Geertruidenberg,Zorgcentrum ""Het Hoge Veer"") Scheepswerflaan 47 , 4941 GZ Raamsdonksveer. Source: https://www.geertruidenberg.nl/sites/default/files/Geertruidenberg_12_zorgcentrum%20Het%20Hoge%20Veer_GR26.pdf
# 6479, 797 (Heusden, Sint Janshof) Pastoor van Akenstraat 30 	5251BL. Source: https://heusden.verkiezinginbeeld.nl/lijst
# 7423, 1525 (Teylingen, De Molen van Sassenheim) H. Knoopstraat 1 2171PW. Sourve: https://teylingen.nl/wp-content/uploads/sites/4/2026/03/Tey_PV_GSB_GR26_ZH-2.pdf
# 7534, 1640 (Leudal, Meeting Point De Nassaurie) Nassauplein 10-12,6096AZ Grathem Source: https://www.leudal.nl/stemlokalen
# 8696, 1916 (Leidschendam-Voorburg, Kindcentrum Cascade) Delflandlaan 6 2273CS. Source: https://allecijfers.nl/stembureau/kindcentrum-cascade-voorburg/
# 8697 and 8698 (Leidschendam-Voorburg, Sporthal Essesteijn & Sporthal Essesteijn (2)) Elzendreef 20 2272EB. Source: https://allecijfers.nl/stembureau/sporthal-essesteijn-voorburg/
# 8716 (Leidschendam-Voorburg, Dorpspunt) Doctor van Noortstraat 90 2266HA. Source: https://allecijfers.nl/stembureau/dorpspunt-leidschendam/

library(sf)

# WARNING: LOADING bag-light.gpkg WILL TAKE ANYWHERE BETWEEN 10-20 MINUTES.

bag <- st_read("bag-light.gpkg",
               layer = "verblijfsobject",
               quiet = TRUE)


library(stringr)

##############################################################################################################
#                             External addresses that need XY from bag-light.gpkg
##############################################################################################################

external_addresses <- tribble(~vote_booth_id, ~GemeenteCode, ~location_name, ~straat, ~huisnummer, ~huisletter, ~huisnummertoevoeging, ~postcode,
                              542, 106, "Peelerhof", "Walakker", 87, NA, NA, "9407BT",
                              1055, 166, "Amandelboom", "Wederiklaan", 2, NA, NA, "8265DC",
                              1178, 177, "Sporthal Hoogerheyne", "Brinkweg", 50, NA, NA, "8141NH",
                              1348, 200, 'Postduivenvereniging "Holthuizen"', "Hoenderparkweg", 14, NA, NA, "7335GT",
                              1726, 228, "Theehuis De Roek", "Roeklaan", 24, NA, NA, "6733SR",
                              2120, 293, "Huize Vredenburg", "Klapstraat", 112, NA, NA, "6931CM",
                              2549, 342, "Sporthal De Bunt", "Oude Utrechtseweg", 10, NA, NA, "3768CC",
                              2758, 345, "Gymzaal Trommelaar", "Duivenwal-west", 300, NA, NA, "3905AV",
                              3517, 383, "Pastorie RK Kerk", "Dorpsstraat", 113, NA, NA, "1901EK",
                              3762, 394, "Bennebroekerweg 600", "Bennebroekerweg", 600, NA, NA, "2135AA",
                              5058, 575, "Stayokay Noordwijk", "Langevelderlaan", 45, NA, NA, "2204BC",
                              5061, 575, "Puyckendam", "Pilarenlaan", 4, NA, "P4", "2211NA",
                              5879, 715, "Geref. Kerk De Levensbron", "Sloelaan", 40, NA, NA, "4535EG",
                              6320, 779, "Mauritsstaete", "Stadsweg", 9, "A", "01", "4931HV",
                              6323, 779, 'Zorgcentrum "Het Hoge Veer"', "Scheepswerflaan", 47, NA, NA, "4941GZ",
                              6479, 797, "Sint Janshof", "Kees Klerxstraat", 40, NA, NA, "5251BL",
                              7423, 1525, "De Molen van Sassenheim", "H. Knoopstraat", 1, NA, NA, "2171PW",
                              7534, 1640, "Meeting Point De Nassaurie", "Nassauplein", 10, NA, NA, "6096AZ",
                              8696, 1916, "Kindcentrum Cascade", "Delflandlaan", 6, NA, NA, "2273CS",
                              8697, 1916, "Sporthal Essesteijn", "Elzendreef", 20, NA, NA, "2272EB",
                              8698, 1916, "Sporthal Essesteijn (2)", "Elzendreef", 20, NA, NA, "2272EB",
                              8716, 1916, "Dorpspunt", "Doctor van Noortstraat", 90, NA, NA, "2266HA") |>
  mutate(postcode_clean = str_to_upper(str_replace_all(postcode, "\\s+", "")),
         huisletter_clean = str_to_upper(huisletter),
         huisnummertoevoeging_clean = str_to_upper(huisnummertoevoeging))



##############################################################################################################
#                                 Clean-up of the manual address table
##############################################################################################################

external_addresses_clean <- external_addresses |>
  mutate(postcode_clean = str_to_upper(str_replace_all(postcode, "\\s+", "")),
         straat_clean = str_to_upper(straat),
         straat_clean = str_replace_all(straat_clean, "[[:punct:]]", " "),
         straat_clean = str_squish(straat_clean),
         huisnummer_clean = as.integer(huisnummer),
         huisletter_clean_manual = str_to_upper(str_squish(huisletter)),
         toevoeging_clean_manual = str_to_upper(str_replace_all(huisnummertoevoeging, "\\s+", "")))

##############################################################################################################
#                         Format BAG databse for the needed postcodes
##############################################################################################################

needed_postcodes <- external_addresses_clean |>
  distinct(postcode_clean) |>
  pull(postcode_clean)

bag_subset <- bag |>
  mutate(postcode_clean = str_to_upper(str_replace_all(postcode, "\\s+", "")),
         huisnummer_clean = as.integer(huisnummer),
         huisletter_clean_bag = str_to_upper(str_squish(huisletter)),
         toevoeging_clean_bag = str_to_upper(str_replace_all(toevoeging, "\\s+", "")),
         straat_clean_full = str_to_upper(openbare_ruimte_naam),
         straat_clean_full = str_replace_all(straat_clean_full, "[[:punct:]]", " "),
         straat_clean_full = str_squish(straat_clean_full),
         straat_clean_short = str_to_upper(openbare_ruimte_naam_kort),
         straat_clean_short = str_replace_all(straat_clean_short, "[[:punct:]]", " "),
         straat_clean_short = str_squish(straat_clean_short)) |>
  filter(postcode_clean %in% needed_postcodes)


##############################################################################################################
#                            BAG matching table with street-name variants
##############################################################################################################

bag_subset_long <- bind_rows(
  bag_subset |>
    mutate(straat_clean = straat_clean_full,
           straat_match_type = "full"),
  
  bag_subset |>
    mutate(straat_clean = straat_clean_short,
           straat_match_type = "short")) |>
  filter(!is.na(straat_clean), 
         straat_clean != "") |>
  distinct()

##############################################################################################################
#                           Match external addresses to BAG
##############################################################################################################

bag_matches <- external_addresses_clean |>
  left_join(bag_subset_long,
            by = c("postcode_clean","straat_clean","huisnummer_clean"),
            suffix = c("_manual", "_bag"),
            relationship = "many-to-many") |>
  filter(coalesce(huisletter_clean_manual, "") == coalesce(huisletter_clean_bag, ""),
         coalesce(toevoeging_clean_manual, "") == coalesce(toevoeging_clean_bag, ""))


coords <- st_coordinates(bag_matches$geom)

bag_matches <- bag_matches |>
  mutate(X = coords[, "X"],
         Y = coords[, "Y"])


bag_patch <- bag_matches |>
  st_drop_geometry() |>
  transmute(vote_booth_id,
            GemeenteCode,
            X,
            Y,
            Straatnaam = openbare_ruimte_naam,
            Huisnummer = huisnummer_bag,
            Huisletter = huisletter_bag,
            Huisnummertoevoeging = toevoeging) |>
  distinct(vote_booth_id, GemeenteCode, .keep_all = TRUE)

# Once this has been done we can finally merge all databases.

missing_coordinates_patch <- missing_coordinates_vl_fixed |>
  transmute(vote_booth_id,
            GemeenteCode,
            X = X_location,
            Y = Y_location,
            Latitude = Latitude_location,
            Longitude = Longitude_location, 
            `BAG Nummeraanduiding ID` = `BAG Nummeraanduiding ID_location`,
            Straatnaam = Straatnaam_location,
            Huisnummer = Huisnummer_location, 
            Huisletter = Huisletter_location,
            Huisnummertoevoeging = Huisnummertoevoeging_location)

voting_unique_linked_complete <- voting_unique_linked |>
  rows_patch(missing_coordinates_patch,
             by = c("vote_booth_id", "GemeenteCode")) |>
  rows_patch(bag_patch,
             by = c("vote_booth_id", "GemeenteCode"))

# As a last step, given that the mobile stembureaus move through various postcodes, they will be excluded. 
# It is also a fact that not every stembureau has its route, so even if we opted for doing a centroid, we don't have the information to do this.
# There are also some stembureaus that are not part of the continental Netherlands or whose information was not found. 
# These will also be excluded. 

# There are a total of 73 Mobiel stemburaus and 18 rows of stembureaus that have no information
# or that are not part of the Continental Netherlands will be excluded

voting_unique_linked_complete_final <- voting_unique_linked_complete |>
  filter(!str_detect(StembureauNaam, regex("mobiel", ignore_case = TRUE)),
         !is.na(X))

write.csv(voting_unique_linked_complete_final,
          "voting_data_stembureaus_complete.csv")

# As a final step,and for possible future spatial analysis, we need to guarantee that the Postcode
# is the one linked to the XY coordinates. As such, we note the associated with the source used to locate or correct that stembureau:
# first BAG/external-address matches, then manual location_unique patches, then the original location-linkage postcode,
# then the corrected linkage postcode, and finally the original voting-data postcode. This makes the postcode information 
# more consistent with the X and Y coordinates used in the final spatial dataset. The original postcode variables are kept separately 
# so that changes from the original voting data can be checked and documented.

external_postcode_patch <- external_addresses_clean |>
  transmute(vote_booth_id,
            GemeenteCode,
            postcode_external_bag = postcode_clean)

manual_location_postcode_patch <- manual_pairs |>
  filter(!lookup_Postcode %in% c("MOBILE STEMBUREAU - EXCLUDE",
                                 "NO INFORMATION",
                                 "NO INFORMATION - EXCLUDE",
                                 "EXTERNAL ADDRESS"),
         !is.na(lookup_Postcode)) |>
  transmute(vote_booth_id,
            GemeenteCode,
            postcode_manual_location = lookup_Postcode)

voting_unique_linked_complete_final_geo <- voting_unique_linked_complete_final |>
  left_join(external_postcode_patch,
            by = c("vote_booth_id", "GemeenteCode")) |>
  left_join(manual_location_postcode_patch,
            by = c("vote_booth_id", "GemeenteCode")) |>
  mutate(postcode_spatial = coalesce(postcode_external_bag,
                                     postcode_manual_location,
                                     Postcode_location_original,
                                     Postcode_final,
                                     Postcode),
         postcode_spatial_source = case_when(!is.na(postcode_external_bag) ~ "external BAG address",
                                             !is.na(postcode_manual_location) ~ "manual location_unique patch",
                                             !is.na(Postcode_location_original) ~ "location linkage postcode",
                                             !is.na(Postcode_final) ~ "corrected linkage postcode",
                                             !is.na(Postcode) ~ "original voting postcode",
                                             TRUE ~ "missing"),
         postcode_original_vote = Postcode,
         postcode_linkage_final = Postcode_final,
         postcode_coordinate_lookup = Postcode_location_original,
         postcode_changed_from_original = !is.na(Postcode) & !is.na(postcode_spatial) & Postcode != postcode_spatial,
         postcode_missing_for_spatial = is.na(postcode_spatial))


write.csv(voting_unique_linked_complete_final_geo,
          "voting_data_stembureaus_complete_geo.csv")
