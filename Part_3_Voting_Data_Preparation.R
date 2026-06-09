###################################################################################################
#                                   Loading the data
###################################################################################################

library(readr)
library(dplyr)
library(readxl)

Kieskompas_partijcoordinaten_2025 <- read_excel("Kieskompas partijcoordinaten 2025(1).xlsx", 
                                                   skip = 2)

Kieskompas_partijcoordinaten_2025 <- Kieskompas_partijcoordinaten_2025 |>
  rename("party name" = "...1")

voting_data_linked <- read_csv("voting_data_stembureaus_complete_geo.csv",
                               col_select = -1)

voting_data <- read_csv("voting_data_with_id.csv",
                        col_select = -1)



###################################################################################################
#                                  Preparing the data
###################################################################################################

voting_data_in_linked <- voting_data |>
  semi_join(voting_data_linked |>
              distinct(vote_booth_id, GemeenteCode),
            by = c("vote_booth_id", "GemeenteCode"))





# To confirm if the mobiel and Caribbean NL stembureaus, plus the ones without informationhave been excluded we run the following code

# We should have a total of 90

voting_data |>
  anti_join(voting_data_linked |>
              distinct(vote_booth_id),
            by = "vote_booth_id") |>
  summarise(n_rows = n(),
            n_stembureaus = n_distinct(vote_booth_id))

# This confirms we have 90 stembureaus that will not be included.

# In case they are needed we save them in a separate database

excluded_from_linked <- voting_data |>
  anti_join(voting_data_linked |>
              distinct(vote_booth_id, GemeenteCode),
            by = c("vote_booth_id", "GemeenteCode"))


cols_to_add <- setdiff(names(voting_data_linked),
                       names(voting_data))

voting_data_with_locations <- voting_data_in_linked |>
  left_join(voting_data_linked |>
              select(vote_booth_id,
                     GemeenteCode,
                     all_of(cols_to_add)),
            by = c("vote_booth_id", "GemeenteCode"),
            relationship = "many-to-one")


metadata_cols <- setdiff(names(voting_data_linked),
                         c("vote_booth_id",
                           "PartijNaam",
                           "AantalStemmen"))

voting_data_linked_sum <- voting_data_with_locations |>
  group_by(vote_booth_id, 
           PartijNaam) |>
  summarise(AantalStemmen = sum(AantalStemmen,
                                na.rm = TRUE),
            across(all_of(metadata_cols),
                   first),
            .groups = "drop")

###################################################################################################
#                  Summing booths that share the same physical location
###################################################################################################

# We now join the different datasets. Some parties appear more than once in some voting booths
# as such it is important to add up those instances. Furthermore, some stembureaus exist only because
# the booth itself had no more space so they brought in a new booth, but this booth is still part of that location.
# As such, we will sum the results of voting booths that are in the same location. We will recognize this patterns by 
# voting booths that share the exact same coordinates and GemeenteCode

physical_location_cols <- c("GemeenteCode",
                            "postcode_spatial",
                            "X",
                            "Y")

same_location_booths <- voting_data_linked_sum |>
  distinct(vote_booth_id,
           GemeenteCode,
           postcode_spatial,
           X,
           Y,
           StembureauNaam) |>
  group_by(across(all_of(physical_location_cols))) |>
  filter(n_distinct(vote_booth_id) > 1) |>
  ungroup() |>
  arrange(GemeenteCode,
          postcode_spatial,
          X,
          Y,
          vote_booth_id)

booths_to_sum <- same_location_booths |>
  distinct(vote_booth_id)

voting_booths_to_sum <- voting_data_linked_sum |>
  semi_join(booths_to_sum,
            by = "vote_booth_id")

voting_booths_not_to_sum <- voting_data_linked_sum |>
  anti_join(booths_to_sum,
            by = "vote_booth_id")

metadata_cols_location <- setdiff(names(voting_data_linked_sum),
                                  c("vote_booth_id",
                                    "PartijNaam",
                                    "AantalStemmen",
                                    physical_location_cols,
                                    "StembureauNaam"))

voting_booths_to_sum_summed <- voting_booths_to_sum |>
  group_by(across(all_of(physical_location_cols)),
           PartijNaam) |>
  summarise(AantalStemmen = sum(AantalStemmen,
                                na.rm = TRUE),
            vote_booth_ids_combined = paste(sort(unique(vote_booth_id)),
                                            collapse = "; "),
            n_booths_combined = n_distinct(vote_booth_id),
            location_is_combined = TRUE,
            StembureauNaam_combined = paste(sort(unique(StembureauNaam)),
                                            collapse = "; "),
            vote_booth_id = NA_real_,
            across(all_of(metadata_cols_location),
                   first),
            .groups = "drop")

voting_booths_not_to_sum_ready <- voting_booths_not_to_sum |>
  mutate(vote_booth_ids_combined = as.character(vote_booth_id),
         n_booths_combined = 1L,
         location_is_combined = FALSE,
         StembureauNaam_combined = StembureauNaam)

voting_data_location_sum_pre <- bind_rows(voting_booths_not_to_sum_ready,
                                          voting_booths_to_sum_summed)

location_id_lookup <- voting_data_location_sum_pre |>
  distinct(GemeenteCode,
           postcode_spatial,
           X,
           Y) |>
  arrange(GemeenteCode,
          postcode_spatial,
          X,
          Y) |>
  mutate(analysis_location_id = row_number())

voting_data_location_sum <- voting_data_location_sum_pre |>
  left_join(location_id_lookup,
            by = c("GemeenteCode",
                   "postcode_spatial",
                   "X",
                   "Y"))

write.csv(voting_data_location_sum,
          "voting_data_location_sum.csv")
length(unique(voting_data_location_sum$StembureauNaam_combined))

# We now check how many stembureaus we have at each of the steps we've taken so far


stembureau_location_checks <- tibble(check = c("Original unique stembureaus in voting_data",
                                               "Unique stembureaus retained after linkage",
                                               "Unique stembureaus excluded before analysis",
                                               "Unique stembureaus involved in physical-location combining",
                                               "Final physical locations that are combined locations",
                                               "Final physical locations that are not combined locations",
                                               "Final unique physical locations ready for analysis",
                                               "Reduction in number of units caused by combining"),
                                     value = c(n_distinct(voting_data$vote_booth_id),
                                               n_distinct(voting_data_in_linked$vote_booth_id),
                                               n_distinct(excluded_from_linked$vote_booth_id),
                                               n_distinct(booths_to_sum$vote_booth_id),
                                               voting_data_location_sum |>
                                                 filter(location_is_combined == TRUE) |>
                                                 distinct(analysis_location_id) |>
                                                 nrow(),
                                               voting_data_location_sum |>
                                                 filter(location_is_combined == FALSE) |>
                                                 distinct(analysis_location_id) |>
                                                 nrow(),
                                               n_distinct(voting_data_location_sum$analysis_location_id),
                                               n_distinct(voting_data_in_linked$vote_booth_id) - n_distinct(voting_data_location_sum$analysis_location_id)))

stembureau_location_checks

# We are left with 9164 unique stembureaus that are in different locations.



###################################################################################################
#                             Linking Party coordinates to voting data
###################################################################################################


voting_data_location_sum <- read_csv("voting_data_location_sum.csv",
                                     col_select = -1)


voting_data_location_sum <- voting_data_location_sum |>
  mutate(party_kieskompas = case_when(PartijNaam == "PVV (Partij voor de Vrijheid)" ~ "pvv",
                                      PartijNaam == "50PLUS" ~ "50plus",
                                      PartijNaam == "Volt" ~ "volt",
                                      PartijNaam == "VVD" ~ "vvd",
                                      PartijNaam == "SP (Socialistische Partij)" ~ "sp",
                                      PartijNaam == "Partij voor de Dieren" ~ "pvdd",
                                      PartijNaam == "Nieuw Sociaal Contract (NSC)" ~ "nsc",
                                      PartijNaam == "Staatkundig Gereformeerde Partij (SGP)" ~ "sgp",
                                      PartijNaam == "GROENLINKS / Partij van de Arbeid (PvdA)" ~ "pvdagl",
                                      PartijNaam == "JA21" ~ "ja21",
                                      PartijNaam == "Forum voor Democratie" ~ "fvd",
                                      PartijNaam == "D66" ~ "d66",
                                      PartijNaam == "DENK" ~ "denk",
                                      PartijNaam == "ChristenUnie" ~ "cu",
                                      PartijNaam == "CDA" ~ "cda",
                                      PartijNaam == "BBB" ~ "bbb",
                                      TRUE ~ NA_character_))

# This adds the ideological coordinates Links-Rechts and Progressief-Conservatief

voting_data_location_sum <- voting_data_location_sum |>
  left_join(Kieskompas_partijcoordinaten_2025,
            by = c("party_kieskompas" = "party name"))

# We store the excluded parties in case they are necessary for a later analysis.

excluded_parties <- voting_data_location_sum |>
  filter(is.na(party_kieskompas) |
           is.na(`Links-Rechts`) |
           is.na(`Progressief-Conservatief`)) |>
  distinct(PartijNaam,
           party_kieskompas) |>
  arrange(PartijNaam)

###################################################################################################
#                                 Visual check for the databases having added up values 
###################################################################################################


# These are the columns that define the same physical location.
# If multiple vote_booth_id values share these exact values, they were fused.

physical_location_cols <- c("GemeenteCode",
                            "postcode_spatial",
                            "X",
                            "Y")


# The following section is optional and inteded only to have a more detailed view of how the data looked before and after
# combining them after equal locations were detected and to confirm if the votes were summed accordingly.

###################################################################################################
#                  Part 1 of Check: Extract the individual booths BEFORE they were fused
###################################################################################################

# We need to find the booths that had the same physical location.


same_location_booths_before <- voting_data_linked_sum |>
  distinct(vote_booth_id,
           GemeenteCode,
           postcode_spatial,
           X,
           Y,
           StembureauNaam) |>
  group_by(across(all_of(physical_location_cols))) |>
  filter(n_distinct(vote_booth_id) > 1) |>
  ungroup() |>
  arrange(GemeenteCode,
          postcode_spatial,
          X,
          Y,
          vote_booth_id)


same_location_booths_before
###################################################################################################
#                 Part 2 of Check: Extract the voting rows for those booths BEFORE fusion
###################################################################################################

# This shows the party-level vote rows for the booths that were later fused.

same_location_votes_before <- voting_data_linked_sum |>
  semi_join(same_location_booths_before |>
              distinct(vote_booth_id),
            by = "vote_booth_id") |>
  select(vote_booth_id,
         GemeenteCode,
         postcode_spatial,
         X,
         Y,
         StembureauNaam,
         PartijNaam,
         AantalStemmen) |>
  arrange(GemeenteCode,
          postcode_spatial,
          X,
          Y,
          PartijNaam,
          vote_booth_id)

same_location_votes_before

###################################################################################################
#                   Part 3 of Check. Extract the fused rows AFTER summation
###################################################################################################

# This shows the final fused locations.
# These are the rows where several original vote_booth_id values were combined.

same_location_votes_after <- voting_data_location_sum |>
  filter(location_is_combined == TRUE) |>
  select(analysis_location_id,
         vote_booth_id,
         vote_booth_ids_combined,
         n_booths_combined,
         location_is_combined,
         GemeenteCode,
         postcode_spatial,
         X,
         Y,
         StembureauNaam_combined,
         PartijNaam,
         AantalStemmen) |>
  arrange(GemeenteCode,
          postcode_spatial,
          X,
          Y,
          PartijNaam)

same_location_votes_after

# Now that we have confirmed that the databases have worked
# We Create the final dataset for ideological/polarisation analysis.
# This keeps only parties that have a Kieskompas code and both ideological coordinates.

voting_data_kieskompas <- voting_data_location_sum |>
  filter(!is.na(party_kieskompas),
         !is.na(`Links-Rechts`),
         !is.na(`Progressief-Conservatief`))


# Check how many party rows each final physical voting location has.
# Since the final unit is now the physical location, use analysis_location_id,
# not vote_booth_id.

rows_per_location <- voting_data_kieskompas |>
  count(analysis_location_id,
        name = "n_rows") |>
  arrange(desc(n_rows))


write.csv(voting_data_kieskompas,
          "voting_data_kieskompas.csv")

voting_data_kieskompas <- read_csv("voting_data_kieskompas.csv",
                                   col_select = -1)

voting_data_kieskompas_analysis <- voting_data_kieskompas |>
  select(analysis_location_id,
         GemeenteCode,
         GemeenteNaam,
         postcode_spatial,
         X,
         Y,
         `BAG Nummeraanduiding ID`,
         Straatnaam,
         Huisnummer,
         Huisletter,
         Huisnummertoevoeging,
         StembureauNaam_combined,
         vote_booth_ids_combined,
         location_is_combined,
         party_kieskompas,
         AantalStemmen,
         `Links-Rechts`,
         `Progressief-Conservatief`)

# We prepare the dataset for unification with the COROP, province and gemeente columns.

voting_data_kieskompas_analysis_sf <- voting_data_kieskompas_analysis |>
  st_as_sf(coords = c("X", "Y"),
           crs = 28992,
           remove = FALSE)

# We load the dataset containing all the information. Presently, this works only to bring over the 
# tags. We will need to reload each of these layers when we create the maps to overimpose the borders 
# The joining occurs by detecting where the XY coordinates fall within the boundaries of each of these divisions
# For drawing the borders over the maps we'll make later, we will need to reload these corop, province and gemeente objects.

gebied_file <- "cbsgebiedsindelingen2025.gpkg"

corop <- st_read("cbsgebiedsindelingen2025.gpkg",
                 layer = "coropgebied_gegeneraliseerd",
                 quiet = TRUE) |>
  select(corop_code = statcode,
         corop_name = statnaam)

province <- st_read("cbsgebiedsindelingen2025.gpkg",
                    layer = "provincie_gegeneraliseerd",
                    quiet = TRUE) |>
  select(province_code = statcode,
         province_name = statnaam)

gemeente <- st_read("cbsgebiedsindelingen2025.gpkg",
                    layer = "gemeente_niet_gegeneraliseerd",
                    quiet = TRUE) |>
  select(gemeente_code_spatial = statcode,
         gemeente_name_spatial = statnaam)

voting_data_kieskompas_analysis_admin_sf <- voting_data_kieskompas_analysis_sf |>
  st_join(corop,
          join = st_within) |>
  st_join(province,
          join = st_within) |>
  st_join(gemeente,
          join = st_within)

complete_geospatial_data <- voting_data_kieskompas_analysis_admin_sf |>
  st_drop_geometry()

write.csv(complete_geospatial_data,
          "complete_geospatial_data.csv")

###################################################################################################
#                  Save final geospatial voting dataset as GeoPackage
###################################################################################################

# This saves the final stembureau-party geospatial dataset.
# Each row represents one party at one final physical voting location.
# The object includes address information, XY coordinates, Kieskompas ideological coordinates,
# and the administrative tags for COROP, province, and gemeente.

st_write(voting_data_kieskompas_analysis_admin_sf,
         "voting_data_kieskompas_analysis_admin.gpkg",
         delete_dsn = TRUE)

