###################################################################################################
#                                   Loading the data
###################################################################################################

library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(gstat)

complete_geospatial_data <- read_csv("complete_geospatial_data.csv",
                                     col_select = -1,
                                     show_col_types = FALSE)

complete_geospatial_data <- complete_geospatial_data |>
  rename(links_rechts = `Links-Rechts`,
         progressief_conservatief = `Progressief-Conservatief`)



# For a later part of the analysis, we'll use the Links-Rechts, Progressief-Conservatief, and the eucledian distances between
# these parties for mapping purposes. 
party_coordinates <- complete_geospatial_data |>
  distinct(party_kieskompas,
           links_rechts,
           progressief_conservatief) |>
  arrange(party_kieskompas)

party_distance_matrix <- party_coordinates |>
  select(links_rechts,
         progressief_conservatief) |>
  dist(method = "euclidean") |>
  as.matrix()

rownames(party_distance_matrix) <- party_coordinates$party_kieskompas
colnames(party_distance_matrix) <- party_coordinates$party_kieskompas

party_distance_matrix

###################################################################################################
#                               Normalization constants 
###################################################################################################


# For the purposes of this thesis we'll normalize the values with the worst case scenario constat.
# In other words we'll use the case of when the most distant parties in each ideological scale have half of
# the total share of population.

alpha_value <- 1

dmax_links_rechts <- max(complete_geospatial_data$links_rechts, na.rm = TRUE) -
  min(complete_geospatial_data$links_rechts, na.rm = TRUE)

ER_max_links_rechts_raw <- 2*(.5^((1 + alpha_value)) *.5* dmax_links_rechts)

K_links_rechts <- 1 / ER_max_links_rechts_raw


# We do the same for the Progressief-Conservatief dimension

dmax_progressief_conservatief <- max(complete_geospatial_data$progressief_conservatief, na.rm = TRUE) -
  min(complete_geospatial_data$progressief_conservatief)

ER_max_progressief_conservatief_raw <- 2*(.5^((1 + alpha_value)) *.5* dmax_progressief_conservatief)

K_progressief_conservatief <- 1 / ER_max_progressief_conservatief_raw

# We do the same for the Eucledian distances

dmax_2d_euclidean <- max(party_distance_matrix, na.rm = TRUE)

ER_max_2d_euclidean_raw <- 2 * (.5^(1 + alpha_value) * .5 * dmax_2d_euclidean)

K_2d_euclidean <- 1 / ER_max_2d_euclidean_raw


#The eucledian value is purposefuly left with NA as they don't represent positions on ideological scales, but distances

normalization_constants <- tibble(axis = c("links_rechts",
                                           "progressief_conservatief",
                                           "2d_euclidean"),
                                  alpha = c(alpha_value,
                                            alpha_value,
                                            alpha_value),
                                  min_position = c(min(complete_geospatial_data$links_rechts, na.rm = TRUE),
                                                   min(complete_geospatial_data$progressief_conservatief, na.rm = TRUE),
                                                   NA),
                                  max_position = c(max(complete_geospatial_data$links_rechts, na.rm = TRUE),
                                                   max(complete_geospatial_data$progressief_conservatief, na.rm = TRUE),
                                                   NA),
                                  D_max = c(dmax_links_rechts,
                                            dmax_progressief_conservatief,
                                            dmax_2d_euclidean),
                                  ER_raw_max = c(ER_max_links_rechts_raw,
                                                 ER_max_progressief_conservatief_raw,
                                                 ER_max_2d_euclidean_raw),
                                  K_normalization_value = c(K_links_rechts,
                                                            K_progressief_conservatief,
                                                            K_2d_euclidean))

normalization_constants

###################################################################################################
#        Esteban-Ray index calculation for both ideological scales and Eucledian distances
###################################################################################################

# This ER index function will be able to work with the ideological scales only. We'll need to create a function
# exclusive to the Euclidean distances. It is important to mention that this function can ONLY be used if a grouping
# is done to the data later. Otherwise it will not differentiate between locations

# For normalization purposes, when calculating the ER index value we'll set K to 1/normalization constants that we
# already calculated in the previous section.

esteban_ray <- function(data, position_var = "links_rechts", alpha = 1, K = 1) {
  
  # We create the party shares
  
  p <- data$AantalStemmen / sum(data$AantalStemmen)
  
  # We extract the ideological scale values of the parties
  
  y <- data[[position_var]]
  
  # We now create a matrix with all the ideological distances
  
  ideological_distance_ij <- abs(outer(y,
                                       y,
                                       "-"))
  
  # Penalization component for party i
  
  p_i_identification <- p^(1 + alpha)
  
  # pairwise pi^(1+alpha)*pj
  
  population_weight_ij <- outer(p_i_identification,
                                p)
  
  # K* sum_i sum_j p_i^(1 + alpha)*p_j*|y_i - y_j|
  
  ER <- K * sum(population_weight_ij * ideological_distance_ij)
  
  return(ER)
}


# We create now the function for the Eucledian distances ER calculation

esteban_ray_euclidean <- function(data, lr_var = "links_rechts", pc_var = "progressief_conservatief", alpha = 1, K = 1) {
  
  # We create the party shares
  
  p <- data$AantalStemmen / sum(data$AantalStemmen)
  
  # We extract the ideological scale values of the parties
  
  links_rechts <- data[[lr_var]]
  progressief_conservatief <- data[[pc_var]]
  
  # We now create a matrix with all pairwise Links-Rechts distances
  
  links_rechts_distance_ij <- outer(links_rechts,
                                    links_rechts,
                                    "-")
  
  # We now create a matrix with all pairwise Progressief-Conservatief distances
  
  progressief_conservatief_distance_ij <- outer(progressief_conservatief,
                                                progressief_conservatief,
                                                "-")
  
  # We combine both ideological distances into one Euclidean distance matrix
  
  euclidean_distance_ij <- sqrt(links_rechts_distance_ij^2 +
                                  progressief_conservatief_distance_ij^2)
  
  # Penalization component for party i
  
  p_i_identification <- p^(1 + alpha)
  
  # Pairwise p_i^(1 + alpha) * p_j
  
  population_weight_ij <- outer(p_i_identification,
                                p)
  
  # K * sum_i sum_j p_i^(1 + alpha) * p_j * sqrt((LR_i - LR_j)^2 + (PC_i - PC_j)^2)
  
  ER <- K * sum(population_weight_ij * euclidean_distance_ij)
  
  return(ER)
}


###################################################################################################
# Calculating raw and normalized scores at the stembureau, gemeente, corop, province and nation level
###################################################################################################

stembureau_party_votes <- complete_geospatial_data |>
  group_by(analysis_location_id,
           StembureauNaam_combined,
           X,
           Y,
           GemeenteCode,
           GemeenteNaam,
           gemeente_name_spatial,
           corop_name,
           province_name,
           party_kieskompas) |>
  summarise(AantalStemmen = sum(AantalStemmen, na.rm = TRUE),
            links_rechts = first(links_rechts),
            progressief_conservatief = first(progressief_conservatief),
            .groups = "drop") |>
  group_by(analysis_location_id) |>
  mutate(total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE)) |>
  ungroup()


#   Calculating raw and normalized scores at the stembureau level

er_stembureau <- stembureau_party_votes |>
  group_by(analysis_location_id) |>
  summarise(StembureauNaam_combined = first(StembureauNaam_combined),
            X = first(X),
            Y = first(Y),
            GemeenteCode = first(GemeenteCode),
            GemeenteNaam = first(GemeenteNaam),
            gemeente_name_spatial = first(gemeente_name_spatial),
            corop_name = first(corop_name),
            province_name = first(province_name),
            total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE),
            ER_links_rechts_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                         links_rechts = links_rechts),
                                              position_var = "links_rechts",
                                              alpha = alpha_value,
                                              K = 1),
            ER_links_rechts_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                          links_rechts = links_rechts),
                                               position_var = "links_rechts",
                                               alpha = alpha_value,
                                               K = K_links_rechts),
            .groups = "drop")

# We now add the columns for the Progressief-Conservatief dimension

er_stembureau_pc <- stembureau_party_votes |>
  group_by(analysis_location_id) |>
  summarise(ER_progressief_conservatief_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                     progressief_conservatief = progressief_conservatief),
                                                          position_var = "progressief_conservatief",
                                                          alpha = alpha_value,
                                                          K = 1),
            ER_progressief_conservatief_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                      progressief_conservatief = progressief_conservatief),
                                                           position_var = "progressief_conservatief",
                                                           alpha = alpha_value,
                                                           K = K_progressief_conservatief),
            .groups = "drop")

er_stembureau <- er_stembureau |>
  left_join(er_stembureau_pc,
            by = "analysis_location_id")

# We now add the columns for the Euclidean 2D dimension

er_stembureau_euclidean <- stembureau_party_votes |>
  group_by(analysis_location_id) |>
  summarise(ER_2d_euclidean_raw = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                   links_rechts = links_rechts,
                                                                   progressief_conservatief = progressief_conservatief),
                                                        lr_var = "links_rechts",
                                                        pc_var = "progressief_conservatief",
                                                        alpha = alpha_value,
                                                        K = 1),
            ER_2d_euclidean_norm = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                    links_rechts = links_rechts,
                                                                    progressief_conservatief = progressief_conservatief),
                                                         lr_var = "links_rechts",
                                                         pc_var = "progressief_conservatief",
                                                         alpha = alpha_value,
                                                         K = K_2d_euclidean),
            .groups = "drop")

er_stembureau <- er_stembureau |>
  left_join(er_stembureau_euclidean,
            by = "analysis_location_id")

write.csv(er_stembureau,
          "er_stembureau.csv")

write.csv(stembureau_party_votes,
          "stembureau_party_votes.csv")

# We now calculate the table at the gemeente level. The first step is to create a table that adds up all the votes for 
# at the gemeente level as a total and per party 

gemeente_party_votes <- complete_geospatial_data |>
  group_by(GemeenteCode,
           GemeenteNaam,
           gemeente_name_spatial,
           corop_name,
           province_name,
           party_kieskompas) |>
  summarise(AantalStemmen = sum(AantalStemmen, na.rm = TRUE),
            links_rechts = first(links_rechts),
            progressief_conservatief = first(progressief_conservatief),
            n_stembureaus = n_distinct(analysis_location_id),
            .groups = "drop") |>
  group_by(GemeenteCode) |>
  mutate(total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE)) |>
  ungroup()

er_gemeente <- gemeente_party_votes |>
  group_by(GemeenteCode) |>
  summarise(GemeenteNaam = first(GemeenteNaam),
            gemeente_name_spatial = first(gemeente_name_spatial),
            corop_name = first(corop_name),
            province_name = first(province_name),
            n_stembureaus = first(n_stembureaus),
            total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE),
            ER_links_rechts_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                         links_rechts = links_rechts),
                                              position_var = "links_rechts",
                                              alpha = alpha_value,
                                              K = 1),
            ER_links_rechts_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                          links_rechts = links_rechts),
                                               position_var = "links_rechts",
                                               alpha = alpha_value,
                                               K = K_links_rechts),
            .groups = "drop")

# We repeat the previous procedure

er_gemeente_pc <- gemeente_party_votes |>
  group_by(GemeenteCode) |>
  summarise(ER_progressief_conservatief_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                     progressief_conservatief = progressief_conservatief),
                                                          position_var = "progressief_conservatief",
                                                          alpha = alpha_value,
                                                          K = 1),
            ER_progressief_conservatief_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                      progressief_conservatief = progressief_conservatief),
                                                           position_var = "progressief_conservatief",
                                                           alpha = alpha_value,
                                                           K = K_progressief_conservatief),
            .groups = "drop")

er_gemeente <- er_gemeente |>
  left_join(er_gemeente_pc,
            by = "GemeenteCode")

er_gemeente_euclidean <- gemeente_party_votes |>
  group_by(GemeenteCode) |>
  summarise(ER_2d_euclidean_raw = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                   links_rechts = links_rechts,
                                                                   progressief_conservatief = progressief_conservatief),
                                                        lr_var = "links_rechts",
                                                        pc_var = "progressief_conservatief",
                                                        alpha = alpha_value,
                                                        K = 1),
            ER_2d_euclidean_norm = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                    links_rechts = links_rechts,
                                                                    progressief_conservatief = progressief_conservatief),
                                                         lr_var = "links_rechts",
                                                         pc_var = "progressief_conservatief",
                                                         alpha = alpha_value,
                                                         K = K_2d_euclidean),
            .groups = "drop")

er_gemeente <- er_gemeente |>
  left_join(er_gemeente_euclidean,
            by = "GemeenteCode")

write.csv(er_gemeente,
          "er_gemeente.csv")

write.csv(gemeente_party_votes,
          "gemeente_party_votes.csv")


# We do the same, but now for the corop level. In the same fashion as with the previous steps, we first create
# a dataset with the summed up votes per corop region. 

corop_party_votes <- complete_geospatial_data |>
  group_by(corop_name,
           province_name,
           party_kieskompas) |>
  summarise(AantalStemmen = sum(AantalStemmen, na.rm = TRUE),
            links_rechts = first(links_rechts),
            progressief_conservatief = first(progressief_conservatief),
            n_gemeenten = n_distinct(GemeenteCode),
            n_stembureaus = n_distinct(analysis_location_id),
            .groups = "drop") |>
  group_by(corop_name) |>
  mutate(total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE)) |>
  ungroup()

# We repeat the procedure, but at the corop level

er_corop <- corop_party_votes |>
  group_by(corop_name) |>
  summarise(province_name = first(province_name),
            n_gemeenten = first(n_gemeenten),
            n_stembureaus = first(n_stembureaus),
            total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE),
            ER_links_rechts_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                         links_rechts = links_rechts),
                                              position_var = "links_rechts",
                                              alpha = alpha_value,
                                              K = 1),
            ER_links_rechts_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                          links_rechts = links_rechts),
                                               position_var = "links_rechts",
                                               alpha = alpha_value,
                                               K = K_links_rechts),
            .groups = "drop")

er_corop_pc <- corop_party_votes |>
  group_by(corop_name) |>
  summarise(ER_progressief_conservatief_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                     progressief_conservatief = progressief_conservatief),
                                                          position_var = "progressief_conservatief",
                                                          alpha = alpha_value,
                                                          K = 1),
            ER_progressief_conservatief_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                      progressief_conservatief = progressief_conservatief),
                                                           position_var = "progressief_conservatief",
                                                           alpha = alpha_value,
                                                           K = K_progressief_conservatief),
            .groups = "drop")

er_corop <- er_corop |>
  left_join(er_corop_pc,
            by = "corop_name")

er_corop_euclidean <- corop_party_votes |>
  group_by(corop_name) |>
  summarise(ER_2d_euclidean_raw = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                   links_rechts = links_rechts,
                                                                   progressief_conservatief = progressief_conservatief),
                                                        lr_var = "links_rechts",
                                                        pc_var = "progressief_conservatief",
                                                        alpha = alpha_value,
                                                        K = 1),
            ER_2d_euclidean_norm = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                    links_rechts = links_rechts,
                                                                    progressief_conservatief = progressief_conservatief),
                                                         lr_var = "links_rechts",
                                                         pc_var = "progressief_conservatief",
                                                         alpha = alpha_value,
                                                         K = K_2d_euclidean),
            .groups = "drop")

er_corop <- er_corop |>
  left_join(er_corop_euclidean,
            by = "corop_name")

write.csv(er_corop,
          "er_corop.csv")

write.csv(corop_party_votes,
          "corop_party_votes.csv")

# We repeat the procedure, now for province level ER calculation. Since the procedure is the same
# no further notes will be done.

province_party_votes <- complete_geospatial_data |>
  group_by(province_name,
           party_kieskompas) |>
  summarise(AantalStemmen = sum(AantalStemmen, na.rm = TRUE),
            links_rechts = first(links_rechts),
            progressief_conservatief = first(progressief_conservatief),
            n_corops = n_distinct(corop_name),
            n_gemeenten = n_distinct(GemeenteCode),
            n_stembureaus = n_distinct(analysis_location_id),
            .groups = "drop") |>
  group_by(province_name) |>
  mutate(total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE)) |>
  ungroup()


er_province <- province_party_votes |>
  group_by(province_name) |>
  summarise(n_corops = first(n_corops),
            n_gemeenten = first(n_gemeenten),
            n_stembureaus = first(n_stembureaus),
            total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE),
            ER_links_rechts_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                         links_rechts = links_rechts),
                                              position_var = "links_rechts",
                                              alpha = alpha_value,
                                              K = 1),
            ER_links_rechts_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                          links_rechts = links_rechts),
                                               position_var = "links_rechts",
                                               alpha = alpha_value,
                                               K = K_links_rechts),
            .groups = "drop")


er_province_pc <- province_party_votes |>
  group_by(province_name) |>
  summarise(ER_progressief_conservatief_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                     progressief_conservatief = progressief_conservatief),
                                                          position_var = "progressief_conservatief",
                                                          alpha = alpha_value,
                                                          K = 1),
            ER_progressief_conservatief_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                      progressief_conservatief = progressief_conservatief),
                                                           position_var = "progressief_conservatief",
                                                           alpha = alpha_value,
                                                           K = K_progressief_conservatief),
            .groups = "drop")

er_province <- er_province |>
  left_join(er_province_pc,
            by = "province_name")

er_province_euclidean <- province_party_votes |>
  group_by(province_name) |>
  summarise(ER_2d_euclidean_raw = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                   links_rechts = links_rechts,
                                                                   progressief_conservatief = progressief_conservatief),
                                                        lr_var = "links_rechts",
                                                        pc_var = "progressief_conservatief",
                                                        alpha = alpha_value,
                                                        K = 1),
            ER_2d_euclidean_norm = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                    links_rechts = links_rechts,
                                                                    progressief_conservatief = progressief_conservatief),
                                                         lr_var = "links_rechts",
                                                         pc_var = "progressief_conservatief",
                                                         alpha = alpha_value,
                                                         K = K_2d_euclidean),
            .groups = "drop")

er_province <- er_province |>
  left_join(er_province_euclidean,
            by = "province_name")



write.csv(er_province,
          "er_province.csv")

write.csv(province_party_votes,
          "province_party_votes.csv")

# We end this section by computing the nationwide polarization. 

national_party_votes <- complete_geospatial_data |>
  mutate(country_name = "Netherlands") |>
  group_by(country_name,
           party_kieskompas) |>
  summarise(AantalStemmen = sum(AantalStemmen, na.rm = TRUE),
            links_rechts = first(links_rechts),
            progressief_conservatief = first(progressief_conservatief),
            n_provinces = n_distinct(province_name),
            n_corops = n_distinct(corop_name),
            n_gemeenten = n_distinct(GemeenteCode),
            n_stembureaus = n_distinct(analysis_location_id),
            .groups = "drop") |>
  group_by(country_name) |>
  mutate(total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE)) |>
  ungroup()

er_national <- national_party_votes |>
  group_by(country_name) |>
  summarise(n_provinces = first(n_provinces),
            n_corops = first(n_corops),
            n_gemeenten = first(n_gemeenten),
            n_stembureaus = first(n_stembureaus),
            total_votes_kieskompas = sum(AantalStemmen, na.rm = TRUE),
            ER_links_rechts_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                         links_rechts = links_rechts),
                                              position_var = "links_rechts",
                                              alpha = alpha_value,
                                              K = 1),
            ER_links_rechts_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                          links_rechts = links_rechts),
                                               position_var = "links_rechts",
                                               alpha = alpha_value,
                                               K = K_links_rechts),
            .groups = "drop")

er_national_pc <- national_party_votes |>
  group_by(country_name) |>
  summarise(ER_progressief_conservatief_raw = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                     progressief_conservatief = progressief_conservatief),
                                                          position_var = "progressief_conservatief",
                                                          alpha = alpha_value,
                                                          K = 1),
            ER_progressief_conservatief_norm = esteban_ray(data.frame(AantalStemmen = AantalStemmen,
                                                                      progressief_conservatief = progressief_conservatief),
                                                           position_var = "progressief_conservatief",
                                                           alpha = alpha_value,
                                                           K = K_progressief_conservatief),
            .groups = "drop")

er_national <- er_national |>
  left_join(er_national_pc,
            by = "country_name")

er_national_euclidean <- national_party_votes |>
  group_by(country_name) |>
  summarise(ER_2d_euclidean_raw = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                   links_rechts = links_rechts,
                                                                   progressief_conservatief = progressief_conservatief),
                                                        lr_var = "links_rechts",
                                                        pc_var = "progressief_conservatief",
                                                        alpha = alpha_value,
                                                        K = 1),
            ER_2d_euclidean_norm = esteban_ray_euclidean(data.frame(AantalStemmen = AantalStemmen,
                                                                    links_rechts = links_rechts,
                                                                    progressief_conservatief = progressief_conservatief),
                                                         lr_var = "links_rechts",
                                                         pc_var = "progressief_conservatief",
                                                         alpha = alpha_value,
                                                         K = K_2d_euclidean),
            .groups = "drop")

er_national <- er_national |>
  left_join(er_national_euclidean,
            by = "country_name")


write.csv(er_national,
          "er_national.csv")

write.csv(national_party_votes,
          "national_party_votes.csv")


###################################################################################################
#                         Results tables for observed ER values
###################################################################################################

###################################################################################################
#                         Table 1: Normalization constants
###################################################################################################

normalization_table_results <- normalization_constants |>
  transmute(dimension = recode(axis,
                               links_rechts = "Links-Rechts",
                               progressief_conservatief = "Progressief-Conservatief",
                               `2d_euclidean` = "2D Euclidean"),
            alpha = alpha,
            minimum_axis_position = min_position,
            maximum_axis_position = max_position,
            maximum_pairwise_distance = D_max,
            theoretical_raw_ER_maximum = ER_raw_max,
            normalization_multiplier = K_normalization_value) |>
  mutate(across(where(is.numeric),
                ~ round(.x, 2)))

normalization_table_results

###################################################################################################
#                         Table 2: Raw and normalized ER summary by level
###################################################################################################

er_all_levels <- bind_rows(er_stembureau |>
                             transmute(level = "Stembureau",
                                       unit_name = StembureauNaam_combined,
                                       ER_links_rechts_raw = ER_links_rechts_raw,
                                       ER_links_rechts_norm = ER_links_rechts_norm,
                                       ER_progressief_conservatief_raw = ER_progressief_conservatief_raw,
                                       ER_progressief_conservatief_norm = ER_progressief_conservatief_norm,
                                       ER_2d_euclidean_raw = ER_2d_euclidean_raw,
                                       ER_2d_euclidean_norm = ER_2d_euclidean_norm),
                           er_gemeente |>
                             transmute(level = "Gemeente",
                                       unit_name = GemeenteNaam,
                                       ER_links_rechts_raw = ER_links_rechts_raw,
                                       ER_links_rechts_norm = ER_links_rechts_norm,
                                       ER_progressief_conservatief_raw = ER_progressief_conservatief_raw,
                                       ER_progressief_conservatief_norm = ER_progressief_conservatief_norm,
                                       ER_2d_euclidean_raw = ER_2d_euclidean_raw,
                                       ER_2d_euclidean_norm = ER_2d_euclidean_norm),
                           er_corop |>
                             transmute(level = "COROP",
                                       unit_name = corop_name,
                                       ER_links_rechts_raw = ER_links_rechts_raw,
                                       ER_links_rechts_norm = ER_links_rechts_norm,
                                       ER_progressief_conservatief_raw = ER_progressief_conservatief_raw,
                                       ER_progressief_conservatief_norm = ER_progressief_conservatief_norm,
                                       ER_2d_euclidean_raw = ER_2d_euclidean_raw,
                                       ER_2d_euclidean_norm = ER_2d_euclidean_norm),
                           er_province |>
                             transmute(level = "Province",
                                       unit_name = province_name,
                                       ER_links_rechts_raw = ER_links_rechts_raw,
                                       ER_links_rechts_norm = ER_links_rechts_norm,
                                       ER_progressief_conservatief_raw = ER_progressief_conservatief_raw,
                                       ER_progressief_conservatief_norm = ER_progressief_conservatief_norm,
                                       ER_2d_euclidean_raw = ER_2d_euclidean_raw,
                                       ER_2d_euclidean_norm = ER_2d_euclidean_norm),
                           er_national |>
                             transmute(level = "National",
                                       unit_name = country_name,
                                       ER_links_rechts_raw = ER_links_rechts_raw,
                                       ER_links_rechts_norm = ER_links_rechts_norm,
                                       ER_progressief_conservatief_raw = ER_progressief_conservatief_raw,
                                       ER_progressief_conservatief_norm = ER_progressief_conservatief_norm,
                                       ER_2d_euclidean_raw = ER_2d_euclidean_raw,
                                       ER_2d_euclidean_norm = ER_2d_euclidean_norm))

er_summary_table <- bind_rows(er_all_levels |>
                                transmute(level = level,
                                          dimension = "Links-Rechts",
                                          raw_ER = ER_links_rechts_raw,
                                          normalized_ER = ER_links_rechts_norm),
                              er_all_levels |>
                                transmute(level = level,
                                          dimension = "Progressief-Conservatief",
                                          raw_ER = ER_progressief_conservatief_raw,
                                          normalized_ER = ER_progressief_conservatief_norm),
                              er_all_levels |>
                                transmute(level = level,
                                          dimension = "2D Euclidean",
                                          raw_ER = ER_2d_euclidean_raw,
                                          normalized_ER = ER_2d_euclidean_norm)) |>
  group_by(level,
           dimension) |>
  summarise(n_units = n(),
            raw_min = min(raw_ER, na.rm = TRUE),
            raw_mean = mean(raw_ER, na.rm = TRUE),
            raw_max = max(raw_ER, na.rm = TRUE),
            norm_min = min(normalized_ER, na.rm = TRUE),
            norm_mean = mean(normalized_ER, na.rm = TRUE),
            norm_max = max(normalized_ER, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(where(is.numeric),
                ~ round(.x, 2)))

er_summary_table

###################################################################################################
#                         Table 3: National observed ER values
###################################################################################################

national_er_table <- er_national |>
  transmute(country_name = country_name,
            n_stembureaus = n_stembureaus,
            ER_links_rechts_raw = ER_links_rechts_raw,
            ER_links_rechts_norm = ER_links_rechts_norm,
            ER_progressief_conservatief_raw = ER_progressief_conservatief_raw,
            ER_progressief_conservatief_norm = ER_progressief_conservatief_norm,
            ER_2d_euclidean_raw = ER_2d_euclidean_raw,
            ER_2d_euclidean_norm = ER_2d_euclidean_norm) |>
  mutate(across(where(is.numeric),
                ~ round(.x, 2)))

national_er_table

###################################################################################################
#                         Table 4: Province observed ER values
###################################################################################################

province_er_table <- er_province |>
  select(province_name,
         n_stembureaus,
         ER_links_rechts_raw,
         ER_links_rechts_norm,
         ER_progressief_conservatief_raw,
         ER_progressief_conservatief_norm,
         ER_2d_euclidean_raw,
         ER_2d_euclidean_norm) |>
  arrange(desc(ER_2d_euclidean_norm)) |>
  mutate(across(where(is.numeric),
                ~ round(.x, 2)))

province_er_table

###################################################################################################
#                         Save results tables
###################################################################################################

write.csv(normalization_table_results,
          "normalization_table_results.csv")

write.csv(er_summary_table,
          "er_summary_table.csv")

write.csv(national_er_table,
          "national_er_table.csv")

write.csv(province_er_table,
          "province_er_table.csv")



###################################################################################################
#                         Table 5: Top polarized Links-Rechts
###################################################################################################

top_polarized_links_rechts <- bind_rows(er_province |>
                                          slice_max(order_by = ER_links_rechts_norm,
                                                    n = 1,
                                                    with_ties = FALSE) |>
                                          transmute(level = "Province",
                                                    unit_name = province_name,
                                                    normalized_ER = ER_links_rechts_norm),
                                        er_corop |>
                                          slice_max(order_by = ER_links_rechts_norm,
                                                    n = 1,
                                                    with_ties = FALSE) |>
                                          transmute(level = "COROP",
                                                    unit_name = corop_name,
                                                    normalized_ER = ER_links_rechts_norm),
                                        er_gemeente |>
                                          slice_max(order_by = ER_links_rechts_norm,
                                                    n = 1,
                                                    with_ties = FALSE) |>
                                          transmute(level = "Gemeente",
                                                    unit_name = GemeenteNaam,
                                                    normalized_ER = ER_links_rechts_norm),
                                        er_stembureau |>
                                          slice_max(order_by = ER_links_rechts_norm,
                                                    n = 1,
                                                    with_ties = FALSE) |>
                                          transmute(level = "Stembureau",
                                                    unit_name = StembureauNaam_combined,
                                                    normalized_ER = ER_links_rechts_norm)) |>
  mutate(normalized_ER = round(normalized_ER, 2))

top_polarized_links_rechts

###################################################################################################
#                         Table 6: Top polarized Progressive-Conservative
###################################################################################################

top_polarized_progressief_conservatief <- bind_rows(er_province |>
                                                      slice_max(order_by = ER_progressief_conservatief_norm,
                                                                n = 1,
                                                                with_ties = FALSE) |>
                                                      transmute(level = "Province",
                                                                unit_name = province_name,
                                                                normalized_ER = ER_progressief_conservatief_norm),
                                                    er_corop |>
                                                      slice_max(order_by = ER_progressief_conservatief_norm,
                                                                n = 1,
                                                                with_ties = FALSE) |>
                                                      transmute(level = "COROP",
                                                                unit_name = corop_name,
                                                                normalized_ER = ER_progressief_conservatief_norm),
                                                    er_gemeente |>
                                                      slice_max(order_by = ER_progressief_conservatief_norm,
                                                                n = 1,
                                                                with_ties = FALSE) |>
                                                      transmute(level = "Gemeente",
                                                                unit_name = GemeenteNaam,
                                                                normalized_ER = ER_progressief_conservatief_norm),
                                                    er_stembureau |>
                                                      slice_max(order_by = ER_progressief_conservatief_norm,
                                                                n = 1,
                                                                with_ties = FALSE) |>
                                                      transmute(level = "Stembureau",
                                                                unit_name = StembureauNaam_combined,
                                                                normalized_ER = ER_progressief_conservatief_norm)) |>
  mutate(normalized_ER = round(normalized_ER, 2))

top_polarized_progressief_conservatief

###################################################################################################
#                         Table 7: Top polarized 2D Euclidean
###################################################################################################

top_polarized_euclidean <- bind_rows(er_province |>
                                       slice_max(order_by = ER_2d_euclidean_norm,
                                                 n = 1,
                                                 with_ties = FALSE) |>
                                       transmute(level = "Province",
                                                 unit_name = province_name,
                                                 normalized_ER = ER_2d_euclidean_norm),
                                     er_corop |>
                                       slice_max(order_by = ER_2d_euclidean_norm,
                                                 n = 1,
                                                 with_ties = FALSE) |>
                                       transmute(level = "COROP",
                                                 unit_name = corop_name,
                                                 normalized_ER = ER_2d_euclidean_norm),
                                     er_gemeente |>
                                       slice_max(order_by = ER_2d_euclidean_norm,
                                                 n = 1,
                                                 with_ties = FALSE) |>
                                       transmute(level = "Gemeente",
                                                 unit_name = GemeenteNaam,
                                                 normalized_ER = ER_2d_euclidean_norm),
                                     er_stembureau |>
                                       slice_max(order_by = ER_2d_euclidean_norm,
                                                 n = 1,
                                                 with_ties = FALSE) |>
                                       transmute(level = "Stembureau",
                                                 unit_name = StembureauNaam_combined,
                                                 normalized_ER = ER_2d_euclidean_norm)) |>
  mutate(normalized_ER = round(normalized_ER, 2))

top_polarized_euclidean

###################################################################################################
#                         Table 8: Least polarized Links-Rechts
###################################################################################################

least_polarized_links_rechts <- bind_rows(er_province |>
                                            slice_min(order_by = ER_links_rechts_norm,
                                                      n = 1,
                                                      with_ties = FALSE) |>
                                            transmute(level = "Province",
                                                      unit_name = province_name,
                                                      normalized_ER = ER_links_rechts_norm),
                                          er_corop |>
                                            slice_min(order_by = ER_links_rechts_norm,
                                                      n = 1,
                                                      with_ties = FALSE) |>
                                            transmute(level = "COROP",
                                                      unit_name = corop_name,
                                                      normalized_ER = ER_links_rechts_norm),
                                          er_gemeente |>
                                            slice_min(order_by = ER_links_rechts_norm,
                                                      n = 1,
                                                      with_ties = FALSE) |>
                                            transmute(level = "Gemeente",
                                                      unit_name = GemeenteNaam,
                                                      normalized_ER = ER_links_rechts_norm),
                                          er_stembureau |>
                                            slice_min(order_by = ER_links_rechts_norm,
                                                      n = 1,
                                                      with_ties = FALSE) |>
                                            transmute(level = "Stembureau",
                                                      unit_name = StembureauNaam_combined,
                                                      normalized_ER = ER_links_rechts_norm)) |>
  mutate(normalized_ER = round(normalized_ER, 2))

least_polarized_links_rechts

###################################################################################################
#                         Table 9: Least polarized Progressive-Conservative
###################################################################################################

least_polarized_progressief_conservatief <- bind_rows(er_province |>
                                                        slice_min(order_by = ER_progressief_conservatief_norm,
                                                                  n = 1,
                                                                  with_ties = FALSE) |>
                                                        transmute(level = "Province",
                                                                  unit_name = province_name,
                                                                  normalized_ER = ER_progressief_conservatief_norm),
                                                      er_corop |>
                                                        slice_min(order_by = ER_progressief_conservatief_norm,
                                                                  n = 1,
                                                                  with_ties = FALSE) |>
                                                        transmute(level = "COROP",
                                                                  unit_name = corop_name,
                                                                  normalized_ER = ER_progressief_conservatief_norm),
                                                      er_gemeente |>
                                                        slice_min(order_by = ER_progressief_conservatief_norm,
                                                                  n = 1,
                                                                  with_ties = FALSE) |>
                                                        transmute(level = "Gemeente",
                                                                  unit_name = GemeenteNaam,
                                                                  normalized_ER = ER_progressief_conservatief_norm),
                                                      er_stembureau |>
                                                        slice_min(order_by = ER_progressief_conservatief_norm,
                                                                  n = 1,
                                                                  with_ties = FALSE) |>
                                                        transmute(level = "Stembureau",
                                                                  unit_name = StembureauNaam_combined,
                                                                  normalized_ER = ER_progressief_conservatief_norm)) |>
  mutate(normalized_ER = round(normalized_ER, 2))

least_polarized_progressief_conservatief

###################################################################################################
#                         Table 10: Least polarized 2D Euclidean
###################################################################################################

least_polarized_euclidean <- bind_rows(er_province |>
                                         slice_min(order_by = ER_2d_euclidean_norm,
                                                   n = 1,
                                                   with_ties = FALSE) |>
                                         transmute(level = "Province",
                                                   unit_name = province_name,
                                                   normalized_ER = ER_2d_euclidean_norm),
                                       er_corop |>
                                         slice_min(order_by = ER_2d_euclidean_norm,
                                                   n = 1,
                                                   with_ties = FALSE) |>
                                         transmute(level = "COROP",
                                                   unit_name = corop_name,
                                                   normalized_ER = ER_2d_euclidean_norm),
                                       er_gemeente |>
                                         slice_min(order_by = ER_2d_euclidean_norm,
                                                   n = 1,
                                                   with_ties = FALSE) |>
                                         transmute(level = "Gemeente",
                                                   unit_name = GemeenteNaam,
                                                   normalized_ER = ER_2d_euclidean_norm),
                                       er_stembureau |>
                                         slice_min(order_by = ER_2d_euclidean_norm,
                                                   n = 1,
                                                   with_ties = FALSE) |>
                                         transmute(level = "Stembureau",
                                                   unit_name = StembureauNaam_combined,
                                                   normalized_ER = ER_2d_euclidean_norm)) |>
  mutate(normalized_ER = round(normalized_ER, 2))

least_polarized_euclidean

write.csv(top_polarized_links_rechts,
          "top_polarized_links_rechts.csv")

write.csv(top_polarized_progressief_conservatief,
          "top_polarized_progressief_conservatief.csv")

write.csv(top_polarized_euclidean,
          "top_polarized_euclidean.csv")

write.csv(least_polarized_links_rechts,
          "least_polarized_links_rechts.csv")

write.csv(least_polarized_progressief_conservatief,
          "least_polarized_progressief_conservatief.csv")

write.csv(least_polarized_euclidean,
          "least_polarized_euclidean.csv")
