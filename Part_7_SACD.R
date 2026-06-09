###################################################################################################
#                  Part 7 SACD: Steps 1–6
#                  Prepare grid-cell party-share compositions
###################################################################################################

###################################################################################################
#                  1. Load packages and data
###################################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(compositions)
library(ggplot2)
library(gstat)
library(spdep)
library(spatstat.geom)
library(spatstat.explore)

set.seed(2698218)

voting_data_kieskompas <- read_csv("voting_data_kieskompas.csv",
                                   show_col_types = FALSE) |>
  select(-any_of("...1"))

cbs_500m <- st_read("cbs_vk500_2024_v1.gpkg",
                    quiet = TRUE)

cbs_100m <- st_read("cbs_vk100_2024_v1.gpkg",
                    quiet = TRUE)

dir.create("sacd_outputs",
           showWarnings = FALSE)

# Since the procedures to produce the interpolated maps rely on almost the same procedures, no furhter notes
# will me made. New assumptions created here will have notes.

###################################################################################################
#                  2. Keep only populated CBS grid cells
###################################################################################################

cbs_500m_populated <- cbs_500m |>
  mutate(aantal_inwoners_numeric = suppressWarnings(as.numeric(aantal_inwoners))) |>
  filter(!is.na(aantal_inwoners_numeric),
         aantal_inwoners_numeric > 0)

cbs_100m_populated <- cbs_100m |>
  mutate(aantal_inwoners_numeric = suppressWarnings(as.numeric(aantal_inwoners))) |>
  filter(!is.na(aantal_inwoners_numeric),
         aantal_inwoners_numeric > 0)

###################################################################################################
#                  3. Assign voting rows to 500m and 100m grid cells
###################################################################################################

voting_data_kieskompas_sf <- voting_data_kieskompas |>
  st_as_sf(coords = c("X",
                      "Y"),
           crs = 28992,
           remove = FALSE)

voting_grid_500m <- voting_data_kieskompas_sf |>
  st_join(cbs_500m_populated[, "crs28992res500m"],
          join = st_within)

voting_grid_100m <- voting_data_kieskompas_sf |>
  st_join(cbs_100m_populated[, "crs28992res100m"],
          join = st_within)

###################################################################################################
#                  4. Aggregate party vote counts per observed grid cell
###################################################################################################

party_columns <- voting_data_kieskompas |>
  distinct(PartijNaam) |>
  arrange(PartijNaam) |>
  pull(PartijNaam)

vote_counts_wide_500m <- voting_grid_500m |>
  st_drop_geometry() |>
  filter(!is.na(crs28992res500m)) |>
  mutate(PartijNaam = factor(PartijNaam,
                             levels = party_columns)) |>
  group_by(crs28992res500m,
           PartijNaam) |>
  summarise(AantalStemmen = sum(AantalStemmen,
                                na.rm = TRUE),
            .groups = "drop") |>
  pivot_wider(names_from = PartijNaam,
              values_from = AantalStemmen,
              values_fill = 0,
              names_expand = TRUE)

vote_counts_wide_100m <- voting_grid_100m |>
  st_drop_geometry() |>
  filter(!is.na(crs28992res100m)) |>
  mutate(PartijNaam = factor(PartijNaam,
                             levels = party_columns)) |>
  group_by(crs28992res100m,
           PartijNaam) |>
  summarise(AantalStemmen = sum(AantalStemmen,
                                na.rm = TRUE),
            .groups = "drop") |>
  pivot_wider(names_from = PartijNaam,
              values_from = AantalStemmen,
              values_fill = 0,
              names_expand = TRUE)

###################################################################################################
#                  5. Convert vote counts to party-share compositions
###################################################################################################

vote_composition_shares_500m <- vote_counts_wide_500m |>
  mutate(total_votes = rowSums(across(all_of(party_columns)))) |>
  filter(total_votes > 0) |>
  mutate(across(all_of(party_columns),
                ~ .x / total_votes)) |>
  select(crs28992res500m,
         total_votes,
         all_of(party_columns))

vote_composition_shares_100m <- vote_counts_wide_100m |>
  mutate(total_votes = rowSums(across(all_of(party_columns)))) |>
  filter(total_votes > 0) |>
  mutate(across(all_of(party_columns),
                ~ .x / total_votes)) |>
  select(crs28992res100m,
         total_votes,
         all_of(party_columns))

###################################################################################################
#                  6. Replace zero party shares after grid aggregation
###################################################################################################

# We assign a value of half a vote for every party that has no votes registered. 
# 0.5 will be multiplied by the number of parties that have no votes. That quantity will then be 
# be divided by the amount of parties that do have votes and substracted from them

delta_count <- 0.5

vote_composition_replaced_500m <- vote_composition_shares_500m |>
  rowwise() |>
  mutate(n_zero_parts = sum(c_across(all_of(party_columns)) == 0),
         delta_share = delta_count / total_votes,
         total_delta = n_zero_parts * delta_share,
         across(all_of(party_columns),
                ~ ifelse(.x == 0,
                         delta_share,
                         .x * (1 - total_delta)))) |>
  ungroup() |>
  select(crs28992res500m,
         total_votes,
         all_of(party_columns))

vote_composition_replaced_100m <- vote_composition_shares_100m |>
  rowwise() |>
  mutate(n_zero_parts = sum(c_across(all_of(party_columns)) == 0),
         delta_share = delta_count / total_votes,
         total_delta = n_zero_parts * delta_share,
         across(all_of(party_columns),
                ~ ifelse(.x == 0,
                         delta_share,
                         .x * (1 - total_delta)))) |>
  ungroup() |>
  select(crs28992res100m,
         total_votes,
         all_of(party_columns))

###################################################################################################
#                  Check that replaced compositions still sum to 1
###################################################################################################

composition_sum_check_500m <- vote_composition_replaced_500m |>
  mutate(composition_sum = rowSums(across(all_of(party_columns)))) |>
  summarise(min_sum = min(composition_sum, na.rm = TRUE),
            max_sum = max(composition_sum, na.rm = TRUE),
            mean_sum = mean(composition_sum, na.rm = TRUE))

composition_sum_check_100m <- vote_composition_replaced_100m |>
  mutate(composition_sum = rowSums(across(all_of(party_columns)))) |>
  summarise(min_sum = min(composition_sum, na.rm = TRUE),
            max_sum = max(composition_sum, na.rm = TRUE),
            mean_sum = mean(composition_sum, na.rm = TRUE))

composition_sum_check_500m
composition_sum_check_100m

###################################################################################################
#                  Save checkpoint outputs for Steps 1–6
###################################################################################################

# This part is necessary if the user want to review these objects without running all the previous code.


saveRDS(voting_data_kieskompas,
        "sacd_outputs/voting_data_kieskompas.rds")

saveRDS(cbs_500m_populated,
        "sacd_outputs/cbs_500m_populated.rds")

saveRDS(cbs_100m_populated,
        "sacd_outputs/cbs_100m_populated.rds")

saveRDS(vote_counts_wide_500m,
        "sacd_outputs/vote_counts_wide_500m.rds")

saveRDS(vote_counts_wide_100m,
        "sacd_outputs/vote_counts_wide_100m.rds")

saveRDS(vote_composition_shares_500m,
        "sacd_outputs/vote_composition_shares_500m.rds")

saveRDS(vote_composition_shares_100m,
        "sacd_outputs/vote_composition_shares_100m.rds")

saveRDS(vote_composition_replaced_500m,
        "sacd_outputs/vote_composition_replaced_500m.rds")

saveRDS(vote_composition_replaced_100m,
        "sacd_outputs/vote_composition_replaced_100m.rds")

saveRDS(party_columns,
        "sacd_outputs/party_columns.rds")

write_csv(composition_sum_check_500m,
          "sacd_outputs/composition_sum_check_500m.csv")

write_csv(composition_sum_check_100m,
          "sacd_outputs/composition_sum_check_100m.csv")


###################################################################################################
#                  Part 7 SACD: Steps 7–12
#                  ilr transformation, grid-cell centroids, and spatial diagnostics
###################################################################################################

###################################################################################################
#                  7. Transform grid-cell compositions to ilr coordinates
###################################################################################################

vote_composition_matrix_500m <- vote_composition_replaced_500m |>
  select(all_of(party_columns)) |>
  as.matrix()

vote_composition_acomp_500m <- compositions::acomp(vote_composition_matrix_500m)

vote_composition_ilr_500m <- compositions::ilr(vote_composition_acomp_500m) |>
  as.data.frame()

names(vote_composition_ilr_500m) <- paste0("ilr_",
                                           seq_len(ncol(vote_composition_ilr_500m)))

vote_composition_ilr_500m <- bind_cols(vote_composition_replaced_500m |>
                                         select(crs28992res500m,
                                                total_votes),
                                       vote_composition_ilr_500m)

vote_composition_matrix_100m <- vote_composition_replaced_100m |>
  select(all_of(party_columns)) |>
  as.matrix()

vote_composition_acomp_100m <- compositions::acomp(vote_composition_matrix_100m)

vote_composition_ilr_100m <- compositions::ilr(vote_composition_acomp_100m) |>
  as.data.frame()

names(vote_composition_ilr_100m) <- paste0("ilr_",
                                           seq_len(ncol(vote_composition_ilr_100m)))

vote_composition_ilr_100m <- bind_cols(vote_composition_replaced_100m |>
                                         select(crs28992res100m,
                                                total_votes),
                                       vote_composition_ilr_100m)

ilr_columns_500m <- names(vote_composition_ilr_500m)[grepl("^ilr_",
                                                           names(vote_composition_ilr_500m))]

ilr_columns_100m <- names(vote_composition_ilr_100m)[grepl("^ilr_",
                                                           names(vote_composition_ilr_100m))]

###################################################################################################
#                  Check ilr dimensions
###################################################################################################

length(party_columns)
length(ilr_columns_500m)
length(ilr_columns_100m)

###################################################################################################
#                  8. Create observed grid-cell centroids
###################################################################################################

observed_ilr_grid_500m <- cbs_500m_populated |>
  inner_join(vote_composition_ilr_500m,
             by = "crs28992res500m")

observed_ilr_grid_centroids_500m <- observed_ilr_grid_500m |>
  st_centroid()

observed_ilr_grid_100m <- cbs_100m_populated |>
  inner_join(vote_composition_ilr_100m,
             by = "crs28992res100m")

observed_ilr_grid_centroids_100m <- observed_ilr_grid_100m |>
  st_centroid()

observed_ilr_coordinates_500m <- st_coordinates(observed_ilr_grid_centroids_500m)

observed_ilr_coordinates_100m <- st_coordinates(observed_ilr_grid_centroids_100m)

###################################################################################################
#                  9. Create prediction grid-cell centroids
###################################################################################################

prediction_grid_centroids_500m <- cbs_500m_populated |>
  mutate(prediction_row_id = row_number()) |>
  st_centroid()

prediction_grid_centroids_100m <- cbs_100m_populated |>
  mutate(prediction_row_id = row_number()) |>
  st_centroid()

###################################################################################################
#                  10. Add distance to nearest observed grid cell
###################################################################################################

nearest_observed_index_500m <- st_nearest_feature(prediction_grid_centroids_500m,
                                                  observed_ilr_grid_centroids_500m)

prediction_grid_centroids_500m <- prediction_grid_centroids_500m |>
  mutate(distance_to_nearest_observed_m = as.numeric(st_distance(prediction_grid_centroids_500m,
                                                                 observed_ilr_grid_centroids_500m[nearest_observed_index_500m, ],
                                                                 by_element = TRUE)))

nearest_observed_index_100m <- st_nearest_feature(prediction_grid_centroids_100m,
                                                  observed_ilr_grid_centroids_100m)

prediction_grid_centroids_100m <- prediction_grid_centroids_100m |>
  mutate(distance_to_nearest_observed_m = as.numeric(st_distance(prediction_grid_centroids_100m,
                                                                 observed_ilr_grid_centroids_100m[nearest_observed_index_100m, ],
                                                                 by_element = TRUE)))

###################################################################################################
#                  11. Moran's I and Monte Carlo Moran's I on ilr grid-cell coordinates
###################################################################################################

make_knn_weights <- function(points_sf, k_neighbours = 20) {
  
  coordinates <- st_coordinates(points_sf)
  
  k_used <- min(k_neighbours,
                nrow(coordinates) - 1)
  
  knn_object <- spdep::knearneigh(coordinates,
                                  k = k_used)
  
  neighbour_object <- spdep::knn2nb(knn_object)
  
  spdep::nb2listw(neighbour_object,
                  style = "W",
                  zero.policy = TRUE)
}

run_ilr_moran_checks <- function(points_sf, ilr_columns, grid_size, k_neighbours = 20, nsim = 999) {
  
  weights_object <- make_knn_weights(points_sf = points_sf,
                                     k_neighbours = k_neighbours)
  
  moran_output <- list()
  
  for (i in seq_along(ilr_columns)) {
    
    ilr_var <- ilr_columns[i]
    ilr_values <- points_sf[[ilr_var]]
    
    moran_asymptotic <- spdep::moran.test(ilr_values,
                                          weights_object,
                                          alternative = "greater",
                                          zero.policy = TRUE)
    
    moran_monte_carlo <- spdep::moran.mc(ilr_values,
                                         weights_object,
                                         nsim = nsim,
                                         alternative = "greater",
                                         zero.policy = TRUE)
    
    moran_output[[i]] <- tibble(grid_size = grid_size,
                                ilr_coordinate = ilr_var,
                                k_neighbours = k_neighbours,
                                moran_i = as.numeric(moran_asymptotic$estimate[["Moran I statistic"]]),
                                expected_i = as.numeric(moran_asymptotic$estimate[["Expectation"]]),
                                variance = as.numeric(moran_asymptotic$estimate[["Variance"]]),
                                p_value = moran_asymptotic$p.value,
                                moran_i_mc = as.numeric(moran_monte_carlo$statistic),
                                p_value_mc = moran_monte_carlo$p.value,
                                n_observations = length(ilr_values))
  }
  
  bind_rows(moran_output)
}

moran_results_ilr_500m <- run_ilr_moran_checks(points_sf = observed_ilr_grid_centroids_500m,
                                               ilr_columns = ilr_columns_500m,
                                               grid_size = "500m",
                                               k_neighbours = 20,
                                               nsim = 999)

moran_results_ilr_100m <- run_ilr_moran_checks(points_sf = observed_ilr_grid_centroids_100m,
                                               ilr_columns = ilr_columns_100m,
                                               grid_size = "100m",
                                               k_neighbours = 20,
                                               nsim = 999)

moran_results_ilr_all <- bind_rows(moran_results_ilr_500m,
                                   moran_results_ilr_100m) |>
  mutate(ilr_number = as.numeric(gsub("ilr_", "", ilr_coordinate)),
         ilr_coordinate = factor(ilr_coordinate,
                                 levels = paste0("ilr_",
                                                 sort(unique(ilr_number)))))

moran_summary_ilr <- moran_results_ilr_all |>
  group_by(grid_size) |>
  summarise(mean_moran_i = mean(moran_i, na.rm = TRUE),
            median_moran_i = median(moran_i, na.rm = TRUE),
            min_moran_i = min(moran_i, na.rm = TRUE),
            max_moran_i = max(moran_i, na.rm = TRUE),
            n_significant_005 = sum(p_value < 0.05, na.rm = TRUE),
            n_significant_mc_005 = sum(p_value_mc < 0.05, na.rm = TRUE),
            n_coordinates = n(),
            .groups = "drop")

moran_results_ilr_all
moran_summary_ilr

ggplot(moran_results_ilr_all,
       aes(x = ilr_coordinate,
           y = moran_i)) +
  geom_col() +
  facet_wrap(~ grid_size,
             scales = "free_y") +
  labs(title = "Moran's I for SACD ilr coordinates",
       subtitle = "Spatial autocorrelation of observed grid-cell compositional coordinates",
       x = "ilr coordinate",
       y = "Moran's I") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))

ggplot(moran_results_ilr_all,
       aes(x = ilr_coordinate,
           y = moran_i_mc)) +
  geom_col() +
  facet_wrap(~ grid_size,
             scales = "free_y") +
  labs(title = "Monte Carlo Moran's I for SACD ilr coordinates",
       subtitle = "Permutation-based spatial autocorrelation of observed grid-cell compositional coordinates",
       x = "ilr coordinate",
       y = "Monte Carlo Moran's I") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))

###################################################################################################
#                  12. Variograms on ilr grid-cell coordinates
###################################################################################################

make_ilr_variogram_table <- function(points_sf, ilr_columns, grid_size) {
  
  variogram_output <- list()
  
  for (i in seq_along(ilr_columns)) {
    
    ilr_var <- ilr_columns[i]
    
    ilr_variogram <- gstat::variogram(as.formula(paste(ilr_var,
                                                       "~ 1")),
                                      data = points_sf) |>
      mutate(grid_size = grid_size,
             ilr_coordinate = ilr_var)
    
    variogram_output[[i]] <- ilr_variogram
  }
  
  bind_rows(variogram_output)
}

variogram_results_ilr_500m <- make_ilr_variogram_table(points_sf = observed_ilr_grid_centroids_500m,
                                                       ilr_columns = ilr_columns_500m,
                                                       grid_size = "500m")

variogram_results_ilr_100m <- make_ilr_variogram_table(points_sf = observed_ilr_grid_centroids_100m,
                                                       ilr_columns = ilr_columns_100m,
                                                       grid_size = "100m")

variogram_results_ilr_all <- bind_rows(variogram_results_ilr_500m,
                                       variogram_results_ilr_100m) |>
  mutate(ilr_number = as.numeric(gsub("ilr_", "", ilr_coordinate)),
         ilr_coordinate = factor(ilr_coordinate,
                                 levels = paste0("ilr_",
                                                 sort(unique(ilr_number)))))

ilr_variance_500m <- tibble(grid_size = "500m",
                            ilr_coordinate = ilr_columns_500m,
                            ilr_variance = sapply(ilr_columns_500m,
                                                  function(x) var(observed_ilr_grid_centroids_500m[[x]],
                                                                  na.rm = TRUE)))

ilr_variance_100m <- tibble(grid_size = "100m",
                            ilr_coordinate = ilr_columns_100m,
                            ilr_variance = sapply(ilr_columns_100m,
                                                  function(x) var(observed_ilr_grid_centroids_100m[[x]],
                                                                  na.rm = TRUE)))

ilr_variance_all <- bind_rows(ilr_variance_500m,
                              ilr_variance_100m)

variogram_results_ilr_standardized <- variogram_results_ilr_all |>
  left_join(ilr_variance_all,
            by = c("grid_size",
                   "ilr_coordinate")) |>
  mutate(gamma_standardized = ifelse(ilr_variance > 0,
                                     gamma / ilr_variance,
                                     NA_real_))

variogram_summary_ilr_standardized <- variogram_results_ilr_standardized |>
  group_by(grid_size,
           dist) |>
  summarise(mean_gamma_standardized = mean(gamma_standardized, na.rm = TRUE),
            median_gamma_standardized = median(gamma_standardized, na.rm = TRUE),
            q25_gamma_standardized = quantile(gamma_standardized, 0.25, na.rm = TRUE),
            q75_gamma_standardized = quantile(gamma_standardized, 0.75, na.rm = TRUE),
            n_ilr_coordinates = n(),
            .groups = "drop")

variogram_results_ilr_all
variogram_summary_ilr_standardized

ggplot(variogram_summary_ilr_standardized,
       aes(x = dist,
           y = mean_gamma_standardized)) +
  geom_ribbon(aes(ymin = q25_gamma_standardized,
                  ymax = q75_gamma_standardized),
              alpha = 0.2) +
  geom_line() +
  geom_point() +
  facet_wrap(~ grid_size,
             scales = "free_y") +
  labs(title = "Average standardized variogram across SACD ilr coordinates",
       subtitle = "Observed grid-cell centroids; ribbon shows interquartile range across ilr coordinates",
       x = "Distance between observed grid-cell centroids",
       y = "Mean standardized semivariance") +
  theme_classic()

###################################################################################################
#                  Save checkpoint outputs for Steps 7–12
###################################################################################################

saveRDS(vote_composition_ilr_500m,
        "sacd_outputs/vote_composition_ilr_500m.rds")

saveRDS(vote_composition_ilr_100m,
        "sacd_outputs/vote_composition_ilr_100m.rds")

saveRDS(ilr_columns_500m,
        "sacd_outputs/ilr_columns_500m.rds")

saveRDS(ilr_columns_100m,
        "sacd_outputs/ilr_columns_100m.rds")

saveRDS(observed_ilr_grid_500m,
        "sacd_outputs/observed_ilr_grid_500m.rds")

saveRDS(observed_ilr_grid_100m,
        "sacd_outputs/observed_ilr_grid_100m.rds")

saveRDS(observed_ilr_grid_centroids_500m,
        "sacd_outputs/observed_ilr_grid_centroids_500m.rds")

saveRDS(observed_ilr_grid_centroids_100m,
        "sacd_outputs/observed_ilr_grid_centroids_100m.rds")

saveRDS(prediction_grid_centroids_500m,
        "sacd_outputs/prediction_grid_centroids_500m.rds")

saveRDS(prediction_grid_centroids_100m,
        "sacd_outputs/prediction_grid_centroids_100m.rds")

saveRDS(moran_results_ilr_all,
        "sacd_outputs/moran_results_ilr_all.rds")

saveRDS(moran_summary_ilr,
        "sacd_outputs/moran_summary_ilr.rds")

saveRDS(variogram_results_ilr_all,
        "sacd_outputs/variogram_results_ilr_all.rds")

saveRDS(variogram_results_ilr_standardized,
        "sacd_outputs/variogram_results_ilr_standardized.rds")

saveRDS(variogram_summary_ilr_standardized,
        "sacd_outputs/variogram_summary_ilr_standardized.rds")

write_csv(moran_results_ilr_all,
          "sacd_outputs/moran_results_ilr_all.csv")

write_csv(moran_summary_ilr,
          "sacd_outputs/moran_summary_ilr.csv")

write_csv(variogram_results_ilr_all,
          "sacd_outputs/variogram_results_ilr_all.csv")

write_csv(variogram_summary_ilr_standardized,
          "sacd_outputs/variogram_summary_ilr_standardized.csv")

###################################################################################################
#                  13. Select IDW power using leave-one-out error across ilr coordinates
###################################################################################################

make_spatstat_window <- function(grid_sf) {
  
  current_bbox <- st_bbox(grid_sf)
  
  spatstat.geom::owin(xrange = c(current_bbox["xmin"],
                                 current_bbox["xmax"]),
                      yrange = c(current_bbox["ymin"],
                                 current_bbox["ymax"]))
}

make_ppp_object <- function(points_sf, mark_column, window_object) {
  
  coordinates <- st_coordinates(points_sf)
  
  spatstat.geom::ppp(x = coordinates[, "X"],
                     y = coordinates[, "Y"],
                     marks = points_sf[[mark_column]],
                     window = window_object)
}

calculate_error_metrics <- function(observed, predicted) {
  
  observed <- as.numeric(observed)
  predicted <- as.numeric(predicted)
  
  keep <- is.finite(observed) & is.finite(predicted)
  
  observed <- observed[keep]
  predicted <- predicted[keep]
  
  prediction_error <- observed - predicted
  
  tibble(n_observations = length(observed),
         mse = mean(prediction_error^2),
         rmse = sqrt(mean(prediction_error^2)),
         mae = mean(abs(prediction_error)),
         mean_error = mean(prediction_error),
         correlation = ifelse(length(observed) > 1,
                              cor(observed,
                                  predicted),
                              NA_real_))
}

run_sacd_power_selection <- function(points_sf, ilr_columns, powers, grid_size, window_object) {
  
  power_output <- list()
  counter <- 1
  
  for (current_power in powers) {
    
    coordinate_output <- list()
    
    for (i in seq_along(ilr_columns)) {
      
      ilr_var <- ilr_columns[i]
      
      ppp_ilr <- make_ppp_object(points_sf = points_sf,
                                 mark_column = ilr_var,
                                 window_object = window_object)
      
      loo_prediction <- spatstat.explore::idw(ppp_ilr,
                                              power = current_power,
                                              at = "points")
      
      current_metrics <- calculate_error_metrics(observed = as.numeric(spatstat.geom::marks(ppp_ilr)),
                                                 predicted = as.numeric(loo_prediction))
      
      current_variance <- var(points_sf[[ilr_var]],
                              na.rm = TRUE)
      
      coordinate_output[[i]] <- current_metrics |>
        mutate(grid_size = grid_size,
               ilr_coordinate = ilr_var,
               power = current_power,
               ilr_variance = current_variance,
               standardized_mse = ifelse(current_variance > 0,
                                         mse / current_variance,
                                         NA_real_),
               .before = 1)
    }
    
    power_output[[counter]] <- bind_rows(coordinate_output) |>
      group_by(grid_size,
               power) |>
      summarise(mean_mse = mean(mse, na.rm = TRUE),
                mean_rmse = mean(rmse, na.rm = TRUE),
                mean_mae = mean(mae, na.rm = TRUE),
                mean_standardized_mse = mean(standardized_mse, na.rm = TRUE),
                mean_correlation = mean(correlation, na.rm = TRUE),
                n_ilr_coordinates = n(),
                .groups = "drop")
    
    counter <- counter + 1
  }
  
  bind_rows(power_output)
}

window_500m <- make_spatstat_window(cbs_500m_populated)

window_100m <- make_spatstat_window(cbs_100m_populated)

powers <- seq(1,
              6,
              by = 0.5)

idw_power_results_ilr_500m <- run_sacd_power_selection(points_sf = observed_ilr_grid_centroids_500m,
                                                       ilr_columns = ilr_columns_500m,
                                                       powers = powers,
                                                       grid_size = "500m",
                                                       window_object = window_500m)

idw_power_results_ilr_100m <- run_sacd_power_selection(points_sf = observed_ilr_grid_centroids_100m,
                                                       ilr_columns = ilr_columns_100m,
                                                       powers = powers,
                                                       grid_size = "100m",
                                                       window_object = window_100m)

idw_power_results_ilr_all <- bind_rows(idw_power_results_ilr_500m,
                                       idw_power_results_ilr_100m)

optimal_power_sacd <- idw_power_results_ilr_all |>
  group_by(grid_size) |>
  slice_min(mean_mse,
            n = 1,
            with_ties = FALSE) |>
  ungroup()

optimal_power_sacd

optimal_power_ilr_500m <- optimal_power_sacd |>
  filter(grid_size == "500m") |>
  pull(power)

optimal_power_ilr_100m <- optimal_power_sacd |>
  filter(grid_size == "100m") |>
  pull(power)

ggplot(idw_power_results_ilr_all,
       aes(x = power,
           y = mean_mse)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ grid_size,
             scales = "free_y") +
  labs(title = "IDW power selection for SACD ilr coordinates",
       subtitle = "Leave-one-out mean MSE across ilr coordinates",
       x = "IDW power / decay factor",
       y = "Mean MSE across ilr coordinates") +
  theme_classic()

saveRDS(idw_power_results_ilr_all,
        "sacd_outputs/idw_power_results_ilr_all.rds")

saveRDS(optimal_power_sacd,
        "sacd_outputs/optimal_power_sacd.rds")

write_csv(idw_power_results_ilr_all,
          "sacd_outputs/idw_power_results_ilr_all.csv")

write_csv(optimal_power_sacd,
          "sacd_outputs/optimal_power_sacd.csv")

###################################################################################################
#                  14. Run final LOO validation and back-transform to party shares
###################################################################################################

run_sacd_loo_ilr_prediction <- function(points_sf, ilr_columns, grid_id_column, selected_power, window_object) {
  
  predicted_ilr_cv <- st_drop_geometry(points_sf)
  
  predicted_ilr_cv <- predicted_ilr_cv[, grid_id_column, drop = FALSE]
  
  for (ilr_var in ilr_columns) {
    
    cat("Running leave-one-out IDW for", ilr_var, "...\n")
    
    ppp_ilr <- make_ppp_object(points_sf = points_sf,
                               mark_column = ilr_var,
                               window_object = window_object)
    
    loo_prediction <- spatstat.explore::idw(ppp_ilr,
                                            power = selected_power,
                                            at = "points")
    
    predicted_ilr_cv[[ilr_var]] <- as.numeric(loo_prediction)
  }
  
  predicted_ilr_cv
}

back_transform_ilr_to_party_shares <- function(predicted_ilr_data, ilr_columns, party_columns, grid_id_column, prefix = "pred_") {
  
  predicted_ilr_matrix <- predicted_ilr_data |>
    select(all_of(ilr_columns)) |>
    as.matrix()
  
  predicted_party_shares <- compositions::ilrInv(predicted_ilr_matrix)
  
  predicted_party_shares <- as.data.frame(predicted_party_shares)
  
  names(predicted_party_shares) <- paste0(prefix,
                                          party_columns)
  
  bind_cols(predicted_ilr_data[, grid_id_column, drop = FALSE],
            predicted_party_shares)
}

predicted_ilr_cv_500m <- run_sacd_loo_ilr_prediction(points_sf = observed_ilr_grid_centroids_500m,
                                                     ilr_columns = ilr_columns_500m,
                                                     grid_id_column = "crs28992res500m",
                                                     selected_power = optimal_power_ilr_500m,
                                                     window_object = window_500m)

predicted_ilr_cv_100m <- run_sacd_loo_ilr_prediction(points_sf = observed_ilr_grid_centroids_100m,
                                                     ilr_columns = ilr_columns_100m,
                                                     grid_id_column = "crs28992res100m",
                                                     selected_power = optimal_power_ilr_100m,
                                                     window_object = window_100m)

predicted_party_shares_cv_500m <- back_transform_ilr_to_party_shares(predicted_ilr_data = predicted_ilr_cv_500m,
                                                                     ilr_columns = ilr_columns_500m,
                                                                     party_columns = party_columns,
                                                                     grid_id_column = "crs28992res500m",
                                                                     prefix = "pred_")

predicted_party_shares_cv_100m <- back_transform_ilr_to_party_shares(predicted_ilr_data = predicted_ilr_cv_100m,
                                                                     ilr_columns = ilr_columns_100m,
                                                                     party_columns = party_columns,
                                                                     grid_id_column = "crs28992res100m",
                                                                     prefix = "pred_")

saveRDS(predicted_ilr_cv_500m,
        "sacd_outputs/predicted_ilr_cv_500m.rds")

saveRDS(predicted_ilr_cv_100m,
        "sacd_outputs/predicted_ilr_cv_100m.rds")

saveRDS(predicted_party_shares_cv_500m,
        "sacd_outputs/predicted_party_shares_cv_500m.rds")

saveRDS(predicted_party_shares_cv_100m,
        "sacd_outputs/predicted_party_shares_cv_100m.rds")

###################################################################################################
#                  15. Create final validation/error tables
###################################################################################################

create_sacd_validation_tables <- function(observed_shares, predicted_shares, observed_ilr, predicted_ilr, party_columns, ilr_columns, grid_id_column, grid_size_value, selected_power_value) {
  
  observed_party_shares <- observed_shares |>
    select(all_of(grid_id_column),
           all_of(party_columns))
  
  comparison_party_shares <- observed_party_shares |>
    left_join(predicted_shares,
              by = grid_id_column)
  
  party_error_output <- list()
  
  for (i in seq_along(party_columns)) {
    
    party <- party_columns[i]
    observed_col <- party
    predicted_col <- paste0("pred_", party)
    
    current_observed <- comparison_party_shares[[observed_col]]
    current_predicted <- comparison_party_shares[[predicted_col]]
    current_error <- current_observed - current_predicted
    
    party_error_output[[i]] <- tibble(grid_size = grid_size_value,
                                      party = party,
                                      selected_power = selected_power_value,
                                      mean_error = mean(current_error, na.rm = TRUE),
                                      mean_absolute_error = mean(abs(current_error), na.rm = TRUE),
                                      rmse = sqrt(mean(current_error^2, na.rm = TRUE)),
                                      correlation = cor(current_observed,
                                                        current_predicted,
                                                        use = "complete.obs"))
  }
  
  party_share_errors <- bind_rows(party_error_output)
  
  observed_ilr_matrix <- observed_ilr |>
    st_drop_geometry() |>
    select(all_of(ilr_columns)) |>
    as.matrix()
  
  predicted_ilr_matrix <- predicted_ilr |>
    select(all_of(ilr_columns)) |>
    as.matrix()
  
  aitchison_distance <- sqrt(rowSums((observed_ilr_matrix - predicted_ilr_matrix)^2))
  
  grid_error_data <- comparison_party_shares |>
    rowwise() |>
    mutate(mean_absolute_party_share_error = mean(abs(c_across(all_of(party_columns)) -
                                                        c_across(all_of(paste0("pred_", party_columns)))),
                                                  na.rm = TRUE),
           max_absolute_party_share_error = max(abs(c_across(all_of(party_columns)) -
                                                      c_across(all_of(paste0("pred_", party_columns)))),
                                                na.rm = TRUE)) |>
    ungroup() |>
    select(all_of(grid_id_column),
           mean_absolute_party_share_error,
           max_absolute_party_share_error) |>
    mutate(grid_size = grid_size_value,
           selected_power = selected_power_value,
           aitchison_distance = aitchison_distance,
           .before = 1)
  
  overall_error_summary <- party_share_errors |>
    summarise(grid_size = first(grid_size),
              selected_power = first(selected_power),
              mean_party_mae = mean(mean_absolute_error, na.rm = TRUE),
              median_party_mae = median(mean_absolute_error, na.rm = TRUE),
              max_party_mae = max(mean_absolute_error, na.rm = TRUE),
              mean_party_rmse = mean(rmse, na.rm = TRUE),
              mean_party_correlation = mean(correlation, na.rm = TRUE),
              mean_grid_mae = mean(grid_error_data$mean_absolute_party_share_error, na.rm = TRUE),
              median_grid_mae = median(grid_error_data$mean_absolute_party_share_error, na.rm = TRUE),
              max_grid_mae = max(grid_error_data$mean_absolute_party_share_error, na.rm = TRUE),
              mean_aitchison_distance = mean(grid_error_data$aitchison_distance, na.rm = TRUE),
              median_aitchison_distance = median(grid_error_data$aitchison_distance, na.rm = TRUE),
              max_aitchison_distance = max(grid_error_data$aitchison_distance, na.rm = TRUE),
              n_parties = n(),
              .groups = "drop")
  
  list(comparison_party_shares = comparison_party_shares,
       party_share_errors = party_share_errors,
       grid_error_data = grid_error_data,
       overall_error_summary = overall_error_summary)
}

validation_500m <- create_sacd_validation_tables(observed_shares = vote_composition_replaced_500m,
                                                 predicted_shares = predicted_party_shares_cv_500m,
                                                 observed_ilr = observed_ilr_grid_centroids_500m,
                                                 predicted_ilr = predicted_ilr_cv_500m,
                                                 party_columns = party_columns,
                                                 ilr_columns = ilr_columns_500m,
                                                 grid_id_column = "crs28992res500m",
                                                 grid_size_value = "500m",
                                                 selected_power_value = optimal_power_ilr_500m)

validation_100m <- create_sacd_validation_tables(observed_shares = vote_composition_replaced_100m,
                                                 predicted_shares = predicted_party_shares_cv_100m,
                                                 observed_ilr = observed_ilr_grid_centroids_100m,
                                                 predicted_ilr = predicted_ilr_cv_100m,
                                                 party_columns = party_columns,
                                                 ilr_columns = ilr_columns_100m,
                                                 grid_id_column = "crs28992res100m",
                                                 grid_size_value = "100m",
                                                 selected_power_value = optimal_power_ilr_100m)

comparison_party_shares_500m <- validation_500m$comparison_party_shares
comparison_party_shares_100m <- validation_100m$comparison_party_shares

party_share_errors_500m <- validation_500m$party_share_errors
party_share_errors_100m <- validation_100m$party_share_errors

grid_error_data_500m <- validation_500m$grid_error_data
grid_error_data_100m <- validation_100m$grid_error_data

overall_party_share_error_500m <- validation_500m$overall_error_summary
overall_party_share_error_100m <- validation_100m$overall_error_summary

party_share_errors_all <- bind_rows(party_share_errors_500m,
                                    party_share_errors_100m)

grid_error_data_all <- bind_rows(grid_error_data_500m,
                                 grid_error_data_100m)

overall_sacd_error_summary <- bind_rows(overall_party_share_error_500m,
                                        overall_party_share_error_100m)

party_share_errors_all
grid_error_data_all
overall_sacd_error_summary

party_share_errors_all
overall_sacd_error_summary

saveRDS(comparison_party_shares_500m,
        "sacd_outputs/comparison_party_shares_500m.rds")

saveRDS(comparison_party_shares_100m,
        "sacd_outputs/comparison_party_shares_100m.rds")

saveRDS(party_share_errors_all,
        "sacd_outputs/party_share_errors_all.rds")

saveRDS(grid_error_data_all,
        "sacd_outputs/grid_error_data_all.rds")

saveRDS(overall_sacd_error_summary,
        "sacd_outputs/overall_sacd_error_summary.rds")

write_csv(party_share_errors_all,
          "sacd_outputs/party_share_errors_all.csv")

write_csv(grid_error_data_all,
          "sacd_outputs/grid_error_data_all.csv")

write_csv(overall_sacd_error_summary,
          "sacd_outputs/overall_sacd_error_summary.csv")

###################################################################################################
#                  16. Run final global IDW interpolation of ilr coordinates
###################################################################################################

run_final_sacd_idw <- function(observed_points, prediction_points, ilr_columns, grid_id_column, selected_power) {
  
  grid_pred_ilr_values <- st_drop_geometry(prediction_points)
  
  grid_pred_ilr_values <- grid_pred_ilr_values[, grid_id_column, drop = FALSE]
  
  for (ilr_var in ilr_columns) {
    
    cat("Running final global IDW for", ilr_var, "...\n")
    
    idw_result <- gstat::idw(as.formula(paste(ilr_var,
                                              "~ 1")),
                             locations = observed_points,
                             newdata = prediction_points,
                             idp = selected_power,
                             nmax = Inf,
                             maxdist = Inf,
                             debug.level = 0)
    
    grid_pred_ilr_values[[paste0(ilr_var, "_idw")]] <- idw_result$var1.pred
  }
  
  grid_pred_ilr_values
}

grid_pred_ilr_values_500m <- run_final_sacd_idw(observed_points = observed_ilr_grid_centroids_500m,
                                                prediction_points = prediction_grid_centroids_500m,
                                                ilr_columns = ilr_columns_500m,
                                                grid_id_column = "crs28992res500m",
                                                selected_power = optimal_power_ilr_500m)

grid_pred_ilr_values_100m <- run_final_sacd_idw(observed_points = observed_ilr_grid_centroids_100m,
                                                prediction_points = prediction_grid_centroids_100m,
                                                ilr_columns = ilr_columns_100m,
                                                grid_id_column = "crs28992res100m",
                                                selected_power = optimal_power_ilr_100m)

saveRDS(grid_pred_ilr_values_500m,
        "sacd_outputs/grid_pred_ilr_values_500m.rds")

saveRDS(grid_pred_ilr_values_100m,
        "sacd_outputs/grid_pred_ilr_values_100m.rds")

###################################################################################################
#                  17. Back-transform final ilr predictions to party shares
###################################################################################################

back_transform_final_idw <- function(grid_pred_ilr_values, ilr_columns, party_columns, grid_id_column) {
  
  ilr_idw_columns <- paste0(ilr_columns,
                            "_idw")
  
  predicted_ilr_matrix <- grid_pred_ilr_values |>
    select(all_of(ilr_idw_columns)) |>
    as.matrix()
  
  predicted_party_shares <- compositions::ilrInv(predicted_ilr_matrix)
  
  predicted_party_shares <- as.data.frame(predicted_party_shares)
  
  names(predicted_party_shares) <- paste0("pred_",
                                          party_columns)
  
  bind_cols(grid_pred_ilr_values[, grid_id_column, drop = FALSE],
            predicted_party_shares)
}

predicted_party_shares_500m <- back_transform_final_idw(grid_pred_ilr_values = grid_pred_ilr_values_500m,
                                                        ilr_columns = ilr_columns_500m,
                                                        party_columns = party_columns,
                                                        grid_id_column = "crs28992res500m")

predicted_party_shares_100m <- back_transform_final_idw(grid_pred_ilr_values = grid_pred_ilr_values_100m,
                                                        ilr_columns = ilr_columns_100m,
                                                        party_columns = party_columns,
                                                        grid_id_column = "crs28992res100m")

###################################################################################################
#                  Check that predicted party shares sum to 1
###################################################################################################

predicted_share_sum_check_500m <- predicted_party_shares_500m |>
  mutate(predicted_share_sum = rowSums(across(starts_with("pred_")))) |>
  summarise(min_sum = min(predicted_share_sum, na.rm = TRUE),
            max_sum = max(predicted_share_sum, na.rm = TRUE),
            mean_sum = mean(predicted_share_sum, na.rm = TRUE))

predicted_share_sum_check_100m <- predicted_party_shares_100m |>
  mutate(predicted_share_sum = rowSums(across(starts_with("pred_")))) |>
  summarise(min_sum = min(predicted_share_sum, na.rm = TRUE),
            max_sum = max(predicted_share_sum, na.rm = TRUE),
            mean_sum = mean(predicted_share_sum, na.rm = TRUE))

predicted_share_sum_check_500m
predicted_share_sum_check_100m

saveRDS(predicted_party_shares_500m,
        "sacd_outputs/predicted_party_shares_500m.rds")

saveRDS(predicted_party_shares_100m,
        "sacd_outputs/predicted_party_shares_100m.rds")

write_csv(predicted_share_sum_check_500m,
          "sacd_outputs/predicted_share_sum_check_500m.csv")

write_csv(predicted_share_sum_check_100m,
          "sacd_outputs/predicted_share_sum_check_100m.csv")

###################################################################################################
#                  18. Attach predicted shares to CBS grid polygons
###################################################################################################

grid_pred_party_shares_500m <- cbs_500m_populated |>
  left_join(predicted_party_shares_500m,
            by = "crs28992res500m") |>
  mutate(distance_to_nearest_observed_m = prediction_grid_centroids_500m$distance_to_nearest_observed_m)

grid_pred_party_shares_100m <- cbs_100m_populated |>
  left_join(predicted_party_shares_100m,
            by = "crs28992res100m") |>
  mutate(distance_to_nearest_observed_m = prediction_grid_centroids_100m$distance_to_nearest_observed_m)

###################################################################################################
#                  Attach LOO validation errors to observed grid polygons
###################################################################################################

grid_error_polygons_500m <- observed_ilr_grid_500m |>
  select(crs28992res500m) |>
  left_join(grid_error_data_500m,
            by = "crs28992res500m")

grid_error_polygons_100m <- observed_ilr_grid_100m |>
  select(crs28992res100m) |>
  left_join(grid_error_data_100m,
            by = "crs28992res100m")

saveRDS(grid_pred_party_shares_500m,
        "sacd_outputs/grid_pred_party_shares_500m.rds")

saveRDS(grid_pred_party_shares_100m,
        "sacd_outputs/grid_pred_party_shares_100m.rds")

saveRDS(grid_error_polygons_500m,
        "sacd_outputs/grid_error_polygons_500m.rds")

saveRDS(grid_error_polygons_100m,
        "sacd_outputs/grid_error_polygons_100m.rds")

st_write(grid_pred_party_shares_500m,
         "sacd_outputs/grid_pred_party_shares_500m.gpkg",
         delete_dsn = TRUE)

st_write(grid_pred_party_shares_100m,
         "sacd_outputs/grid_pred_party_shares_100m.gpkg",
         delete_dsn = TRUE)

st_write(grid_error_polygons_500m,
         "sacd_outputs/grid_error_polygons_500m.gpkg",
         delete_dsn = TRUE)

st_write(grid_error_polygons_100m,
         "sacd_outputs/grid_error_polygons_100m.gpkg",
         delete_dsn = TRUE)


# For the code that graphs the assumptions and the individual prediction maps, please go to file
# Assumptions_check_SACD