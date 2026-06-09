###################################################################################################
#                   Part 6: IDW diagnostics, validation, and interpolation
#                   Electoral polarization interpolation on CBS 500m and 100m grids
###################################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(gstat)
library(spdep)
library(spatstat.geom)
library(spatstat.explore)

set.seed(2698218)

###################################################################################################
#                   1. Load data
###################################################################################################

cbs_500m <- st_read("cbs_vk500_2024_v1.gpkg")

cbs_100m <- st_read("cbs_vk100_2024_v1.gpkg")

er_stembureau <- read_csv("er_stembureau.csv") |>
  select(-any_of("...1"))

###################################################################################################
#                   2. Prepare spatial objects
###################################################################################################

er_stembureau_sf <- er_stembureau |>
  st_as_sf(coords = c("X",
                      "Y"),
           crs = 28992,
           remove = FALSE)

st_crs(er_stembureau_sf)

cbs_500m_populated <- cbs_500m |>
  mutate(aantal_inwoners_numeric = suppressWarnings(as.numeric(aantal_inwoners)),
         prediction_row_id = row_number()) |>
  filter(!is.na(aantal_inwoners_numeric),
         aantal_inwoners_numeric > 0)

cbs_100m_populated <- cbs_100m |>
  mutate(aantal_inwoners_numeric = suppressWarnings(as.numeric(aantal_inwoners)),
         prediction_row_id = row_number()) |>
  filter(!is.na(aantal_inwoners_numeric),
         aantal_inwoners_numeric > 0)

###################################################################################################
#                   3. Assign stembureau ER values to populated grid cells
###################################################################################################

stembureau_grid_join_500m <- er_stembureau_sf |>
  st_join(cbs_500m_populated,
          join = st_within)

stembureau_grid_join_100m <- er_stembureau_sf |>
  st_join(cbs_100m_populated,
          join = st_within)

###################################################################################################
#                   4. Aggregate stembureau ER values into observed grid-cell ER values
###################################################################################################

# It is important to note that sometimes, more than one stembureau will fall within the same grid cell.
# As such, this thesis opted for taking the weighted average of all the stembureaus that are in the same grid cell
# accoridng to their total number of votes. 

grid_scores_500m <- stembureau_grid_join_500m |>
  st_drop_geometry() |>
  filter(!is.na(crs28992res500m),
         !is.na(total_votes_kieskompas)) |>
  group_by(crs28992res500m) |>
  summarise(ER_links_rechts_weighted = weighted.mean(ER_links_rechts_norm,
                                                     total_votes_kieskompas,
                                                     na.rm = TRUE),
            ER_progressief_conservatief_weighted = weighted.mean(ER_progressief_conservatief_norm,
                                                                 total_votes_kieskompas,
                                                                 na.rm = TRUE),
            ER_2d_euclidean_weighted = weighted.mean(ER_2d_euclidean_norm,
                                                     total_votes_kieskompas,
                                                     na.rm = TRUE),
            total_votes_cell = sum(total_votes_kieskompas,
                                   na.rm = TRUE),
            n_stembureaus = n(),
            .groups = "drop")

grid_scores_100m <- stembureau_grid_join_100m |>
  st_drop_geometry() |>
  filter(!is.na(crs28992res100m),
         !is.na(total_votes_kieskompas)) |>
  group_by(crs28992res100m) |>
  summarise(ER_links_rechts_weighted = weighted.mean(ER_links_rechts_norm,
                                                     total_votes_kieskompas,
                                                     na.rm = TRUE),
            ER_progressief_conservatief_weighted = weighted.mean(ER_progressief_conservatief_norm,
                                                                 total_votes_kieskompas,
                                                                 na.rm = TRUE),
            ER_2d_euclidean_weighted = weighted.mean(ER_2d_euclidean_norm,
                                                     total_votes_kieskompas,
                                                     na.rm = TRUE),
            total_votes_cell = sum(total_votes_kieskompas,
                                   na.rm = TRUE),
            n_stembureaus = n(),
            .groups = "drop")

###################################################################################################
#                   5. Create observed grid-cell centroid points
###################################################################################################

# These are the cells whose interpolation will be carried out. We need to separate between the cells where
# an actual observed value is, and the ones where the predictions will be carried out. These two chuncks of code
# do preciesely that.

observed_grid_500m <- cbs_500m_populated |>
  inner_join(grid_scores_500m,
             by = "crs28992res500m")

observed_points_500m <- observed_grid_500m |>
  st_centroid()

observed_grid_100m <- cbs_100m_populated |>
  inner_join(grid_scores_100m,
             by = "crs28992res100m")

observed_points_100m <- observed_grid_100m |>
  st_centroid()

###################################################################################################
#                   6. Create prediction locations: all populated grid-cell centroids
###################################################################################################

prediction_points_500m <- cbs_500m_populated |>
  st_centroid()

prediction_points_100m <- cbs_100m_populated |>
  st_centroid()

###################################################################################################
#                   7. Add distance to nearest observed grid cell
###################################################################################################

# We proceed to find the nearest observed grid cell centroid and we store the distance.

nearest_observed_index_500m <- st_nearest_feature(prediction_points_500m,
                                                  observed_points_500m)

prediction_points_500m <- prediction_points_500m |>
  mutate(distance_to_nearest_observed_m = as.numeric(st_distance(prediction_points_500m,
                                                                 observed_points_500m[nearest_observed_index_500m, ],
                                                                 by_element = TRUE)))

nearest_observed_index_100m <- st_nearest_feature(prediction_points_100m,
                                                  observed_points_100m)

prediction_points_100m <- prediction_points_100m |>
  mutate(distance_to_nearest_observed_m = as.numeric(st_distance(prediction_points_100m,
                                                                 observed_points_100m[nearest_observed_index_100m, ],
                                                                 by_element = TRUE)))

###################################################################################################
#                   8. Helper functions
###################################################################################################

# For carrying out the assumptions, we need to create certain helper functions that will facilitate the procedure.

# For the Moran's I test we need to create a spatial structure so that the calculations can be carried out.
# we need to specify how many neighbors we want to take into account for making the distance weighing. Since the test
# doesn't allow for Inf neighbors as the IDW formula. So we set a number of 20. It is important to meantion that
# if teh k_neighbours argument is changed, so will the Moran's I results. 

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


# This function carries out the Moran's I test for the different ER dimensions. The columns argument indicates
# which ER variables should be tested, while the labels argument gives those variables a cleaner name in the output.
# The weights_object argument provides the spatial neighbour structure created previously. The level_name argument
# is used only to keep track of whether the results belong to the 500m or 100m grid. The function runs both the
# asymptotic Moran's I test and a Monte Carlo version with 999 simulations. The alternative is set to "greater"
# because we are interested in testing whether nearby grid cells have more similar ER values than would be expected
# under spatial randomness.


run_moran_checks <- function(points_sf, weights_object, columns, labels, level_name) {
  
  moran_output <- list()
  
  for (i in seq_along(columns)) {
    
    current_column <- columns[i]
    current_label <- labels[i]
    current_values <- points_sf[[current_column]]
    
    moran_asymptotic <- spdep::moran.test(current_values,
                                          weights_object,
                                          alternative = "greater",
                                          zero.policy = TRUE)
    
    moran_monte_carlo <- spdep::moran.mc(current_values,
                                         weights_object,
                                         nsim = 999,
                                         alternative = "greater",
                                         zero.policy = TRUE)
    
    moran_output[[i]] <- tibble(level = level_name,
                                er_dimension = current_label,
                                moran_i_asymptotic = as.numeric(moran_asymptotic$estimate[["Moran I statistic"]]),
                                p_value_asymptotic = moran_asymptotic$p.value,
                                moran_i_monte_carlo = as.numeric(moran_monte_carlo$statistic),
                                p_value_monte_carlo = moran_monte_carlo$p.value,
                                n_observations = length(current_values))
  }
  
  bind_rows(moran_output)
}

# This function creates the empirical variogram tables for the different ER dimensions. The columns argument tells
# the function which ER variables should be used, while the labels argument gives those variables clearer names in
# the output. The level_name argument is used to identify whether the results belong to the
# 500m or 100m grid. The variogram helps to show whether nearby grid cells tend to be more similar to each other than
# grid cells that are farther apart. 

make_variogram_table <- function(points_sf, columns, labels, level_name) {
  
  variogram_output <- list()
  
  for (i in seq_along(columns)) {
    
    current_column <- columns[i]
    current_label <- labels[i]
    
    current_formula <- as.formula(paste0(current_column,
                                         " ~ 1"))
    
    variogram_output[[i]] <- gstat::variogram(current_formula,
                                              data = points_sf) |>
      mutate(level = level_name,
             er_dimension = current_label)
  }
  
  bind_rows(variogram_output)
}

# This function calculates the error metrics used to evaluate the IDW predictions. The observed argument contains
# the real ER values, while the predicted argument contains the IDW-predicted ER values. The function first removes
# values that are not finite so that the calculations are not distorted by missing or invalid values. It then calculates
# the prediction error as observed minus predicted. From this, it calculates the MSE, RMSE, MAE, mean error, and the
# correlation between observed and predicted values. These metrics are later used to compare how well the IDW
# predictions reproduce the observed grid-cell ER values.

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

# This function converts the observed sf point data into a spatstat point pattern object. This is necessary because
# the leave-one-out IDW power selection is done with the spatstat package. The points_sf argument contains the observed
# grid-cell centroids, while mark_column specifies which ER value should be attached to each point. The window_object
# argument defines the spatial extent within which the point pattern exists. The ER values are stored as marks, meaning
# that each point carries its own observed ER value.

make_ppp_object <- function(points_sf, mark_column, window_object) {
  
  coordinates <- st_coordinates(points_sf)
  
  spatstat.geom::ppp(x = coordinates[, "X"],
                     y = coordinates[, "Y"],
                     marks = points_sf[[mark_column]],
                     window = window_object)
}

# This function tests different IDW decay powers and calculates the prediction error for each one. The ppp_object
# argument contains the observed points and their ER values, while the powers argument contains the set of decay values
# that should be tested. A low power gives relatively more influence to distant points, while a high power gives much
# more influence to nearby points. The function uses leave-one-out prediction by setting at = "points", meaning that
# each observed point is predicted from the remaining observed points. The grid_size and er_dimension arguments are
# added to the output so that the results can be identified later. The selected power is the one with the lowest MSE.

calculate_power_curve <- function(ppp_object, powers, grid_size, er_dimension) {
  
  power_results <- list()
  observed_values <- as.numeric(spatstat.geom::marks(ppp_object))
  
  for (i in seq_along(powers)) {
    
    current_power <- powers[i]
    
    loo_prediction <- spatstat.explore::idw(ppp_object,
                                            power = current_power,
                                            at = "points")
    
    current_metrics <- calculate_error_metrics(observed = observed_values,
                                               predicted = as.numeric(loo_prediction))
    
    power_results[[i]] <- current_metrics |>
      mutate(grid_size = grid_size,
             er_dimension = er_dimension,
             power = current_power,
             .before = 1)
  }
  
  bind_rows(power_results)
}

# This function creates the leave-one-out residual table after the best IDW power has been selected. The points_sf
# argument contains the observed grid-cell centroids, while the ppp_object contains the same points in spatstat format.
# The id_column argument identifies the grid-cell ID column, which differs between the 500m and 100m grids. The selected_power
# argument tells the function which IDW decay power to use. The function then stores the observed ER value, the
# leave-one-out predicted ER value, the residual, the absolute residual, and the squared residual. These residuals are
# later used to calculate the final error table.

create_loo_residuals <- function(points_sf, ppp_object, id_column, grid_size, er_dimension, selected_power) {
  
  observed_values <- as.numeric(spatstat.geom::marks(ppp_object))
  
  loo_prediction <- spatstat.explore::idw(ppp_object,
                                          power = selected_power,
                                          at = "points")
  
  points_sf |>
    mutate(grid_size = grid_size,
           er_dimension = er_dimension,
           selected_power = selected_power,
           observed_er = observed_values,
           loo_predicted_er = as.numeric(loo_prediction),
           residual = observed_er - loo_predicted_er,
           absolute_residual = abs(residual),
           squared_residual = residual^2) |>
    select(all_of(id_column),
           grid_size,
           er_dimension,
           selected_power,
           observed_er,
           loo_predicted_er,
           residual,
           absolute_residual,
           squared_residual,
           n_stembureaus,
           total_votes_cell)
}

# This function carries out the final IDW interpolation. The observed_points argument contains the grid-cell centroids
# where ER values are known, while the prediction_points argument contains the grid-cell centroids where ER values should
# be predicted. The observed_column argument tells the function which ER dimension should be interpolated. The selected_power
# argument controls the distance decay: higher values make nearby observed points much more influential, while lower values
# allow distant observed points to have more influence. The arguments nmax = Inf and maxdist = Inf mean that all observed
# points are allowed to contribute to the prediction and that no maximum distance cut-off is imposed.

run_final_idw <- function(observed_points, prediction_points, observed_column, selected_power) {
  
  current_formula <- as.formula(paste0(observed_column,
                                       " ~ 1"))
  
  gstat::idw(current_formula,
             locations = observed_points,
             newdata = prediction_points,
             idp = selected_power,
             nmax = Inf,
             maxdist = Inf,
             debug.level = 0)
}

# This function summarises the range and distribution of either observed or IDW-predicted ER values. The values argument
# contains the numeric ER values to be summarised. The level_name, er_dimension, and value_type arguments are used to label
# the output, so that it is clear whether the summary refers to the 500m or 100m grid, which ER dimension is being described,
# and whether the values are observed or predicted. The function returns the minimum, first quartile, median, mean, third
# quartile, maximum, and standard deviation.

summarise_range <- function(values, level_name, er_dimension, value_type) {
  
  values <- as.numeric(values)
  
  tibble(level = level_name,
         er_dimension = er_dimension,
         value_type = value_type,
         minimum = min(values, na.rm = TRUE),
         q1 = quantile(values, 0.25, na.rm = TRUE),
         median = median(values, na.rm = TRUE),
         mean = mean(values, na.rm = TRUE),
         q3 = quantile(values, 0.75, na.rm = TRUE),
         maximum = max(values, na.rm = TRUE),
         sd = sd(values, na.rm = TRUE))
}

###################################################################################################
#                   9. Moran's I diagnostics on observed grid-cell ER values
###################################################################################################

# Within this section we run the aforementioned formulas to do the assumptions check on every dimension
# for every ideological axis.

grid_er_columns <- c("ER_links_rechts_weighted",
                     "ER_progressief_conservatief_weighted",
                     "ER_2d_euclidean_weighted")

grid_er_labels <- c("Links-Rechts",
                    "Progressief-Conservatief",
                    "2D Euclidean")

weights_500m <- make_knn_weights(points_sf = observed_points_500m,
                                 k_neighbours = 20)

weights_100m <- make_knn_weights(points_sf = observed_points_100m,
                                 k_neighbours = 20)

moran_results_500m <- run_moran_checks(points_sf = observed_points_500m,
                                       weights_object = weights_500m,
                                       columns = grid_er_columns,
                                       labels = grid_er_labels,
                                       level_name = "500m observed grid cells")

moran_results_100m <- run_moran_checks(points_sf = observed_points_100m,
                                       weights_object = weights_100m,
                                       columns = grid_er_columns,
                                       labels = grid_er_labels,
                                       level_name = "100m observed grid cells")

moran_results_all <- bind_rows(moran_results_500m,
                               moran_results_100m)

###################################################################################################
#                   10. Variogram diagnostics on observed grid-cell ER values
###################################################################################################

variogram_results_500m <- make_variogram_table(points_sf = observed_points_500m,
                                               columns = grid_er_columns,
                                               labels = grid_er_labels,
                                               level_name = "500m observed grid cells")

variogram_results_100m <- make_variogram_table(points_sf = observed_points_100m,
                                               columns = grid_er_columns,
                                               labels = grid_er_labels,
                                               level_name = "100m observed grid cells")

variogram_results_all <- bind_rows(variogram_results_500m,
                                   variogram_results_100m)

variogram_results_all

ggplot(variogram_results_all,
       aes(x = dist,
           y = gamma)) +
  geom_point() +
  geom_line() +
  facet_grid(er_dimension ~ level,
             scales = "free_y") +
  labs(title = "Grid-level variograms of weighted normalized ER values",
       subtitle = "CBS 500m and 100m observed grid-cell centroids",
       x = "Distance between observed grid-cell centroids",
       y = "Semivariance") +
  theme_classic()

###################################################################################################
#                   11. Create point-pattern objects for LOO IDW power selection
###################################################################################################

bbox_500m <- st_bbox(cbs_500m_populated)

window_500m <- spatstat.geom::owin(xrange = c(bbox_500m["xmin"],
                                              bbox_500m["xmax"]),
                                   yrange = c(bbox_500m["ymin"],
                                              bbox_500m["ymax"]))

bbox_100m <- st_bbox(cbs_100m_populated)

window_100m <- spatstat.geom::owin(xrange = c(bbox_100m["xmin"],
                                              bbox_100m["xmax"]),
                                   yrange = c(bbox_100m["ymin"],
                                              bbox_100m["ymax"]))

ppp_lr_500m <- make_ppp_object(points_sf = observed_points_500m,
                               mark_column = "ER_links_rechts_weighted",
                               window_object = window_500m)

ppp_pc_500m <- make_ppp_object(points_sf = observed_points_500m,
                               mark_column = "ER_progressief_conservatief_weighted",
                               window_object = window_500m)

ppp_euclidean_500m <- make_ppp_object(points_sf = observed_points_500m,
                                      mark_column = "ER_2d_euclidean_weighted",
                                      window_object = window_500m)

ppp_lr_100m <- make_ppp_object(points_sf = observed_points_100m,
                               mark_column = "ER_links_rechts_weighted",
                               window_object = window_100m)

ppp_pc_100m <- make_ppp_object(points_sf = observed_points_100m,
                               mark_column = "ER_progressief_conservatief_weighted",
                               window_object = window_100m)

ppp_euclidean_100m <- make_ppp_object(points_sf = observed_points_100m,
                                      mark_column = "ER_2d_euclidean_weighted",
                                      window_object = window_100m)

###################################################################################################
#                   12. Select IDW decay power using leave-one-out MSE
###################################################################################################

# For us to select an appropriate power decay value, we can use the objects that we've created and run 
# LOO MSE measurement on observed values. As mentioned, we won't limit the prediction to any k number of neighbors. 

powers <- seq(0.1,
              6,
              by = 0.1)

idw_power_results_lr_500m <- calculate_power_curve(ppp_object = ppp_lr_500m,
                                                   powers = powers,
                                                   grid_size = "500m",
                                                   er_dimension = "Links-Rechts")

idw_power_results_pc_500m <- calculate_power_curve(ppp_object = ppp_pc_500m,
                                                   powers = powers,
                                                   grid_size = "500m",
                                                   er_dimension = "Progressief-Conservatief")

idw_power_results_euclidean_500m <- calculate_power_curve(ppp_object = ppp_euclidean_500m,
                                                          powers = powers,
                                                          grid_size = "500m",
                                                          er_dimension = "2D Euclidean")

idw_power_results_lr_100m <- calculate_power_curve(ppp_object = ppp_lr_100m,
                                                   powers = powers,
                                                   grid_size = "100m",
                                                   er_dimension = "Links-Rechts")

idw_power_results_pc_100m <- calculate_power_curve(ppp_object = ppp_pc_100m,
                                                   powers = powers,
                                                   grid_size = "100m",
                                                   er_dimension = "Progressief-Conservatief")

idw_power_results_euclidean_100m <- calculate_power_curve(ppp_object = ppp_euclidean_100m,
                                                          powers = powers,
                                                          grid_size = "100m",
                                                          er_dimension = "2D Euclidean")

idw_power_results_all <- bind_rows(idw_power_results_lr_500m,
                                   idw_power_results_pc_500m,
                                   idw_power_results_euclidean_500m,
                                   idw_power_results_lr_100m,
                                   idw_power_results_pc_100m,
                                   idw_power_results_euclidean_100m)

optimal_power_results <- idw_power_results_all |>
  group_by(grid_size,
           er_dimension) |>
  slice_min(mse,
            n = 1,
            with_ties = FALSE) |>
  ungroup()

optimal_power_results

ggplot(idw_power_results_all,
       aes(x = power,
           y = mse)) +
  geom_line() +
  facet_grid(er_dimension ~ grid_size,
             scales = "free_y") +
  labs(title = "IDW power selection by leave-one-out MSE",
       subtitle = "Weighted ER values assigned to populated CBS grid cells",
       x = "IDW power / decay factor",
       y = "Mean squared error") +
  theme_classic()

###################################################################################################
#                   13. Extract selected decay powers
###################################################################################################

optimal_power_lr_500m <- optimal_power_results |>
  filter(grid_size == "500m",
         er_dimension == "Links-Rechts") |>
  pull(power)

optimal_power_pc_500m <- optimal_power_results |>
  filter(grid_size == "500m",
         er_dimension == "Progressief-Conservatief") |>
  pull(power)

optimal_power_euclidean_500m <- optimal_power_results |>
  filter(grid_size == "500m",
         er_dimension == "2D Euclidean") |>
  pull(power)

optimal_power_lr_100m <- optimal_power_results |>
  filter(grid_size == "100m",
         er_dimension == "Links-Rechts") |>
  pull(power)

optimal_power_pc_100m <- optimal_power_results |>
  filter(grid_size == "100m",
         er_dimension == "Progressief-Conservatief") |>
  pull(power)

optimal_power_euclidean_100m <- optimal_power_results |>
  filter(grid_size == "100m",
         er_dimension == "2D Euclidean") |>
  pull(power)

###################################################################################################
#                   14. Final LOO residuals and final error table
###################################################################################################

# We save the results for later analysis if needed

loo_residuals_lr_500m <- create_loo_residuals(points_sf = observed_points_500m,
                                              ppp_object = ppp_lr_500m,
                                              id_column = "crs28992res500m",
                                              grid_size = "500m",
                                              er_dimension = "Links-Rechts",
                                              selected_power = optimal_power_lr_500m)

loo_residuals_pc_500m <- create_loo_residuals(points_sf = observed_points_500m,
                                              ppp_object = ppp_pc_500m,
                                              id_column = "crs28992res500m",
                                              grid_size = "500m",
                                              er_dimension = "Progressief-Conservatief",
                                              selected_power = optimal_power_pc_500m)

loo_residuals_euclidean_500m <- create_loo_residuals(points_sf = observed_points_500m,
                                                     ppp_object = ppp_euclidean_500m,
                                                     id_column = "crs28992res500m",
                                                     grid_size = "500m",
                                                     er_dimension = "2D Euclidean",
                                                     selected_power = optimal_power_euclidean_500m)

loo_residuals_lr_100m <- create_loo_residuals(points_sf = observed_points_100m,
                                              ppp_object = ppp_lr_100m,
                                              id_column = "crs28992res100m",
                                              grid_size = "100m",
                                              er_dimension = "Links-Rechts",
                                              selected_power = optimal_power_lr_100m)

loo_residuals_pc_100m <- create_loo_residuals(points_sf = observed_points_100m,
                                              ppp_object = ppp_pc_100m,
                                              id_column = "crs28992res100m",
                                              grid_size = "100m",
                                              er_dimension = "Progressief-Conservatief",
                                              selected_power = optimal_power_pc_100m)

loo_residuals_euclidean_100m <- create_loo_residuals(points_sf = observed_points_100m,
                                                     ppp_object = ppp_euclidean_100m,
                                                     id_column = "crs28992res100m",
                                                     grid_size = "100m",
                                                     er_dimension = "2D Euclidean",
                                                     selected_power = optimal_power_euclidean_100m)

loo_residuals_500m <- bind_rows(loo_residuals_lr_500m,
                                loo_residuals_pc_500m,
                                loo_residuals_euclidean_500m)

loo_residuals_100m <- bind_rows(loo_residuals_lr_100m,
                                loo_residuals_pc_100m,
                                loo_residuals_euclidean_100m)

loo_residuals_all <- bind_rows(loo_residuals_500m,
                               loo_residuals_100m)

final_idw_error_results <- loo_residuals_all |>
  st_drop_geometry() |>
  group_by(grid_size,
           er_dimension,
           selected_power) |>
  summarise(n_observations = n(),
            mse = mean(squared_residual, na.rm = TRUE),
            rmse = sqrt(mean(squared_residual, na.rm = TRUE)),
            mae = mean(absolute_residual, na.rm = TRUE),
            mean_error = mean(residual, na.rm = TRUE),
            correlation = cor(observed_er,
                              loo_predicted_er,
                              use = "complete.obs"),
            .groups = "drop")

###################################################################################################
#                   15. Residual and correlation diagnostics
###################################################################################################

residual_summary_by_stembureaus <- loo_residuals_all |>
  st_drop_geometry() |>
  mutate(stembureau_group = ifelse(n_stembureaus == 1,
                                   "One stembureau in grid cell",
                                   "Multiple stembureaus in grid cell")) |>
  group_by(grid_size,
           er_dimension,
           stembureau_group) |>
  summarise(n_cells = n(),
            mean_residual = mean(residual, na.rm = TRUE),
            mean_absolute_residual = mean(absolute_residual, na.rm = TRUE),
            rmse = sqrt(mean(squared_residual, na.rm = TRUE)),
            .groups = "drop")


ggplot(loo_residuals_all |>
         st_drop_geometry(),
       aes(x = observed_er,
           y = loo_predicted_er)) +
  geom_point(alpha = 0.35) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed") +
  facet_grid(er_dimension ~ grid_size,
             scales = "free") +
  labs(title = "Observed versus leave-one-out IDW-predicted ER values",
       subtitle = "Points closer to the dashed line have lower prediction error",
       x = "Observed weighted ER value",
       y = "LOO-predicted ER value") +
  theme_classic()

ggplot(loo_residuals_all |>
         st_drop_geometry(),
       aes(x = residual)) +
  geom_histogram(bins = 40) +
  facet_grid(er_dimension ~ grid_size,
             scales = "free") +
  labs(title = "Distribution of leave-one-out IDW residuals",
       x = "Residual: observed ER minus LOO-predicted ER",
       y = "Number of observed grid cells") +
  theme_classic()

ggplot(loo_residuals_all |>
         st_drop_geometry(),
       aes(x = loo_predicted_er,
           y = residual)) +
  geom_point(alpha = 0.35) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  facet_grid(er_dimension ~ grid_size,
             scales = "free") +
  labs(title = "LOO residuals against predicted ER values",
       x = "LOO-predicted ER value",
       y = "Residual: observed ER minus LOO-predicted ER") +
  theme_classic()


###################################################################################################
#                   16. Final global IDW interpolation
###################################################################################################

# IN here create the interpolation with the selected power decay value, and with the specified ideological dimension

idw_lr_500m <- run_final_idw(observed_points = observed_points_500m,
                             prediction_points = prediction_points_500m,
                             observed_column = "ER_links_rechts_weighted",
                             selected_power = optimal_power_lr_500m)

idw_pc_500m <- run_final_idw(observed_points = observed_points_500m,
                             prediction_points = prediction_points_500m,
                             observed_column = "ER_progressief_conservatief_weighted",
                             selected_power = optimal_power_pc_500m)

idw_euclidean_500m <- run_final_idw(observed_points = observed_points_500m,
                                    prediction_points = prediction_points_500m,
                                    observed_column = "ER_2d_euclidean_weighted",
                                    selected_power = optimal_power_euclidean_500m)

idw_lr_100m <- run_final_idw(observed_points = observed_points_100m,
                             prediction_points = prediction_points_100m,
                             observed_column = "ER_links_rechts_weighted",
                             selected_power = optimal_power_lr_100m)

idw_pc_100m <- run_final_idw(observed_points = observed_points_100m,
                             prediction_points = prediction_points_100m,
                             observed_column = "ER_progressief_conservatief_weighted",
                             selected_power = optimal_power_pc_100m)

idw_euclidean_100m <- run_final_idw(observed_points = observed_points_100m,
                                    prediction_points = prediction_points_100m,
                                    observed_column = "ER_2d_euclidean_weighted",
                                    selected_power = optimal_power_euclidean_100m)

###################################################################################################
#                   17. Attach final predictions back to CBS grid polygons
###################################################################################################

cbs_500m_idw <- cbs_500m_populated |>
  mutate(IDW_links_rechts = idw_lr_500m$var1.pred,
         IDW_progressief_conservatief = idw_pc_500m$var1.pred,
         IDW_2d_euclidean = idw_euclidean_500m$var1.pred,
         distance_to_nearest_observed_m = prediction_points_500m$distance_to_nearest_observed_m) |>
  left_join(grid_scores_500m,
            by = "crs28992res500m")

cbs_100m_idw <- cbs_100m_populated |>
  mutate(IDW_links_rechts = idw_lr_100m$var1.pred,
         IDW_progressief_conservatief = idw_pc_100m$var1.pred,
         IDW_2d_euclidean = idw_euclidean_100m$var1.pred,
         distance_to_nearest_observed_m = prediction_points_100m$distance_to_nearest_observed_m) |>
  left_join(grid_scores_100m,
            by = "crs28992res100m")

###################################################################################################
#                   18. Observed versus IDW-predicted values
###################################################################################################

range_checks <- bind_rows(summarise_range(values = observed_points_500m$ER_links_rechts_weighted,
                                          level_name = "500m",
                                          er_dimension = "Links-Rechts",
                                          value_type = "Observed weighted grid-cell ER"),
                          summarise_range(values = cbs_500m_idw$IDW_links_rechts,
                                          level_name = "500m",
                                          er_dimension = "Links-Rechts",
                                          value_type = "Global IDW prediction"),
                          summarise_range(values = observed_points_500m$ER_progressief_conservatief_weighted,
                                          level_name = "500m",
                                          er_dimension = "Progressief-Conservatief",
                                          value_type = "Observed weighted grid-cell ER"),
                          summarise_range(values = cbs_500m_idw$IDW_progressief_conservatief,
                                          level_name = "500m",
                                          er_dimension = "Progressief-Conservatief",
                                          value_type = "Global IDW prediction"),
                          summarise_range(values = observed_points_500m$ER_2d_euclidean_weighted,
                                          level_name = "500m",
                                          er_dimension = "2D Euclidean",
                                          value_type = "Observed weighted grid-cell ER"),
                          summarise_range(values = cbs_500m_idw$IDW_2d_euclidean,
                                          level_name = "500m",
                                          er_dimension = "2D Euclidean",
                                          value_type = "Global IDW prediction"),
                          summarise_range(values = observed_points_100m$ER_links_rechts_weighted,
                                          level_name = "100m",
                                          er_dimension = "Links-Rechts",
                                          value_type = "Observed weighted grid-cell ER"),
                          summarise_range(values = cbs_100m_idw$IDW_links_rechts,
                                          level_name = "100m",
                                          er_dimension = "Links-Rechts",
                                          value_type = "Global IDW prediction"),
                          summarise_range(values = observed_points_100m$ER_progressief_conservatief_weighted,
                                          level_name = "100m",
                                          er_dimension = "Progressief-Conservatief",
                                          value_type = "Observed weighted grid-cell ER"),
                          summarise_range(values = cbs_100m_idw$IDW_progressief_conservatief,
                                          level_name = "100m",
                                          er_dimension = "Progressief-Conservatief",
                                          value_type = "Global IDW prediction"),
                          summarise_range(values = observed_points_100m$ER_2d_euclidean_weighted,
                                          level_name = "100m",
                                          er_dimension = "2D Euclidean",
                                          value_type = "Observed weighted grid-cell ER"),
                          summarise_range(values = cbs_100m_idw$IDW_2d_euclidean,
                                          level_name = "100m",
                                          er_dimension = "2D Euclidean",
                                          value_type = "Global IDW prediction"))

###################################################################################################
#                   Final IDW maps: 500m and 100m
#                   Links-Rechts, Progressief-Conservatief, and 2D Euclidean
###################################################################################################

###################################################################################################
#                   1. Define map limits and breaks: 500m
###################################################################################################

lr_500_limits <- range(cbs_500m_idw$IDW_links_rechts,
                       na.rm = TRUE)

lr_500_breaks <- seq(lr_500_limits[1],
                     lr_500_limits[2],
                     length.out = 3)

pc_500_limits <- range(cbs_500m_idw$IDW_progressief_conservatief,
                       na.rm = TRUE)

pc_500_breaks <- seq(pc_500_limits[1],
                     pc_500_limits[2],
                     length.out = 3)

euclidean_500_limits <- range(cbs_500m_idw$IDW_2d_euclidean,
                              na.rm = TRUE)

euclidean_500_breaks <- seq(euclidean_500_limits[1],
                            euclidean_500_limits[2],
                            length.out = 3)

###################################################################################################
#                   2. Define map limits and breaks: 100m
###################################################################################################

lr_100_limits <- range(cbs_100m_idw$IDW_links_rechts,
                       na.rm = TRUE)

lr_100_breaks <- seq(lr_100_limits[1],
                     lr_100_limits[2],
                     length.out = 3)

pc_100_limits <- range(cbs_100m_idw$IDW_progressief_conservatief,
                       na.rm = TRUE)

pc_100_breaks <- seq(pc_100_limits[1],
                     pc_100_limits[2],
                     length.out = 3)

euclidean_100_limits <- range(cbs_100m_idw$IDW_2d_euclidean,
                              na.rm = TRUE)

euclidean_100_breaks <- seq(euclidean_100_limits[1],
                            euclidean_100_limits[2],
                            length.out = 3)

###################################################################################################
#                   Load Netherlands province boundaries and national outline
###################################################################################################
nl_provinces <- st_read("cbsgebiedsindelingen2025.gpkg",
                        layer = "provincie_gegeneraliseerd",
                        quiet = TRUE)

nl_provinces <- nl_provinces |>
  st_transform(st_crs(cbs_500m_idw))

nl_outline <- st_union(st_geometry(nl_provinces)) |>
  st_as_sf()

st_crs(nl_outline) <- st_crs(nl_provinces)

###################################################################################################
#                   3. 500m map: Links-Rechts
###################################################################################################

ggplot(cbs_500m_idw) +
  geom_sf(aes(fill = IDW_links_rechts),
          color = NA) +
  geom_sf(data = nl_outline,
          fill = NA,
          color = "black",
          linewidth = 0.35)+
  scale_fill_gradient(name = "IDW ER",
                      low = "darkgray",
                      high = "red",
                      limits = lr_500_limits,
                      breaks = lr_500_breaks,
                      labels = round(lr_500_breaks, 2),
                      na.value = "white") +
  labs(title = "IDW interpolation of Links-Rechts ER",
       subtitle = "CBS 500m populated grid cells") +
  theme_void()

###################################################################################################
#                   4. 500m map: Progressief-Conservatief
###################################################################################################

ggplot(cbs_500m_idw) +
  geom_sf(aes(fill = IDW_progressief_conservatief),
          color = NA) +
  geom_sf(data = nl_outline,
          fill = NA,
          color = "black",
          linewidth = 0.35)+
  scale_fill_gradient(name = "IDW ER",
                      low = "darkgray",
                      high = "blue",
                      limits = pc_500_limits,
                      breaks = pc_500_breaks,
                      labels = round(pc_500_breaks, 2),
                      na.value = "white") +
  labs(title = "IDW interpolation of Progressief-Conservatief ER",
       subtitle = "CBS 500m populated grid cells") +
  theme_void()

###################################################################################################
#                   5. 500m map: 2D Euclidean
###################################################################################################

ggplot(cbs_500m_idw) +
  geom_sf(aes(fill = IDW_2d_euclidean),
          color = NA) +
  geom_sf(data = nl_outline,
          fill = NA,
          color = "black",
          linewidth = 0.35)+
  scale_fill_gradient(name = "IDW ER",
                      low = "darkgray",
                      high = "orange",
                      limits = euclidean_500_limits,
                      breaks = euclidean_500_breaks,
                      labels = round(euclidean_500_breaks, 2),
                      na.value = "white") +
  labs(title = "IDW interpolation of 2D Euclidean ER",
       subtitle = "CBS 500m populated grid cells") +
  theme_void()

###################################################################################################
#                   6. 100m map: Links-Rechts
###################################################################################################

ggplot(cbs_100m_idw) +
  geom_sf(aes(fill = IDW_links_rechts),
          color = NA) +
  geom_sf(data = nl_outline,
          fill = NA,
          color = "black",
          linewidth = 0.35)+
  scale_fill_gradient(name = "IDW ER",
                      low = "darkgray",
                      high = "red",
                      limits = lr_100_limits,
                      breaks = lr_100_breaks,
                      labels = round(lr_100_breaks, 2),
                      na.value = "white") +
  labs(title = "IDW interpolation of Links-Rechts ER",
       subtitle = "CBS 100m populated grid cells") +
  theme_void()

###################################################################################################
#                   7. 100m map: Progressief-Conservatief
###################################################################################################

ggplot(cbs_100m_idw) +
  geom_sf(aes(fill = IDW_progressief_conservatief),
          color = NA) +
  geom_sf(data = nl_outline,
          fill = NA,
          color = "black",
          linewidth = 0.35)+
  scale_fill_gradient(name = "IDW ER",
                      low = "darkgray",
                      high = "blue",
                      limits = pc_100_limits,
                      breaks = pc_100_breaks,
                      labels = round(pc_100_breaks, 2),
                      na.value = "white") +
  labs(title = "IDW interpolation of Progressief-Conservatief ER",
       subtitle = "CBS 100m populated grid cells") +
  theme_void()

###################################################################################################
#                   8. 100m map: 2D Euclidean
###################################################################################################

ggplot(cbs_100m_idw) +
  geom_sf(aes(fill = IDW_2d_euclidean),
          color = NA) +
  geom_sf(data = nl_outline,
          fill = NA,
          color = "black",
          linewidth = 0.35)+
  scale_fill_gradient(name = "IDW ER",
                      low = "darkgray",
                      high = "orange",
                      limits = euclidean_100_limits,
                      breaks = euclidean_100_breaks,
                      labels = round(euclidean_100_breaks, 2),
                      na.value = "white") +
  labs(title = "IDW interpolation of 2D Euclidean ER",
       subtitle = "CBS 100m populated grid cells") +
  theme_void()
###################################################################################################
#                   8 Optional saving
###################################################################################################

# Although not necessary, we highly recommend running this part sothat the whole code doesn't have to be rerun from scratch.

 write_csv(moran_results_all,
           "moran_results_idw_diagnostics.csv")

 write_csv(variogram_results_all,
           "variogram_results_idw_diagnostics.csv")

 write_csv(idw_power_results_all,
           "idw_power_results_all.csv")

 write_csv(optimal_power_results,
           "optimal_power_results.csv")

 write_csv(final_idw_error_results,
           "final_idw_error_results.csv")

 write_csv(range_checks,
           "idw_range_checks.csv")

 write_csv(residual_summary_by_stembureaus,
           "residual_summary_by_stembureaus.csv")

 write_csv(loo_residuals_500m |>
             st_drop_geometry(),
           "loo_residuals_500m.csv")

 write_csv(loo_residuals_100m |>
             st_drop_geometry(),
           "loo_residuals_100m.csv")

 st_write(cbs_500m_idw,
          "cbs_500m_idw.gpkg",
          delete_dsn = TRUE)

 st_write(cbs_100m_idw, "cbs_100m_idw.gpkg",
         delete_dsn = TRUE)