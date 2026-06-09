###################################################################################################
#                   Part 8: Kullback-Leibler divergence between vote-share compositions
###################################################################################################

library(readr)
library(dplyr)
library(tidyr)

###################################################################################################
#                   1. Load party-level vote-count tables
###################################################################################################

gemeente_party_votes <- readr::read_csv("gemeente_party_votes.csv",
                                        show_col_types = FALSE) |>
  dplyr::select(-dplyr::any_of(c("...1", ".1")))

corop_party_votes <- readr::read_csv("corop_party_votes.csv",
                                     show_col_types = FALSE) |>
  dplyr::select(-dplyr::any_of(c("...1", ".1")))

province_party_votes <- readr::read_csv("province_party_votes.csv",
                                        show_col_types = FALSE) |>
  dplyr::select(-dplyr::any_of(c("...1", ".1")))

national_party_votes <- readr::read_csv("national_party_votes.csv",
                                        show_col_types = FALSE) |>
  dplyr::select(-dplyr::any_of(c("...1", ".1")))

###################################################################################################
#                   2. Define party list and smoothing constant
###################################################################################################

# KL divergence cannot handle cases where q_i = 0 and p_i > 0.
# Therefore, a small pseudo-count is added before converting vote counts into shares.
# total_votes remains the raw observed vote total; smoothing is applied only to the KL composition.

delta_count <- 0.5

party_list <- sort(unique(c(gemeente_party_votes$party_kieskompas,
                            corop_party_votes$party_kieskompas,
                            province_party_votes$party_kieskompas,
                            national_party_votes$party_kieskompas)))

###################################################################################################
#                   3. Helper function: create smoothed vote-share compositions
###################################################################################################

# This function is created because the same compositional transformation has to be applied to the gemeente,
# COROP, province, and national vote-count tables. The function first makes sure that every area contains all
# parties in party_list, even if a party received zero votes in that area. This is necessary because the KL
# divergence calculations compare party-share vectors, and those vectors must have the same parties in the same
# order at every administrative level.


# The grouping_vars argument identifies the level at which the composition is created, such as GemeenteCode,
# corop_name, province_name, or country_name. The id_vars argument keeps additional descriptive variables, such
# as municipality, COROP, or province names, so that the output can still be interpreted after the calculation.
# The pseudo-count is added inside this function before vote shares are calculated.


make_composition_table <- function(data, grouping_vars, id_vars = NULL) {
  
  id_columns <- c(grouping_vars,
                  id_vars)
  
  data_summed <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_columns,
                                                  "party_kieskompas")))) |>
    dplyr::summarise(AantalStemmen = sum(AantalStemmen, na.rm = TRUE),
                     .groups = "drop")
  
  area_keys <- data_summed |>
    dplyr::distinct(dplyr::across(dplyr::all_of(id_columns)))
  
  full_grid <- tidyr::expand_grid(area_keys,
                                  party_kieskompas = party_list)
  
  full_grid |>
    dplyr::left_join(data_summed,
                     by = c(id_columns,
                            "party_kieskompas")) |>
    dplyr::mutate(AantalStemmen = tidyr::replace_na(AantalStemmen, 0)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_columns))) |>
    dplyr::mutate(total_votes = sum(AantalStemmen, na.rm = TRUE),
                  vote_share = (AantalStemmen + delta_count) /
                    (total_votes + delta_count * length(party_list))) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(id_columns),
                  party_kieskompas,
                  vote_share,
                  total_votes) |>
    tidyr::pivot_wider(names_from = party_kieskompas,
                       values_from = vote_share)
}

###################################################################################################
#                   4. Helper function: calculate KL divergence
###################################################################################################

calculate_kl <- function(p_vector, q_vector) {
  
  p_vector <- as.numeric(p_vector)
  q_vector <- as.numeric(q_vector)
  
  if (any(is.na(p_vector)) | any(is.na(q_vector))) {
    return(NA_real_)
  }
  
  if (any(p_vector <= 0) | any(q_vector <= 0)) {
    stop("KL divergence requires strictly positive values. Check smoothing.")
  }
  
  sum(p_vector * log(p_vector / q_vector))
}

###################################################################################################
#                   5. Create composition tables at each administrative level
###################################################################################################

gemeente_compositions <- make_composition_table(data = gemeente_party_votes,
                                                grouping_vars = "GemeenteCode",
                                                id_vars = c("GemeenteNaam",
                                                            "gemeente_name_spatial",
                                                            "corop_name",
                                                            "province_name"))

corop_compositions <- make_composition_table(data = corop_party_votes,
                                             grouping_vars = "corop_name",
                                             id_vars = "province_name")

province_compositions <- make_composition_table(data = province_party_votes,
                                                grouping_vars = "province_name")

national_compositions <- make_composition_table(data = national_party_votes,
                                                grouping_vars = "country_name")

party_columns <- party_list

###################################################################################################
#                   6. Sanity checks: party shares must sum to 1
###################################################################################################

stopifnot(all(abs(rowSums(gemeente_compositions[, party_columns]) - 1) < 1e-9))
stopifnot(all(abs(rowSums(corop_compositions[, party_columns]) - 1) < 1e-9))
stopifnot(all(abs(rowSums(province_compositions[, party_columns]) - 1) < 1e-9))
stopifnot(all(abs(rowSums(national_compositions[, party_columns]) - 1) < 1e-9))

stopifnot(nrow(national_compositions) == 1)

national_vector <- national_compositions |>
  dplyr::select(dplyr::all_of(party_columns)) |>
  dplyr::slice(1) |>
  as.numeric()

###################################################################################################
#                   7. Direction of KL divergence
###################################################################################################

# Directional KL is calculated as KL(target || reference).
# For gemeente/COROP/province comparisons, the smaller administrative unit is treated as the target
# and the broader administrative unit is treated as the reference.
#
# Examples:
#   KL(gemeente || COROP)
#   KL(gemeente || province)
#   KL(gemeente || nation)
#   KL(COROP || province)
#   KL(COROP || nation)
#   KL(province || nation)
#
# For province-to-province comparisons, there is no natural broader reference unit.
# Therefore, both directions are calculated and then averaged:
#   symmetric KL = 0.5 * KL(A || B) + 0.5 * KL(B || A)

###################################################################################################
#                   8. Gemeente vs COROP
###################################################################################################

gemeente_vs_corop_data <- gemeente_compositions |>
  dplyr::left_join(corop_compositions |>
                     dplyr::select(corop_name,
                                   dplyr::all_of(party_columns)) |>
                     dplyr::rename_with(~ paste0("ref_", .x),
                                        dplyr::all_of(party_columns)),
                   by = "corop_name")

stopifnot(!any(is.na(gemeente_vs_corop_data[, paste0("ref_", party_columns)])))

gemeente_vs_corop <- gemeente_vs_corop_data |>
  dplyr::rowwise() |>
  dplyr::mutate(kl_gemeente_vs_corop = calculate_kl(dplyr::c_across(dplyr::all_of(party_columns)),
                                                    dplyr::c_across(dplyr::all_of(paste0("ref_", party_columns))))) |>
  dplyr::ungroup() |>
  dplyr::select(GemeenteCode,
                GemeenteNaam,
                gemeente_name_spatial,
                corop_name,
                province_name,
                total_votes,
                kl_gemeente_vs_corop)

###################################################################################################
#                   9. Gemeente vs province
###################################################################################################

gemeente_vs_province_data <- gemeente_compositions |>
  dplyr::left_join(province_compositions |>
                     dplyr::select(province_name,
                                   dplyr::all_of(party_columns)) |>
                     dplyr::rename_with(~ paste0("ref_", .x),
                                        dplyr::all_of(party_columns)),
                   by = "province_name")

stopifnot(!any(is.na(gemeente_vs_province_data[, paste0("ref_", party_columns)])))

gemeente_vs_province <- gemeente_vs_province_data |>
  dplyr::rowwise() |>
  dplyr::mutate(kl_gemeente_vs_province = calculate_kl(dplyr::c_across(dplyr::all_of(party_columns)),
                                                       dplyr::c_across(dplyr::all_of(paste0("ref_", party_columns))))) |>
  dplyr::ungroup() |>
  dplyr::select(GemeenteCode,
                GemeenteNaam,
                gemeente_name_spatial,
                corop_name,
                province_name,
                total_votes,
                kl_gemeente_vs_province)

###################################################################################################
#                   10. Gemeente vs nation
###################################################################################################

gemeente_vs_nation <- gemeente_compositions |>
  dplyr::rowwise() |>
  dplyr::mutate(kl_gemeente_vs_nation = calculate_kl(dplyr::c_across(dplyr::all_of(party_columns)),
                                                     national_vector)) |>
  dplyr::ungroup() |>
  dplyr::select(GemeenteCode,
                GemeenteNaam,
                gemeente_name_spatial,
                corop_name,
                province_name,
                total_votes,
                kl_gemeente_vs_nation)

###################################################################################################
#                   11. Combine gemeente KL results
###################################################################################################

kl_gemeente <- gemeente_vs_corop |>
  dplyr::left_join(gemeente_vs_province |>
                     dplyr::select(GemeenteCode,
                                   kl_gemeente_vs_province),
                   by = "GemeenteCode") |>
  dplyr::left_join(gemeente_vs_nation |>
                     dplyr::select(GemeenteCode,
                                   kl_gemeente_vs_nation),
                   by = "GemeenteCode")

###################################################################################################
#                   12. COROP vs province
###################################################################################################

corop_vs_province_data <- corop_compositions |>
  dplyr::left_join(province_compositions |>
                     dplyr::select(province_name,
                                   dplyr::all_of(party_columns)) |>
                     dplyr::rename_with(~ paste0("ref_", .x),
                                        dplyr::all_of(party_columns)),
                   by = "province_name")

stopifnot(!any(is.na(corop_vs_province_data[, paste0("ref_", party_columns)])))

corop_vs_province <- corop_vs_province_data |>
  dplyr::rowwise() |>
  dplyr::mutate(kl_corop_vs_province = calculate_kl(dplyr::c_across(dplyr::all_of(party_columns)),
                                                    dplyr::c_across(dplyr::all_of(paste0("ref_", party_columns))))) |>
  dplyr::ungroup() |>
  dplyr::select(corop_name,
                province_name,
                total_votes,
                kl_corop_vs_province)

###################################################################################################
#                   13. COROP vs nation
###################################################################################################

corop_vs_nation <- corop_compositions |>
  dplyr::rowwise() |>
  dplyr::mutate(kl_corop_vs_nation = calculate_kl(dplyr::c_across(dplyr::all_of(party_columns)),
                                                  national_vector)) |>
  dplyr::ungroup() |>
  dplyr::select(corop_name,
                province_name,
                total_votes,
                kl_corop_vs_nation)

###################################################################################################
#                   14. Combine COROP KL results
###################################################################################################

kl_corop <- corop_vs_province |>
  dplyr::left_join(corop_vs_nation |>
                     dplyr::select(corop_name,
                                   kl_corop_vs_nation),
                   by = "corop_name")

###################################################################################################
#                   15. Province vs nation
###################################################################################################

kl_province_vs_nation <- province_compositions |>
  dplyr::rowwise() |>
  dplyr::mutate(kl_province_vs_nation = calculate_kl(dplyr::c_across(dplyr::all_of(party_columns)),
                                                     national_vector)) |>
  dplyr::ungroup() |>
  dplyr::select(province_name,
                total_votes,
                kl_province_vs_nation)

###################################################################################################
#                   16. Province vs province pairwise KL
###################################################################################################

province_a <- province_compositions |>
  dplyr::select(province_name,
                total_votes,
                dplyr::all_of(party_columns)) |>
  dplyr::rename(province_a = province_name,
                total_votes_a = total_votes) |>
  dplyr::rename_with(~ paste0("a_", .x),
                     dplyr::all_of(party_columns))

province_b <- province_compositions |>
  dplyr::select(province_name,
                total_votes,
                dplyr::all_of(party_columns)) |>
  dplyr::rename(province_b = province_name,
                total_votes_b = total_votes) |>
  dplyr::rename_with(~ paste0("b_", .x),
                     dplyr::all_of(party_columns))

kl_province_pairwise <- merge(province_a,
                              province_b,
                              by = NULL) |>
  dplyr::filter(province_a != province_b) |>
  dplyr::rowwise() |>
  dplyr::mutate(kl_a_vs_b = calculate_kl(dplyr::c_across(dplyr::all_of(paste0("a_", party_columns))),
                                         dplyr::c_across(dplyr::all_of(paste0("b_", party_columns)))),
                kl_b_vs_a = calculate_kl(dplyr::c_across(dplyr::all_of(paste0("b_", party_columns))),
                                         dplyr::c_across(dplyr::all_of(paste0("a_", party_columns)))),
                kl_symmetric = 0.5 * kl_a_vs_b + 0.5 * kl_b_vs_a) |>
  dplyr::ungroup() |>
  dplyr::select(province_a,
                province_b,
                total_votes_a,
                total_votes_b,
                kl_a_vs_b,
                kl_b_vs_a,
                kl_symmetric)

###################################################################################################
#                   17. Province vs province unique unordered pairs
###################################################################################################

kl_province_pairwise_unique <- kl_province_pairwise |>
  dplyr::filter(province_a < province_b) |>
  dplyr::arrange(dplyr::desc(kl_symmetric))

###################################################################################################
#                   18. Summary tables
###################################################################################################

kl_gemeente_summary <- kl_gemeente |>
  dplyr::summarise(mean_kl_gemeente_vs_corop = mean(kl_gemeente_vs_corop, na.rm = TRUE),
                   median_kl_gemeente_vs_corop = median(kl_gemeente_vs_corop, na.rm = TRUE),
                   max_kl_gemeente_vs_corop = max(kl_gemeente_vs_corop, na.rm = TRUE),
                   mean_kl_gemeente_vs_province = mean(kl_gemeente_vs_province, na.rm = TRUE),
                   median_kl_gemeente_vs_province = median(kl_gemeente_vs_province, na.rm = TRUE),
                   max_kl_gemeente_vs_province = max(kl_gemeente_vs_province, na.rm = TRUE),
                   mean_kl_gemeente_vs_nation = mean(kl_gemeente_vs_nation, na.rm = TRUE),
                   median_kl_gemeente_vs_nation = median(kl_gemeente_vs_nation, na.rm = TRUE),
                   max_kl_gemeente_vs_nation = max(kl_gemeente_vs_nation, na.rm = TRUE),
                   n_gemeenten = dplyr::n())

kl_corop_summary <- kl_corop |>
  dplyr::summarise(mean_kl_corop_vs_province = mean(kl_corop_vs_province, na.rm = TRUE),
                   median_kl_corop_vs_province = median(kl_corop_vs_province, na.rm = TRUE),
                   max_kl_corop_vs_province = max(kl_corop_vs_province, na.rm = TRUE),
                   mean_kl_corop_vs_nation = mean(kl_corop_vs_nation, na.rm = TRUE),
                   median_kl_corop_vs_nation = median(kl_corop_vs_nation, na.rm = TRUE),
                   max_kl_corop_vs_nation = max(kl_corop_vs_nation, na.rm = TRUE),
                   n_corops = dplyr::n())

kl_province_summary <- kl_province_vs_nation |>
  dplyr::summarise(mean_kl_province_vs_nation = mean(kl_province_vs_nation, na.rm = TRUE),
                   median_kl_province_vs_nation = median(kl_province_vs_nation, na.rm = TRUE),
                   max_kl_province_vs_nation = max(kl_province_vs_nation, na.rm = TRUE),
                   n_provinces = dplyr::n())

kl_province_pairwise_summary <- kl_province_pairwise_unique |>
  dplyr::summarise(mean_symmetric_kl = mean(kl_symmetric, na.rm = TRUE),
                   median_symmetric_kl = median(kl_symmetric, na.rm = TRUE),
                   max_symmetric_kl = max(kl_symmetric, na.rm = TRUE),
                   n_unique_pairs = dplyr::n())

###################################################################################################
#                   19. Highest-divergence areas
###################################################################################################

top_gemeente_vs_corop <- kl_gemeente |>
  dplyr::arrange(dplyr::desc(kl_gemeente_vs_corop)) |>
  dplyr::slice_head(n = 10)

top_gemeente_vs_province <- kl_gemeente |>
  dplyr::arrange(dplyr::desc(kl_gemeente_vs_province)) |>
  dplyr::slice_head(n = 10)

top_gemeente_vs_nation <- kl_gemeente |>
  dplyr::arrange(dplyr::desc(kl_gemeente_vs_nation)) |>
  dplyr::slice_head(n = 10)

top_corop_vs_province <- kl_corop |>
  dplyr::arrange(dplyr::desc(kl_corop_vs_province)) |>
  dplyr::slice_head(n = 10)

top_corop_vs_nation <- kl_corop |>
  dplyr::arrange(dplyr::desc(kl_corop_vs_nation)) |>
  dplyr::slice_head(n = 10)

top_province_vs_nation <- kl_province_vs_nation |>
  dplyr::arrange(dplyr::desc(kl_province_vs_nation)) |>
  dplyr::slice_head(n = 10)

top_province_pairwise <- kl_province_pairwise_unique |>
  dplyr::arrange(dplyr::desc(kl_symmetric)) |>
  dplyr::slice_head(n = 10)


###################################################################################################
#                   20. Save outputs
###################################################################################################

# this part is optional, however, given how long the code can take to run, this is recommended if individual
# objects want to be looked at later. 

dir.create("kl_outputs",
           showWarnings = FALSE)

readr::write_csv(kl_gemeente,
                 "kl_outputs/kl_gemeente.csv")

readr::write_csv(kl_corop,
                 "kl_outputs/kl_corop.csv")

readr::write_csv(kl_province_vs_nation,
                 "kl_outputs/kl_province_vs_nation.csv")

readr::write_csv(kl_province_pairwise,
                 "kl_outputs/kl_province_pairwise_ordered.csv")

readr::write_csv(kl_province_pairwise_unique,
                 "kl_outputs/kl_province_pairwise_unique.csv")

readr::write_csv(kl_gemeente_summary,
                 "kl_outputs/kl_gemeente_summary.csv")

readr::write_csv(kl_corop_summary,
                 "kl_outputs/kl_corop_summary.csv")

readr::write_csv(kl_province_summary,
                 "kl_outputs/kl_province_summary.csv")

readr::write_csv(kl_province_pairwise_summary,
                 "kl_outputs/kl_province_pairwise_summary.csv")

readr::write_csv(top_gemeente_vs_corop,
                 "kl_outputs/top_gemeente_vs_corop.csv")

readr::write_csv(top_gemeente_vs_province,
                 "kl_outputs/top_gemeente_vs_province.csv")

readr::write_csv(top_gemeente_vs_nation,
                 "kl_outputs/top_gemeente_vs_nation.csv")

readr::write_csv(top_corop_vs_province,
                 "kl_outputs/top_corop_vs_province.csv")

readr::write_csv(top_corop_vs_nation,
                 "kl_outputs/top_corop_vs_nation.csv")

readr::write_csv(top_province_vs_nation,
                 "kl_outputs/top_province_vs_nation.csv")

readr::write_csv(top_province_pairwise,
                 "kl_outputs/top_province_pairwise.csv")