###################################################################################################
#                   Mapping ER values by administrative level and IDW
###################################################################################################

library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)

###################################################################################################
#                        Loading the data
###################################################################################################

er_gemeente <- read_csv("er_gemeente.csv",
                        col_select = -1) 

er_corop <- read_csv("er_corop.csv",
                     col_select = -1)

er_province <- read_csv("er_province.csv",
                        col_select = -1) 

er_national <- read_csv("er_national.csv",
                        col_select = -1) 

er_stembureau <- read_csv("er_stembureau.csv",
                          col_select = -1)

###################################################################################################
#                         Loading the administrative boundary layers
###################################################################################################

gebied_file <- "cbsgebiedsindelingen2025.gpkg"

corop <- st_read(gebied_file,
                 layer = "coropgebied_gegeneraliseerd") |>
  select(corop_code = statcode,
         corop_name = statnaam)

province <- st_read(gebied_file,
                    layer = "provincie_gegeneraliseerd") |>
  select(province_code = statcode,
         province_name = statnaam)

gemeente <- st_read(gebied_file,
                    layer = "gemeente_niet_gegeneraliseerd") |>
  select(gemeente_code_spatial = statcode,
         gemeente_name_spatial = statnaam)

###################################################################################################
#                         Attach ER values back to the geometries
###################################################################################################

er_gemeente_sf <- gemeente |>
  left_join(er_gemeente,
            by = "gemeente_name_spatial")

er_corop_sf <- corop |>
  left_join(er_corop,
            by = "corop_name")

er_province_sf <- province |>
  left_join(er_province,
            by = "province_name")

er_national_sf <- province |>
  mutate(country_name = "Netherlands") |>
  group_by(country_name) |>
  summarise(.groups = "drop") |>
  left_join(er_national,
            by = "country_name")

###################################################################################################
#                         Define shared scale limits per ER dimension
###################################################################################################

# For the maps, we need to create the appropriate tags for the limits that will be used in each of the maps
# The best way to do this is by focusing on finding the ranges of each of the scales. As such, we do this and save them to be used later.

links_rechts_limits <- range(c(er_gemeente_sf$ER_links_rechts_norm,
                               er_corop_sf$ER_links_rechts_norm,
                               er_province_sf$ER_links_rechts_norm,
                               er_national_sf$ER_links_rechts_norm),
                             na.rm = TRUE)

progressief_conservatief_limits <- range(c(er_gemeente_sf$ER_progressief_conservatief_norm,
                                           er_corop_sf$ER_progressief_conservatief_norm,
                                           er_province_sf$ER_progressief_conservatief_norm,
                                           er_national_sf$ER_progressief_conservatief_norm),
                                         na.rm = TRUE)

euclidean_limits <- range(c(er_gemeente_sf$ER_2d_euclidean_norm,
                            er_corop_sf$ER_2d_euclidean_norm,
                            er_province_sf$ER_2d_euclidean_norm,
                            er_national_sf$ER_2d_euclidean_norm),
                          na.rm = TRUE)

###################################################################################################
#                         General formatting of maps
###################################################################################################

# We create the base mapping function that will be used for every map. Note that we've already assigned everything to the noramlized values.

make_er_map <- function(map_data, border_data = NULL, er_column, scale_limits, high_colour, title_text, subtitle_text = NULL) {
  
  base_map <- ggplot() +
    geom_sf(data = map_data,
            aes(fill = .data[[er_column]]),
            color = "grey30",
            linewidth = 0.12)
  
  if (!is.null(border_data)) {
    base_map <- base_map +
      geom_sf(data = border_data,
              fill = NA,
              color = "grey35",
              linewidth = 0.08)
  }
  
  base_map +
    scale_fill_gradient(name = "Normalized ER value",
                        low = "grey95",
                        high = high_colour,
                        limits = scale_limits,
                        breaks = c(scale_limits[1],
                                   mean(scale_limits),
                                   scale_limits[2]),
                        labels = round(c(scale_limits[1],
                                         mean(scale_limits),
                                         scale_limits[2]), 2),
                        na.value = "grey90") +
    labs(title = title_text,
         subtitle = subtitle_text) +
    theme_void() 
}

###################################################################################################
#                                  Creating mapping function
###################################################################################################


# We create the following mapping function necessary to deal with the labeling of the items. This will help us by
# cutting the code  that would otherwise be used for individual maps.

make_er_map_with_labels <- function(map_data, border_data = NULL, er_column, scale_limits, high_colour, title_text, subtitle_text, label_size = 2.6) {
  
  label_data <- map_data |>
    filter(!is.na(.data[[er_column]])) |>
    st_point_on_surface() |>
    mutate(er_label = round(.data[[er_column]], 2))
  
  base_map <- ggplot() +
    geom_sf(data = map_data,
            aes(fill = .data[[er_column]]),
            color = "grey30",
            linewidth = 0.12)
  
  if (!is.null(border_data)) {
    base_map <- base_map +
      geom_sf(data = border_data,
              fill = NA,
              color = "grey35",
              linewidth = 0.08)
  }
  
  base_map +
    geom_sf_text(data = label_data,
                 aes(label = er_label),
                 size = label_size,
                 color = "black",
                 check_overlap = TRUE) +
    scale_fill_gradient(name = "Normalized ER value",
                        low = "grey95",
                        high = high_colour,
                        limits = scale_limits,
                        breaks = c(scale_limits[1],
                                   mean(scale_limits),
                                   scale_limits[2]),
                        labels = round(c(scale_limits[1],
                                         mean(scale_limits),
                                         scale_limits[2]), 2),
                        na.value = "grey90") +
    labs(title = title_text) +
    theme_void() 
}

###################################################################################################
#                         Gemeente mapping
###################################################################################################

# The following section all refer to the creation of the maps. As such, no notes will be made.

map_gemeente_links_rechts <- make_er_map(map_data = er_gemeente_sf,
                                         border_data = gemeente,
                                         er_column = "ER_links_rechts_norm",
                                         scale_limits = links_rechts_limits,
                                         high_colour = "red",
                                         title_text = "Links-Rechts ER Index Gemeente Score")

map_gemeente_progressief_conservatief <- make_er_map(map_data = er_gemeente_sf,
                                                     border_data = gemeente,
                                                     er_column = "ER_progressief_conservatief_norm",
                                                     scale_limits = progressief_conservatief_limits,
                                                     high_colour = "blue",
                                                     title_text = "Progressief-Conservatief ER Index Gemeente Score",
                                                     subtitle_text = "Progressief-Conservatief dimension")

map_gemeente_euclidean <- make_er_map(map_data = er_gemeente_sf,
                                      border_data = gemeente,
                                      er_column = "ER_2d_euclidean_norm",
                                      scale_limits = euclidean_limits,
                                      high_colour = "orange",
                                      title_text = "Euclidean ER Index Gemeente Score")

map_gemeente_links_rechts
map_gemeente_progressief_conservatief
map_gemeente_euclidean

###################################################################################################
#                         COROP mapping 
###################################################################################################

map_corop_links_rechts <- make_er_map_with_labels(map_data = er_corop_sf,
                                                  border_data = corop,
                                                  er_column = "ER_links_rechts_norm",
                                                  scale_limits = links_rechts_limits,
                                                  high_colour = "red",
                                                  title_text = "Links-Rechts ER Index COROP Score",
                                                  label_size = 2.1)

map_corop_progressief_conservatief <- make_er_map_with_labels(map_data = er_corop_sf,
                                                              border_data = corop,
                                                              er_column = "ER_progressief_conservatief_norm",
                                                              scale_limits = progressief_conservatief_limits,
                                                              high_colour = "blue",
                                                              title_text = "Progressief-Conservatief ER Index COROP Score",
                                                              label_size = 2.1)

map_corop_euclidean <- make_er_map_with_labels(map_data = er_corop_sf,
                                               border_data = corop,
                                               er_column = "ER_2d_euclidean_norm",
                                               scale_limits = euclidean_limits,
                                               high_colour = "orange",
                                               title_text = "Euclidean ER Index COROP Score",
                                               label_size = 2.1)

map_corop_links_rechts
map_corop_progressief_conservatief
map_corop_euclidean

###################################################################################################
#                         Province mapping
###################################################################################################

map_province_links_rechts <- make_er_map_with_labels(map_data = er_province_sf,
                                                     border_data = province,
                                                     er_column = "ER_links_rechts_norm",
                                                     scale_limits = links_rechts_limits,
                                                     high_colour = "red",
                                                     title_text = "Links-Rechts ER Index Province Score",
                                                     label_size = 3)

map_province_progressief_conservatief <- make_er_map_with_labels(map_data = er_province_sf,
                                                                 border_data = province,
                                                                 er_column = "ER_progressief_conservatief_norm",
                                                                 scale_limits = progressief_conservatief_limits,
                                                                 high_colour = "blue",
                                                                 title_text = "Progressief-Conservatief ER Index Province Score",
                                                                 label_size = 3)

map_province_euclidean <- make_er_map_with_labels(map_data = er_province_sf,
                                                  border_data = province,
                                                  er_column = "ER_2d_euclidean_norm",
                                                  scale_limits = euclidean_limits,
                                                  high_colour = "orange",
                                                  title_text = "Euclidean ER Index Province Score",
                                                  label_size = 3)

map_province_links_rechts
map_province_progressief_conservatief
map_province_euclidean

###################################################################################################
#                         National mapping
###################################################################################################

map_national_links_rechts <- make_er_map_with_labels(map_data = er_national_sf,
                                                     border_data = NULL,
                                                     er_column = "ER_links_rechts_norm",
                                                     scale_limits = links_rechts_limits,
                                                     high_colour = "red",
                                                     title_text = "Links-Rechts ER Index Country Score",
                                                     label_size = 4.0)

map_national_progressief_conservatief <- make_er_map_with_labels(map_data = er_national_sf,
                                                                 border_data = NULL,
                                                                 er_column = "ER_progressief_conservatief_norm",
                                                                 scale_limits = progressief_conservatief_limits,
                                                                 high_colour = "blue",
                                                                 title_text = "Progressief-Conservatief ER Index Country Score",
                                                                 label_size = 4.0)

map_national_euclidean <- make_er_map_with_labels(map_data = er_national_sf,
                                                  border_data = NULL,
                                                  er_column = "ER_2d_euclidean_norm",
                                                  scale_limits = euclidean_limits,
                                                  high_colour = "orange",
                                                  title_text = "Euclidean ER Index Country Score",
                                                  label_size = 4.0)

map_national_links_rechts
map_national_progressief_conservatief
map_national_euclidean



###################################################################################################
#                         COROP: Highest normalized ER values
###################################################################################################

# To better understand the behavior of the ER values at different scales we also plan on doing histograms 
# of the highest and lowest values at different administrative boundaries. As such we first isolate the necessary data.
# since the procedure is the same for every level no further notes will be added. 

top1_corop_er_lr <- er_corop |>
  arrange(desc(ER_links_rechts_norm)) |>
  mutate(rank = row_number()) |>
  select(rank,
         corop_name,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas,
         n_gemeenten,
         n_stembureaus) |>
  slice_head(n = 1)

top1_corop_er_pc <- er_corop |>
  arrange(desc(ER_progressief_conservatief_norm)) |>
  mutate(rank = row_number()) |>
  select(rank,
         corop_name,
         province_name,
         ER_progressief_conservatief_norm,
         ER_progressief_conservatief_raw,
         total_votes_kieskompas,
         n_gemeenten,
         n_stembureaus) |>
  slice_head(n = 1)

top1_corop_er_lr
top1_corop_er_pc

###################################################################################################
#                         COROP: Lowest normalized ER values
###################################################################################################

lowest1_corop_er_lr <- er_corop |>
  arrange(ER_links_rechts_norm) |>
  mutate(rank = row_number()) |>
  select(rank,
         corop_name,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas,
         n_gemeenten,
         n_stembureaus) |>
  slice_head(n = 1)

lowest1_corop_er_pc <- er_corop |>
  arrange(ER_progressief_conservatief_norm) |>
  mutate(rank = row_number()) |>
  select(rank,
         corop_name,
         province_name,
         ER_progressief_conservatief_norm,
         ER_progressief_conservatief_raw,
         total_votes_kieskompas,
         n_gemeenten,
         n_stembureaus) |>
  slice_head(n = 1)

lowest1_corop_er_lr
lowest1_corop_er_pc

###################################################################################################
#                         Gemeente: Highest normalized ER values
###################################################################################################

top1_gemeente_er_lr <- er_gemeente |>
  arrange(desc(ER_links_rechts_norm)) |>
  mutate(rank = row_number()) |>
  select(rank,
         GemeenteCode,
         GemeenteNaam,
         gemeente_name_spatial,
         corop_name,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas,
         n_stembureaus) |>
  slice_head(n = 1)

top1_gemeente_er_pc <- er_gemeente |>
  arrange(desc(ER_progressief_conservatief_norm)) |>
  mutate(rank = row_number()) |>
  select(rank,
         GemeenteCode,
         GemeenteNaam,
         gemeente_name_spatial,
         corop_name,
         province_name,
         ER_progressief_conservatief_norm,
         ER_progressief_conservatief_raw,
         total_votes_kieskompas,
         n_stembureaus) |>
  slice_head(n = 1)

top1_gemeente_er_lr
top1_gemeente_er_pc

###################################################################################################
#                         Gemeente: Lowest normalized ER values
###################################################################################################

lowest1_gemeente_er_lr <- er_gemeente |>
  arrange(ER_links_rechts_norm) |>
  mutate(rank = row_number()) |>
  select(rank,
         GemeenteCode,
         GemeenteNaam,
         gemeente_name_spatial,
         corop_name,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas,
         n_stembureaus) |>
  slice_head(n = 1)

lowest1_gemeente_er_pc <- er_gemeente |>
  arrange(ER_progressief_conservatief_norm) |>
  mutate(rank = row_number()) |>
  select(rank,
         GemeenteCode,
         GemeenteNaam,
         gemeente_name_spatial,
         corop_name,
         province_name,
         ER_progressief_conservatief_norm,
         ER_progressief_conservatief_raw,
         total_votes_kieskompas,
         n_stembureaus) |>
  slice_head(n = 1)

lowest1_gemeente_er_lr
lowest1_gemeente_er_pc

###################################################################################################
#                         Province: Highest normalized ER values
###################################################################################################

top1_province_er_lr <- er_province |>
  arrange(desc(ER_links_rechts_norm)) |>
  mutate(rank = row_number()) |>
  select(rank,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas) |>
  slice_head(n = 1)

top1_province_er_pc <- er_province |>
  arrange(desc(ER_progressief_conservatief_norm)) |>
  mutate(rank = row_number()) |>
  select(rank,
         province_name,
         ER_progressief_conservatief_norm,
         ER_progressief_conservatief_raw,
         total_votes_kieskompas) |>
  slice_head(n = 1)

top1_province_er_lr
top1_province_er_pc

###################################################################################################
#                         Province: Lowest normalized ER values
###################################################################################################

lowest1_province_er_lr <- er_province |>
  arrange(ER_links_rechts_norm) |>
  mutate(rank = row_number()) |>
  select(rank,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas) |>
  slice_head(n = 1)

lowest1_province_er_pc <- er_province |>
  arrange(ER_progressief_conservatief_norm) |>
  mutate(rank = row_number()) |>
  select(rank,
         province_name,
         ER_progressief_conservatief_norm,
         ER_progressief_conservatief_raw,
         total_votes_kieskompas) |>
  slice_head(n = 1)

lowest1_province_er_lr
lowest1_province_er_pc

###################################################################################################
#                         Stembureau: Highest normalized LR ER value
###################################################################################################

top1_stembureau_er_lr <- er_stembureau |>
  arrange(desc(ER_links_rechts_norm)) |>
  mutate(rank = row_number()) |>
  select(rank,
         analysis_location_id,
         StembureauNaam_combined,
         GemeenteNaam,
         gemeente_name_spatial,
         corop_name,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas) |>
  slice_head(n = 1)

###################################################################################################
#                         Stembureau: Lowest normalized LR ER value
###################################################################################################

lowest1_stembureau_er_lr <- er_stembureau |>
  arrange(ER_links_rechts_norm) |>
  mutate(rank = row_number()) |>
  select(rank,
         analysis_location_id,
         StembureauNaam_combined,
         GemeenteNaam,
         gemeente_name_spatial,
         corop_name,
         province_name,
         ER_links_rechts_norm,
         ER_links_rechts_raw,
         total_votes_kieskompas) |>
  slice_head(n = 1)

top1_stembureau_er_lr
lowest1_stembureau_er_lr



###################################################################################################
#                         Load party-level vote tables
###################################################################################################


stembureau_party_votes <- read_csv("stembureau_party_votes.csv",
                                   col_select = -1)

gemeente_party_votes <- read_csv("gemeente_party_votes.csv",
                                 col_select = -1)

corop_party_votes <- read_csv("corop_party_votes.csv",
                              col_select = -1)

province_party_votes <- read_csv("province_party_votes.csv",
                                 col_select = -1)

###################################################################################################
#                         COROP: highest Links-Rechts ER bar chart
###################################################################################################

for (i in seq_len(nrow(top1_corop_er_lr))) {
  
  selected_corop <- top1_corop_er_lr$corop_name[i]
  
  plot_data <- corop_party_votes |>
    filter(corop_name == selected_corop) |>
    arrange(links_rechts)
  
  plot_corop_lr_highest <- ggplot(plot_data,
                                  aes(x = reorder(party_kieskompas, links_rechts),
                                      y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Highest COROP Links-Rechts ER: ", selected_corop),
         subtitle = paste0("ER index score = ", round(top1_corop_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_corop_lr_highest)
}

###################################################################################################
#                         Stembureau: lowest Links-Rechts ER bar chart
###################################################################################################

for (i in seq_len(nrow(lowest1_stembureau_er_lr))) {
  
  selected_stembureau <- lowest1_stembureau_er_lr$analysis_location_id[i]
  selected_stembureau_name <- lowest1_stembureau_er_lr$StembureauNaam_combined[i]
  
  plot_data <- stembureau_party_votes |>
    filter(analysis_location_id == selected_stembureau) |>
    arrange(links_rechts)
  
  plot_stembureau_lr_lowest <- ggplot(plot_data,
                                      aes(x = reorder(party_kieskompas, links_rechts),
                                          y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Lowest stembureau Links-Rechts ER: ", selected_stembureau_name),
         subtitle = paste0("ER index score = ", round(lowest1_stembureau_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_stembureau_lr_lowest)
}

###################################################################################################
#                         Stembureau: highest Links-Rechts ER bar chart
###################################################################################################

for (i in seq_len(nrow(top1_stembureau_er_lr))) {
  
  selected_stembureau <- top1_stembureau_er_lr$analysis_location_id[i]
  selected_stembureau_name <- top1_stembureau_er_lr$StembureauNaam_combined[i]
  
  plot_data <- stembureau_party_votes |>
    filter(analysis_location_id == selected_stembureau) |>
    arrange(links_rechts)
  
  plot_stembureau_lr_highest <- ggplot(plot_data,
                                       aes(x = reorder(party_kieskompas, links_rechts),
                                           y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Highest stembureau Links-Rechts ER: ", selected_stembureau_name),
         subtitle = paste0("ER index score = ", round(top1_stembureau_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_stembureau_lr_highest)
}


###################################################################################################
#                         COROP: lowest Links-Rechts ER bar chart
###################################################################################################


for (i in seq_len(nrow(lowest1_corop_er_lr))) {
  
  selected_corop <- lowest1_corop_er_lr$corop_name[i]
  
  plot_data <- corop_party_votes |>
    filter(corop_name == selected_corop) |>
    arrange(links_rechts)
  
  plot_corop_lr_lowest <- ggplot(plot_data,
                                 aes(x = reorder(party_kieskompas, links_rechts),
                                     y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Lowest COROP Links-Rechts ER: ", selected_corop),
         subtitle = paste0("ER index score = ", round(lowest1_corop_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_corop_lr_lowest)
}

###################################################################################################
#                         COROP: highest Progressief-Conservatief ER bar chart
###################################################################################################

for (i in seq_len(nrow(top1_corop_er_pc))) {
  
  selected_corop <- top1_corop_er_pc$corop_name[i]
  
  plot_data <- corop_party_votes |>
    filter(corop_name == selected_corop) |>
    arrange(progressief_conservatief)
  
  plot_corop_pc_highest <- ggplot(plot_data,
                                  aes(x = reorder(party_kieskompas, progressief_conservatief),
                                      y = AantalStemmen)) +
    geom_col(fill = "blue",
             alpha = 0.75) +
    labs(title = paste0("Highest COROP Progressief-Conservatief ER: ", selected_corop),
         subtitle = paste0("ER index score = ", round(top1_corop_er_pc$ER_progressief_conservatief_norm[i], 2)),
         x = "Progressief-Conservatief Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_corop_pc_highest)
}

###################################################################################################
#                         COROP: lowest Progressief-Conservatief ER bar chart
###################################################################################################

for (i in seq_len(nrow(lowest1_corop_er_pc))) {
  
  selected_corop <- lowest1_corop_er_pc$corop_name[i]
  
  plot_data <- corop_party_votes |>
    filter(corop_name == selected_corop) |>
    arrange(progressief_conservatief)
  
  plot_corop_pc_lowest <- ggplot(plot_data,
                                 aes(x = reorder(party_kieskompas, progressief_conservatief),
                                     y = AantalStemmen)) +
    geom_col(fill = "blue",
             alpha = 0.75) +
    labs(title = paste0("Lowest COROP Progressief-Conservatief ER: ", selected_corop),
         subtitle = paste0("ER index score = ", round(lowest1_corop_er_pc$ER_progressief_conservatief_norm[i], 2)),
         x = "Progressief-Conservatief Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_corop_pc_lowest)
}

###################################################################################################
#                         Gemeente: highest Links-Rechts ER bar chart
###################################################################################################

for (i in seq_len(nrow(top1_gemeente_er_lr))) {
  
  selected_gemeente <- top1_gemeente_er_lr$GemeenteCode[i]
  selected_gemeente_name <- top1_gemeente_er_lr$GemeenteNaam[i]
  
  plot_data <- gemeente_party_votes |>
    filter(GemeenteCode == selected_gemeente) |>
    arrange(links_rechts)
  
  plot_gemeente_lr_highest <- ggplot(plot_data,
                                     aes(x = reorder(party_kieskompas, links_rechts),
                                         y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Highest gemeente Links-Rechts ER: ", selected_gemeente_name),
         subtitle = paste0("ER index score = ", round(top1_gemeente_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_gemeente_lr_highest)
}

###################################################################################################
#                         Gemeente: lowest Links-Rechts ER bar chart
###################################################################################################

for (i in seq_len(nrow(lowest1_gemeente_er_lr))) {
  
  selected_gemeente <- lowest1_gemeente_er_lr$GemeenteCode[i]
  selected_gemeente_name <- lowest1_gemeente_er_lr$GemeenteNaam[i]
  
  plot_data <- gemeente_party_votes |>
    filter(GemeenteCode == selected_gemeente) |>
    arrange(links_rechts)
  
  plot_gemeente_lr_lowest <- ggplot(plot_data,
                                    aes(x = reorder(party_kieskompas, links_rechts),
                                        y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Lowest gemeente Links-Rechts ER: ", selected_gemeente_name),
         subtitle = paste0("ER index score = ", round(lowest1_gemeente_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_gemeente_lr_lowest)
}

###################################################################################################
#                         Gemeente: highest Progressief-Conservatief ER bar chart
###################################################################################################

for (i in seq_len(nrow(top1_gemeente_er_pc))) {
  
  selected_gemeente <- top1_gemeente_er_pc$GemeenteCode[i]
  selected_gemeente_name <- top1_gemeente_er_pc$GemeenteNaam[i]
  
  plot_data <- gemeente_party_votes |>
    filter(GemeenteCode == selected_gemeente) |>
    arrange(progressief_conservatief)
  
  plot_gemeente_pc_highest <- ggplot(plot_data,
                                     aes(x = reorder(party_kieskompas, progressief_conservatief),
                                         y = AantalStemmen)) +
    geom_col(fill = "blue",
             alpha = 0.75) +
    labs(title = paste0("Highest gemeente Progressief-Conservatief ER: ", selected_gemeente_name),
         subtitle = paste0("ER index score = ", round(top1_gemeente_er_pc$ER_progressief_conservatief_norm[i], 2)),
         x = "Progressief-Conservatief Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_gemeente_pc_highest)
}

###################################################################################################
#                         Gemeente: lowest Progressief-Conservatief ER bar chart
###################################################################################################

for (i in seq_len(nrow(lowest1_gemeente_er_pc))) {
  
  selected_gemeente <- lowest1_gemeente_er_pc$GemeenteCode[i]
  selected_gemeente_name <- lowest1_gemeente_er_pc$GemeenteNaam[i]
  
  plot_data <- gemeente_party_votes |>
    filter(GemeenteCode == selected_gemeente) |>
    arrange(progressief_conservatief)
  
  plot_gemeente_pc_lowest <- ggplot(plot_data,
                                    aes(x = reorder(party_kieskompas, progressief_conservatief),
                                        y = AantalStemmen)) +
    geom_col(fill = "blue",
             alpha = 0.75) +
    labs(title = paste0("Lowest gemeente Progressief-Conservatief ER: ", selected_gemeente_name),
         subtitle = paste0("ER index score = ", round(lowest1_gemeente_er_pc$ER_progressief_conservatief_norm[i], 2)),
         x = "Progressief-Conservatief Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_gemeente_pc_lowest)
}

###################################################################################################
#                         Province: highest Links-Rechts ER bar chart
###################################################################################################

for (i in seq_len(nrow(top1_province_er_lr))) {
  
  selected_province <- top1_province_er_lr$province_name[i]
  
  plot_data <- province_party_votes |>
    filter(province_name == selected_province) |>
    arrange(links_rechts)
  
  plot_province_lr_highest <- ggplot(plot_data,
                                     aes(x = reorder(party_kieskompas, links_rechts),
                                         y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Highest province Links-Rechts ER: ", selected_province),
         subtitle = paste0("ER index score = ", round(top1_province_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_province_lr_highest)
}

###################################################################################################
#                         Province: lowest Links-Rechts ER bar chart
###################################################################################################

for (i in seq_len(nrow(lowest1_province_er_lr))) {
  
  selected_province <- lowest1_province_er_lr$province_name[i]
  
  plot_data <- province_party_votes |>
    filter(province_name == selected_province) |>
    arrange(links_rechts)
  
  plot_province_lr_lowest <- ggplot(plot_data,
                                    aes(x = reorder(party_kieskompas, links_rechts),
                                        y = AantalStemmen)) +
    geom_col(fill = "red",
             alpha = 0.75) +
    labs(title = paste0("Lowest province Links-Rechts ER: ", selected_province),
         subtitle = paste0("ER index score = ", round(lowest1_province_er_lr$ER_links_rechts_norm[i], 2)),
         x = "Links-Rechts Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_province_lr_lowest)
}

###################################################################################################
#                         Province: highest Progressief-Conservatief ER bar chart
###################################################################################################


for (i in seq_len(nrow(top1_province_er_pc))) {
  
  selected_province <- top1_province_er_pc$province_name[i]
  
  plot_data <- province_party_votes |>
    filter(province_name == selected_province) |>
    arrange(progressief_conservatief)
  
  plot_province_pc_highest <- ggplot(plot_data,
                                     aes(x = reorder(party_kieskompas, progressief_conservatief),
                                         y = AantalStemmen)) +
    geom_col(fill = "blue",
             alpha = 0.75) +
    labs(title = paste0("Highest province Progressief-Conservatief ER: ", selected_province),
         subtitle = paste0("ER index score = ", round(top1_province_er_pc$ER_progressief_conservatief_norm[i], 2)),
         x = "Progressief-Conservatief Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_province_pc_highest)
}


###################################################################################################
#                         Province: lowest Progressief-Conservatief ER bar chart
###################################################################################################

for (i in seq_len(nrow(lowest1_province_er_pc))) {
  
  selected_province <- lowest1_province_er_pc$province_name[i]
  
  plot_data <- province_party_votes |>
    filter(province_name == selected_province) |>
    arrange(progressief_conservatief)
  
  plot_province_pc_lowest <- ggplot(plot_data,
                                    aes(x = reorder(party_kieskompas, progressief_conservatief),
                                        y = AantalStemmen)) +
    geom_col(fill = "blue",
             alpha = 0.75) +
    labs(title = paste0("Lowest province Progressief-Conservatief ER: ", selected_province),
         subtitle = paste0("ER index score = ", round(lowest1_province_er_pc$ER_progressief_conservatief_norm[i], 2)),
         x = "Progressief-Conservatief Kieskompas position",
         y = "Total votes") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  
  print(plot_province_pc_lowest)
}

###################################################################################################
#                         Save all maps and bar charts as separate PNG files
###################################################################################################

dir.create("figures",
           showWarnings = FALSE)

###################################################################################################
#                         Save gemeente maps
###################################################################################################

ggsave(filename = "figures/map_gemeente_links_rechts.png",
       plot = map_gemeente_links_rechts,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_gemeente_progressief_conservatief.png",
       plot = map_gemeente_progressief_conservatief,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_gemeente_euclidean.png",
       plot = map_gemeente_euclidean,
       width = 8,
       height = 7,
       dpi = 300)

###################################################################################################
#                         Save COROP maps
###################################################################################################

ggsave(filename = "figures/map_corop_links_rechts.png",
       plot = map_corop_links_rechts,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_corop_progressief_conservatief.png",
       plot = map_corop_progressief_conservatief,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_corop_euclidean.png",
       plot = map_corop_euclidean,
       width = 8,
       height = 7,
       dpi = 300)

###################################################################################################
#                         Save province maps
###################################################################################################

ggsave(filename = "figures/map_province_links_rechts.png",
       plot = map_province_links_rechts,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_province_progressief_conservatief.png",
       plot = map_province_progressief_conservatief,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_province_euclidean.png",
       plot = map_province_euclidean,
       width = 8,
       height = 7,
       dpi = 300)

###################################################################################################
#                         Save national maps
###################################################################################################

ggsave(filename = "figures/map_national_links_rechts.png",
       plot = map_national_links_rechts,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_national_progressief_conservatief.png",
       plot = map_national_progressief_conservatief,
       width = 8,
       height = 7,
       dpi = 300)

ggsave(filename = "figures/map_national_euclidean.png",
       plot = map_national_euclidean,
       width = 8,
       height = 7,
       dpi = 300)

###################################################################################################
#                         Save stembureau bar charts
###################################################################################################

ggsave(filename = "figures/bar_stembureau_lr_lowest.png",
       plot = plot_stembureau_lr_lowest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_stembureau_lr_highest.png",
       plot = plot_stembureau_lr_highest,
       width = 10,
       height = 6,
       dpi = 300)

###################################################################################################
#                         Save COROP bar charts
###################################################################################################

ggsave(filename = "figures/bar_corop_lr_highest.png",
       plot = plot_corop_lr_highest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_corop_lr_lowest.png",
       plot = plot_corop_lr_lowest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_corop_pc_highest.png",
       plot = plot_corop_pc_highest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_corop_pc_lowest.png",
       plot = plot_corop_pc_lowest,
       width = 10,
       height = 6,
       dpi = 300)

###################################################################################################
#                         Save gemeente bar charts
###################################################################################################

ggsave(filename = "figures/bar_gemeente_lr_highest.png",
       plot = plot_gemeente_lr_highest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_gemeente_lr_lowest.png",
       plot = plot_gemeente_lr_lowest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_gemeente_pc_highest.png",
       plot = plot_gemeente_pc_highest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_gemeente_pc_lowest.png",
       plot = plot_gemeente_pc_lowest,
       width = 10,
       height = 6,
       dpi = 300)

###################################################################################################
#                         Save province bar charts
###################################################################################################

ggsave(filename = "figures/bar_province_lr_highest.png",
       plot = plot_province_lr_highest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_province_lr_lowest.png",
       plot = plot_province_lr_lowest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_province_pc_highest.png",
       plot = plot_province_pc_highest,
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "figures/bar_province_pc_lowest.png",
       plot = plot_province_pc_lowest,
       width = 10,
       height = 6,
       dpi = 300)
