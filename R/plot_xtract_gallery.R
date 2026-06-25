#' Multi-Slice Coronal Gallery Mapping for XTRACT Data
#'
#' @param tract_data A data.frame containing columns 'region' (tract names) and 'value'.
#' @return A faceted ggplot object showing sequential coronal structural views.
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom sf st_as_sf
plot_xtract_gallery <- function(tract_data) {

  # --- FIX 1: Wake up the sf column rendering engine for clean sessions ---
  requireNamespace("sf", quietly = TRUE)

  val_col <- names(tract_data)[names(tract_data) != "region"][1]

  # --- FIX 2: Sort the slice levels numerically so they march sequentially ---
  unique_slices <- unique(all_bg_layers$slice_id)
  numeric_sort  <- unique_slices[order(as.numeric(gsub("[^0-9]", "", unique_slices)))]

  all_bg_layers$slice_id    <- factor(all_bg_layers$slice_id, levels = numeric_sort)
  all_tract_layers$slice_id <- factor(all_tract_layers$slice_id, levels = numeric_sort)

  # Merge user metrics with our comprehensive 2D tract slice layer compilation
  plot_df <- dplyr::left_join(all_tract_layers, tract_data, by = "region")

  ggplot2::ggplot() +
    # Render all the gray background structures across all slices
    ggplot2::geom_sf(data = all_bg_layers, fill = "transparent", color = "#EAEAEA", size = 0.1) +
    # Fill in user values where they exist
    ggplot2::geom_sf(data = plot_df, ggplot2::aes(fill = .data[[val_col]]), color = "black", size = 0.2) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(0,1), na.value = "transparent") +
    ggplot2::facet_wrap(~slice_id, ncol = 4) +
    ggplot2::theme_void() +
    ggplot2::theme(
      # --- FIX 3: Nuke the spatial bounding frame and box outlines completely ---
      panel.border     = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      # -------------------------------------------------------------------------
      strip.text       = ggplot2::element_text(face = "bold", size = 10)
    )
}
