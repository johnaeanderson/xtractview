#' Multi-Slice Sagittal Gallery Mapping for XTRACT Data
#'
#' @param tract_data A data.frame containing columns 'region' (tract names) and 'value'.
#' @return A faceted ggplot object showing sequential sagittal structural views.
#' @export
#' @import ggplot2
#' @import dplyr
plot_xtract_sag_gallery <- function(tract_data) {
  
  val_col <- names(tract_data)[names(tract_data) != "region"][1]
  # --- FIX: Extract numeric values, sort them, and apply them as ordered factor levels ---
  unique_slices <- unique(all_sag_bg$slice_id)
  numeric_sort  <- unique_slices[order(as.numeric(gsub("[^0-9]", "", unique_slices)))]
  
  all_sag_bg$slice_id     <- factor(all_sag_bg$slice_id, levels = numeric_sort)
  all_sag_tracts$slice_id <- factor(all_sag_tracts$slice_id, levels = numeric_sort)
  # -------------------------------------------------------------------------------------
  # Merge user metrics with our new sagittal 2D map collection
  plot_df <- dplyr::left_join(all_sag_tracts, tract_data, by = "region")
  
  ggplot2::ggplot() +
    # Render sagittal brain backgrounds
    ggplot2::geom_sf(data = all_sag_bg, fill = "transparent", color = "#EAEAEA", size = 0.1) +
    # Map user data values
    ggplot2::geom_sf(data = plot_df, ggplot2::aes(fill = .data[[val_col]]), color = "black", size = 0.2) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(0,1), na.value = "transparent") +
    ggplot2::theme_void() +
    # Group panels dynamically side-by-side
    ggplot2::facet_wrap(~slice_id, ncol = 3) + 
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )
}