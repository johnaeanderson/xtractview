#' Multi-Slice Gallery Mapping for XTRACT Data
#'
#' @param tract_data A data.frame containing columns 'region' (tract names) and 'value'.
#' @return A faceted ggplot object showing sequential structural views.
#' @export
#' @import ggplot2
#' @import dplyr
plot_xtract_gallery <- function(tract_data) {
  
  val_col <- names(tract_data)[names(tract_data) != "region"][1]
  
  # Merge user metrics with our comprehensive 2D tract slice layer compilation
  plot_df <- dplyr::left_join(all_tract_layers, tract_data, by = "region")
  
  ggplot2::ggplot() +
    # Render all the gray background structures across all slices
    ggplot2::geom_sf(data = all_bg_layers, fill = "transparent", color = "#EAEAEA", size = 0.1) +
    # Fill in user values where they exist
    ggplot2::geom_sf(data = plot_df, ggplot2::aes(fill = .data[[val_col]]), color = "black", size = 0.2) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(0,1), na.value = "transparent") +
    ggplot2::theme_void() +
    # --- MAGIC STEP: Facet wrap automatically separates the slices into an ordered grid ---
    ggplot2::facet_wrap(~slice_id, ncol = 4) + 
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10), # Slice labels
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )
}