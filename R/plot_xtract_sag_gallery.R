#' Multi-Slice Sagittal Gallery Mapping for XTRACT Data
#'
#' @param tract_data A data.frame containing columns 'region' (tract names) and 'value'.
#' @return A faceted ggplot object showing sequential sagittal structural views.
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom sf st_as_sf
plot_xtract_sag_gallery <- function(tract_data) {

  # --- FIX: Force R to wake up the sf vector methods before the join evaluates ---
  requireNamespace("sf", quietly = TRUE)

  val_col <- names(tract_data)[names(tract_data) != "region"][1]

  # Ensure numeric sorting for the factor levels
  unique_slices <- unique(all_sag_bg$slice_id)
  numeric_sort  <- unique_slices[order(as.numeric(gsub("[^0-9]", "", unique_slices)))]

  all_sag_bg$slice_id     <- factor(all_sag_bg$slice_id, levels = numeric_sort)
  all_sag_tracts$slice_id <- factor(all_sag_tracts$slice_id, levels = numeric_sort)

  # This join will now succeed perfectly!
  plot_df <- dplyr::left_join(all_sag_tracts, tract_data, by = "region")

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = all_sag_bg, fill = "transparent", color = "#EAEAEA", size = 0.1) +
    ggplot2::geom_sf(data = plot_df, ggplot2::aes(fill = .data[[val_col]]), color = "black", size = 0.2) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(0,1), na.value = "transparent") +
    ggplot2::theme_void() +
    ggplot2::facet_wrap(~slice_id, ncol = 3) +
    ggplot2::theme(
      panel.border     = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.text       = ggplot2::element_text(face = "bold", size = 10)
    )
}
