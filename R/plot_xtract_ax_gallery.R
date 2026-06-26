#' Multi-Slice Axial Gallery Mapping for XTRACT Data
#'
#' @param tract_data A data.frame containing columns 'region' (tract names) and 'value'.
#' @param limit_min Optional numeric. Lower bound for the color scale. Defaults to min data value.
#' @param limit_max Optional numeric. Upper bound for the color scale. Defaults to max data value.
#' @param midpoint Optional numeric. Center point for the color scale. Defaults to halfway between min and max.
#' @return A faceted ggplot object showing sequential axial structural views.
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom sf st_as_sf
plot_xtract_ax_gallery <- function(tract_data, limit_min = NULL, limit_max = NULL, midpoint = NULL) {

  # --- FIX 1: Initialize spatial vector methods for fresh R sessions ---
  requireNamespace("sf", quietly = TRUE)

  val_col <- names(tract_data)[names(tract_data) != "region"][1]

  # --- NEW: Dynamic Scale Calculation ---
  if (is.null(limit_min)) limit_min <- min(tract_data[[val_col]], na.rm = TRUE)
  if (is.null(limit_max)) limit_max <- max(tract_data[[val_col]], na.rm = TRUE)
  if (is.null(midpoint))  midpoint  <- mean(c(limit_min, limit_max), na.rm = TRUE)

  # Ensure numeric sorting for the factor levels so Z-slices march upwards correctly
  unique_slices <- unique(all_ax_bg$slice_id)
  numeric_sort  <- unique_slices[order(as.numeric(gsub("[^0-9]", "", unique_slices)))]

  all_ax_bg$slice_id     <- factor(all_ax_bg$slice_id, levels = numeric_sort)
  all_ax_tracts$slice_id <- factor(all_ax_tracts$slice_id, levels = numeric_sort)

  plot_df <- dplyr::left_join(all_ax_tracts, tract_data, by = "region")

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = all_ax_bg, fill = "transparent", color = "#EAEAEA", size = 0.1) +
    ggplot2::geom_sf(data = plot_df, ggplot2::aes(fill = .data[[val_col]]), color = "black", size = 0.2) +

    # --- NEW: Apply the dynamic limits to the gradient ---
    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = midpoint,
      limits = c(limit_min, limit_max),
      na.value = "transparent"
    ) +

    ggplot2::facet_wrap(~slice_id, ncol = 3) +
    ggplot2::theme_void() +
    ggplot2::theme(
      # --- FIX 2: Zero out the box outlines and line frames completely ---
      panel.border     = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      # ------------------------------------------------------------------
      strip.text       = ggplot2::element_text(face = "bold", size = 10)
    )
}
