#' Multi-Slice Coronal Gallery Mapping for XTRACT Data
#'
#' @param tract_data A data.frame containing columns 'region' (tract names) and 'value'.
#' @param limit_min Optional numeric. Lower bound for the color scale. Defaults to min data value.
#' @param limit_max Optional numeric. Upper bound for the color scale. Defaults to max data value.
#' @param midpoint Optional numeric. Center point for the color scale. Defaults to halfway between min and max.
#' @return A faceted ggplot object showing sequential coronal structural views.
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom sf st_as_sf
plot_xtract_gallery <- function(tract_data, limit_min = NULL, limit_max = NULL, midpoint = NULL) {

  requireNamespace("sf", quietly = TRUE)

  val_col <- names(tract_data)[names(tract_data) != "region"][1]

  # Smarter Dynamic Midpoint for Statistical Maps
  if (is.null(limit_min)) limit_min <- min(tract_data[[val_col]], na.rm = TRUE)
  if (is.null(limit_max)) limit_max <- max(tract_data[[val_col]], na.rm = TRUE)

  if (is.null(midpoint)) {
    if (limit_min < 0 && limit_max > 0) {
      midpoint <- 0
    } else {
      midpoint <- mean(c(limit_min, limit_max), na.rm = TRUE)
    }
  }

  # Sort the slice levels numerically
  unique_slices <- unique(all_bg_layers$slice_id)
  numeric_sort  <- unique_slices[order(as.numeric(gsub("[^0-9]", "", unique_slices)))]

  all_bg_layers$slice_id    <- factor(all_bg_layers$slice_id, levels = numeric_sort)
  all_tract_layers$slice_id <- factor(all_tract_layers$slice_id, levels = numeric_sort)

  plot_df <- dplyr::left_join(all_tract_layers, tract_data, by = "region")

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = all_bg_layers, fill = NA, color = "#EAEAEA", size = 0.1) +
    ggplot2::geom_sf(data = plot_df, ggplot2::aes(fill = .data[[val_col]]), color = "black", size = 0.2) +

    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = midpoint,
      limits = c(limit_min, limit_max),
      na.value = NA
    ) +

    ggplot2::facet_wrap(~slice_id, ncol = 4) +

    # --- FIX: Flip X-Axis to Neurological Convention (Left is Left) ---
    ggplot2::scale_x_reverse() +
    # ------------------------------------------------------------------

  ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.border     = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background  = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text       = ggplot2::element_text(face = "bold", size = 10)
    )
}
