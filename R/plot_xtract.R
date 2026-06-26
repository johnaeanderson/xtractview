#' Multi-Planar Anatomical Mapping for XTRACT Data
#'
#' @description
#' Renders a 4-panel multi-planar plot mapping tract metrics onto 2D structural templates.
#'
#' @param tract_data A \code{data.frame} or tibble containing the data to map.
#'   Must include a column named exactly \code{"region"} containing valid XTRACT tract names,
#'   and at least one numeric metric column (e.g., \code{Mean_FA}, \code{MD}, \code{Volume}).
#' @param limit_min Optional numeric. Lower bound for the color scale. Defaults to min data value.
#' @param limit_max Optional numeric. Upper bound for the color scale. Defaults to max data value.
#' @param midpoint Optional numeric. Center point for the color scale. Defaults to halfway between min and max.
#'
#' @return A combined patchwork ggplot object showing 4 anatomical views.
#' @export
#'
#' @examples
#' # --- Example of how to format user data ---
#' my_data <- data.frame(
#'   region = c("Corticospinal Tract L", "Arcuate Fasciculus R", "Forceps Minor"),
#'   Mean_FA = c(0.55, 0.62, 0.41)
#' )
#'
#' # Render the plot
#' plot_xtract(my_data)
plot_xtract <- function(tract_data, limit_min = NULL, limit_max = NULL, midpoint = NULL) {

  # --- FIX: Initialize spatial vector methods for fresh R sessions ---
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("patchwork", quietly = TRUE)

  val_col <- names(tract_data)[names(tract_data) != "region"][1]

  # --- NEW: Dynamic Scale Calculation ---
  if (is.null(limit_min)) limit_min <- min(tract_data[[val_col]], na.rm = TRUE)
  if (is.null(limit_max)) limit_max <- max(tract_data[[val_col]], na.rm = TRUE)
  if (is.null(midpoint))  midpoint  <- mean(c(limit_min, limit_max), na.rm = TRUE)

  render_baked_view <- function(baked_list, title_text) {
    bg_sf     <- baked_list$bg
    tracts_sf <- baked_list$tracts
    bbox      <- baked_list$bbox

    if(nrow(tracts_sf) == 0 || nrow(bg_sf) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title_text))
    }

    plot_df <- dplyr::left_join(tracts_sf, tract_data, by = "region")

    ggplot2::ggplot() +
      # Changed brain structural background to transparent
      ggplot2::geom_sf(data = bg_sf, fill = "transparent", color = "#EAEAEA", size = 0.1) +
      ggplot2::geom_sf(data = plot_df, ggplot2::aes(fill = .data[[val_col]]), color = "black", size = 0.2) +

      # --- NEW: Apply the dynamic limits to the gradient ---
      ggplot2::scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = midpoint,
        limits = c(limit_min, limit_max),
        na.value = "transparent"
      ) +

      # Added datum = NA to guarantee no spatial coordinate boxes are drawn
      ggplot2::coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = TRUE, datum = NA) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
        legend.position  = "none",
        panel.border     = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
      ) +
      ggplot2::ggtitle(title_text)
  }

  p_axial   <- render_baked_view(axial_data,   "Axial View")
  p_coronal <- render_baked_view(coronal_data, "Coronal View")
  p_left    <- render_baked_view(left_data,    "Sagittal (Left)")
  p_right   <- render_baked_view(right_data,   "Sagittal (Right)")

  multi_view_plot <- (p_axial + p_coronal) / (p_left + p_right) +
    patchwork::plot_layout(guides = 'collect') +
    patchwork::plot_annotation(
      title = "XTRACT Multi-Planar Anatomical Mapping",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA) # Transparent wrapper
      )
    )

  return(multi_view_plot)
}
