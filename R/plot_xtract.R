#' Multi-Planar Anatomical Mapping for XTRACT Data
#'
#' @param tract_data A data.frame containing columns 'region' (tract names) and 'value'.
#' @return A combined patchwork ggplot object showing 4 anatomical views.
#' @export
#' @import ggplot2
#' @import dplyr
#' @import patchwork
plot_xtract <- function(tract_data) {

  val_col <- names(tract_data)[names(tract_data) != "region"][1]

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
      # na.value = "transparent" ensures unmapped tracts are invisible
      ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, na.value = "transparent") +
      ggplot2::coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = TRUE) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
        legend.position = "none",
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
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
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA) # Transparent wrapper
      )
    )

  return(multi_view_plot)
}
