#' xtractview: Multi-Planar Anatomical Mapping for XTRACT Data
#'
#' @description
#' The \code{xtractview} package provides a fast, memory-optimized 2D vector-based
#' approach to visualize white matter tracts from FSL's XTRACT pipeline directly
#' within R using \code{ggplot2}. It bypasses heavy 3D rendering engines by
#' utilizing pre-baked 2D spatial geometries across Axial, Coronal, and Sagittal planes.
#'
#' @section Main Functions:
#' \+ \code{\link{plot_xtract}}: Renders a standard 4-panel multi-planar mapping summary.
#' \+ \code{\link{plot_xtract_gallery}}: Renders sequential coronal slice grids.
#' \+ \code{\link{plot_xtract_ax_gallery}}: Renders sequential axial slice grids.
#' \+ \code{\link{plot_xtract_sag_gallery}}: Renders sequential sagittal slice grids.
#'
#' @section Public Data Objects:
#' \+ \code{\link{tract_names}}: A character vector listing all 42 supported white matter tracts.
#'
#' @docType package
#' @name xtractview
#' @aliases xtractview-package
NULL
