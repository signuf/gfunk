super_cols <- function(...) {

  super_colors <- c(
    `sage`        = "#a9a57c",
    `opal`        = "#406362",
    `straw`       = "#d2cb6c",
    `dark blue`   = "#95a39d",
    `camel`       = "#c89f5d",
    `grullo`      = "#b1a089")

  cols <- c(...)

  if (is.null(cols)) return (super_colors)

  super_colors[cols]
}

#' Return SUPE_R themed palette
#'
#' @param pallette Palette to create color ramp from
#' @return list of color names
#' @export
super_pal <- function(palette = "main", reverse = FALSE, ...) {

  super_palettes <- list(
    `main`  = super_cols("sage", "opal", "straw"),

    `cool`  = super_cols("opal", "dark blue", "grullo"),

    `hot`   = super_cols("straw", "camel"),

    `mixed` = super_cols("sage", "opal", "straw", "dark blue", "camel"),

    `grey`  = super_cols("opal", "dark blue", "grullo")
  )

  pal <- super_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Scale color with SUPE_R
#'
#' @param pallette Palette to create color ramp from
#' @param discrete Logical
#' @param reverse Reverse palette
#' @export
scale_color_super <- function(palette = "cool", discrete = TRUE, reverse = FALSE, n_colors = 16, ...) {
  pal <- super_pal(palette = palette, reverse = reverse)

  if (discrete) discrete_scale("colour", paste0("super_", palette), palette = pal, ...)
  else scale_color_gradientn(colours = pal(n_colors), ...)
}

#' Scale fill with SUPE_R
#'
#' @param pallette Palette to create color ramp from
#' @param discrete Logical
#' @param reverse Reverse palette
#' @export
scale_fill_super <- function(palette = "cool", discrete = TRUE, reverse = FALSE, n_colors = 16, ...) {
  pal <- super_pal(palette = palette, reverse = reverse)

  if (discrete) discrete_scale("fill", paste0("super_", palette), palette = pal, ...)
  else scale_fill_gradientn(colours = pal(n_colors), ...)
}

#' SUPE_R std theme
#'
#' @export
theme_super <- function(text_size = 15, base_size = 15, ...) {
  theme_classic(base_size = base_size, ...) +
    theme(text = element_text(family = "Tw Cen MT", size = text_size),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.title = element_blank(), legend.position="bottom")
}
