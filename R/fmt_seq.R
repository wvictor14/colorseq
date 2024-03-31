#' Color biological sequences
#'
#' Color amino acids by biochemical properties like hydrophobicity, charge,
#' conservation, etc. Nucleotides not yet supported.
#'
#' @param gt_object An existing gt table
#' @param columns The columns to affect
#' @param ... Additional argument passed to
#' @inheritParams colorseq
#'
#' @return a gt table
#' @export
#'
#' @examples
#' library(gt)
#' library(tibble)
#' library(dplyr)
#' data(aa_seq)
#'
#' aa_seq |> gt::gt() |> fmt_seq(seq, spacing = 5)
fmt_seq <- function(
    gt_object, columns, color_scheme = 'chemistry', font = 'Consolas', ...) {
  gt_object |>

    gt::text_transform(
      locations = gt::cells_body(columns = {{ columns }}),
      fn = function(x) colorseq(x, ...)
    ) |>
    gt::tab_style(
      style = gt::cell_text(font = font),
      locations = gt::cells_body(columns = {{ columns }})
    ) |>
    gt::tab_style(
        style = gt::cell_borders(
          sides = c("top", "bottom"),
          weight = gt::px(0)
        ),
        locations = gt::cells_body(columns = {{ columns }})
    )
}


#' Apply colorseq theme to gt table
#'
#' @param gt_object gt object
#'
#' @return gt table
#' @export
#'
#' @examples
#'
#' aa_seq |> gt::gt() |> fmt_seq(seq, spacing = 5) |>  gt_theme_colorseq()
gt_theme_colorseq <- function(gt_object) {
  gt_object |>
    gt::tab_options(
      data_row.padding = gt::px(0)
    )
}
