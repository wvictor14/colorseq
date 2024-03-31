#' Applies html coloring to biological sequences
#'
#' Takes a vector of biological sequences and applies html coloring. Coloring
#' can be done according to biological properties such as biochemistry,  and
#' structure. The rendered html of color sequences can help identify patterns in
#' sequences based on these various properties.
#'
#' @param seq a vector of amino acid sequences
#' @param color_scheme the desired color scheme. Available choices are:
#'
#' - "clustal": https://www.bioinformatics.nl/~berndb/aacolour.html
#'
#' @return vector of html colored sequences
#' @export
#'
#' @examples
#' library(gt)
#' library(tibble)
#' library(dplyr)
#' data(aa_seq)
#'
#' colorseq(aa_seq$seq[1])
#' colorseq(aa_seq$seq[1:2])
#'
#' # compare in gt
#' one_seq <- aa_seq$seq[1]
#' tibble(
#'   seq = colorseq(one_seq),
#'   seq_raw = one_seq
#' ) |>
#'   gt() |>
#'   fmt_markdown()
#'
#' # in a dplyr pipe
#' aa_seq |>
#'   mutate(seq_colored = colorseq(seq)) |>
#'   select(name, everything()) |>
#'   gt() |>
#'   fmt_markdown() |>
#'   tab_style(
#'     style = cell_text(font = 'Cascadia Code'),
#'     locations = cells_body(
#'     columns = contains('seq')
#'   )
#' )
colorseq <- function(seq, color_scheme = 'clustal', spacing = 2) {

  stopifnot(color_scheme %in% names(seq_colors))

  # split elements
  .splitted <- seq |>
    purrr::map(
      ~tibble::tibble(
        position = 1:nchar(.x),
        element = stringr::str_split_1(.x, '')
      )
    ) |>
    dplyr::bind_rows(.id = 'id')

  # apply colors
  .out <- .splitted |>

    # add colors
    dplyr::left_join(seq_colors[[color_scheme]], by = 'element') |>
    dplyr::mutate(color = ifelse(is.na(color), 'grey', color)) |>

    # apply colors
    dplyr::mutate(seq_colored = .apply_style_color(element, color, type = 'background')) |>

    # extract styled
    dplyr::summarize(
      .by = id,
      seq_colored = stringr::str_c(seq_colored, collapse = '')
    ) |>
    dplyr::pull(seq_colored)

  # spacing
  .out <- stringr::str_c(
    glue::glue('<p style = "letter-spacing:{spacing}px">'),
    .out,
    '</p>'
  )

  return(.out)
}

#' Apply style atomic function
#'
#' @param target input string
#' @param color color, can be hash
#' @param type color 'background' or 'text'
#'
#' @return character vector
#'
#' @examples \dpntrun {
#' # turn into tests
#'
#' .apply_style_color('EKLATI', 'yellow', 'text')
#' .apply_style_color('EKLATI', 'yellow', 'background')
#' .apply_style_color('', 'yellow', 'background')
#' .apply_style_color('EKLATI', 'yellow', 'x') #error
#' }
.apply_style_color <- function(target, color, type = 'background') {

  stopifnot(type %in% c('background', 'text'))

  .style <- switch(
    type,
    background = 'background-color',
    text = 'color'
  )

  stringr::str_glue('<a style = "{.style}:{color}">{target}</a>')
}


