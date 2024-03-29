# https://www.bioinformatics.nl/~berndb/aacolour.html
# dev libs
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(gt)
library(ggmsa)
library(msa)
library(purrr)

# color scheme
c_aa_clustal <- tribble(
  ~ group, ~color,
  'G, P, S, T',	'orange',
  'H, K, R',	'red',
  'F, W, Y',	'blue',
  'I, L, M, V',	'green',
) |>
  mutate(element = stringr::str_split(group, ', ')) |>
  tidyr::unnest(element)

# example data
mySequenceFile <- system.file("examples", "exampleAA.fasta", package="msa")
protein_sequences <- readAAStringSet(mySequenceFile)
aa_seq <- tibble(seq = as.character(protein_sequences), name = names(protein_sequences)) |>
  mutate(seq = stringr::str_sub(seq, 1, 20))

# apply style atomic function
.apply_style_color <- function(target, color, type = 'background') {

  stopifnot(type %in% c('background', 'text'))

  .style <- switch(
    type,
    background = 'background-color',
    text = 'color'
  )

  stringr::str_glue('<a style = "{.style}:{color}">{target}</a>')
}

.apply_style_color('EKLATI', 'yellow', 'text')
.apply_style_color('EKLATI', 'yellow', 'background')
.apply_style_color('EKLATI', 'yellow', 'x') #error

.color_sequence <- function(seq, color_scheme = 'clustal') {

  # split elements
  .splitted <- seq |>
    map(
      ~tibble(
        position = 1:nchar(.x),
        element = stringr::str_split_1(.x, '')
      )
    ) |>
    bind_rows(.id = 'id')

  # apply colors
  .out <- .splitted |>

    # add colors
    dplyr::left_join(c_aa_clustal, by = 'element') |>
    dplyr::mutate(color = ifelse(is.na(color), 'grey', color)) |>

    # apply colors
    dplyr::mutate(seq_colored = .apply_style_color(element, color, type = 'background')) |>

    # extract styled
    dplyr::summarize(
      .by = id,
      seq_colored = stringr::str_c(seq_colored, collapse = '')
    ) |>
    dplyr::pull(seq_colored)

  return(.out)
}

# on 1 string
one_seq <- aa_seq$seq[1]
.color_sequence(one_seq)

# compare in gt
tibble(
  seq = .color_sequence(one_seq),
  seq_raw = one_seq
) |>
  gt() |>
  fmt_markdown()

# in dplyr
aa_seq |>
  mutate(seq_colored = .color_sequence(seq)) |>
  select(name, everything()) |>
  gt() |>
  fmt_markdown() |>
  tab_style(
    style = cell_text(font = 'Cascadia Code'),
    locations = cells_body(
      columns = contains('seq')
    )
  )

