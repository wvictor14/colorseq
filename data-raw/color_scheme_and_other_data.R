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
c_aa_clustal <- tibble::tribble(
  ~ group, ~color,
  'G, P, S, T',	'#FAAE32FF',
  'H, K, R',	  '#F06A63FF',
  'F, W, Y',	  '#7D9FC2FF',
  'I, L, M, V',	'#51806AFF',
  'D, E',       '#A092B7FF'
) |>
  dplyr::mutate(element = stringr::str_split(group, ', ')) |>
  tidyr::unnest(element)


c_aa_chemistry <- tibble::tribble(
  ~ group, ~color,
  'P, A, V, M, L, I, G',	'#FAAE32FF',
  'E, D',	                '#F06A63FF',
  'K, R',	                '#7D9FC2FF',
  'N, T, C, Q, S',	      '#51806AFF',
  '-',                    'white'
) |>
  dplyr::mutate(element = stringr::str_split(group, ', ')) |>
  tidyr::unnest(element)


# collect color schemes
seq_colors <- list(
  'clustal' = c_aa_clustal,
  'chemistry' = c_aa_chemistry
)

use_data(seq_colors, overwrite = TRUE)

# example data
protein_sequences <- system.file("extdata", "sample.fasta", package = "ggmsa")
protein_sequences <- readAAStringSet(protein_sequences)
ggmsa(protein_sequences, start = 221, end = 280, char_width = 0.5, seq_name = T) +
  geom_seqlogo() + geom_msaBar()

aa_seq <- tibble(seq = as.character(protein_sequences), name = names(protein_sequences)) |>
  mutate(seq = stringr::str_sub(seq, 221, end = 280))

use_data(aa_seq)
