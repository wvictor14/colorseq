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
  'H, K, R',	  'red',
  'F, W, Y',	  'blue',
  'I, L, M, V',	'green',
  'D, E',       'purple'
) |>
  mutate(element = stringr::str_split(group, ', ')) |>
  tidyr::unnest(element)

# collect color schemes
.seq_colors <- list(
  'clustal' = c_aa_clustal
)

use_data(.seq_colors)

# example data
protein_sequences <- system.file("extdata", "sample.fasta", package = "ggmsa")
protein_sequences <- readAAStringSet(protein_sequences)
ggmsa(protein_sequences, start = 221, end = 280, char_width = 0.5, seq_name = T) +
  geom_seqlogo() + geom_msaBar()

aa_seq <- tibble(seq = as.character(protein_sequences), name = names(protein_sequences)) |>
  mutate(seq = stringr::str_sub(seq, 221, end = 280))

use_data(aa_seq)
