data(aa_seq)
one_seq <- aa_seq$seq[1]

test_that("color works", {
  expect_type(.apply_style_color('EKLATI', 'yellow', 'text'), 'character')
  expect_type(.apply_style_color('EKLATI', 'yellow', 'background'), 'character')
  expect_error(.apply_style_color('EKLATI', 'yellow', 'x'))
})

test_that('colorseq works on vector', {
  expect_type(colorseq(one_seq), 'character')
})

test_that('colorseq works in tibble dplyr pipe', {
  expect_s3_class(aa_seq |> dplyr::mutate(seq_colored = colorseq(seq)), 'data.frame')
})
