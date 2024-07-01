test_that("plot_dot works", {
  paf = read_paf(test_path("fixtures", "example.paf"))
  p = plot_dot(paf) +
    facet_grid_chr() +
    theme_dotplot()
  expect_s3_class(p, "gg")
})
