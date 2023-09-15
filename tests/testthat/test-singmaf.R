test_that("read_singmaf works", {
  x = read_singmaf(test_path("fixtures", "example.sing.maf"))
  expect_identical(nrow(x), 2L)
})
