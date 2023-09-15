test_that("read_axt works", {
  x = read_axt(test_path("fixtures", "example.axt"))
  expect_identical(nrow(x), 2L)
})
