test_that("read_chain works", {
  x = read_chain(test_path("fixtures", "example.chain"))
  expect_identical(nrow(x), 2L)
})
