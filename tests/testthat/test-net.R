test_that("read_net works", {
  x = read_net(test_path("fixtures", "example.net"))
  expect_identical(nrow(x), 2L)
})
