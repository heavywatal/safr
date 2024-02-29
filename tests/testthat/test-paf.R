expect_paf = function(x, rows, qsize_na = NA, size_na = NA) {
  expect_s3_class(x, c("tbl_paf", "tbl_df"))
  expect_identical(dim(x), c(rows, 12L))
  if (isTRUE(qsize_na)) {
    expect_true(all(is.na(x$qsize)))
  } else if (isFALSE(qsize_na)) {
    expect_false(anyNA(x$qsize))
  }
  if (isTRUE(size_na)) {
    expect_true(all(is.na(x$size)))
  } else if (isFALSE(size_na)) {
    expect_false(anyNA(x$size))
  }
  invisible(x)
}

test_that("read/write and conversion work", {
  axt = read_axt(test_path("fixtures", "example.axt")) |>
    as_paf() |>
    expect_paf(2L, TRUE, TRUE)
  chn = read_chain(test_path("fixtures", "example.chain")) |>
    as_paf() |>
    expect_paf(2L, FALSE, FALSE)
  net = read_net(test_path("fixtures", "example.net")) |>
    as_paf() |>
    expect_paf(22L, TRUE, FALSE)
  maf = read_singmaf(test_path("fixtures", "example.sing.maf")) |>
    as_paf() |>
    expect_paf(2L, FALSE, FALSE)

  paf = rbind(axt, chn, net, maf) |>
    as_paf() |>
    expect_paf(28L)
  tmpfile = withr::local_tempfile(fileext = ".paf")
  ret = write_paf(paf, tmpfile)
  expect_identical(ret, paf)
  expect_identical(read_paf(tmpfile), paf)
  expect_identical(paf, read_paf(test_path("fixtures", "example.paf")))
})
