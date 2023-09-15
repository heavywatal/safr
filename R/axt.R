#' axt format
#'
#' - start: ``The first base is numbered 1.''
#' - end: ``The end base is included.''
#' - strand: ``If the strand value is "-", the values of the aligning organism's start and end
#'   fields are relative to the reverse-complemented coordinates of its chromosome.''
#'
#' @seealso <https://genome.ucsc.edu/goldenPath/help/axt.html>
#' @param file paths.
#' @rdname axt
#' @export
read_axt = function(file) {
  if (length(file) > 1L) {
    res = purrr::map(file, read_axt) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE) |>
      stringr::str_subset("^\\d")
    res = readr::read_delim(
      I(lines),
      delim = " ",
      col_names = c("id", "chr", "start", "end", "qchr", "qstart", "qend", "strand", "score"),
      col_types = "iciiciici",
      guess_max = 0L
    )
    .nrow = nrow(res)
    stopifnot(res[["id"]][.nrow] == .nrow - 1L)
  }
  class(res) = c("tbl_axt", class(res))
  res
}

#' @export
as_paf.tbl_axt = function(x) {
  x |>
    dplyr::mutate(start = .data$start - 1L, qstart = .data$qstart - 1L) |>
    dplyr::mutate(qsize = NA_integer_, size = NA_integer_) |>
    dplyr::mutate(width = .data$qend - .data$qstart, match = .data$width) |>
    as_paf.default()
}
