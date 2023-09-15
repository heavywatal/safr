#' chain format
#'
#' The positions are represented as zero-based half-open intervals.
#' e.g., the first 100 bases is represented as \{start: 0, end: 100\}
#'
#' @seealso <https://genome.ucsc.edu/goldenPath/help/chain.html>
#' @param file paths.
#' @rdname chain
#' @export
read_chain = function(file) {
  if (length(file) > 1L) {
    res = purrr::map(file, read_chain) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE) |>
      stringr::str_subset("^chain")
    res = readr::read_delim(
      I(lines),
      delim = " ",
      col_names = c(
        "score", "chr", "size", "tstrand", "start", "end",
        "qchr", "qsize", "strand", "qstart", "qend", "id"
      ),
      col_types = "_iciciiciciii",
      guess_max = 0L
    )
    .nrow = nrow(res)
    stopifnot(res[["tstrand"]] == "+")
    stopifnot(res[["start"]] < res[["end"]])
    stopifnot(res[["qstart"]] < res[["qend"]])
  }
  class(res) = c("tbl_chain", class(res))
  res
}

#' @export
as_paf.tbl_chain = function(x) {
  x |>
    dplyr::mutate(width = .data$qend - .data$qstart, match = .data$width) |>
    as_paf.default()
}