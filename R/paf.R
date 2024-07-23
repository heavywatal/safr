#' Pairwise mApping Format (PAF)
#'
#' The spec is in minimap2 manual:
#' <https://lh3.github.io/minimap2/minimap2.html>.
#' @param x data.frame.
#' @param file path.
#' @rdname paf
#' @export
as_paf = function(x) UseMethod("as_paf")

#' @export
as_paf.default = function(x) {
  res = x |>
    dplyr::select(dplyr::all_of(paf_columns)) |>
    dplyr::mutate(
      chr = wtl::as_factor_numeric(.data$chr),
      qchr = wtl::as_factor_numeric(.data$qchr)
    )
  class(res) = paf_class
  res
}

#' @rdname paf
#' @export
read_paf = function(file) {
  readr::read_tsv(file, col_names = paf_columns, col_types = "ciiicciiiiii") |>
    as_paf()
}

#' @rdname paf
#' @export
write_paf = function(x, file) {
  readr::write_tsv(x, file, na = "", col_names = FALSE)
}

paf_columns = c(
  "qchr", "qsize", "qstart", "qend", "strand", "chr", "size", "start", "end", "match", "width", "score"
)

paf_class = c("tbl_paf", "tbl_df", "tbl", "data.frame")
