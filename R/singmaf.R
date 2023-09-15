#' Single coverage pairwise alignment in MAF format
#'
#' - start: ``This is a zero-based number.''
#' - size: ``This number is equal to the number of non-dash characters.''
#' - strand: ``If the field is "-" then this is the start relative to
#'   the reverse-complemented source sequence.''
#'
#' @seealso <https://genome.ucsc.edu/FAQ/FAQformat.html#format5>
#' @param file paths.
#' @rdname singmaf
#' @export
read_singmaf = function(file) {
  if (length(file) > 1L) {
    res = purrr::map(file, read_singmaf) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE)
    scores = lines |>
      stringr::str_subset("^a ") |>
      readr::parse_number()
    num_blocks = length(scores)
    lines = lines |> stringr::str_subset("^s ")
    raw = readr::read_table(
      I(lines),
      col_names = c("src", "start", "width", "strand", "size"),
      col_types = "_ciici_",
      guess_max = 0L
    )
    long = raw |>
      tidyr::separate_wider_delim("src", ".", names = c("species", "chr")) |>
      dplyr::mutate(end = .data$start + .data$width, .after = "start")
    res = long |>
      dplyr::select(!"species") |>
      dplyr::mutate(id = rep(seq_len(num_blocks), each = 2L)) |>
      dplyr::mutate(prefix = rep(c("", "q"), num_blocks)) |>
      tidyr::pivot_wider(
        values_from = !c("prefix", "id"),
        names_from = "prefix",
        names_glue = "{prefix}{.value}",
        names_vary = "slowest"
      ) |>
      dplyr::select(!c("id", "strand")) |>
      dplyr::rename(strand = "qstrand") |>
      dplyr::mutate(score = as.integer(scores))
  }
  class(res) = c("tbl_singmaf", class(res))
  res
}

#' @export
as_paf.tbl_singmaf = function(x) {
  x |>
    dplyr::mutate(match = pmin(.data$width, .data$qwidth), width = .data$qwidth) |>
    as_paf.default()
}
