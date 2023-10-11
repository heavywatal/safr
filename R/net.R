#' net format
#'
#' - ali: ``Number of bases in alignments in chain.''
#'
#' @seealso <https://genome.ucsc.edu/goldenPath/help/net.html>
#' @param file paths.
#' @rdname net
#' @export
read_net = function(file) {
  if (length(file) > 1L) {
    res = purrr::map(file, read_net) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE) |>
      stringr::str_subset("^net|^ fill")
    groups = lines |>
      stringr::str_starts("net") |>
      cumsum()
    res = split(lines, groups) |>
      purrr::map(read_net_section) |>
      purrr::list_rbind()
  }
  class(res) = c("tbl_net", class(res))
  res
}

read_net_section = function(lines) {
  net_line = stringr::str_split_1(lines[1L], " ")
  target_chr = net_line[2L]
  target_size = as.integer(net_line[3L])
  lines = stringr::str_remove(lines[-1L], "^ fill ")
  res = readr::read_delim(
    I(lines),
    delim = " ",
    col_names = c(
      "start", "twidth", "qchr", "strand", "qstart", "width",
      "id", "score", "ali", "qdup", "type"
    ),
    col_types = "iiccii_i_i_i_i_c",
    guess_max = 0L
  ) |>
    dplyr::mutate(chr = target_chr, size = target_size)
}

#' @export
as_paf.tbl_net = function(x) {
  x |>
    dplyr::mutate(end = .data$start + .data$twidth, qend = .data$qstart + .data$width) |>
    dplyr::mutate(match = .data$ali, qsize = NA_integer_) |>
    as_paf.default()
}
