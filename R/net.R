#' net format
#'
#' - ali: ``Number of bases in alignments in chain.''
#'
#' @seealso <https://genome.ucsc.edu/goldenPath/help/net.html>
#' @param file path.
#' @rdname net
#' @export
read_net = function(file) {
  res = read_net_flat(file) |>
    dplyr::group_nest(.data$chr, .data$size) |>
    dplyr::mutate(data = lapply(.data$data, resolve_gap_meta)) |>
    tidyr::unnest("data")
  class(res) = c("tbl_net", class(res))
  res
}

read_net_flat = function(file) {
  lines = readr::read_lines(file, skip_empty_rows = TRUE) |>
    stringr::str_subset("^#", negate = TRUE) |>
    stringr::str_remove("^ +")
  groups = lines |>
    stringr::str_starts("net") |>
    cumsum()
  res = split(lines, groups) |>
    lapply(read_net_section) |>
    purrr::list_rbind()
}

read_net_section = function(lines) {
  net_line = stringr::str_split_1(lines[1L], " ")
  target_chr = net_line[2L]
  target_size = as.integer(net_line[3L])
  body = stringr::str_split_fixed(lines[-1], " (?=id |tN )", n = 2)
  read_net_fixed(body[, 1]) |>
    dplyr::bind_cols(read_net_optional(body[, 2])) |>
    dplyr::mutate(chr = target_chr, size = target_size)
}

read_net_fixed = function(lines) {
  cols = c("class", "start", "width", "qchr", "strand", "qstart", "qwidth")
  readr::read_delim(
    I(lines),
    delim = " ", col_names = cols, col_types = "ciiccii", guess_max = 0L
  )
}

read_net_optional = function(lines) {
  data.frame(
    id = stringr::str_extract(lines, "(?<=\\bid )\\d+") |> as.integer(),
    score = stringr::str_extract(lines, "(?<=\\bscore )\\d+") |> as.integer(),
    ali = stringr::str_extract(lines, "(?<=\\bali )\\d+") |> as.integer(),
    type = stringr::str_extract(lines, "(?<=\\btype )\\w+")
  )
}

resolve_fill_gap = function(ranges) {
  fills = ranges[ranges@elementMetadata$class == "fill"]
  gaps = ranges[ranges@elementMetadata$class == "gap"]
  x = fills
  # TODO: chimeric alignments can be merged accidentally
  while (length(gaps) > 0L || length(fills) > 0L) {
    x = IRanges::setdiff(x, gaps)
    fills = fills[IRanges::overlapsAny(fills, gaps, type = "within")]
    x = IRanges::union(x, fills)
    is_transloc = IRanges::overlapsAny(gaps, fills, type = "equal")
    gaps = gaps[IRanges::overlapsAny(gaps, fills, type = "within") & !is_transloc]
  }
  x
}

resolve_gap_meta = function(flat) {
  mdata = flat |>
    dplyr::rename(mstart = "start", mwidth = "width") |>
    dplyr::mutate(mend = .data$mstart + .data$mwidth, .after = "mstart")
  fill = mdata |>
    dplyr::filter(.data$class == "fill")
  gap = mdata |>
    dplyr::filter(.data$class == "gap") |>
    dplyr::select("qchr", gend = "mend", gstart = "qstart")
  ranges = IRanges::IRanges(flat$start, width = flat$width, class = flat$class) |>
    resolve_fill_gap()
  # TODO: slow and wrong: chimeric alignments cannot have proper mdata
  .within_y = dplyr::join_by(within(x$start, x$end, y$mstart, y$mend))
  fill_meta = ranges |>
    as.data.frame() |>
    dplyr::left_join(fill, by = .within_y) |>
    dplyr::slice_min(.data[["mwidth"]], by = c("start", "end"))
  fill_meta |>
    dplyr::left_join(gap, by = c("qchr", start = "gend")) |>
    dplyr::mutate(qstart = dplyr::coalesce(.data$gstart, .data$qstart)) |>
    dplyr::mutate(ali = pmin(.data$width, .data$ali)) |>
    dplyr::select(!c("class", "mstart", "mend", "mwidth", "qwidth", "gstart"))
}

#' @export
as_paf.tbl_net = function(x) {
  x |>
    dplyr::mutate(qend = .data$qstart + .data$width) |>
    dplyr::mutate(match = .data$ali, qsize = NA_integer_) |>
    as_paf.default()
}

utils::globalVariables(c("x", "y"))
