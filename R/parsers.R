extract_is_number <- function(num) {
    num |>
        stringr::str_extract("\\*(\\d)*[\\.,]{0,1}(\\d)*") |>
        stringr::str_remove("\\*") |>
        stringr::str_replace(",", ".") |>
        as.double()
}
