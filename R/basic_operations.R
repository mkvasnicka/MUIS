# Create MUNI IS Operation
#
# @description `is_operation()` creates the url to read or write
# to IS notebooks.
#
# @param credentials (object of class "IScredentials") credentials
# created by credentials()
# @param op (string) operation, e.g. "predmet-info"
# @param ... (pairs of named strings or string vectors) the other parameter
#
# @return one string; pairs in ... are translated to key=value pairs
# separated by semicolons; if the value is a character vector, it is expanded
#
# @examples \dontrun{
# is_operation(credentials, "predemet-info", a = "a", b = letters[1:3])
# # returns:
# #   "https://is.muni.cz/export/pb_blok_api?klic=tCK5EYgS4HQvQ12_;fakulta=1456;kod=BPE_MIE1;operace=predemet-info;a=a;b=a;b=b;b=c"
# }
is_operation <- function(credentials, op, ...) {
    stopifnot(is_valid_credentials(credentials))
    params <- list(...)
    credentials$op <- op
    basic_string <- glue::glue(
        "https://is.muni.cz/export/pb_blok_api?klic={key};",
        "fakulta={faculty};kod={course};operace={op}",
        .envir = as.environment(credentials)
    )
    if (length(params) > 0) {
        params <- purrr::map(
            names(params),
            function(n) {
                stringr::str_c(n, params[[n]], sep = "=")
            }
        ) |>
            unlist() |>
            stringr::str_c(collapse = ";")
        basic_string <- stringr::str_c(basic_string, params, sep = ";")
    }
    basic_string |>
        utils::URLencode()
}


get_node <- function(xml, node) {
    xml |>
        rvest::html_element(xpath = node) |>
        xml2::as_list() |>
        purrr::map(~ ifelse(length(.) == 0, NA_character_, .)) |>
        unlist()
}
