#' Gets answers from a test (odpovednik) from IS.
#'
#' This function retrieves answers from a specified test in IS.
#' It requires valid UCO credentials and test URL.
#'
#' @description 
#' `get_answers()` returns a string with the content of the test answers.
#'
#' `get_answers_table()` returns a tibble with the content of the test answers.
#'
#' @param test_url A string. The URL of the test .qdesc file.
#'   It may be shortened---you may omit "https://is.muni.cz/auth"
#'   and start with the leading "/el/" part.
#' @param cred The UCO credentials.
#' @param type A string. The type of the output. Default is "simple".
#' @param format A string. The format of the output. Default is "csv".
#' @param ... Additional parameters for the URL query.
#'
#' @examples
#' \dontrun{
#' get_answers(
#'  "https://is.muni.cz/auth/el/econ/podzim2023/MPE_MIVS/odp/tb/kviz/kviztema11.qdesc",
#'  cred
#' )
#' get_answers_table(
#'   "/el/econ/podzim2023/MPE_MIVS/odp/tb/kviz/kviztema11.qdesc",
#'   cred
#' )
#' }
#' @export
# TODO: implement ... for other parameters for the URL query
get_answers <- function(test_url, cred, type = "simple", format = "csv", ...) {
    stopifnot(is_valid_uco_credentials(cred))
    url <- paste0(
        "https://is.muni.cz/auth/elearning/test_export?testurl=",
        curl::curl_escape(test_url), ";",
        "type=", type, ";",
        "format=", format
    )
    curl::curl_fetch_memory(url, handle = cred$handle)$content |>
        rawToChar()
}


#' @rdname get_answers
#' export
# TODO: implement ... for other parameters for the URL query
get_answers_table <- function(test_url, cred, ...) {
    get_answers(test_url, cred) |>
        readr::read_delim(
            delim = ";", col_types = readr::cols(
                uco = readr::col_integer(),
                jmeno = readr::col_character(),
                pocet = readr::col_double(),
                cas_provedeni = readr::col_datetime(format = "%d. %m. %Y %H:%M"),
                obor = readr::col_character(),
                studium = readr::col_character()
            )
        ) |>
        dplyr::transmute(
            uco = uco,
            student_name = jmeno,
            points = pocet,
            time = cas_provedeni
            # time = lubridate::with_tz(cas_provedeni, "Europe/Prague")
        )
}




# ```{r}
# # simple example
# library(curl)
# library(readr)
# url <- "https://is.muni.cz/auth/elearning/test_export?testurl=https://is.muni.cz/auth/el/econ/podzim2023/MPE_MIVS/odp/tb/prubeztftest/prubtftest1prez.qdesc;type=simple;format=csv"
# h <- curl::new_handle()
# curl::handle_setopt(
#   handle = h,
#   httpauth = 1,
#   userpwd = "uco:primarni_heslo"
# )
# resp <- curl::curl_fetch_memory(
#   url,
#   handle = h
# )
# ddd <- resp$content |> rawToChar()
# ooo <- readr::read_delim(
#   ddd,
#   delim = ";", col_types = readr::cols(
#     uco = col_double(),
#     jmeno = col_character(),
#     pocet = col_double(),
#     cas_provedeni = col_datetime(format = "%D. %m. %Y %H:%M"),
#     obor = col_character(),
#     studium = col_character()
#   )
# )
# ```

# ```{r}
# # function to read the answers into a data frame
# get_answers <- function(test_url, handle) {
#   url <- paste0(
#     "https://is.muni.cz/auth/elearning/test_export?testurl=",
#     curl::curl_escape(test_url),
#     ";type=simple;format=csv"
#   )
#   resp <- curl::curl_fetch_memory(url, handle = handle)$content |> 
#     rawToChar() |>
#     readr::read_delim(
#       delim = ";", col_types = readr::cols(
#         uco = readr::col_integer(),
#         jmeno = readr::col_character(),
#         pocet = readr::col_double(),
#         cas_provedeni = readr::col_datetime(format = "%d. %m. %Y %H:%M"),
#         obor = readr::col_character(),
#         studium = readr::col_character()
#       )
#     ) |> 
#     dplyr::transmute(
#       uco = uco,
#       student_name = jmeno,
#       points = pocet,
#       time = cas_provedeni
#       # time = lubridate::with_tz(cas_provedeni, "Europe/Prague")
#     )
# }
# ```

# ```{r}
# # usage
# url <- "https://is.muni.cz/auth/el/econ/podzim2023/MPE_MIVS/odp/tb/prubeztftest/prubtftest1prez.qdesc"
# h <- curl::new_handle()
# curl::handle_setopt(
#   handle = h,
#   httpauth = 1,
#   userpwd = "uco:primarni_heslo"
# )
# tab <- get_answers(url, h)
# ```


# Načítání souborů z odevzdávárny AVED
# ====================================

# Přes neosobní UČO by se měly dát číst i soubory z odevzdávárny AVED pro nový systém
# zpětné vazby.

# Dokumentace: https://is.muni.cz/auth/napoveda/technicka/spravce_souboru_api


