STUDENT_ATTENDED <- stringr::str_c(
    "\\b",
    "\\u00fa\\u010dast",
    "\\b"
)


#' Get Names of All Presence Notebooks
#'
#' @description `list_presence_notebooks()` downloads
#' names of all existing presence blocks within several courses
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#'
#' @return a tibble of names of all existing blocks; it includes columns:
#' name, shortcut, course, and credential
#'
#' @note It is protected against errors and logs them.
#'
#' @examples \dontrun{
#' blocks <- list_presence_notebooks(micprez, mivs)
#' }
#'
#' @export
list_presence_notebooks <- function(credentials) {
    list_notebooks(credentials) |>
        dplyr::filter(stringr::str_detect(shortcut, "prez_\\d{2}"))
}


#' Read Students' Presence from IS
#'
#' `read_presence_notebook()` reads IS presence notebook and returns
#' its values as a tibble.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#' @param notebook (string) shortcut of the presence notebook
#'
#' @return A tibble with columns "uco", "content", "date", and "attendance".
#'
#' @note The function extracts UČO, content, number of attendances and
#' number of missed seminars; it is supposed that the content is in form
#' "DD.(M)M.: (ne)účast".
#'
#' @examples \dontrun{
#' read_presence_notebook(mivs, "test2")
#' }
#'
#' @export
read_presence_notebook <- function(credentials, notebook) {
    notebook <- read_notebook(credentials, notebook) |>
        dplyr::filter(!is.na(content))
    if (nrow(notebook) == 0) {
        return(
            tibble::tibble(
                uco = integer(0),
                content = character(0),
                date = as.Date(integer(0)),
                attendance = character(0)
            )
        )
    }
    inner <- stringr::str_match_all(
        notebook$content,
        stringr::regex("^(\\d{1,2}\\.\\s*\\d{1,2}\\.):\\s*(.*)$",
            multiline = TRUE
        )
    ) |>
        purrr::map(function(x) {
            colnames(x) <- c("content", "date", "attendance")
            tibble::as_tibble(x)
        })
    notebook |>
        dplyr::select(-content) |>
        dplyr::mutate(inner = inner) |>
        tidyr::unnest(inner) |>
        dplyr::mutate(date = lubridate::dmy(
            stringr::str_c(date, lubridate::year(Sys.Date()))
        ))
}


#' Read presence points of all students
#'
#' @description `read_all_presence_points()` returns a tibble
#' containing the number of attendances of all students enrolled
#' in several courses.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#'
#' @return a tibble which includes two columns: uco and number of points
#'
#' @note It is protected against errors; it logs.
#
#' @examples \dontrun{
#' pts <- read_all_presence_points(micprez, mivs)
#' }
#'
#' @export
read_all_presence_points <- function(credentials) {
    notebooks <- list_presence_notebooks(credentials) |> 
        dplyr::select(credentials, shortcut)
    points <- purrr::pmap(notebooks, ~ read_presence_notebook(..1, ..2))
    points |>
        dplyr::bind_rows() |>
        dplyr::mutate(
            attended = stringr::str_detect(attendance, STUDENT_ATTENDED)
        ) |>
        dplyr::group_by(uco) |>
        dplyr::arrange(date, .by_group = TRUE) |>
        dplyr::summarize(
            attendance_string = stringr::str_c(
                lubridate::day(date[attended]), ".",
                lubridate::month(date[attended]), ".",
                collapse = ", "
            ),
            attendance_points = sum(attended)
        ) |> 
        dplyr::mutate(
            course = credentials$course,
            credentials = list(credentials)
        ) |> 
        dplyr::select(course, everything())
}
