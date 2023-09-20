#' Get List of Existing IS Notebooks
#'
#' `list_notebooks()` returns a tibble of existing IS notebook.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#'
#' @return A tibble of existing notebooks with columns "name" and "shortcut".
#'
#' @examples \dontrun{
#' list_notebooks(mivs)
#' }
#' 
#' @note was `get_list_of_is_blocks()`
#'
#' @export
list_notebooks <- function(credentials) {
    stopifnot(is_valid_credentials(credentials))
    blocks <- is_operation(credentials, "bloky-seznam") |>
        xml2::read_xml() |>
        xml2::xml_find_all("//POZN_BLOK")
    tibble::tibble(
        course = credentials$course,
        name = get_node(blocks, "JMENO"),
        shortcut = get_node(blocks, "ZKRATKA"),
        credentials = list(credentials)
    )
}


#' Check that notebook exists.
#'
#' `notebook_exists()` checks whether a notebook exists in a course.
#'
#' @param credentials (object of class "IScredentials") credentials
#'   created by credentials()
#' @param shortcut (string) notebook shortcut name
#' 
#' @return logical scalar
#' 
#' @examples \dontrun{
#' notebook_exists(mivs, "bodysemin02")
#' }
notebook_exists <- function(credentials, shortcut) {
    stopifnot(is_valid_credentials(credentials))
    notebooks <- list_notebooks(credentials)
    shortcut %in% notebooks$shortcut
}


#' Read content of an IS notebook
#'
#' `read_notebook()` reads the content of an IS notebook and returns it
#' as a tibble.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#' @param notebook (string) the shortcut of the notebook
#'
#' @return A tibble with columns "uco" and "content".
#'
#' @examples \dontrun{
#' read_notebook(mivs, "test")
#' }
#'
#' @note was `read_is_block()`
#'
#' @export
read_notebook <- function(credentials, notebook) {
    stopifnot(is_valid_credentials(credentials))
    students <- is_operation(
        credentials,
        "blok-dej-obsah",
        zkratka = notebook
    ) |>
        xml2::read_xml() |>
        xml2::xml_find_all("//STUDENT")
    tibble::tibble(
        course = credentials$course,
        notebook = notebook,
        uco = get_node(students, "UCO"),
        content = get_node(students, "OBSAH"),
        credentials = list(credentials)
    ) |>
        dplyr::mutate(uco = as.integer(uco))
}


#' Read Points from IS Notebook
#'
#' @description `read_point_notebook()` reads IS notebook and
#' returns points stored there as a tibble.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#' @param notebook (string) the short name of the notebook
#'
#' @return a tibble with columns uco and points
#'
#' @note The function extracts only UČO and points;
#' the points are expected to be in the following formats:
#' "*11", "*1.2", "*1,3", "*0.5", etc.
#'
#' @note was `read_is_point_block()`
#'
#' @examples \dontrun{
#' read_point_notebook(mivs, "test2")
#' }
#'
#' @export
read_point_notebook <- function(credentials, notebook) {
    stopifnot(is_valid_credentials(credentials))
    read_notebook(credentials, notebook) |>
        dplyr::mutate(points = extract_is_number(content)) |>
        dplyr::select(-content)
}
