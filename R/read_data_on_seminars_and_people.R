#' Get list of all students enrolled in a course
#'
#' @description `get_students()` returns a tibble of students
#' enrolled in a given course.
#'
#' @param credentials (object of class "IScredentials") credentials created
#' by credentials()
#'
#' @return a tibble with columns uco, first_name, and last_name
#'
#' @examples \dontrun{
#' get_students(cred)
#' }
#'
#' @note There is some info on studium (faculty, program, ..., and
#' whether it is open or closed), see
#' <https://is.muni.cz/auth/napoveda/technicka/bloky_api?#predmet-seznam>
#' 
#' @note was `get_list_of_all_students()`
#'
#' @export
get_students <- function(credentials) {
    stopifnot(is_valid_credentials(credentials))
    students <- is_operation(credentials, "predmet-seznam") |>
        xml2::read_xml() |>
        xml2::xml_find_all("//STUDENT")
    tibble::tibble(
        course = credentials$course,
        uco = get_node(students, "UCO"),
        first_name = get_node(students, "JMENO"),
        last_name = get_node(students, "PRIJMENI"),
        role = "student",
        credentials = list(credentials)
    )
}


#' Get list of all open seminars
#'
#' @description `list_open_seminars()` returns a vector
#' of names of open seminars.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#'
#' @return a string vector of seminar names
#'
#' @note It is expected that open and only open seminars have empty
#' note POZNAMKA.
#'
#' @examples \dontrun{
#' open_seminars <- list_open_seminars(cred)
#' }
#'
#' @note was `get_list_of_open_seminars()`
#' @export
list_open_seminars <- function(credentials) {
    stopifnot(is_valid_credentials(credentials))
    seminars <- is_operation(credentials, "predmet-info") |>
        xml2::read_xml() |>
        xml2::xml_find_all("//SEMINAR")
    names <- seminars |>
        xml2::xml_find_all("./OZNACENI") |>
        xml2::xml_text()
    notes <- seminars |>
        xml2::xml_find_all("./POZNAMKA") |>
        xml2::xml_text()
    names[notes == ""]
}


#' Get seminar teachers
#'
#' @description `get_teachers()` returns a tibble of
#' seminar teachers.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#' @param seminars (optional, vector of seminar names)
#' if given, it gets the name, and UČO of the teachers of the given
#' seminars; otherwise, it returns it for all teachers of all open seminars
#'
#' @return a tibble with columns seminar, uco, first_name, and last_name
#'
#' @examples \dontrun{
#' get_teachers(cred)
#' get_teachers(cred, c("01", "02"))
#' }
#' 
#' @note was `get_seminar_teachers()`
#'
#' @export
get_teachers <- function(credentials, seminars = NULL) {
    stopifnot(is_valid_credentials(credentials))
    if (is.null(seminars)) {
        seminars <- list_open_seminars(credentials)
    }
    teachers <- is_operation(credentials, "seminar-cvicici-seznam",
        seminar = seminars
    ) |>
        xml2::read_xml() |>
        xml2::xml_find_all("//CVICICI")
    first_name <- teachers |>
        xml2::xml_find_all("./JMENO") |>
        xml2::xml_text()
    last_name <- teachers |>
        xml2::xml_find_all("./PRIJMENI") |>
        xml2::xml_text()
    uco <- teachers |>
        xml2::xml_find_all("./UCO") |>
        xml2::xml_text() |>
        as.integer()
    tibble::tibble(
        course = credentials$course,
        seminar = seminars,
        uco,
        first_name,
        last_name,
        role = "teacher",
        credentials = list(credentials)
    )
}


#' Get students enrolled in given seminars
#'
#' @description `get_seminar_students()` returns a tibble of students
#' enrolled in given seminars.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#' @param seminars An optional vector of seminar names. If given, the
#' function returns the name, and UČO of the students enrolled into
#' the given seminars; otherwise, it returns it all open seminars.
#'
#' @return A tibble of students enrolled in the given seminars.
#'
#' @note There is some info on studium (faculty, program, ..., and
#' whether it is open or closed), see 
#' https://is.muni.cz/auth/napoveda/technicka/bloky_api?fakulta=1456;obdobi=7903;predmet=1313283#predmet-info
#'
#' @examples \dontrun{
#' get_seminar_students(cred)
#' get_seminar_students(cred, c("01", "02"))
#' }
#'
#' @export
get_seminar_students <- function(credentials, seminars = NULL) {
    stopifnot(is_valid_credentials(credentials))
    if (is.null(seminars)) {
        seminars <- list_open_seminars(credentials)
    }
    get_students <- function(semin) {
        semin_id <- semin |>
            xml2::xml_find_first("./OZNACENI") |>
            xml2::xml_text()
        students <- semin |>
            xml2::xml_find_all("./STUDENT")
        first_name <- students |>
            xml2::xml_find_all("./JMENO") |>
            xml2::xml_text()
        last_name <- students |>
            xml2::xml_find_all("./PRIJMENI") |>
            xml2::xml_text()
        uco <- students |>
            xml2::xml_find_all("./UCO") |>
            xml2::xml_text() |>
            as.integer()
        tibble::tibble(seminar = semin_id, uco, first_name, last_name)
    }
    semins <- is_operation(credentials, "seminar-seznam",
        seminar = seminars
    ) |>
        xml2::read_xml() |>
        xml2::xml_find_all("//SEMINAR")
    semins |>
        purrr::map(get_students) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
            course = credentials$course,
            role = "student",
            credentials = list(credentials)
        ) |>
        dplyr::select(course, everything())
}
