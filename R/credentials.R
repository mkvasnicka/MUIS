#' Create MUNI IS Credentials
#'
#' @description
#' `credentials()` returns an object that stores all that is
#' necessary to access a course in IS.
#'
#' @param key (string) key to IS API
#' @param faculty (string or numeric) faculty id number
#' @param course (string) course shortcut
#'
#' @return S3 object of class "ISNotebookCredentials"
#'
#' @examples \dontrun{
#' # Microeconomics 1 at ESF MU
#' mic <- credentials(key = "Z6o4VCwTOPQYGWI9",
#'                    faculty = 1456,
#'                    course = "BPE_MIE1")
#' }
#' @export
credentials <- function(key, faculty, course) {
    cred <- list(key = key, faculty = faculty, course = course)
    class(cred) <- "ISNotebookCredentials"
    cred
}


# method to print ISNotebookCredentials objects
#' @export
print.ISNotebookCredentials <- function(x, ...) {
    cat("Credentials to access course '", x$course, "' in IS.",
        sep = "")
    invisible(x)
}


# simple test to check whether the credentials are valid ISNotebookCredentials objects
is_valid_credentials <- function(credentials) {
    if (!inherits(credentials, "ISNotebookCredentials"))
        return(FALSE)
    if (!all(c("key", "faculty", "course") %in% names(credentials)))
        return(FALSE)
    TRUE
}
