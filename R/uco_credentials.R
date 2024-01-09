#' Create MUNI IS UCO Credentials
#'
#' @description
#' `uco_credentials()` returns an object that stores all that is
#' necessary to access files in IS.
#'
#' @param uco (string) non-personal UCO
#' @param password (string) password for the UCO
#'
#' @note See https://is.muni.cz/auth/elearning/test_export?help=1.
#'
#' @return S3 object of class "ISUCOCredentials"
#'
#' @examples \dontrun{
#' # Microeconomics 1 at ESF MU
#' cred <- uco_credentials(
#'    uco = "123456",
#'   password = "password"
#' )
#' }
#' @export
uco_credentials <- function(uco, password) {
    h <- curl::new_handle()
    curl::handle_setopt(
        handle = h,
        httpauth = 1,
        userpwd = stringr::str_c(uco, ":", password)
    )

    cred <- list(uco = uco, handle = h)
    class(cred) <- "ISUCOCredentials"
    cred
}


# method to print ISUCOCredentials objects
#' @export
print.ISUCOCredentials <- function(x, ...) {
    cat("Credentials to files in IS via UCO ", x$uco, ".",
        sep = ""
    )
    invisible(x)
}


# simple test to check whether the credentials are valid ISNotebookCredentials objects
is_valid_uco_credentials <- function(credentials) {
    if (!inherits(credentials, "ISUCOCredentials"))
        return(FALSE)
    if (!all(c("uco", "handle") %in% names(credentials)))
        return(FALSE)
    if (!inherits(credentials$handle, "curl_handle"))
        return(FALSE)
    TRUE
}
