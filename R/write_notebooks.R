#' Create New IS Notebook
#'
#' @description `create_notebook()` creates a new IS notebook
#' for a course given in credentials.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#' @param name (string) a name of the new notebook
#' @param shortcut (string) a short-cut name of the new notebook
#' @param initialize (logical) whether the block should be initialized,
#' i.e., whether the students should be added
#' @param init_value (string) what string should be written to the newly
#' created notebook (default is "")
#' @param can_see (logical) whether the block can be seen by students
#' @param can_add (logical) whether new students can be added to the notebook
#' @param see_stat (logical) whether students can see statistics of the notebook
#'
#' @return none; it only creates a block in IS
#'
#' @examples \dontrun{
#' create_notebook(micprez, "Test Block", "TB")
#' }
#'
#' @export
create_notebook <- function(
    credentials,
    name,
    shortcut,
    initialize = TRUE,
    init_value = "",
    can_see = TRUE,
    can_add = TRUE,
    see_stat = TRUE) {
    stopifnot(is_valid_credentials(credentials))
    op <- is_operation(
        credentials,
        "blok-novy",
        jmeno = name,
        zkratka = shortcut # ,
        # nahlizi = ifelse(can_see, "a", "n"),
        # nedoplnovat = ifelse(can_add, "a", "n"),
        # statistika = ifelse(see_stat, "a", "n")
    )
    curl::curl(op, "r") |> close()
    if (initialize) {
        initialize_is_block(credentials, shortcut, init_value = init_value)
    }
}


# initialize_is_block(credentials, block, init_value) initializes newly created
# IS blocks; it can also be used to add students to blocks
#
# reason: a newly created IS block is empty, i.e. no student is enrolled into the
# block; this function writes init_value to the block for all students enrolled
# in the course; this way, it adds them to the block
#
# the function is safe: it never rewrites any content that is in the block, even
# an empty strings; if the student has been already added to the block, his or
# her block is not rewritten
#
# inputs:
# - credentials (object of class "IScredentials") ... an access to IS as created
#   by credentials()
# - block (string) ... a short name of an IS block
# - init_value (string) ... what string should be written to non-initalized
#   blocks when they are initialized
initialize_is_block <- function(credentials, block, init_value = "") {
    stopifnot(is_valid_credentials(credentials))
    students <- get_students(credentials) |>
        dplyr::select(uco) |>
        dplyr::mutate(value = init_value)
    write_notebook(credentials, block, students, overwrite = FALSE)
}


# create_notebook_of_seminar_points(credentials, block_number) creates a new IS
# blocks for seminar points in a course given in credentials
#
# inputs:
# - credentials (object of class "IScredentials") ... an access to IS as created
#   by credentials()
# - block_number ... (integer vector) the id numbers of blocks; if more then one
#   is given, several blocks are created at once
# - initialize (logical scalar) ... whether the block should be initialized, see
#   initialize_is_block(); default is TRUE
# - init_value (string) ... what string should be written to non-initalized
#   blocks when they are initialized
#
# output:
# - none; it only creates a block in IS
#
# usage:
#   create_notebook_of_seminar_points(micprez, 8:13)
# result:
#   it creates six blocks in IS with names "Body_za_seminar_k_tematu_##" and
#   short names "bodysemin##" where ## is 08, 09, ..., 13
#
# create_notebook_of_seminar_points <- function(credentials, block_number,
#                                                initialize = TRUE,
#                                                init_value = "*") {
#     stopifnot(is_valid_credentials(credentials))
#     block_number <- str_pad(block_number, side = "left", width = 2, pad = "0")
#     walk(block_number,
#         ~ create_notebook(credentials,
#                           str_c("Body za seminář k tématu ", .),
#                           str_c("bodysemin", .),
#                           initialize = initialize, init_value = init_value,
#                           see_stat = FALSE)
#     )
# }


# write_one_value_to_notebook(credentials, block, uco, content) writes content
# to the IS block to a student with the uco
#
# inputs:
# - credentials (object of class "IScredentials") ... an access to IS as created
#   by credentials()
# - block (string) ... a short name of an IS block
# - uco (string) ... a student's uco (just one)
# - content (string) ... the contnet that sould be writen there
# - overwrite (logical scalar) ... whether the content should be overwritten
#   (default is TRUE)
#
# output:
# - none; it only writes to IS
#
# notes:
# - this works for just one student; for more students, see write_notebook()
write_one_value_to_notebook <- function(credentials, notebook, uco, content,
                                        overwrite = TRUE) {
    stopifnot(is_valid_credentials(credentials))
    overwrite <- if (overwrite) "a" else "n"
    op <- is_operation(credentials, "blok-pis-student-obsah",
        zkratka = notebook, uco = uco, obsah = content,
        prepis = overwrite
    )
    curl::curl(op, "r") |> close()
}


#' Write table of values to IS notebook
#' 
#' @description `write_notebook()` writes content to the IS block.
#'
#' @param credentials (object of class "IScredentials") credentials
#' created by credentials()
#' @param notebook (string) a short name of an IS notebook
#' @param tab (tibble) what should be written and for which students; it must
#' contain the following columns:
#' - uco (string) students' ucos
#' - value (string) the content that should be written there
#' @param overwrite (logical scalar) whether the content should be overwritten
#' (default is TRUE)
#'
#' @return none; it only writes to IS
#'
#' @examples \dontrun{
#' tab <- students |>
#'     filter(course == "mivs") |>
#'     select(uco = student_uco, value = full_string)
#' write_notebook(mivs, "test2", tab)
#' }
#'
#' @export
write_notebook <- function(credentials, notebook, tab, overwrite = TRUE) {
    stopifnot(is_valid_credentials(credentials))
    purrr::walk(
        1:nrow(tab),
        ~ write_one_value_to_notebook(credentials, notebook,
            uco = tab$uco[.],
            content = tab$value[.],
            overwrite = overwrite
        )
    )
}
