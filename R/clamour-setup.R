#' Setup a clamour project
#'
#' This function creates the directory structure necessary for analysing Twitter
#' hashtag activity and displaying the results on a website.
#'
#' @param path A path for the project. If it exists it will be used. If it does
#'   exist it will be created, provided the parent path exists.
#' @param rstudio If `TRUE`, calls [usethis::use_rstudio()] to make the new
#'   project into an [RStudio
#'   Project](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects).
#'   If `FALSE` a sentinel `.here` file is placed so that the directory can be
#'   recognized as a project by the [here](https://here.r-lib.org) or
#'   [rprojroot](https://rprojroot.r-lib.org) packages.
#' @param open If `TRUE`, [activates][usethis::proj_activate()] the new project:
#'
#'   * If RStudio desktop, the package is opened in a new session.
#'   * If on RStudio server, the current RStudio project is activated.
#'   * Otherwise, the working directory and active project is changed.
#'
#' @references
#' Based on [usethis::create_package()](https://github.com/r-lib/usethis/blob/master/R/create.R)
#'
#' @return Path to the newly created project, invisibly.
#' @export
clamour_setup <- function(path, rstudio = rstudioapi::isAvailable(),
                          open = interactive()) {

    path <- fs::path_expand(path)
    dir <- fs::path_dir(path)

    if (!fs::file_exists(dir)) {
        usethis::ui_stop("Directory {usethis::ui_path(dir)} does not exist.")
    }
    if (!fs::is_dir(dir)) {
        usethis::ui_stop("{usethis::ui_path(dir)} is not a directory.")
    }

    if (fs::file_exists(path) && !fs::dir_exists(path)) {
        usethis::ui_stop(
            "{usethis::ui_path(path)} exists but is not a directory"
        )
    }

    if (fs::dir_exists(path)) {
        usethis::ui_done("Using existing directory {usethis::ui_path(path)}")
    } else {
        fs::dir_create(path, recurse = TRUE)
        usethis::ui_done("Creating {usethis::ui_path(path)}")
    }

    old_project <- usethis::proj_set(path, force = TRUE)
    on.exit(usethis::proj_set(old_project), add = TRUE)

    usethis::use_directory("analysis")
    usethis::use_template("_site.yml", "analysis/_site.yml",
                          package = "clamour")
    usethis::use_template("index.Rmd", "analysis/index.Rmd",
                          package = "clamour")
    usethis::use_template("about.Rmd", "analysis/about.Rmd",
                          package = "clamour")

    usethis::use_directory("docs")
    usethis::use_directory("data")

    if (rstudio) {
        usethis::use_rstudio()
    }

    if (open) {
        if (usethis::proj_activate(path)) {
            # Working directory/active project changed; so don't undo on exit
            on.exit()
        }
    }

    invisible(usethis::proj_get())
}