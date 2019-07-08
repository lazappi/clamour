#' Setup a clamour project
#'
#' This function creates the directory structure necessary for analysing Twitter
#' hashtag activity and displaying the results on a website. It will also prompt
#' you to install packages required by the default analysis and to authorise
#' [rtweet](https://rtweet.info/).
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
    usethis::use_directory("analysis/figures", ignore = TRUE)
    usethis::use_template("_site.yml", "analysis/_site.yml",
                          package = "clamour")
    usethis::use_template("index.Rmd", "analysis/index.Rmd",
                          package = "clamour")
    usethis::use_template("about.Rmd", "analysis/about.Rmd",
                          package = "clamour")
    usethis::use_template("_analysis.Rmd", "analysis/_analysis.Rmd",
                          package = "clamour")
    clamour_new(
        name          = "EXAMPLE",
        hashtag       = "#rstats",
        description   = "An example analysis",
        start_day     = "2019-07-04",
        end_day       = "2019-07-06",
        timezone      = "Australia/Melbourne",
        theme         = "theme_light",
        accent        = "dodgerblue",
        accent2       = NULL,
        kcore         = 2,
        topics_k      = 6,
        bigram_filter = 3,
        fixed         = TRUE,
        seed          = 1
    )

    usethis::use_directory("docs")
    usethis::use_directory("data")
    if (!fs::file_exists(usethis::proj_path("data/EXAMPLE.Rds"))) {
        fs::file_copy(fs::path_package("clamour", "templates/example_data.Rds"),
                      usethis::proj_path("data/EXAMPLE.Rds"))
        usethis::ui_done("Copying {usethis::ui_path('data/EXAMPLE.Rds')}")
    }

    if (rstudio) {
        usethis::use_rstudio()
    }

    usethis::use_template("README.md", "README.md", package = "clamour")

    libs <- readLines(fs::path_package("clamour",
                                       "templates/hashtag_template.Rmd"))
    libs <- libs[grep("library", libs)]
    libs <- gsub('library\\(\\"', "", libs)
    libs <- gsub('\\"\\)', "", libs)
    libs <- libs[!(libs %in% utils::installed.packages())]
    n_libs <- length(libs)

    if (n_libs >= 1 && interactive()) {
        usethis::ui_info(
            "{n_libs} packages used in the default analysis are not installed:"
        )
        usethis::ui_line(usethis::ui_value(libs))
        install <- usethis::ui_yeah("Do you want to install them now?")
        if (install) {
            utils::install.packages(libs)
        }
    }

    usethis::ui_todo(
        "Run the following code to make sure rtweet is authorised:"
    )
    rt_test <- "rt <- rtweet::search_tweets('#rstats', n = 1000)"
    usethis::ui_line("{usethis::ui_code(rt_test)}")

    if (open) {
        if (usethis::proj_activate(path)) {
            # Working directory/active project changed; so don't undo on exit
            on.exit()
        }
    }

    invisible(usethis::proj_get())
}