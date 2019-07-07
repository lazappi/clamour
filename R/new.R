#' New clamour analysis
#'
#' Creates a new analysis file based on the clamour template.
#'
#' @param name Name of the analysis. This is used to name files so should
#'   usually be short and simple.
#' @param hashtag The hashtag to use for this analysis, starting with "#".
#' @param description A more detailed description of the analysis, details of
#'   the event etc.
#' @param start_day First day to be included in the analysis.
#' @param end_day Last day to be included in the analysis.
#' @param timezone Timezone to be used for the analysis.
#' @param theme Name of a [ggplot2](https://ggplot2.tidyverse.org/) theme
#'   function to use for plots.
#' @param accent Accent colour to use for plots.
#' @param accent2 Secondary accent colour to use for plots. If `NULL` this will
#'   be created based on `accent`.
#' @param kcore k-core to use for filtering the mentions network.
#' @param topics_k Number of topics to use for topic modelling.
#' @param bigram_filter Number of co-occurances for filtering the bigram graph.
#' @param fixed Whether to continue to search and download new tweets. Set to
#'   `TRUE` to fix the dataset used for analysis.
#' @param seed Random seed set at the start of analysis.
#' @param dir Directory to save the new analysis file.
#'
#' @return Path to the newly created file, invisibly
#' @export
clamour_new <- function(name, hashtag,
                        description = paste("An analysis of", hashtag),
                        start_day = Sys.Date(), end_day = Sys.Date() + 1,
                        timezone = Sys.timezone(), theme = "theme_light",
                        accent = "dodgerblue", accent2 = NULL, kcore = 2,
                        topics_k = 6, bigram_filter = 3,
                        fixed = FALSE, seed = 1, dir = "analysis") {

    if (!startsWith(hashtag, "#")) {
        usethis::ui_stop(
            "hashtag should start with #: {usethis::ui_value(hashtag)}"
        )
    }

    if (is.null(accent2)) {
        accent2 <- grDevices::rgb(
            t((255 - (255 - grDevices::col2rgb(accent)[, 1]) / 2)),
            maxColorValue = 255
        )
    }

    if (!fs::is_dir(usethis::proj_path(dir))) {
        usethis::ui_stop("{usethis::ui_path(dir)} is not a directory.")
    }

    name_path <- gsub("[^[:alnum:] ]", "", name)
    name_path <- gsub(" ", "_", name_path)
    name_path <- paste0(name_path, ".Rmd")

    data_list = list(
        TITLE       = name,
        DESCRIPTION = description,
        HASHTAG     = hashtag,
        START_DAY   = start_day,
        END_DAY     = end_day,
        TIMEZONE    = timezone,
        THEME       = theme,
        ACCENT      = accent,
        ACCENT2     = accent2,
        KCORE       = kcore,
        TOPICS      = topics_k,
        BIGRAM      = bigram_filter,
        FIXED       = fixed,
        SEED        = seed
    )

    usethis::use_template("hashtag_template.Rmd", fs::path(dir, name_path),
                          package = "clamour", data = data_list)

    invisible(fs::path(dir, name_path))
}