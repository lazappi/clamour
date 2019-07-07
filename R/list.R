#' clamour table
#'
#' Read the YAML front matter from a set of clamour analysis files and produce
#' a tidy data.frame.
#'
#' @param path Path to directory to search for `.Rmd` files
#' @param ignore Vector or files to ignore
#'
#' @return data.frame containing information on analysis files
#'
#' @export
clamour_table <- function(path = ".",
                          ignore = c("index.Rmd", "about.Rmd",
                                     "_analysis.Rmd")) {

    rmds <- fs::dir_ls(path, glob = "*.Rmd")
    rmds <- rmds[!(rmds %in% ignore)]
    rmd_paths <- fs::path(path, rmds)

    yamls <- lapply(rmd_paths, rmarkdown::yaml_front_matter)

    rows <- lapply(seq_len(length(yamls)), function(idx) {
        yaml <- yamls[[idx]]
        rmd <- rmds[idx]
        data.frame(
            File        = rmd,
            Name        = yaml$title,
            Description = yaml$subtitle,
            Hashtag     = yaml$params$hashtag,
            StartDay    = as.Date(yaml$params$start_day),
            EndDay      = as.Date(yaml$params$end_day),
            Timezone    = yaml$params$timezone,
            Theme       = yaml$params$theme,
            Accent      = yaml$params$accent,
            Accent2     = yaml$params$accent2,
            kCore       = yaml$params$kcore,
            Topics      = yaml$params$topics_k,
            Bigram      = yaml$params$bigram_filter,
            Fixed       = yaml$params$fixed,
            Seed        = yaml$params$seed,

            stringsAsFactors = FALSE
        )
    })

    df <- do.call("rbind", rows)
    rownames(df) <- NULL

    return(df)
}

#' clamour list
#'
#' Produce a Markdown string with details of all the clamour analysis files in a
#' directory. This function is primarily designed to be used in the index of a
#' website so the list is automatically updated.
#'
#' @param path Path to directory to search for `.Rmd` files
#' @param ignore Vector or files to ignore
#'
#' @return Markdown string describing analysis files
#'
#' @export
clamour_list <- function(path = ".",
                         ignore = c("index.Rmd", "about.Rmd",
                                    "_analysis.Rmd")) {

    analyses <- clamour_table(path = path, ignore = ignore)

    str <- ""

    for (idx in seq_len(nrow(analyses))) {
        analysis <- analyses[idx, ]

        html <- gsub(".Rmd", ".html", analysis$File)

        str <- paste0(
            str,
            "* [", analysis$Name, "](", html, ")",
            " - ", analysis$Description,
            " (", analysis$StartDay, " - ", analysis$EndDay, ")",
            "\n"
        )
    }

    return(str)
}