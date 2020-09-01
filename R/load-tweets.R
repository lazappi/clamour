#' Load tweets
#'
#' Load tweets for a clamour analysis
#'
#' @param query Query string to use for collecting new tweets.
#' @param cache_file Path to an `.Rds` file to store cached tweets.
#' @param timezone Time zone associated with tweets.
#' @param fixed If `TRUE` only read tweets from cache and do not download new
#'   tweets.
#'
#' @details
#' If a cache file exists tweets will first be read from here. New tweets will
#' also be downloaded if `fixed = FALSE`. Cached and new tweets are combined and
#' only the most recent version of each tweet is kept. Additional `datetime`,
#' `date` and `hour` fields are created using the supplied `timezone`.
#'
#' @return tibble containing tweets
#' @export
clamour_load_tweets <- function(query, cache_file = NULL,
                                timezone = Sys.timezone(),
                                fixed = FALSE) {

    cache_exists <- !is.null(cache_file) & fs::file_exists(cache_file)

    if (fixed & !cache_exists) {
        stop_msg <- paste(
            usethis::ui_code("fixed = TRUE"),
            "but", usethis::ui_code("cache_file"), "does not exist:",
            usethis::ui_path(as.character(cache_file)) # Convert NULL to char
        )
        usethis::ui_stop(stop_msg)
    }

    if (cache_exists) {
        cached_tweets <- readRDS(cache_file)
        usethis::ui_done(paste(
            "Loaded tweets from", usethis::ui_path(cache_file)
        ))
        usethis::ui_info(paste(
            usethis::ui_value(nrow(cached_tweets)),
            "tweets loaded from cache")
        )
    }

    if (fixed) {
        tweets <- cached_tweets
    } else {
        usethis::ui_todo("Downloading new tweets...")
        new_tweets <- rtweet::search_tweets(query, 10000) %>%
            dplyr::mutate(collected_at = Sys.time())
        usethis::ui_info(paste(
            "Found", usethis::ui_value(nrow(new_tweets)), "tweets")
        )

        if (cache_exists) {
            tweets <- new_tweets %>%
                rbind(cached_tweets) %>%
                dplyr::group_by(status_id) %>%
                dplyr::top_n(1, collected_at) %>%
                dplyr::ungroup()
        } else {
            tweets <- new_tweets
        }
    }

    if (!is.null(cache_file)) {
        usethis::ui_todo("Saving tweets to cache...")
        saveRDS(tweets, cache_file)
        usethis::ui_done(paste("Cached saved to", usethis::ui_path(cache_file)))
    }

    tweets <- tweets %>%
        dplyr::mutate(
            date     = lubridate::as_date(created_at, tz = timezone),
            datetime = lubridate::as_datetime(created_at, tz = timezone),
            hour     = lubridate::hour(datetime)
        )
    usethis::ui_info(paste(
        "Full dataset contains", usethis::ui_value(nrow(tweets)), "tweets"
    ))

    return(tweets)
}
