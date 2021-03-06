% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-tweets.R
\name{clamour_load_tweets}
\alias{clamour_load_tweets}
\title{Load tweets}
\usage{
clamour_load_tweets(
  query,
  cache_file = NULL,
  timezone = Sys.timezone(),
  fixed = FALSE
)
}
\arguments{
\item{query}{Query string to use for collecting new tweets.}

\item{cache_file}{Path to an \code{.Rds} file to store cached tweets.}

\item{timezone}{Time zone associated with tweets.}

\item{fixed}{If \code{TRUE} only read tweets from cache and do not download new
tweets.}
}
\value{
tibble containing tweets
}
\description{
Load tweets for a clamour analysis
}
\details{
If a cache file exists tweets will first be read from here. New tweets will
also be downloaded if \code{fixed = FALSE}. Cached and new tweets are combined and
only the most recent version of each tweet is kept. Additional \code{datetime},
\code{date} and \code{hour} fields are created using the supplied \code{timezone}.
}
