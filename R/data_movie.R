#' Data about movies
#'
#' A dataset containing information about films, how popular they were, and
#' the extent to which they feature women.
#' 
#' This is a dataset that has been altered from the original 
#' dataset \code{movies} in the \code{jtools} package. The edits have 
#' been made for pedagogical purposes. 
#' Please consult \code{\link[jtools]{movies}} for the full dataset.
#'
#' @format A data frame with 831 rows and 9 variables:
#' \describe{
#'   \item{year}{The year of the movie's US theatrical release}
#'   \item{runtime}{The length of the movie in hours}
#'   \item{genre5}{Ordered factor, The movie's primary genre per IMDB, 
#'   fit into one of 5 broad categories: Action, Comedy, Drama, Thriller, 
#'   Other (reference category)}
#'   \item{metascore}{The movie's score on MetaCritic, ranging from 0 to 100}
#'   \item{imdb_rating}{The movie's rating on IMDB, ranging from 0 to 10}
#'   \item{bechdel_scale}{A granular measure of the bechdel test, 
#'   indicating not just whether the movie passed or failed but how close it 
#'   got to passing if it did fail, scale from 0 to 4.}
#'   \item{us_gross}{The movie's US gross in 2013 US dollars, per 10 million}
#'   \item{int_gross}{The movie's international gross in 2013 US dollars, 
#'   per 10 million}
#'   \item{budget}{The movie's budget in 2013 US dollars, per 10 million}
#'   
#' }
#' @source 
#' 
#' Prepared for R in \code{\link[jtools]{movies}}.
#' 
#' These data are aggregated from several sources. Metadata is gathered from
#' IMDB. Other information, particularly about the lines, is collected from
#' [The Pudding](https://github.com/matthewfdaniels/scripts/). The data
#' regarding the Bechdel Test, as well as about finances, comes from 
#' FiveThirtyEight and its associated R package (`fivethirtyeight` and its 
#' dataset, `bechdel`).
#' 
#' @name movie
#' @docType data
#' @keywords datasets
NULL
