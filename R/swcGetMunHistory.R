#' Get municipality mutation history
#'
#' The history goes back as far as the mutation number becocmes 1000
#'   (first time registration)
#'
#' @param munId Municipality Id as integer
#'
#' @examples
#' waedenswil_history <- swcGetMunHistory(293)
#'
#' @export
swcGetMunHistory <- function(munId){

  mutations <- swcGetMutations() %>%
    dplyr::select(
      mMutationNumber,
      mMutationId,
      cAbbreviation,
      mHistId.x, mId.x,
      mShortName.x,
      mHistId.y, mId.y,
      mShortName.y,
      mAdmissionMode,
      mMutationDate)

  t <-
    dplyr::filter(mutations, mId.y == munId) %>%
    dplyr::filter(mHistId.y == max(mHistId.y))

  success <- 1

  while (success != 1000) {

    vector <- 1

    t <- purrr::reduce(
      .x = vector,
      ~add_past(..1,mutations = mutations),
      .init = t
    )

    success <- as.numeric(max(unique(na.omit(t[1]))))
  }

  t
}



#' Add the past to original data
#'
#' The past is searched by the mHistId.x of the origial file
#'
#' @param x The original data as tibble.
#'
#' @param mutations Mutation file
#'
#' @examples
#' mutations <- swcGetMutations()
#'
#' t <-
#'  dplyr::filter(mutations, mId.y == 293) %>%
#'  dplyr::filter(mHistId.y == max(mHistId.y))
#'
#' t_past <- add_past(t, mutations)
#'
#' @export
add_past <- function(x, mutations){

  t_added <- dplyr::filter(mutations, mHistId.y %in% x$mHistId.x) %>% full_join(x, by = c("mHistId.y" = "mHistId.x"))

}















