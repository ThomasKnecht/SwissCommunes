library(dplyr)

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




add_past <- function(x, mutations){

  t_added <- dplyr::filter(mutations, mHistId.y %in% x$mHistId.x) %>% full_join(x, by = c("mHistId.y" = "mHistId.x"))

}















