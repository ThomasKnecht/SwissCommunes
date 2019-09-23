#' Get municipality merges for a certain year and cantona
#'
#' @param year The year where the merges took place
#'
#' @param canton The abbreviation as characger
#'   of a canton from which the merges should be returned.
#'   If NULL, all merges are returned
#'
#' @examples
#' zuerich_merges <- swcGetMunMerges(year = 2019, canton = "ZH")
#'
#' @export
swcGetMunMerges <- function(year = NULL, canton = NULL) {
  if (is.null(canton)) {
    mutations <- swcGetMutations()
  } else {
    mutations <- swcGetMutations(canton = canton)
  }

  mun.mut.year <- subset(
    mutations,
    mMutationDate >= as.Date(paste0(year, "-01-01")) &
      mMutationDate < as.Date(paste0(year, "-12-31"))
  )

  mun.mut.all <- merge(
    mun.mut.year,
    mun.mut.year,
    by.x = "mHistId.y", by.y = "mHistId.x", all = T, incomparables = NA
  )

  mun.mut <- subset(mun.mut.all, !is.na(get("mMutationNumber.x")))

  mun.mut.red <- dplyr::filter(mun.mut,
    !(mMutationNumber.x %in% unique(na.omit(mun.mut$mMutationNumber.y)))
  )

  mun.mut.fus <- dplyr::filter(
    mun.mut.red,
    !grepl(c("Reassignment|name"), mAdmissionMode.x)
  )


  mun.mut.fus <- dplyr::mutate(
    mun.mut.fus,
    name_to = ifelse(!is.na(mShortName.y.y), mShortName.y.y, mShortName.y.x)
  )

  mun.mut.final <- dplyr::select(mun.mut.fus,
    canton = cAbbreviation.x,
    mId_from = mId.x.x,
    name_from = mShortName.x.x,
    mId_to = mId.y.x,
    name_to
  )
}
