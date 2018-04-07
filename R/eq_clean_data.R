#' @title Clean NOAA data frame
#'
#' @description This function takes \strong{raw} NOAA data frame and returns a \strong{clean} data frame
#' The clean dataframe will have a new column DATE which is created by uniting the YEAR, MONTH and DATE column
#' As the Date class is not good at handling BC dates, we had to do some integer arithemetic to get to negative years
#'
#' @param noaa_df NOAA data frame
#'
#' @return This function returns a tbl_df object
#'
#' @import dplyr
#' @export eq_clean_data
#'
#' @note NOAA data is downloaded from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#' @note YEAR in NOAA data can take values from -2150 to Present [Format +/-yyyy (-is B.C, +is A.D.)]
#' @note MONTH in NOAA data can take values from 1-12
#' @note DAY in NOAA data can take values from 1-31 (where months apply)
#' @note this is part of Module 1 of Capsone Project
#'
eq_clean_data <- function(noaa_df){

  # General Handling
  noaa_df <- noaa_df %>%
    dplyr::mutate(MONTH     = ifelse(is.na(MONTH), 1, MONTH),
                  DAY       = ifelse(is.na(DAY), 1, DAY),
                  LATITUDE  = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE)) %>%

  # Handling AD dates
  noaa_df_pos <- noaa_df %>%
    dplyr::filter(YEAR >= 0) %>%
    dplyr::mutate(DATE = as.Date(paste0(YEAR, "-", MONTH, "-", DAY)))

  # Handling BC dates
  noaa_df_neg <- noaa_df %>%
    dplyr::filter(YEAR < 0) %>%
    dplyr::mutate(DATE = as.Date(as.numeric(as.Date(paste0(-YEAR, "-", MONTH, "-", DAY))) - 2*as.numeric(as.Date(paste0(-YEAR, "-01-01"))) - 2*719528 + 1, origin="1970-01-01"))

  noaa_df <- rbind(noaa_df_neg, noaa_df_pos)

  return(eq_location_clean(noaa_df))
}


#' @title Clean LOCATION_NAME in NOAA data frame
#'
#' @description This function removes all instances of COUNTRY: in LOCATION_NAME and coverts all casing to title
#'
#' @param noaa_df NOAA data frame
#'
#' @return This function returns a tbl_df object
#'
#' @import stringr
#' @import dplyr
#' @export eq_location_clean
#'
#' @note NOAA data is downloaded from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#' @note LOCATION_NAME describes the Country, State, Province or City where the Earthquake occurred
#' @note this is part of Module 1 of Capsone Project
#'
eq_location_clean <- function(noaa_df){

  noaa_df <- noaa_df %>%
    dplyr::group_by(I_D) %>%
    dplyr::mutate(LOCATION_NAME = base::gsub(pattern=paste0(COUNTRY, ":"), replacement="", x=LOCATION_NAME, fixed = T)) %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_to_title(stringr::str_trim(LOCATION_NAME, side="both"))) %>%
    dplyr::ungroup()

  return(noaa_df)
}
