#' Read the FARS (Fatality Analysis Reporting System) data set
#'
#' \code{fars_read} reads the FARS data set from disk. This is a wrapper around the
#' \code{readr} function from the \code{readr} package. An error will be raised if the
#' file does not exist.
#'
#' @param filename A single string, path to a file, or a connection.
#'
#' @return The FARS data set (tibble data frame).
#'
#' @examples
#' \donttest{
#' fars_read(filename = '../data/accident_2013.csv.bz2')
#' fars_read('accident_2013.csv.bz2')
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    read_csv(filename, progress = FALSE)
  })
  tbl_df(data)
}

#' Construct a filename (string) for a subset of the FARS data.
#'
#' \code{make_filename} takes a number (\code{year}, integer or numeric) on input and appends it
#' to a filename string. An error will be raised if \code{year} cannot be coerced to an integer.
#'
#' @param year A single number (integer or numeric).
#'
#' @return A filename (single string)
#'
#' @examples
#' make_filename(year = 2013)
#' make_filename(2014L)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read the FARS data from disk for a sequence of years
#'
#' \code{fars_read_years} reads the FARS data from disk for input years and outputs
#' a list of data frames. A warning will be generated if data for a given year are
#' not available.
#'
#' @param years A list of numbers (years).
#'
#' @return A list of data frames or NULL if the file cannot be found
#'
#' @examples
#' \donttest{
#' fars_read_years(2015)
#' }
#'
#' @importFrom dplyr mutate select %>%
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      mutate(dat, year = year) %>%
        select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Generate a summary (counts per year) for the FARS data
#'
#' \code{fars_summarize_years} reads the FARS data from disk for input years and
#' generates a summary.
#'
#' @param years A list of numbers (years).
#'
#' @return Summary of accidents' counts by year (data frame).
#'
#' @examples
#' \donttest{
#' fars_summarize_years(years = c(2013,2014))
#' fars_summarize_years(2015)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  bind_rows(dat_list) %>%
    group_by(year, MONTH) %>%
    summarize(n = n()) %>%
    spread(year, n)
}

#' Map fatal accidents on a state polygon for a given year.
#'
#' \code{fars_maps_state} generates a maps of fatal accidents for
#' a given state and year. An error is raised for invalid state numbers.
#' A warning is displayed if a year is not present in the data set.
#'
#' @param state.num State code (integer)
#' @param year Year (integer)
#'
#' @return A map of fatal accidents for a given state and year.
#'
#' @examples
#' \donttest{
#' fars_map_state(state.num = 11, year = 2013)
#' fars_map_state(17, 2014)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    points(LONGITUD, LATITUDE, pch = 46)
  })
}
