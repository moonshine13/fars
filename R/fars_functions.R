#' Read csv file
#'
#' This is a simple function that reads csv file. In case if file does not exist
#' appropriate message will be shown.
#'
#' @param filename A character string giving the name of file to be read.
#'
#' @return This function returns a table created from csv file.
#'  Returned object has following classes:tbl_df, tbl, data.frame.
#'
#' @import readr 
#'
#' @import dplyr
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create filename in a given structure
#'
#' This is a simple function that creates filename in a appropriate structure:
#'  "accident_%d.csv.bz2"
#'
#' @param year An integer or character string that can be converted to integer
#'  meaning year
#'
#' @return This function returns a character string returns a character vector containing
#'  a formatted combination of text and variable values in a format:
#'  accident_%d.csv.bz2".
#'
#' @examples
#' make_filename("2015")
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Filter and select data for given years.
#'
#' This is a simple function that filter data for given years. In case if data for given year
#'  does not exist appropriate message is shown and NULL value is returned for this year.
#'  Function selects only MONTH and year variables from a data source.
#'
#' @param years A numeric or character vector that can be convereged to numeric vector,
#'  meaning years to be filter.
#'
#' @return This function returns a list with data frames for each year, containing two
#'  variables MONTH (int) and year (chr).
#'
#' @import dplyr
#'
#' @examples
#' fars_read_years(c(2013,2014))
#' fars_read_years(c("2013","2014"))
#' fars_read_years(2014)
#' fars_read_years("2014")
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    print(file)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize data for given years.
#'
#' This is a simple function that creates summary table with counts of number of records
#'  per each month in a specified years.
#'
#' @param years A numeric or character vector that can be convereged to numeric vector,
#'  meaning years to be filter.
#'
#' @return This function returns a table with counts.
#'  Returned object has following classes: tbl_df, tbl, data.frame.
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @examples
#' fars_summarize_years(c(2014,2015))
#' fars_summarize_years(c("2014"))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Create map with state shape and accidents.
#'
#' This is a simple function that reads csv file for particular year.
#' Filter data for particular state. In case if state does not exist
#' appropriate message will be shown. If there is no accidents in particular state
#' within specified year appropriate message will be shown.
#' Function plots state shape and marks accidents as a dots.
#'
#' @param state An integer or character string that can be converted to integer
#'  meaning state
#'
#' @param year An integer or character string that can be converted to integer
#'  meaning year
#'
#' @return This function returns a map with state shape and marked accidents.
#'
#' @import graphics
#'
#' @import maps
#'
#' @import dplyr
#'
#' @examples
#' fars_map_state(1,2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
