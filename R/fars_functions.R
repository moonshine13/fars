#' Read  file into a data frame
#' 
#' This is a support function to read a delimited file into a data frame.
#' 
#' @param  filename characters vectors, containing file names or paths
#' 
#' @return This function returns a data frame. 
#' 
#' @section \code{Error:}
#' If \code{filename} does not exist, the function will throw an error.
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @examples
#' fars_read("accient_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#' fars_read("accident_2015.csv.bz2")

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Name  accident file year
#'
#' This is a support function to format the accident file string year
#' 
#' @param year a numeric string or integer containing the year accident
#' 
#' @return This function returns the string accident file name combined with the \code{year} 
#' 
#' @seealso 
#' \code{as.integr}
#' \code{sprintf}
#' 
#' @examples
#' make_filename(2013)
#' make_filename("2013")

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read accident year records in a list
#' 
#' This is a support function to read the FARS  years  records in a list. 
#' 
#' @param years is a vector or list  containing the accidents years.
#' 
#' @return This function returns a list containing the FARS months and years records.
#' 
#' @section \code{Error:}
#' If \code{years} contains an invalid year, the function gives a warning message.
#' if \code{years} does not contain a valid year, the function returns a null list
#' 
#' @seealso 
#' \code{make_filename}
#' \code{fars_read}
#' 
#' @importFrom dplyr mutate select %>%
#' 
#' @examples
#' fars_read_years(c("2013","2014"))
#' fars_read_years(c(2013,2014))
#' fars_read_years(list("2013",2014))


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
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

#' Count accidents by month and years
#' 
#' This function summarize the number of accidents in the FARS dataset by month and years.
#' 
#' @param years is a vector or list  containing the accidents years.
#' 
#' @return This function returns a data frame containing the FARS dataset records 
#' specified in \code{years}.
#' 
#' @section \code{Error:}
#' If \code{years} contains an invalid year, the function gives a warning message.
#' if \code{years} does not contain a valid year, the function returns an error
#' 
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' 
#' @seealso \code{fars_read_years}
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#' fars_summarize_years(c("2013"))
#' fars_summarize_years(list("2013",2014))
#' 
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map the number of accidents by State and year
#' 
#' This function plot the number of accidents in the FARS dataset by states and years.
#' 
#' @param state.num vector or list representing the state number
#' @param year vector or list representing the accidents years
#' 
#' @section \code{Error:}
#' If \code{years} contains an invalid year, the function gives a warning message.
#' if \code{years} does not contain a valid year, the function returns an error
#' if \code{state.num} does not contain a valid state number, the function returns an error
#' Error in get(dbname): object 'stateMapEnv' not found. It seems the object is missing in the maps library
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples
#' fars_map_state(1,2013)
#' fars_map_state(c(1,2),c(2013,2014))
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