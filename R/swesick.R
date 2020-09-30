#' Download data for quarterly ongoing illnesses in Sweden
#' @description The function downloads data for quarterly ongoing illnesses in Sweden from 2005 and onward. The data is provided by the API for the Swedish Social Insurance Agency (Forsakringskassan). 
#' @param year Optional vector to download data for specific years.
#' @return A \code{data.frame} with the downloaded data.
#' @references Swedish Social Insurance Agency. \url{https://catalog.forsakringskassan.se/catalog/9/datasets/21}
#' @examples 
#' sickdata <- swesick()
#' sickdata_0506 <- swesick(year = c(2005,2006))
#' @export

swesick <- function(year=NULL){
  
  if(is.null(year)==FALSE){
    stopifnot(is.numeric(year)==TRUE)
    stopifnot(min(year)>=2005 & max(year)<=(as.numeric(substr(date(), (nchar(date())-3), nchar(date())))-1))
  }
  
  if(is.null(year)==TRUE){
    year <- 2005:(as.numeric(substr(date(), (nchar(date())-3), nchar(date())))-1)
  }
 
  requests <- paste("https://catalog.forsakringskassan.se/rowstore/dataset/9e3a7dba-b872-4239-8cb3-f40cd4d21a90?ar=", year, sep="")
  
  dataframes <- lapply(requests, function(x){
    request <- httr::GET(x)
    string <- rawToChar(request$content)
    Encoding(string) <- "UTF-8"
    convert <- jsonlite::fromJSON(string)
    frame <- convert$results
  })
  
  sick <- do.call(rbind, dataframes)
  return(sick)
}
