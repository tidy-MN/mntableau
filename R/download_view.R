#' Publish a R data frame as a Tableau published data source
#'
#' Requires reticulate and Python interpreter
#'
#' @param server A Tableau server object. See `tableau_connect()`.
#' @param name The name of the view to download.
#' @param file The file path to download to. Must have an extension of ".csv", ".png", or ".pdf".
#' @param workbook_name The name of the workbook that the view is within. Use if other views in the server share the same name. Optional.
#' @param filters A named list of items each having length 1 where the names represent the names of the fields to be filtered on and the values represent the items to keep. Optional.
#' @param parameters A named list of items each having length 1 where the names represent the names of the parameters and the values represent the parameter values. Optional.
#' @param options A named list of options to be passed to methods. See `download_options` for a data frame of all options available.
#' @examples
#' /dontrun{
#'
#' keyring::key_set("tableau", "nageld1")
#'
#' server <- tableau_connect("mdh_internal_dev",
#' username = "nageld1",
#' password = keyring::key_get("tableau", "nageld1")
#' )
#' 
#' view_name <- "Health Indicators"
#' workbook_name <- "World Indicators mntableau"
#' file <- tempfile(fileext = ".pdf")
#' filters <- list(c("Region" = "The Americas"))
#' 
#' download_view(
#'   server,
#'   name = view_name,
#'   file = file,
#'   workbook_name = workbook_name,
#'   filters = filters
#' )
#' }
#'
#' @export

download_view <- function(server, name, file,
                          workbook_name = NULL,
                          filters = list(),
                          parameters = list(),
                          options = list()) {
  
  py <- reticulate::import_builtins()
  TSC <- reticulate::import("tableauserverclient")
  
  file_ext <- regmatches(file, regexpr('\\.[^.]+$', file))
  if (!(file_ext %in% c(".csv", ".png", ".pdf", ".xlsx"))) {
    stop("file must have an extension of .csv, .png, .pdf, or .xlsx")
  }
  
  if (length(filters) > 0) {
    if (any(lengths(filters) != 1)) {
      stop("all list items in filters must be length 1")
    }
  }

  if (length(parameters) > 0) {
    if (any(lengths(parameters) != 1)) {
      stop("all list items in parameters must be length 1")
    }
  }
  
  id <- get_view_id(server, name, workbook_name)
  
  #select appropriate method
  method <- switch(file_ext,
                   ".csv" = "populate_csv",
                   ".png" = "populate_image",
                   ".pdf" = "populate_pdf",
                   ".xlsx" = "populate_excel"
                   )
  
  options_method <- switch(file_ext,
                           ".csv" = "CSVRequestOptions",
                           ".png" = "ImageRequestOptions",
                           ".pdf" = "PDFRequestOptions",
                           ".xlsx" = "ExcelRequestOptions"
  )
  
  write_method <- switch(file_ext,
                         ".csv" = "csv",
                         ".png" = "image",
                         ".pdf" = "pdf",
                         ".xlsx" = "excel"
  )
  
  #call appropriate TSC options method with options
  request_options <- do.call(TSC[options_method], options)
  
  for (i in filters){
    request_options$vf(names(i), unname(i))
  }

  for (i in parameters){
    #request_options parameter method doesn't work, using vf does
    request_options$vf(names(i), unname(i))
  }  
    
  view_item <- server$views$get_by_id(id)
  server$views[method](view_item, req_options = request_options)
  
  #cast to raw vector
  raw <- unlist(lapply(py$tuple(view_item[write_method]), as.raw))
  bin_file <- file(file, "wb")
  writeBin(raw, bin_file)
  close(bin_file)
}