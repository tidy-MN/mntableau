#' Publish a R data frame as a Tableau published data source
#'
#' Requires reticulate and Python interpreter
#'
#' @param server A Tableau server object. See `tableau_connect()`.
#' @param name The name of the view to download.
#' @param file The file path to download to. Must have an extension of ".csv", ".png", or ".pdf".
#' @param workbook_name The name of the workbook that the view is within. Use if other views in the server share the same name. Optional.
#' @param filters A list of named items each having length 1 where the names represent the names of the fields to be filtered on and the values represent the items to keep. Optional.
#' @param parameters A list of named items each having length 1 where the names represent the names of the parameters and the values represent the parameter values. Optional.
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
#' file <- "C:/Users/nageld1/Downloads/test.pdf"
#' 
#' filters <- list(c("Customer Name" = "Aaron Hawkins"))
#' 
#' download_view(server, "Subtotals example", file, filters = filters)
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
  if (!(file_ext %in% c(".csv", ".png", ".pdf"))) {
    stop('file must have an extension of .csv, .png, or .pdf')
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
                   ".pdf" = "populate_pdf"
                   )
  
  options_method <- switch(file_ext,
                           ".csv" = "CSVRequestOptions",
                           ".png" = "ImageRequestOptions",
                           ".pdf" = "PDFRequestOptions"
  )
  
  request_options <- TSC[options_method]()
  
  for (i in filters){
    request_options$vf(names(i), unname(i))
  }

  for (i in parameters){
    request_options$parameters(names(i), unname(i))
  }  
    
  view_item <- server$views$get_by_id(id)
  server$views[method](view_item, req_options = request_options)
  
  #write to file
  with(reticulate::`%as%`(py$open(file, 'wb'), output), {
    #remove . from extension for view_item attribute
    output$write(view_item[sub("\\.", "", file_ext)])
  })
}