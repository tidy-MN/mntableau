#' Download a workbook from a Tableau site
#'
#' Requires reticulate and Python interpreter
#'
#' @param conn A Tableau connection object. See `tableau_connect()`.
#' @param name The name of the workbook. Case sensitive.
#' @param file The name of the file to output to.
#' @param project_name The name of the project that the workbook is in. Use if the workbook name is not unique on your Tableau site. Optional.
#' @param format One of "tbwx", "twb", "pdf", or "ppt". Defaults to"twbx" if NULL and no extension is provided in `file`.
#' @examples
#' /dontrun{
#'
#' keyring::key_set("tableau", "nageld1")
#'
#' conn <- tableau_connect("mdh_internal_dev",
#' username = "nageld1",
#' password = keyring::key_get("tableau", "nageld1")
#' )
#'
#' get_project_id(conn, "MNIT-MDH")
#' get_project_id(conn, "Newborn", parent_name = "PHL")
#' }
#'
#' 

download_workbook <- function(conn, name, file, project_name = NULL, format = NULL) {

  py <- reticulate::import_builtins()
  open <- reticulate::import("builtins")$open
  querying <- reticulate::import("tableau_api_lib.utils.querying")
  workbooks <- querying$get_workbooks_dataframe(conn)

  if(!is.null(project_name)) {
    project_id <- get_project_id(conn, project_name)
    workbooks$location <- sapply(workbooks$location, function(x) x['id'])
    id <- workbooks[workbooks$location == project_id & workbooks$name == name, "id"]
  } else id <- workbooks[workbooks$name == name, "id"]

  if(length(id) < 1) stop(paste0('No workbook found with name "', name, '"'))
  if(length(id) > 1) stop(paste0(
    'Multiple workbooks found with name "', name, '". Use a unique workbook name or the project_name argument to choose a project.')
  )

  if (is.null(format)) {
    ext <- regmatches(file, regexpr("(?<=\\.)(twbx|twb|pdf|ppt)$", file, perl = TRUE))
    format <- ifelse(length(ext), ext, "twbx")
  } else if (
    !format %in% c("twbx", "twb", "pdf", "ppt")
  ) stop("Invalid format. Argument format must be either twbx, twb, pdf, ppt or left as NULL.")

  method <- switch(format,
                   twbx = "download_workbook",
                   twb = "download_workbook",
                   pdf = "download_workbook_pdf",
                   ppt = "download_workbook_powerpoint"
  )

  response <- conn[method](id)
  with(open(file, mode="wb"), {file$write(response$content)}, as = 'file')

}
