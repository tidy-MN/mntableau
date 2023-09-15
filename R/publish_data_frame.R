#' Publish a R data frame as a Tableau published data source
#'
#' Requires reticulate and Python interpreter
#'
#' @param data A data frame that can be published as a data source.
#' @param conn A Tableau connection object. See `tableau_connect()`.
#' @param name The name of the published data source.
#' @param project_name The name of the project to publish to (must exist already). Case sensitive.
#' @param parent_project_name The name of the parent project of `project_name`. Use if `project_name` is not a unique project name on your Tableau site. Optional.
#' @param datasource_description A string describing the published data source. Optional.
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
#' publish_data_frame(tableau_sites, conn,
#' name = "MN Tableau sites",
#' project_name = "published_data_sources",
#' parent_project_name = "tableau_api_lib testing",
#' datasource_description = "MN state government Tableau site info"
#' )
#' }
#'
#' @export

publish_data_frame <- function(data,
                               conn,
                               name,
                               project_name,
                               parent_project_name = NULL,
                               datasource_description = NULL) {

  if(!is.data.frame(data)) stop("data must be a data frame")
  if(length(name) != 1) stop("name must be length 1")
  if(length(datasource_description) > 1) stop("datasource_description must be length 1")

  hyper <- reticulate::import("tableauhyperio")

  project_id <- get_project_id(conn, project_name, parent_project_name)

  tmp <- tempfile(fileext = ".hyper")
  hyper$to_hyper(data, tmp)

  py$conn <- conn
  py$tmp <- tmp
  py$ds_name <- name
  py$project_id <- project_id
  py$datasource_description <- datasource_description

  py_eval(
    "conn$publish_data_source(
      datasource_file_path = tmp,
      datasource_name = ds_name,
      project_id = project_id,
      datasource_description = datasource_description
    )
    "
  )
}
