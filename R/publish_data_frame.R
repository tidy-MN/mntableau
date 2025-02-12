#' Publish a R data frame as a Tableau published data source
#'
#' Requires reticulate and Python interpreter
#'
#' @param data A data frame that can be published as a data source.
#' @param server A Tableau server object. See `tableau_connect()`.
#' @param name The name of the published data source.
#' @param project_name The name of the project to publish to (must exist already). Case sensitive.
#' @param parent_project_name The name of the parent project of `project_name`. Use if `project_name` is not a unique project name on your Tableau site. Optional.
#' @param datasource_description A string describing the published data source. Optional.
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
#' publish_data_frame(tableau_sites, server,
#' name = "MN Tableau sites",
#' project_name = "published_data_sources",
#' parent_project_name = "tableau_api_lib testing",
#' datasource_description = "MN state government Tableau site info"
#' )
#' }
#'
#' @export

publish_data_frame <- function(data,
                               server,
                               name,
                               project_name,
                               parent_project_name = NULL,
                               datasource_description = NULL) {
  
  if(!is.data.frame(data)) stop("data must be a data frame")
  if(length(name) != 1) stop("name must be length 1")
  if(length(datasource_description) > 1) stop("datasource_description must be length 1")
  
  TSC <- reticulate::import("tableauserverclient")
  
  project_id <- get_project_id(server, project_name, parent_project_name)
  
  #Create parquet tempfile and write data to it
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data, tmp)
  
  datasource <- TSC$DatasourceItem(
    project_id = project_id,
    name = name
  )
  
  #set datasource description (default is NULL)
  datasource$description <- datasource_description
  
  server$datasources$publish(
    datasource,
    file = tmp,
    mode = TSC$Server$PublishMode$Overwrite
  )
}
