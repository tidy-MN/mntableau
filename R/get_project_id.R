#' Get the project id for a given project name and optional parent project
#'
#' Requires reticulate and Python interpreter
#'
#' @param server A Tableau server object. See `tableau_connect()`.
#' @param name The name of the project. Case sensitive.
#' @param parent_name The name of the parent project of `name`. Use if `name` is not a unique project name on your Tableau site. Optional.
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
#' get_project_id(server, "MNIT-MDH")
#' get_project_id(server, "Newborn", parent_name = "PHL")
#' }
#'
#' @export

get_project_id <- function(server, name, parent_name = NULL) {
  projects <- get_item_df(server, 'projects')
  
  if(!is.null(parent_name)) {
    parent_id <- projects[projects$name == parent_name, "id"]
    id <- projects[projects$parent_id == parent_id & projects$name == name, "id"]
  } else id <- projects[projects$name == name, "id"]
  
  if(length(id) < 1) stop(paste0('No project found with name "', name, '"'))
  if(length(id) > 1) stop(paste0(
    'Multiple projects found with name "', name, '". Use a unique project name or the parent_name argument to choose a parent project.')
  )
  
  return(id)
}
