#' Open a connection to a MN state Tableau site
#'
#' Requires reticulate and Python interpreter
#'
#' @param site_name Name of the Tableau site you want to connect to. Use `tableau_sites` to view valid sites.
#' @param username Your Tableau server username. Do not include if using a token.
#' @param password Your Tableau server password. Recommend using the `keyring` package to set your password. Do not include if using a token.
#' @param token_name Your Tableau server token name. Do not include if using a username/password.
#' @param token_secret Your Tableau server token secret. Recommend using the `keyring` package to set your token secret. Do not include if using a username/password.
#' @examples
#' /dontrun{
#'
#' keyring::key_set("tableau", "nageld1")
#'
#' conn <- tableau_connect("mdh_internal_dev",
#' username = "nageld1",
#' password = keyring::key_get("tableau", "nageld1")
#' )
#' }
#'
#' @export

tableau_connect <- function(
    site_name,
    username = NULL,
    password = NULL,
    token_name = NULL,
    token_secret = NULL
    ) {

  if(length(site_name) != 1) stop("Must provide exactly one site_name")
  if(!site_name %in% tableau_sites$name) stop(paste0(
    "site_name must one of:\n", paste(tableau_sites$name, collapse = "\n")))

  if ((is.null(username) | is.null(password)) &
      (is.null(token_name) | is.null(token_secret))
      ) stop('You must provide a username and password or a token name and token secret')

  if(!is.null(username) & !is.null(token_name)) stop('Only provide a username and password or a token, not both')

  py <- reticulate::import_builtins()
  warnings <- reticulate::import('warnings')
  TableauServerConnection <- reticulate::import("tableau_api_lib")$TableauServerConnection

  site <- tableau_sites[tableau_sites$name == site_name, -1]

  credentials <- if(!is.null(username)) list(
    username = username,
    password = password
  ) else list(
    personal_access_token_name = token_name,
    personal_access_token_secret = token_secret
  )

  site <- list(selected_site = c(
    as.list(site),
    api_version = '2.4',
    credentials
  )
  )

  #Set connection
  conn <- TableauServerConnection(site, env = 'selected_site')
  #API version will be incorrect. Ignore warning.
  with(warnings$catch_warnings(),
       {
         warnings$simplefilter("ignore")
         conn$sign_in()
       })

  #Change API version to current
  site[['selected_site']][['api_version']] <- conn$server_info()$json()[['serverInfo']][['restApiVersion']]
  #Sign out to sign in again with correct API version
  conn$sign_out()

  #Sign in again with correct API version
  conn <- TableauServerConnection(site, env = 'selected_site')
  conn$sign_in()
  return(conn)

}
