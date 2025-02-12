#' Open a server connection to a MN state Tableau site
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
#' server <- tableau_connect("mdh_internal_dev",
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

  TSC <- reticulate::import("tableauserverclient")

  site <- tableau_sites[tableau_sites$name == site_name, -1]
  #use username/password if username is provided
  tableau_auth <- if(!is.null(username)) TSC$TableauAuth(
    username = username,
    password = password,
    site_id = site$site_name
    #use token if no username provided
  ) else TSC$PersonalAccessTokenAuth(
    token_name = token_name,
    personal_access_token = token_secret,
    site_id = site$site_name
  )
  
  server <- TSC$Server(site$server, use_server_version = TRUE)
  
  server$auth$sign_in(tableau_auth)
  
  return(server)
  
}
