get_view_id <- function(server, name, workbook_name = NULL) {
  views <- get_item_df(server, 'views')
  {
    if (is.null(workbook_name))
      id <- views[views$name == name, "id"]
    else {
      workbook_id <- get_workbook_id(server, workbook_name)
      id <- views[views$name == name & views$workbook_id == workbook_id, "id"]
    }
  }
  
  if(length(id) < 1) stop(paste0('No view found with name "', name, '"'))
  if(length(id) > 1) stop(paste0(
    'Multiple views found with name "', name, '". Use a unique view name or the workbook_name argument to choose a specific workbook.')
  )
  
  return(id)
}