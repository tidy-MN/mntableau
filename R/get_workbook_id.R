get_workbook_id <- function(server, name) {
  workbooks <- get_item_df(server, 'workbooks')
  id <- workbooks[workbooks$name == name, "id"]
  if(length(id) < 1) stop(paste0('No workbook found with workbook name "', name, '"'))
  if(length(id) > 1) stop(paste0(
    'Multiple workbooks found with name "', name, '". Use a unique workbbok name or the parent_project argument to choose a specific project.')
  )
  return(id)
}
