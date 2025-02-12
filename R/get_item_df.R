get_item_df <- function(server, item) {
  py <- reticulate::import_builtins()
  TSC <- reticulate::import("tableauserverclient")
  
  objects <- list()
  page <- 0L
  api_method <- server[item]
  new <- list()
  while (page == 0L | length(new) == 1000L) {
    page <- page + 1L
    new <- api_method$get(
      TSC$RequestOptions(
        pagenumber = page,
        pagesize = 1000L)
    )[[1L]]
    
    objects[[page]] <- new
  }
  
  #flatten objects list
  objects <- unlist(objects, recursive = FALSE)
  
  #extract as list then convert Python classes
  objects <- lapply(
    objects,
    \(x) lapply(
      x['__dict__'],
      \(y) {
        #cast None to NA
        if (is.null(y)) NA
        #cast sets to list
        else if (any(class(y) %in% "python.builtin.set")) {
          y <- py$list(y)
          #if not a single item list, wrap in another list
          if (length(y) != 1L) list(y) else y
        }
        #if not a single item list, wrap in another list
        else if (length(y) != 1L) list(y) else y
        }
    )
  )

  #cast to dataframe and rbind
  df <- do.call(rbind, lapply(objects, list2DF))
  #remove leading underscores
  names(df) <- sub('^_', '', names(df))
  return(df)
}