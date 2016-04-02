tworzenie_folderow <- function( nazwa_folderu,kandydat )
      for( i in seq_along(kandydat)) dir.create(paste0(nazwa_folderu,names(kandydat[i])))
