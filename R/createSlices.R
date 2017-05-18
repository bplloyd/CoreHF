# RETURNS TIME SLICES FOR ROLLING LISTS
createSlices = function(data, initialWindow = 12, fixedWindow = TRUE, forward = F, skip=0, on = NULL){
  y = 1:nrow(data)
  stops <- (seq(along = y))[initialWindow:length(y)]
  if (fixedWindow){
    starts <- stops - initialWindow + 1
  } else {
    starts <- rep(1, length(stops))
  }

  if(!is.null(on)){
    keep = ((1:length(stops)) %% on) == 0
    stops = stops[keep]
    starts = starts[keep]
  }

  slices = mapply(seq, starts, stops, SIMPLIFY = FALSE)

  if(class(data)[1] != "xts"){
    if(!is.null(row.names(data))){
      if(!forward){
        names(slices) = row.names(data)[stops]
      } else {
        names(slices) =  row.names(data)[starts]
      }
    }
  } else {
    if(!forward){
      names(slices) = zoo::index(data)[stops]
    } else {
      names(slices) =  zoo::index(data)[starts]
    }
  }


  thin <- function(x, skip = 2){
    n <- length(x)
    x[seq(1, n, by = skip)]
  }


  # epoints = as.character.Date(zoo::index(data)[xts::endpoints(data, on = on)])
  # slices = slices[which(names(slices) %in% epoints)]
  if (skip > 0){
    slices <- thin(slices, skip = skip + 1)
  }
  return(slices)
}
