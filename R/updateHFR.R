updateHFRX= function(filepath){
  hfrx.raw <- read_csv(filepath,
      col_types = cols(Date = col_date(format = "%m/%d/%Y")))

  hfrx.raw = hfrx.raw[!apply(hfrx.raw, 1, function(r)all(is.na(r))),]
  hfrx.lu = unique(hfrx.raw[, c("Index", "Ticker")])
  
  hfrx = reshape2::dcast(hfrx.raw, Date ~ Ticker, value.var = "Return")
  hfrx = xts(hfrx[, 2:ncol(hfrx)], order.by = as.yearmon(hfrx$Date))
  devtools::use_data(hfrx, overwrite = TRUE)
  devtools::load_all()
}


updateHFRI = function(filepath){
  hfri.raw <- read_csv(filepath,
                       col_types = cols(Date = col_date(format = "%m/%d/%Y")))
  
  hfri.raw = hfri.raw[!apply(hfri.raw, 1, function(r)all(is.na(r))),]
  hfri.lu = unique(hfri.raw[, c("Index", "Ticker")])
  
  hfri = reshape2::dcast(hfri.raw, Date ~ Ticker, value.var = "Return")
  hfri = xts(hfri[, 2:ncol(hfri)], order.by = as.yearmon(hfri$Date))
  devtools::use_data(hfri, overwrite = TRUE)
  devtools::load_all()
}



