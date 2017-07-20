updateHFs = function(path = file.path("G:/PORTFOLIO MANGEMENT/Bryan Lloyd/2017/Core/HF_RETURNS.xlsx"), overwrite = TRUE){
  HF_RETURNS <- readxl::read_excel("G:/PORTFOLIO MANGEMENT/Bryan Lloyd/2017/Core/HF_RETURNS.xlsx",
                                    sheet = "HF Returns", col_types = c("date",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric"))
  HF_RETURNS = HF_RETURNS[!apply(HF_RETURNS, 1, function(r)all(is.na(r))), ]
  HF_RETURNS = xts(HF_RETURNS[, 2:ncol(HF_RETURNS)], order.by = as.yearmon(HF_RETURNS$Date))
  HF_RETURNS = HF_RETURNS[!apply(HF_RETURNS, 1, function(r)all(is.na(r))),]
  hf=HF_RETURNS
  devtools::use_data(hf, overwrite = overwrite)
  
  
  
  HF_ALLOCS <- readxl::read_excel("G:/PORTFOLIO MANGEMENT/Bryan Lloyd/2017/Core/HF_RETURNS.xlsx",
                          sheet = "HF Allocs", col_types = c("date",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric"))
  
  HF_ALLOCS = HF_ALLOCS[!apply(HF_ALLOCS, 1, function(r)all(is.na(r))), ]
  HF_ALLOCS = xts(HF_ALLOCS[, 2:ncol(HF_ALLOCS)], order.by = as.yearmon(HF_ALLOCS$Date))
  HF_ALLOCS = HF_ALLOCS[!apply(HF_ALLOCS, 1, function(r)all(is.na(r))),]
  allocs=HF_ALLOCS
  devtools::use_data(allocs, overwrite = overwrite)

  devtools::load_all()
  
}