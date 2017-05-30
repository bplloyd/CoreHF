getSPTR = function()
{
    sp = Quandl::Quandl(code = "YAHOO/INDEX_GSPC", type = "xts")
    return(sp)
}
