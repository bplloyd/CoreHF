exportXTS = function(data, filename, sheet) {
    df = data.frame(Date = (as.numeric(index(data)) + 25569), data, row.names = NULL)
    XLConnect::writeWorksheetToFile(file = filename, data = df, sheet = sheet)
}
