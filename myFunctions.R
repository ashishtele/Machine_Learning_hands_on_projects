## commonly used functions in R
####################################
##  report system time
####################################
st <- function() {Sys.time()}

####################################
##  return object
####################################
## provide this function a text string and it will return the object with 
## named the same as the text string.  This is useful for functions that loop 
## throughd data frames or other R objects

retObj <- function(x) eval(parse(text=x))


####################################
##  Pause R before continuing process
####################################

readkey <- function()
{
  cat ("Press [enter] to continue or [...] to break process")
  line <- readline()
  
}


####################################
##  helper function for working with NAs (like ifError in excel)
####################################

if.na <- function(x,y) {ifelse(is.na(x),y,x)}

na0 <- function(x) {ifelse(is.na(x),0,x)}

## test equality and always deliver T/F (never NA), even if one of values is NA
same <- function(x,y) { ifelse(is.na(x) | is.na(y), FALSE, ifelse(x==y, TRUE, FALSE)) }

## not sure how to execute the logic on this one yet.  need to do nothing if condition fails
##replaceif <- function(x,y,z) {ifelse(x,y,z)}

####################################
##  format number (by converting to text string)
####################################

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


comma <- function(x, digits = 0, format = "f", ...) {
  format(x, big.mark=",", digits=digits)
}

####################################
##  create quick tab function & countif functions
####################################

tab <- function(x) {
  ##cat("SUMMARY OF XXX  \n")
  cat("---------------------------------\n")
  cat("OVERVIEW OF VARIABLE\n")
  cat("    Rows of Data: ", NROW(x), "\n", sep="")  ## have to use upercase version of this function or will not print (unique to functions)
  cat("   Unique Values: ", length(unique(x)), "\n", sep="")
  cat("Duplicate Values: ", sum(duplicated(x)), "\n", sep="")
  cat("  Missing Values: ", sum(is.na(x)), " (", percent(sum(is.na(x))/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
  cat("---------------------------------\n")
  cat("FREQUENCY TABLE (all values in default order)\n")
  df <- as.data.frame.table(addmargins(table(x, useNA='always')))
  df$Percent <- df$Freq / NROW(x)
  df$Percent <- percent(df$Percent)
  row.names(df) <- NULL
  ##head(df, 15, addrownumbers=TRUE)
  df
}

tabTop <- function(x) {
  ##cat("SUMMARY OF XXX  \n")
  cat("---------------------------------\n")
  cat("OVERVIEW OF VARIABLE\n")
  cat("    Rows of Data: ", NROW(x), "\n", sep="")  ## have to use upercase version of this function or will not print (unique to functions)
  cat("   Unique Values: ", length(unique(x)), "\n", sep="")
  cat("Duplicate Values: ", sum(duplicated(x)), "\n", sep="")
  cat("  Missing Values: ", sum(is.na(x)), " (", percent(sum(is.na(x))/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
  cat("---------------------------------\n")
  cat("FREQUENCY TABLE (top 15 values, descending by freq)\n")
  df <- as.data.frame.table(addmargins(table(x, useNA='always')))
  df$Percent <- df$Freq / NROW(x)
  df$Percent <- percent(df$Percent)
  body <- head(df, -2)
  bottom <- tail(df, 2)
  body <- body[order(-body$Freq),]
  df <- rbind(body, bottom)
  row.names(df) <- NULL
  head(df, 15, addrownumbers=TRUE)
  }

tab2 <- function(x, y) addmargins(table(x, y, useNA='always'))

prop2a <- function(x, y) addmargins( prop.table(table(x, y, useNA='always')) ) * 100
prop2v <- function(x, y) addmargins( prop.table(table(x, y, useNA='always'), 2) ) * 100
prop2h <- function(x, y) addmargins( prop.table(table(x, y, useNA='always'), 1) ) * 100

## this function below doesnt really work
##prop2v2 <- function(x, y) percent(addmargins( prop.table(table(x, y, useNA='always'), 2) ))


countif <- function(x, y) {
  result = sum(x==y, na.rm=T)
  missing = sum(is.na(x))
  cat("# satisfying equality:", result, "\n")
  cat("     # missing values:", missing, "\n")
}

countMissing <- function(x) {
  ## calculate counts
  missing = sum(is.na(x))
  if (mode(x) == "character") emptyStrings = sum(x=="", na.rm=TRUE) else emptyStrings = 0
  totalRows = NROW(x)
  nonMissing = totalRows - missing - emptyStrings
  
  ## present results
  cat("#           TOTAL ROWS: ", totalRows, " (", percent(totalRows/NROW(x)), ")\n", sep="")
  cat("# Missing Values (NAs): ", missing, " (", percent(missing/NROW(x)), ")\n", sep="")
  cat("  # Empty Strings (\"\"): ", emptyStrings, " (", percent(emptyStrings/NROW(x)), ")\n", sep="")
  cat("   # Non-missing Value: ", nonMissing, " (", percent(nonMissing/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}

countDups <- function(x) {
  cat("    Rows of Data: ", NROW(x), "\n", sep="")  ## have to use upercase version of this function or will not print (unique to functions)
  cat("   Unique Values: ", length(unique(x)), "\n", sep="")
  cat("Duplicate Values: ", sum(duplicated(x)), "\n", sep="")
  cat("  Missing Values: ", sum(is.na(x)), " (", percent(sum(is.na(x))/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}


####################################
##  report memory usage and create abbreviated name for same function at bottom
####################################

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "Mb")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

####################################
##  report memory usage and create abbreviated name for same function at bottom
####################################

## not sure but seems like starting function with period causes it be "hidden" but availalbe in memory
.ls.objects2 <- function (pos = 1, pattern, order.by = "Size_MB", decreasing=TRUE, head = TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
  # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
  # a data frame of the objects and their associated storage needs.
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size) / 10^6 # megabytes
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size_MB", "Rows", "Columns")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}


####################################
##  report memory usage and create abbreviated name for same function at bottom
####################################

showMemoryUse <- function(sort="size", decreasing=FALSE, limit) {
  
  objectList <- ls(parent.frame())
  
  oneKB <- 1024
  oneMB <- 1048576
  oneGB <- 1073741824
  
  memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x)))))
  
  memListing <- sapply(memoryUse, function(size) {
    if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
    else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
    else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
    else return(paste(size, "bytes"))
  })
  
  memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)
  
  if (sort=="alphabetical") memListing <- memListing[order(memListing$objectName,decreasing=decreasing),] 
  else memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"
  
  if(!missing(limit)) memListing <- memListing[1:limit,]
  
  print(memListing, row.names=FALSE)
  return(invisible(memListing))
}

  
####################################
##  Update data in PowerPoint chart
####################################

updatePptChart <- function(ppt, slideNumber, chartName, excel, sheetName="sheet1") {
  ## should have a call here to error out if there is already a ppt excel open
  ## first list name of open excels (probably unecesary actually)
  ## then check for an excel named "Chart in Microsoft PowerPoint"
  
  ## copy and paste Derm data
  write.table(derm_cos_alloc, "clipboard", sep="\t", row.names=F)
  ppt[["Slides"]]$Item(slideNumber)[["Shapes"]]$Item(chartName)[["Chart"]][["ChartData"]]$Activate()
  ex$Workbooks()$Item("Chart in Microsoft PowerPoint")[["Sheets"]]$Item("sheet1")$Range("A1")$Select()
  ex$Workbooks()$Item("Chart in Microsoft PowerPoint")[["Sheets"]]$Item("sheet1")$Paste()
  
  ## this call to the ChartType property causes the charts ranges to be updated--not sure why
  ppt[["Slides"]]$Item(slideNumber)[["Shapes"]]$Item(chartName)[["Chart"]][["ChartType"]]
  ex$Workbooks()$Item("Chart in Microsoft PowerPoint")$close()
}

####################################
##  Update cell in PowerPoint table
####################################

updatePptTableCell <- function(ppt, slideNumber, tableName, cellRow, cellCol, newValue) {
  pres <- ppt
  pres[["Slides"]]$Item(slideNumber)$Select()  ## code seems to error out if you don't explicitly select/navigate to slide
  pres[["Slides"]]$Item(slideNumber)[["Shapes"]]$Item(tableName)[["Table"]]$Cell(cellRow,cellCol)$Select()
  pres[["Slides"]]$Item(slideNumber)[["Shapes"]]$Item(tableName)[["Table"]]$Cell(cellRow,cellCol)$Shape()$TextFrame()$TextRange()$Delete()  
  pres[["Slides"]]$Item(slideNumber)[["Shapes"]]$Item(tableName)[["Table"]]$Cell(cellRow,cellCol)$Shape()$TextFrame()$TextRange()$InsertBefore(newValue)  
  
}

####################################
##  paste data into brand new excel
####################################
view <- function(df) {
  require(RDCOMClient)
  ex <- COMCreate("Excel.Application")
  newbook <- ex$Workbooks()$Add()
  newsheet <- newbook[["Sheets"]]$item("sheet1")
  newsheet$Range("A10")$Select()
  write.table(df, "clipboard-16834", sep="\t", row.names=F)
  newsheet$Paste()
}

copy <- function(df) {
  write.table(df, "clipboard-16834", sep="\t", row.names=F)
}

####################################
##  quick excel export of multiple sheets
####################################
## this works, (found online) but excluding for now because if requires xlsx, which 


save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      xlsx::write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

## example of usage below
##save.xlsx("myworkbook.xlsx", mtcars, Titanic, AirPassengers, state.x77)


kwikXL <- function (df, filePath)   {
  require(openxlsx, quietly = TRUE)
  XLout <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb = XLout, sheetName = "sheet1")
  openxlsx::writeDataTable(wb = XLout, sheet = "sheet1", x=df, startRow=10, startCol=1)
  openxlsx::saveWorkbook(XLout,  filePath, overwrite = TRUE)
  print(paste("Saved to", filePath))
}



