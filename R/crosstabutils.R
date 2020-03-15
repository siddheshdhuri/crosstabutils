# Cross Tab Utils
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Function to aggregate data frame given aggregate columns
#'
#' @param aggBy column names of the columns to aggregate data by
#' @param data data.frame to perform operations on
#'
#' @return aggregate data.frame
#'
#' @export
getAggData <- function (aggBy, data, seg_agg_cols, customer_id, contract_id, product_id, contract_value)   {
  agg.by <- lapply(aggBy, as.symbol)
  agg.data <- data %>% group_by_(.dots = agg.by) %>%
              select(one_of(seg_agg_cols)) %>%
              summarise(Customers = n_distinct(as.symbol(customer_id)),
                        Contracts = n_distinct(as.symbol(contract_id)),
                        Products = n_distinct(as.symbol(product_id)),
                        TOV = sum(as.symbol(contract_value),  na.rm = TRUE),
                        APVC = sum(as.symbol(contract_value),  na.rm = TRUE) / Customers
  )

  return(agg.data)
}

#' Function to transpose column given data frame
#'
#' @param xaxis column name to transpose as columns
#' @param yaxis character vector of column names aggregated as vertical columns
#' @param valuevar column with value to be aggergated
#' @data data.frame
#'
#' @return crostab data.frame
#'
#' @export
getTransposeData <- function(xaxis,yaxis,valuevar,data,
                             seg_agg_cols, customer_id, contract_id, product_id, contract_value){

  if(valuevar == "Penetration") valuevar <- "Customers"

  agg.data <- getAggData(c(xaxis,yaxis), data,
                         seg_agg_cols, customer_id, contract_id, product_id, contract_value)

  dependent <- yaxis
  factors <- setdiff(xaxis,dependent)
  if(length(factors) < 1) factors <- xaxis
  formula.var <- as.formula(paste( paste(factors,collapse = "+"), paste("~",dependent)))

  func = switch (valuevar,
                 Customers = "sum",
                 Contracts = "sum",
                 Products = "sum",
                 TOV = "sum",
                 APVC = "mean",
                 AOV = "mean",
                 MOV = "median")

  transposed.data <- reshape2::dcast(agg.data,
                                     formula.var,
                                     value.var = valuevar ,
                                     fun.aggregate = getFunction(func),
                                     na.rm = TRUE)


  if(yaxis == "SEGMENT"){
    # order segment column header names
    col.names <- setdiff(colnames(transposed.data),xaxis)

    col.order <- col.names[order(match(col.names,segments.order))]

    transposed.data <- transposed.data[,c(xaxis,col.order)]
  }else  if(yaxis == "NEW_SEGMENT"){
    # order segment column header names
    col.names <- setdiff(colnames(transposed.data),xaxis)

    col.order <- col.names[order(match(col.names,new.segments.order))]

    transposed.data <- transposed.data[,c(xaxis,col.order)]
  }else  if(yaxis == "SEGMENT_SIZE"){
    # order segment column header names
    col.names <- setdiff(colnames(transposed.data),xaxis)

    col.order <- col.names[order(match(col.names,new.segmentsize.order))]

    transposed.data <- transposed.data[,c(xaxis,col.order)]
  }

  return(transposed.data)
}


#' Function to append universe columns to data
#'
appendUniverseData <- function(univ.col, xaxis, valuevar, data){

  if(any(business.cols %in% xaxis)){
    return("Data aggregated by business specific columns, adding universe columns might result in duplication")
  }

  agg.by <- lapply(xaxis, as.symbol)
  univ.col <- as.symbol(univ.col)

  agg.univ <- univ %>%
    group_by_(.dots = agg.by) %>%
    select(HQ_Level) %>%
    # select(HQ_Level, SITE_Level, URL_Level) %>%
    summarise(
      Universe = sum(univ.col, na.rm=TRUE)
      # HQ_Universe = sum(HQ_Level, na.rm=TRUE),
      # Site_Universe = sum(Site_Level, na.rm=TRUE),
      # URL_Universe = sum(URL_Level, na.rm=TRUE)
    )

  # Append Universe Column
  summary.data <- merge(data,
                        agg.univ,
                        by=xaxis,
                        all.x = TRUE)

  # summary.data["Totals",] <- colSums(summary.data)
  # summary.data$Totals <- rowSums(summary.data)

  if(valuevar == "Penetration"){
    summ.col.names <- colnames(summary.data)
    summ.col.names <- setdiff(summ.col.names, c(xaxis,"Universe"))

    for(col in summ.col.names){
      summary.data[,paste0(col,"_Penetration")] <- calc.penetration(summary.data[,col],summary.data[,"Universe"])
    }

    # Keep only the columns with penetration percentage
    keep.cols <- c(xaxis,paste0(summ.col.names,"_Penetration"))

    summary.data <- summary.data[,keep.cols]
  }

  return(summary.data)
}


#' function to get summary data for display
#'
#' @param data data.frame to summarise
#' @param xaxis horizontal column
#' @param yaxis character vector of vertical column names
#' @valuevar column with value to be aggergated
#'
#' @return summarised data.frame
#'
#' @export
getSummaryData <- function(data, xaxis, yaxis, valuevar,
                           seg_agg_cols, customer_id, contract_id, product_id, contract_value) {

  agg.data <- getAggData(xaxis, data,
                         seg_agg_cols, customer_id, contract_id, product_id, contract_value)

  if("NONE" == yaxis){
    transposed.data <- agg.data
  }else{
    transposed.data <- getTransposeData(xaxis,yaxis,valuevar,data)
  }


  # Add formating for cells
  transposed.data <- setColumnFormat(transposed.data)

  return(transposed.data)

}



#' function to set format for table columns
#'
#' @param table.to.format data.table to format
#'
#' @return data.table with formated columns
#'
#' @export
setColumnFormat <- function(table.to.format){

  numeric.cols <- colnames(table.to.format[,sapply(table.to.format, is.numeric), drop=FALSE])

  data.table <- DT::datatable(table.to.format,
                              filter = "top",
                              selection = list(target="cell"),
                              options = list(scrollX = TRUE,
                                             pageLength = 10,
                                             lengthMenu = c(10, 50, 100)
                              )
  )

  for(i in 1:length(numeric.cols)){

    col.name <- numeric.cols[i]

    col.color <- switch (paste0("case",i%%5),
                         case0 = "lightblue",
                         case1 = "lightgray",
                         case2 = "beige",
                         case3 = "lavender",
                         case4 = "LemonChiffon"
    )

    data.table <- data.table %>%
      DT::formatStyle(
        col.name,
        background = styleColorBar(range(table.to.format[,col.name],
                                         na.rm = TRUE),
                                   col.color)
      )
  }

  return(data.table)

}

#' Function to calculate penetration into universe
#'
#' @param cust number of customers
#' @param univ number of entities in universe
#'
#' @return penetration as percentage
#'
#' @export
calc.penetration <- function(cust , univ){
  penetration <- round(cust/univ*100,2)
}


#' Function that find if a value is between two values
#'
#' @param x value to be verified
#' @param a lower limit
#' @param b upper limit
#'
#' @return TRUE if value between bounds
#'
#' @export
is.between <- function(x, a, b) {
  x > a & x < b
}


#' function to find string with space contains multiple strings
#'
#' @param column name in which to search string
#' @param toMatch string to map
#'
#' @return TRUE if string found
#'
#' @export
'%containswithspace%' <- function(column ,toMatch) {
  x <- TRUE
  if(nchar(toMatch) > 1){

    toMatch <- paste0("\\b",gsub(toMatch, pattern="\\n$",replacement=""),"\\b") %>%
      gsub(pattern="\\n",replacement="\\\\b|\\\\b") %>% gsub(pattern="\\s*,\\s*",replacement="\\\\b|\\\\b")

    x <- stringr::str_detect(column, regex(toMatch, ignore_case = T))
  }

  return(x)
}


#' function to find string contains multiple strings
#'
#' @param column name in which to search string
#' @param toMatch string to map
#'
#' @return TRUE if string found
#'
#' @export
'%contains%' <- function(column ,toMatch) {
  x <- TRUE
  if(nchar(toMatch) > 1){
    toMatch <- toMatch %>%
      gsub(pattern="\\n$",replacement="") %>%
      gsub(pattern="\\s+",replacement=" ") %>%
      gsub(pattern="\\s*,\\s*",replacement="|") %>%
      gsub(pattern=" ",replacement="|") %>%
      gsub(pattern="\\n",replacement="|")

    print(toMatch)

    x <- stringr::str_detect(column, regex(toMatch, ignore_case = T))
  }

  return(x)
}


#' function to plot data.frame as heatmap
#'
#' @param plotdf data.frame to be plotted
#' @param plotdf2 second data.frame percentages to be computed
#' @param var1 first variable to aggregate by
#' @param var2 second variable to aggregate by
#' @param excludevar
#' @param excludeval
#' @param includeval
#' @rotate to rotate heatmap
#'
#' @return heatmap plot as ggplot object
#'
#' @export
plotheatmap <- function(plotdf, plotdf2=NULL, var1, var2,
                        excludevar = NULL,
                        excludeval = NULL,
                        includeval = NULL,
                        rotate=FALSE){

  plotdf <- plotdf[, c(var1,var2)]
  plotdf[] <- lapply(plotdf, as.character)

  if(!is.null(excludevar)){

    plotdf <- plotdf %>% filter(plotdf[[excludevar]] %in% setdiff(includeval,excludeval))

  }

  countsdf <- table(plotdf[[var1]], plotdf[[var2]])

  # if(!is.null(plotdf2)){
  #
  #   plotdf2 <- plotdf2[, c(var1,var2)]
  #   plotdf2[] <- lapply(plotdf2, as.character)
  #
  #   plotdf2 <- plotdf2 %>% filter(plotdf2[[var1]] %in% unique(plotdf[[var1]]))
  #   plotdf2 <- plotdf2 %>% filter(plotdf2[[var2]] %in% unique(plotdf[[var2]]))
  #
  #   countsdf2 <- table(plotdf2[[var1]], plotdf2[[var2]])
  #
  #   countsdf <- countsdf / (countsdf + countsdf2) * 100
  #
  # }else{
  #   countsdf <- countsdf / sum(countsdf) * 100
  # }

  #library(tidyverse)

  countsdf <- as.matrix(countsdf)

  dat2 <- countsdf %>% tbl_df()
  colnames(dat2) <- c(var1, var2, "n")

  pl <- NULL
  if(rotate){
    pl <- ggplot(dat2, aes(var2, var1)) +
      geom_tile(aes(fill = n)) +
      geom_text(aes(label = paste(round(n, 1)," %"))) +
      scale_fill_gradient(low = "white", high = "red")
  }else{
    pl <- ggplot(dat2, aes(var1, var2)) +
      geom_tile(aes(fill = n)) +
      geom_text(aes(label = n)) +
      scale_fill_gradient(low = "white", high = "red")
  }

  return(pl)

}
