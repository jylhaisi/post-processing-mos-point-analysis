### PLOTTING FUNCTIONS ###


plot_points_on_map <- function(plot_data,plot_data_area,breaks,colorscale,point_size,plotting_title) {
  # Plots points to a map on a user defined lat-lon box, colorscale, limits and legend location
  #
  # Args:
  #   plot_data: a data matrix (first column data, second lon, third lat)
  #   plot_data_area: defines the data points which constitute the plotting area
  #   breaks : a vector defining breaks for plotting
  #   colorscale: the colorscale that is used in plot (if colorscale==points, do not plot any scale)
  #   point_size: size of point to be plotted
  #   plotting_title: plotting title
  #   
  #   
  #
  # Returns:
  #   A plot

  
  # Removing missing values from plotting data
  plot_data <- plot_data[which(rowSums(is.na(plot_data))==0),]
  plot_data_area <- plot_data_area[which(rowSums(is.na(plot_data_area))==0),]
  if (is.null(breaks)) {
    breaks <- c( -2000, -500, -300, -200, -100, 100, 200, 300, 500, 2000)
  }
  par(mar=c(1.7,1,4,1),xpd=FALSE)
  plotvar <- plot_data[,1]
  tplot <- plot(plot_data_area[,2], plot_data_area[,3], axes=FALSE, cex=.5, type="n", xlab="", ylab="", main=plotting_title)
  map("world", add=TRUE, lwd=.5, col="grey95", fill=TRUE)
  if (colorscale != "points") {
    # Define number of colours to be used in plot
    nclr <- (length(breaks) - 1)
    brk <- breaks
    # Define colour palette to be used
    plotclr <- brewer.pal(nclr,colorscale)
    # Define colour intervals and colour code variable for plotting
    class <- classIntervals(plotvar, nclr, fixedBreaks= brk, style = "fixed")
    colcode <- findColours(class, plotclr)
    points(plot_data[,2], plot_data[,3], pch = 16, col= colcode, cex = point_size)
    legend("bottomleft", legend = names(attr(colcode, "table")), fill = attr(colcode, "palette"), cex = 0.7, bty = "n")
    rm(nclr)
    rm(brk)
    rm(plotclr)
    rm(class)
    rm(colcode)
  } else {
    points(plot_data[,2], plot_data[,3], cex = point_size, pch = 16, col="red")
  }
  rm(plotvar)
  
  return(tplot)
}






### SOME USEFUL DATA HANDLING FUNCTIONS ###


# This function adds objects to the end of a list
lappend <- function(lst, obj, listname) {
  lst[[length(lst)+1]] <- obj
  names(lst)[[length(lst)]] <- listname
  return(lst)
}

# Test if a not in b
`%notin%` <- function(x,y) !(x %in% y)

# If a is null then return b
`%||%` <- function(a, b) if (!is.null(a)) a else b


# Which converts true/false values to integers, this does the opposite (n corresponds to vector length where true values are assigned to)
unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}
# unwhich(which(x), 10)



# # Very useful functions of Johanna Piipponen
# source("functions_Johanna.R")