#'Create a Basic Scatter Plot
#'
#'scatterplot.basic() creates a plot including a grid similar to the use of ggplot with the geom_point function. It is used to visualize data specifically in a basic scatter plot. Note that four global variables named "xdata", "ydata", "xname", and "yname" will be created. After use of this function, you may have to reset graphical parameters to default using the par() function in the Graphics R package.
#'
#' @param dat a data frame
#' @param xname a character variable
#' @param yname a character variable
#'
#' @export
#'@examples
#'my.data <- data.frame(cbind(1:5,2:6,3:7))
#'scatterplot.basic(my.data,"X1","X3")
#'
#'@importFrom graphics axTicks grid par points abline

scatterplot.basic <- function(dat,xname,yname){
  #####creating global variables, which will be useful for related functions#####
  assign(".xdata", value = dat[[xname]], envir = .GlobalEnv)
  assign(".ydata", value = dat[[yname]], envir = .GlobalEnv)
  assign(".xname", value = xname, envir = .GlobalEnv)
  assign(".yname", value = yname, envir = .GlobalEnv)

  #####creating error messages#####
  if(is.data.frame(dat)==F)stop("`dat` must be a data frame.")
  else if(is.character(xname)==F)stop("`xname` must be a character.")
  else if(is.character(yname)==F)stop("`yname` must be a character.")
  else{
  #####basic settings of the plot set#####
  # *setting margins
  # *setting font
  # *setting size and color of axes
  # *setting the distance from x and y axis labels
  # *setting the distance between tick marks and labels
    par(mfrow = c(1,1), mar = c(2.7,2.6,0.4,0.4), font = 7, font.axis = 1, fg = "black",  col.axis = "gray39", cex.axis = .75, las = 1, tcl = -0.20, mgp = c(1.2,0.35,0), cex.lab = 0.96, xpd = F)

  # *obtaining range of numbers for the x and y axes (the if statements are required in order to match the display used in the ggplot2 package)
    yrange <- if(max(dat[yname]) - floor(max(dat[yname])) < 0.5 & max(dat[yname]) - floor(max(dat[yname])) != 0){
      c(min(dat[yname]), round(max(dat[yname])) + 0.5)
    }else if(max(dat[yname]) - floor(max(dat[yname])) > 0.5){
      c(min(dat[yname]), round(max(dat[yname])) + 0.07)
    }else{
      c(min(dat[yname]), max(dat[yname]))
    }
    assign(".yrange", value = yrange, envir = .GlobalEnv)

    xrange <- if(max(dat[xname]) - floor(max(dat[xname])) < 0.5 & max(dat[xname]) - floor(max(dat[xname])) != 0){
      c(min(dat[xname]), round(max(dat[xname])) + 0.5)
    }else if(max(dat[xname]) - floor(max(dat[xname])) > 0.5){
      c(min(dat[xname]), round(max(dat[xname])) + 0.07)
    }else{
      c(min(dat[xname]), max(dat[xname]))
    }
    assign(".xrange", value = xrange, envir = .GlobalEnv)

  #####creating basic scatter plot#####
  # *input range for y-axis
  # *input range for x-axis
  # *input x and y axis labels
  # *inputting colors (for title, axes)
  # *changing font to text on plot
    plot(NULL,
         ylim = yrange,
         xlim = xrange,
         xlab = xname,
         ylab = yname,
         col.main = "black",
         col.axis = "gray26",
         cex.axis = 0.75,
         family = "sans")

  #creating grid lines in plot (lines in middle of ticks and lines going thru ticks)
    abline(v = min(dat[xname]):max(dat[xname]), col = "grey92", lwd = 500)
    grid(nx = NULL, ny = NULL, col = "white", lty = "solid", equilogs = F)
    abline(h = axTicks(2) - (axTicks(2)[2]-axTicks(2)[1])/2, col = "white")
    abline(h = axTicks(2) + (axTicks(2)[2]-axTicks(2)[1])/2, col = "white")
    abline(v = axTicks(1) + (axTicks(1)[2]-axTicks(1)[1])/2, col = "white")

  #creating points based on the data inputted
    points(x = dat[[xname]], y = dat[[yname]],
           pch = 20,
           cex = 1,
           col = "black")
  }
}

