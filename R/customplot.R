#' Basic Scatter Plot Editor
#'
#'customize() allows users to change the features of a current plot without rebuilding the plot. The input data is intended to include basic editting features of the basic scatter plot.
#'
#' @param pt.type Required parameter: Point type based on R Base point types (expected input range: integers 0-25).
#' @param pt.col Optional parameter: Character name of color for points based on R Base colors. [Default is "black"]
#' @param pt.size Optional parameter: Numeric for size of points [Default is 1]
#' @param line.choice Optional parameter: Boolean decision of including a line. Note that this must be called to make changes in other variables involving the lines in the plot. [Default is F or FALSE]
#' @param line.type Optional parameter: Numeric indicating the line type based on R Base values for line types. [Default is 1 (solid line)]
#' @param line.col Optional parameter: Character/ string indicating color change of line in plot based on R Base colors. [Default is "black"]
#' @param line.method Optional parameter: Character/ string indicating whether to include a line of best fit or a default line connecting all points (expected input is "default" or "linear"). [Default is "default"]
#' @param change.lab Optional parameter: Boolean decision of including changing axes labels. Note if x.newlab() and y.newlab() are unchanged, the default parameter applies. [Default is F or FALSE]
#' @param x.newlab Optional parameter: Character/ string of x axis title label. [Default is the previous name from original plot]
#' @param y.newlab Optional parameter: Character/ string of y axis title label. [Default is the previous name from original plot]
#' @param title.choice Optional parameter: Boolean decision of including a main title for plot. [Defaault is F or FALSE]
#' @param new.title Optional parameter: Character/ string of main title in plot. [Default is ""]
#' @param new.subtitle Optional parameter: Character/ string of subtitle in plot. [Default is ""]
#'
#' @export
#'
#' @examples
#' #####the scatterplot.basic() plot must be called before use
#' # of the customize() function
#' scatterplot.basic(dat = mtcars, xname = "mpg", yname = "wt")
#'
#' #####the customize() function now can be used to add
#' #change plot features
#' # can change the point type to triangles if desired (value of 2 in R Base)
#' customize(pt.type = 2)
#'
#' #can change the color and size of the points
#' # (note that background color for input 21-25 is "red" by default)
#' customize(pt.type = 2, pt.col = "red", pt.size = 1)
#'
#'#can include a line and titles
#'customize(pt.type = 2, pt.col = "red", pt.size = 1,
#'line.choice = TRUE, line.type = 1, title.choice = TRUE,
#'new.title = "Testing out the customize function",
#'new.subtitle = "It looks like it's working.",
#'line.method = "linear")
#'
#'#can change each input
#'customize(pt.type = 2, pt.col = "red", pt.size = 1,
#'line.choice = TRUE, line.type = 1, title.choice = TRUE,
#'new.title = "Testing out the customize function",
#'new.subtitle = "It looks like it's working.",
#'line.method = "linear", line.col = "blue",
#'change.lab = TRUE, x.newlab = "x-axis",
#'y.newlab = "y-axis")
#'
#'@importFrom graphics lines mtext title
#'
#'@importFrom stats lm

customize <- function(pt.type, pt.col = "black", pt.size = 1, line.choice = F, line.type = 1, line.col = "black", line.method = "default", change.lab = F, x.newlab = .xname, y.newlab = .yname, title.choice = F, new.title = "", new.subtitle = ""){
  #####creating error messages#####
  if(!exists(".xdata"))stop("`.xdata` does not exist. Call `scatterplot.basic()` function to use this function.")
  else if(!exists(".ydata"))stop("`.ydata` does not exist. Call `scatterplot.basic()` function to use this function.")
  else if(!exists(".xname"))stop("`.xname` does not exist. Call `scatterplot.basic()` function to use this function.")
  else if(!exists(".yname"))stop("`.yname` does not exist. Call `scatterplot.basic()` function to use this function.")
  else if (pt.type < 0 | pt.type > 25 | !is.numeric(pt.type))stop("`pt.type` is not valid. Insert a number from 0-25.")
  else if (!is.character(pt.col))stop("`pt.col` must be character corresponding with a color defined by R.")
  else if (!is.numeric(pt.size))stop("`pt.size` must be a numeric.")
  else if (!is.logical(line.choice))stop("`line.choice` must be logical.")
  else if (!is.character(line.col))stop("`line.col` must be character corresponding with a color defined by R.")
  else if (!is.logical(change.lab))stop("`change.lab` must be logical.")
  else if (!is.character(x.newlab))stop("`x.newlab` must be a character.")
  else if (!is.character(y.newlab))stop("`y.newlab` must be a character.")
  else if (title.choice == F & (new.title != "" | new.subtitle != ""))stop("`title.choice` is FALSE. Insert `TRUE` or `T` to change titles.")
  else if (change.lab == F & (x.newlab != .xname | y.newlab != .yname))stop("`change.lab` is FALSE. Insert `TRUE` or `T` to change axis titles.")
  else if (!is.numeric(line.type))stop("`line.type` must be a numeric.")
  else if (line.choice == F & (line.type != 1 | line.col != "black" | line.method != "default"))stop("`line.choice` is FALSE. Insert `TRUE` or `T` to change titles.")

  #run function after passing all possible errors
  else{
  #####removing the axis labels (6 of these lines are necessary to layer to remove the axis label title)#####
    if(change.lab == T){
      title(xlab = .xname, ylab = .yname, col.lab = "white")
      title(xlab = .xname, ylab = .yname, col.lab = "white")
      title(xlab = .xname, ylab = .yname, col.lab = "white")
      title(xlab = .xname, ylab = .yname, col.lab = "white")
      title(xlab = .xname, ylab = .yname, col.lab = "white")
      title(xlab = .xname, ylab = .yname, col.lab = "white")
      title(xlab = x.newlab, ylab = y.newlab, col.lab = "black")
    #updating global variables `xname` and `yname`
      xname <- x.newlab
      yname <- y.newlab
      assign(".xname", value = xname, envir = .GlobalEnv)
      assign(".yname", value = yname, envir = .GlobalEnv)
    }
  #####removing title and setting new title#####
    if(title.choice == T){
      par(mfrow = c(1,1), mar = c(2.7,2.6,2.8,0.4), font = 7, font.axis = 1, fg = "black",  col.axis = "gray39", cex.axis = .75, las = 1, tcl = -0.20, mgp = c(1.2,0.35,0), cex.lab = 0.96, xpd = F)
    #####recreating basic scatter plot#####
    # *input range for y-axis
    # *input range for x-axis
    # *input x and y axis labels
    # *inputting colors (for title, axes)
    # *changing font to text on plot
      plot(NULL,
           ylim = .yrange,
           xlim = .xrange,
           xlab = x.newlab,
           ylab = y.newlab,
           col.main = "black",
           col.axis = "gray26",
           cex.axis = 0.75,
           family = "sans")

      #create title and subtitle
      mtext(side = 3, line = 1, adj = 0, cex = 1, font = 2, new.title)
      mtext(side = 3, line = 0, adj = 0, cex = 0.8, font = 1, new.subtitle)
    }

    if(pt.size != 1){
      #####recreating basic scatter plot#####
      # *input range for y-axis
      # *input range for x-axis
      # *input x and y axis labels
      # *inputting colors (for title, axes)
      # *changing font to text on plot
      plot(NULL,
           ylim = .yrange,
           xlim = .xrange,
           xlab = .xname,
           ylab = .yname,
           col.main = "black",
           col.axis = "gray26",
           cex.axis = 0.75,
           pch = pt.type,
           cex = pt.size,
           family = "sans")
    }

  #####resetting the plot (clearing all points to begin editing plot)#####
    abline(v = min(.xdata):max(.xdata), col = "grey92", lwd = 500)
    grid(nx = NULL, ny = NULL, col = "white", lty = "solid", equilogs = F)
    abline(h = axTicks(2) - (axTicks(2)[2]-axTicks(2)[1])/2, col = "white")
    abline(h = axTicks(2) + (axTicks(2)[2]-axTicks(2)[1])/2, col = "white")
    abline(v = axTicks(1) + (axTicks(1)[2]-axTicks(1)[1])/2, col = "white")

  #####creating points based on the data inputted (customization of points occurs here)#####
    # *picking a point type
    points(x = .xdata, y = .ydata,
           pch = pt.type,
           bg = "red",
           col = pt.col,
           cex = pt.size,
           lwd = 2)

  #####creating lines based on line method#####
    if(line.choice == T){
      if(line.method == "default"){
        lines(.xdata[order(.xdata)], .ydata[order(.xdata)], col = line.col)
      }
      else if(line.method == "linear"){
      abline(lm(.ydata ~ .xdata), col = line.col, lty = line.type)
      }
    }
  }

  #####resetting graphical parameters#####
  par(mfrow = c(1,1), mar = c(2.7,2.6,0.4,0.4), font = 7, font.axis = 1, fg = "black",  col.axis = "gray39", cex.axis = .75, las = 1, tcl = -0.20, mgp = c(1.2,0.35,0), cex.lab = 0.96, xpd = F)
}



