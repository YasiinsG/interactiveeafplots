#' Interactively plotting 3D attainment plots
#'
#' @param x Either a matrix of data values, or a data frame, or a list of data frames of exactly three columns.
#' @param percentiles \code{(numeric())} - Vector indicating which percentile should be plot. The default is to plot only the median attainment curve. If \code{type='cube'} then only the first percentile will be used.
#' @param col A vector of colours used to plot.
#' @param type \code{(character(1))} - string giving the type of plot desired. The following values are possible, \code{'scatter'} and \code{'cube'}.
#' @param psize Size of plotted points.
#' @param xlabel,ylabel,zlabel Axes labels.
#'
#' @return A 3D plot of the attainment points and surfaces
#' @export
#'
#' @import plotly
#' @import moocore
#' @import dplyr
#' @import rgl
#' @import gMOIP
#' @import ggplot2
#'
#' @examples
#' options("warn"=-1)
#' library(interactiveeafplots)
#'
#' extdata_dir <- system.file(package="interactiveeafplots", "data/extdata")
#' x <- read_datasets(file.path(extdata_dir, "spherical-250-10-3d.txt"))
#' # Select only the last 3 datasets
#' x <- x[x[, 4] >= 8, ]
#'
#' interactive3dplot(x, c(0,50,100), col=c("red","orange","yellow"),
#'                   type="scatter", psize=3, xlabel="Objective 1",
#'                   ylabel="Objective 2", zlabel="Objective 3")
#'
#' # Will take a long time to render.
#' # Opens in separate window, so run by yourself.
#' # interactive3dplot(x, c(0,50,100), col=c("red","yellow"), type="cube",
#' #                   psize=3, xlabel="Objective 1", ylabel="Objective 2",
#' #                   zlabel="Objective 3")
#'
interactive3dplot <- function(
    x,
    percentiles=c(0,50,100),
    col=c("black","white"),
    type="point",
    psize=3,
    xlabel="X",
    ylabel ="Y",
    zlabel="Z"){

  eafdata<-as.data.frame(eaf(x,percentiles = percentiles))
  colnames(eafdata) <- c("x","y","z","Scenario")

  labels <- sapply(percentiles, function(pct) {
    if (pct == 0) return("Best")
    if (pct == 50) return("Median")
    if (pct == 100) return("Worst")
    return(paste0(pct, "%"))
  })

  if (type!="cube"){
    eafdata$Scenario <- factor(eafdata$Scenario, levels = percentiles, labels = labels)
  }

  uniqueScenario <- unique(eafdata$Scenario)

  pal<-colorRampPalette(colors=col,space="Lab")
  plotcol<-pal(length(uniqueScenario))

  if (type=="cube"){
    eafdata<-eafdata %>%
      filter(Scenario %in% percentiles[1])

    ini3D(argsPlot3d = list(xlim = c(min(eafdata$x),max(eafdata$x)),
                            ylim = c(min(eafdata$y),max(eafdata$y)),
                            zlim = c(min(eafdata$z),max(eafdata$z))))
    plotPoints3D(eafdata[1:3], argsPlot3d = list(col = plotcol[[2]], size = 5))
    plotCones3D(eafdata[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1, color = plotcol[[1]]),direction = -1)
    finalize3D()
    }

  else{
      myplot<- plot_ly(eafdata,x = ~x, y = ~y, z = ~z,
                       color = ~Scenario,colors=plotcol,opacity=0.8,
                       type = "scatter3d",mode="markers",marker = list(size = psize),
                       name = ~Scenario)%>%
        layout(scene = list(
          xaxis = list(title = xlabel),
          yaxis = list(title = ylabel),
          zaxis = list(title = zlabel)
        ))
    }
    return(myplot)
  }
