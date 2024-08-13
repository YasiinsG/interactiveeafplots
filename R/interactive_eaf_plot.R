#' Interactively plotting the EAF
#'
#' @param x Either a matrix of data values, or a data frame, or a list of data frames of exactly three columns.
#' @param percentiles \code{(numeric())} - Vector indicating which percentile should be plot. The default is to plot only the median attainment curve.
#' @param maximise \code{(logical() | logical(1))} - Whether the objectives must be maximised instead of minimised. Either a single logical value that applies to all objectives or a vector of logical values, with one value per objective.
#' @param col A vector of colours used to plot.
#' @param type \code{(character(1))} - string giving the type of plot desired. The following values are possible, \code{'point'} and \code{'area'}.
#' @param lty Line type. See \code{\link[plot.default()]{plot.default()}}.
#' @param psize Size of plotted points
#' @param pshape Shape of plotted point. See \code{\link[plot.default()]{plot.default()}}.
#' @param legend.pos The position of the legend, valid values are \code{'topright'}, \code{'top'}, \code{'topleft'}, \code{'right'}, \code{'left'}, \code{'bottom'}, \code{'bottomright'}, \code{'bottomleft'} and \code{'center'}. A value of \code{'none'} hides the legend.
#' @param xaxis.side On which side that xaxis is drawn. Valid values are \code{'below'} and \code{'above'}.
#' @param yaxis.side On which side that yaxis is drawn. Valid values are \code{'left'} and \code{'right'}.
#' @param axes A logical value indicating whether both axes should be drawn on the plot.
#' @param xlabel,ylabel Axes labels.
#' @param sci.notation Generate scientific labels
#' @param plot Interactive or none interactive plot. Valid values are \code{'plotly'} and \code{'ggplot'}.
#'
#' @return A plot of the attainment surfaces.
#' @export
#' @import plotly
#' @import moocore
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'
#' @examples
#' options("warn"=-1)
#' library(interactiveeafplots)
#'
#' data(gcp2x2,package='interactiveeafplots')
#' tabucol <- subset(gcp2x2, alg != "TSinN1")
#' tabucol$alg <- tabucol$alg[drop=TRUE]
#' mydata <- dplyr::filter(tabucol, inst == "DSJC500.5")
#' mydata <- mydata[c("time","best","run")]
#' interactiveeafplot(x=mydata, percentiles=c(0,50,100), col=c("yellow","red"),
#'                    maximise=FALSE, type="area", lty="longdash",
#'                    psize=3, pshape=10, legend.pos="topright",
#'                    xaxis.side="top", yaxis.side="left", axes=TRUE,
#'                    sci.notation=FALSE, xlabel="MIN X", ylabel="MIN Y",
#'                    plot="plotly")
#'
#' data(SPEA2minstoptimeRichmond,package='interactiveeafplots')
#' SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
#' interactiveeafplot(SPEA2minstoptimeRichmond, percentiles = c(0,50,100),
#'                    col=c("yellow","red"),maximise=c(TRUE,FALSE), type="point",
#'                    lty="longdash", psize=3, pshape=10, legend.pos="topleft",
#'                    xaxis.side="bottom", yaxis.side="left", axes=TRUE,
#'                    sci.notation=FALSE, xlabel = "C[E]",
#'                    ylabel = "Minimum idle time (minutes)",
#'                    plot = "plotly")
#'
interactiveeafplot <- function(
    x,
    percentiles=c(0,50,100),
    maximise=FALSE,
    col=c("white","black"),
    type="point",
    lty="solid",
    psize=2,
    pshape=16,
    legend.pos="topright",
    xaxis.side="bottom",
    yaxis.side="left",
    axes=TRUE,
    xlabel=NULL,
    ylabel =NULL,
    sci.notation=FALSE,
    plot="plotly"){

  if (all(maximise == c(TRUE, TRUE))) {
    dir<-"vh"
  } else if (all(maximise == c(FALSE, FALSE))) {
    dir<-"hv"
  } else if (all(maximise == c(TRUE, FALSE))) {
    dir<-"vh"
  } else {
    dir<-"vh"
  }

  if (plot=="plotly"){
    limvar <- .Machine$double.xmax
    limvarfill <- .Machine$integer.max
  }else{
    limvar <- Inf
    limvarfill <- Inf
  }

  if(length(maximise)==1){
    if (maximise==TRUE){
      maximise<-c(TRUE,TRUE)
    }
    else if (maximise==FALSE){
      maximise<-c(FALSE,FALSE)
    }
  }

  eafdata<-as.data.frame(eaf(x,maximise = maximise,percentiles = percentiles))
  colnames(eafdata) <- c("Time","Best","Scenario")

  labels <- sapply(percentiles, function(pct) {
    if (pct == 0) return("Best")
    if (pct == 50) return("Median")
    if (pct == 100) return("Worst")
    return(paste0(pct, "%"))
  })

  eafdata$Scenario <- factor(eafdata$Scenario, levels = percentiles, labels = labels)
  newdata2 <- eafdata

  uniqueScenario <- unique(newdata2$Scenario)

  datalist=list()
  datalist2=list()
  i=1
  for (a in uniqueScenario){
    uniqueData <- newdata2 %>% filter(Scenario %in% a)
    firstDataElement <- head(uniqueData,n=1)
    lastDataElement <- tail(uniqueData,n=1)
    datalist[[i]]<- firstDataElement
    datalist2[[i]]<-lastDataElement
    i <- i+1
  }
  big_data = data.frame(do.call(rbind, datalist))
  big_data2 = data.frame(do.call(rbind, datalist2))

  if (all(maximise == c(TRUE, TRUE))) {
    big_data$Best <- limvar * -1
    big_data2$Time <- limvar * -1
  } else if (all(maximise == c(FALSE, FALSE))) {
    big_data$Best <- limvar
    big_data2$Time <- limvar
  } else if (all(maximise == c(TRUE, FALSE))) {
    big_data$Best <- limvar
    big_data2$Time <- limvar * -1
  } else {
    big_data$Best <- limvar *-1
    big_data2$Time <- limvar
  }

  big_data3 <- rbind(big_data2,big_data)
  newdata3<- rbind(newdata2,big_data3)

  if (all(maximise == c(FALSE,TRUE))) {
    newdata3<-arrange(newdata3,newdata3$Best)
    newdata3<-arrange(newdata3,newdata3$Scenario)
  }
  else if (all(maximise == c(FALSE,FALSE))){
    newdata3<-arrange(newdata3,desc(newdata3$Best))
    newdata3<-arrange(newdata3,newdata3$Scenario)
  }
  else if (all(maximise == c(TRUE,FALSE))){
    newdata3<-arrange(newdata3,newdata3$Best)
    newdata3<-arrange(newdata3,newdata3$Scenario)
  }
  else {
    newdata3<-arrange(newdata3,desc(newdata3$Best))
    newdata3<-arrange(newdata3,newdata3$Scenario)
  }

  pal<-colorRampPalette(colors=col,space="Lab")
  if (any(c("white") %in% col)){
    plotcol<-pal(length(uniqueScenario)+1)
  }
  else{
    plotcol<-pal(length(uniqueScenario))
  }

  newdata3 <- newdata3 %>%
    group_by(Scenario) %>%
    mutate(next_Time = lead(Time, default = last(Time)),
           next_Best = lead(Best, default = last(Best))) %>%
    ungroup()

  p <- ggplot(data = newdata3, aes(x=Time, y=Best, color=Scenario)) +
    {if (any(c("white") %in% col)){
      scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
    }
      else{
        scale_color_manual(values = plotcol)
      }
    } +
    labs(color = NULL) +
    geom_step(direction = dir,linetype=lty,show.legend = FALSE) +
    {if(type=="area"){
      if (any(c("white") %in% col)){
        scale_fill_manual(values = plotcol[plotcol != "#FFFFFF"],name="")
      }
      else{
        scale_fill_manual(values = plotcol,name="")
      }
    }
    } +
    {if(psize!=0){
      geom_point(size=psize,shape=pshape,show.legend = FALSE,
                 aes(text= paste("Time: ", Time, "<br>",
                                 "Best: ", Best, "<br>",
                                 "Percentile: ", Scenario, sep = "")))
    }}+
    theme_bw() +
    {if (axes==FALSE){
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())}}+
    {if (is.null(xlabel)){
      xlab(colnames(newdata3)[1])}
      else {
        xlab(xlabel)}} +
    {if (is.null(ylabel)){
      ylab(colnames(newdata3)[2])}
      else {
        ylab(ylabel)}}+
    coord_cartesian(xlim = c(min(newdata2$Time),max(newdata2$Time)),ylim = c(min(newdata2$Best),max(newdata2$Best)))

    if (sci.notation==TRUE){
      p<-p+theme(axis.text.x = element_text(angle = 90))+
        scale_x_continuous(position = xaxis.side,labels = function(x) format(x, scientific = TRUE))+
        scale_y_continuous(position = yaxis.side,labels = function(x) format(x, scientific = TRUE))
    }else{
      p<-p+
        scale_x_continuous(position = xaxis.side)+
        scale_y_continuous(position = yaxis.side)
    }
  if (plot=="ggplot"){
    {if(legend.pos=="top"){
      p<-p+ theme(legend.title = element_blank(),
                  legend.position = "top")
    }
      else if(legend.pos=="topleft"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "left",
                    legend.position = "top")
      }
      else if(legend.pos=="bottomright"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "right",
                    legend.position = "bottom")
      }
      else if(legend.pos=="bottom"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.position = "bottom")
      }
      else if(legend.pos=="bottomleft"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "left",
                    legend.position = "bottom")
      }
      else if(legend.pos=="right"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "right")
      }
      else if(legend.pos=="left"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.position  = "left")
      }
      else if(legend.pos=="center"){
        p<-p+ theme(legend.title = element_blank(),
                    legend.position = "inside")
      }
      else{
        p<-p+ theme(legend.title = element_blank(),
                    legend.justification = "right",
                    legend.position = "top")
      }}
    }

  if (type=="area"){
    if (all(maximise==c(TRUE,TRUE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill*-1 , ymin = Best, ymax = limvarfill*-1, fill = Scenario), colour=NA)
      }
    else if(all(maximise==c(FALSE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill, ymin =limvarfill,ymax=Best, fill = Scenario), colour=NA)
      }
    else if(all(maximise==c(TRUE,FALSE))){
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill*-1, ymin = Best, ymax = limvarfill, fill = Scenario),colour=NA)
      }
    else{
      p<-p+
        geom_rect(aes(xmin = Time, xmax = limvarfill, ymin = next_Best, ymax = limvarfill*-1, fill = Scenario),colour=NA)
      }
  }

  if (plot=="ggplot"){
    p
  }
  else{

    myplot<- ggplotly(p,dynamicTicks = TRUE,tooltip = c("text"))
    for (i in 1:length(myplot$x$data)){
      if (!is.null(myplot$x$data[[i]]$name)){
        myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
      }
    }

    {if(legend.pos=="top"){
      myplot<-myplot%>%
        layout(legend = list(orientation = "h",xanchor="center",yanchor="top", y = 0.99, x = 0.5))
    }
      else if(legend.pos=="topleft"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="left",yanchor="top", y = 0.99, x = 0.005))
      }
      else if(legend.pos=="bottomright"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="right",yanchor="bottom", y = 0.01, x = 0.995))
      }
      else if(legend.pos=="bottom"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "h",xanchor="center",yanchor="bottom", y = 0.01, x = 0.5))
      }
      else if(legend.pos=="bottomleft"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="left",yanchor="bottom", y = 0.01, x = 0.005))
      }
      else if(legend.pos=="right"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="right",yanchor="middle", y = 0.5, x = 0.995))
      }
      else if(legend.pos=="left"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="left",yanchor="middle", y = 0.5, x = 0.005))
      }
      else if(legend.pos=="center"){
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="center",yanchor="middle", y = 0.5, x = 0.425))
      }
      else{
        myplot<-myplot%>%
          layout(legend = list(orientation = "v",xanchor="right",yanchor="top", y = 0.99, x = 0.995))
      }}

    y_padding = (max(newdata2$Best)-min(newdata2$Best))/length(newdata2$Best)
    min_y = min(newdata2$Best) - y_padding
    max_y = max(newdata2$Best) + y_padding

    x_padding = (max(newdata2$Time)-min(newdata2$Time))/length(newdata2$Time)
    min_x = min(newdata2$Time) - x_padding
    max_x = max(newdata2$Time) + x_padding

    if (type=="area"){

      myplot <- myplot %>%
        layout(
          xaxis = list(autorange=FALSE,range=c(min_x,max_x),side = xaxis.side),
          yaxis = list(autorange=FALSE,range=c(min_y,max_y),side = yaxis.side)
        )
    }else{
      myplot <- myplot %>%
        layout(
          xaxis = list(side = xaxis.side),
          yaxis = list(side = yaxis.side)
        )
    }
    if (sci.notation==TRUE){
      myplot<-myplot %>%
        layout(
          xaxis = list(tickformat=".2e"),
          yaxis = list(tickformat=".2e")
        )
    }

    return(myplot)
    }
}
