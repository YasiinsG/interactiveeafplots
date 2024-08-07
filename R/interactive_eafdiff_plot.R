#' Interactively plotting the EAF difference
#'
#' @param data_left,data_right Data frames corresponding to the input data of left and right sides, respectively. Each data frame has at least three columns, the third one being the set of each point. See also \code{\link[read_datasets()]{read_datasets()}}.
#' @param intervals \code{(integer(1)|character())} - The absolute range of the differences \code{[0, 1]} is partitioned into the number of intervals provided. If an integer is provided, then labels for each interval are computed automatically. If a character vector is provided, its length is taken as the number of intervals.
#' @param maximise \code{(logical() | logical(1))} - Whether the objectives must be maximised instead of minimised. Either a single logical value that applies to all objectives or a vector of logical values, with one value per objective.
#' @param grand.lines Whether to plot the grand-best and grand-worst attainment surfaces.
#' @param col A character vector of colors for the magnitude of the differences. Intermediate colors are computed automatically given the value of \code{intervals}. Alternatively, a function such as \code{\link[viridisLite::viridis()]{viridisLite::viridis()}} that generates a colormap given an integer argument.
#' @param type Whether the EAF differences are plotted as points \code{'point'} or whether to color the areas that have at least a certain value \code{'area'}.
#' @param full.eaf Whether to plot the EAF of each side instead of the differences between the EAFs.
#' @param lty Line type. See \code{\link[plot.default()]{plot.default()}}.
#' @param psize Size of plotted point
#' @param pshape Shape of plotted point. See \code{\link[plot.default()]{plot.default()}}.
#' @param legend.pos The position of the legend, valid values are \code{'topright'}, \code{'top'}, \code{'topleft'}, \code{'right'}, \code{'left'}, \code{'bottom'}, \code{'bottomright'}, \code{'bottomleft'} and \code{'center'}. A value of \code{'none'} hides the legend.
#' @param xlabel,ylabel Axes labels.
#' @param sci.notation Generate scientific labels
#' @param plot Interactive or none interactive plot. Valid values are \code{'plotly'} and \code{'ggplot'}.
#' @param title_left,title_right Title for left and right plots, respectively.
#'
#' @return A plot of the attainment difference surfaces.
#' @export
#'
#' @import plotly
#' @import moocore
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import patchwork
#' @import ggplot2
#'
#' @examples
#' # The plots in the website look squashed because of how pkgdown
#' # generates them. They should look fine when you generate them yourself
#' # in a full-sized html window.
#'
#' options("warn"=-1)
#' library(interactiveeafplots)
#'
#' extdata_dir <- system.file(package="interactiveeafplots", "extdata")
#' A1 <- moocore::read_datasets(file.path(extdata_dir, "ALG_1_dat.xz"))
#' A2 <- moocore::read_datasets(file.path(extdata_dir, "ALG_2_dat.xz"))
#'
#' interactiveeafdiffplot(data_left=A1, data_right=A2, maximise = FALSE,
#'                        type = "area", legend.pos = "topright",psize = 1,
#'                        xlabel = "Objective 1", ylabel = "Objective 2",
#'                        title_left="Plot 1", title_right="Plot 2",
#'                        sci.notation = TRUE, grand.lines=TRUE, plot = "plotly",
#'                        full.eaf=TRUE,intervals = 5,
#'                        col = c("red","orange","yellow"))
#'
#'
#' if (requireNamespace("viridisLite", quietly=TRUE)) {
#'   viridis_r <- function(n) viridisLite::viridis(n, direction=-1)
#'   interactiveeafdiffplot(data_left=A1, data_right=A2, maximise = c(FALSE,TRUE),
#'                          type = "area", legend.pos = "bottomright",psize = 1,
#'                          xlabel = "Objective 1", ylabel = "Objective 2",
#'                          title_left="Plot 1", title_right="Plot 2",
#'                          sci.notation = TRUE, grand.lines=TRUE, plot = "plotly",
#'                          full.eaf=FALSE,intervals = 5,col = viridis_r(5))
#' }
#'
interactiveeafdiffplot <- function(
    data_left,
    data_right,
    intervals=5L,
    maximise=FALSE,
    grand.lines = TRUE,
    col=c("white","#808080","black"),
    type="point",
    full.eaf=FALSE,
    lty="solid",
    psize=0.1,
    pshape=16,
    legend.pos="topright",
    xlabel=NULL,
    ylabel =NULL,
    sci.notation=FALSE,
    plot="plotly",
    title_left="Data 1",
    title_right="Data 2"){

  if(length(maximise)==1){
    if (maximise==TRUE){
      maximise<-c(TRUE,TRUE)
    }
    else if (maximise==FALSE){
      maximise<-c(FALSE,FALSE)
    }
  }

  #set line step type
  if (all(maximise == c(TRUE, TRUE))) {
    dir<-"vh"
  } else if (all(maximise == c(FALSE, FALSE))) {
    dir<-"hv"
  } else if (all(maximise == c(TRUE, FALSE))) {
    dir<-"vh"
  } else {
    dir<-"hv"
  }

  if (plot=="plotly"){
    limvar <- .Machine$double.xmax
    limvarfill <- 1e25
  }else{
    limvar <- Inf
    limvarfill <- Inf
  }

  #define intervals and percentiles for data and legend.
  x<-data_left
  y<-data_right
  start <- 0
  end <- 100
  interval_width <- 100/intervals
  intervalsseq <- seq(0, 1, by = interval_width/100)
  interval_labels <- sapply(seq_along(intervalsseq[-length(intervalsseq)]), function(i) {
    paste0("[", round(intervalsseq[i], 5), ", ", round(intervalsseq[i + 1], 5), ")")
  })

  interval_labels <- sapply(seq_along(intervalsseq[-length(intervalsseq)]), function(i) {
    if (i == length(intervalsseq) - 1) {
      paste0("[", round(intervalsseq[i], 5), ", ", round(intervalsseq[i + 1], 5), "]")
    } else {
      paste0("[", round(intervalsseq[i], 5), ", ", round(intervalsseq[i + 1], 5), ")")
    }
  })

  lower_bounds <- seq(from = start, to = end, by = interval_width)
  lower_bounds <- lower_bounds[-length(intervalsseq)]

  # get eaf data
  eafdata <- as.data.frame(eaf(x, maximise = maximise, percentiles = c(lower_bounds)))
  eafdata2 <- as.data.frame(eaf(y, maximise=maximise, percentiles = c(lower_bounds)))
  joint <- rbind_datasets(x, y)
  eafdatajoint <- as.data.frame(eaf(joint, maximise = maximise))
  eafdatajoint1 <- eafdatajoint %>%
    filter(V3 %in% min(V3))
  eafdatajoint2 <- eafdatajoint %>%
    filter(V3 %in% max(V3))
  eafdata50 <- as.data.frame(eaf(x, maximise = maximise, percentiles = 50))
  eafdata250 <- as.data.frame(eaf(y, maximise = maximise, percentiles = 50))
  fulleafdata <- as.data.frame(eaf(x, maximise = maximise))
  fulleafdata2 <- as.data.frame(eaf(y, maximise=maximise))

  dataset1<- eafdata
  dataset2<-eafdata2

  # set column names
  colnames(dataset1) <- c("Time","Best","Scenario")
  colnames(dataset2) <- c("Time","Best","Scenario")
  colnames(eafdatajoint1) <- c("Time","Best","Scenario")
  colnames(eafdatajoint2) <- c("Time","Best","Scenario")
  colnames(eafdata50) <- c("Time","Best","Scenario")
  colnames(eafdata250) <- c("Time","Best","Scenario")
  colnames(fulleafdata) <- c("Time","Best","Scenario")
  colnames(fulleafdata2) <- c("Time","Best","Scenario")

  #relabel intervals
  dataset1$Scenario <- factor(dataset1$Scenario, levels = lower_bounds, labels = interval_labels)
  dataset2$Scenario <- factor(dataset2$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdatajoint1$Scenario <- factor(eafdatajoint1$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdatajoint2$Scenario <- factor(eafdatajoint2$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdata50$Scenario <- factor(eafdata50$Scenario, levels = lower_bounds, labels = interval_labels)
  eafdata250$Scenario <- factor(eafdata250$Scenario, levels = lower_bounds, labels = interval_labels)

  newdata2 <- dataset1
  newdata22 <- dataset2

  if (type=="area"){
    eafdiffdata1 <- as.data.frame(eafdiff(x,y,maximise = maximise,intervals = intervals,rectangles = TRUE))
    eafdiffdata2 <- as.data.frame(eafdiff(y,x,maximise = maximise,intervals = intervals,rectangles = TRUE))}
  else{
    eafdiffdata1 <- as.data.frame(eafdiff(x,y,maximise = maximise,intervals = intervals))
    eafdiffdata2 <- as.data.frame(eafdiff(y,x,maximise = maximise,intervals = intervals))}

  # fulleafdata<-arrange(fulleafdata,desc(fulleafdata$Scenario))

  # Function to extend plot percentiles and lines to large values.
  process_data <- function(eafdata, uniqueScenario) {
    datalist <- list()
    datalist2 <- list()
    i <- 1

    for (a in uniqueScenario) {
      uniqueData <- eafdata %>% filter(Scenario %in% a)
      datalist[[i]] <- head(uniqueData, n = 1)
      datalist2[[i]] <- tail(uniqueData, n = 1)
      i <- i + 1
    }

    big_data <- data.frame(do.call(rbind, datalist))
    big_data2 <- data.frame(do.call(rbind, datalist2))

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

    big_data3 <- rbind(big_data2, big_data)
    rbind(eafdata, big_data3)
  }

  k <- length(interval_labels)
  fulleafdata$Scenario <- floor(fulleafdata$Scenario/20)*20
  fulleafdata$Scenario <- ifelse(fulleafdata$Scenario > 80, 80, fulleafdata$Scenario)
  fulleafdata$Scenario <- factor(fulleafdata$Scenario, levels = lower_bounds, labels = interval_labels)

  fulleafdata2$Scenario <- floor(fulleafdata2$Scenario/20)*20
  fulleafdata2$Scenario <- ifelse(fulleafdata2$Scenario > 80, 80, fulleafdata2$Scenario)
  fulleafdata2$Scenario <- factor(fulleafdata2$Scenario, levels = lower_bounds, labels = interval_labels)


  if (type=="point"){
    eafdiffdata1 <- eafdiffdata1 %>%
      group_by(V3) %>%
      filter(V3 >= 0)%>%
      ungroup()

    eafdiffdata1$V3 <- floor(eafdiffdata1$V3)*100/k
    eafdiffdata1$V3 <- factor(eafdiffdata1$V3, levels = lower_bounds, labels = interval_labels)}
  else{
    eafdiffdata1 <- eafdiffdata1 %>%
      group_by(diff) %>%
      filter(diff >= 0)%>%
      ungroup()

    eafdiffdata1$diff <- floor(eafdiffdata1$diff)*100/k
    eafdiffdata1$diff <- factor(eafdiffdata1$diff, levels = lower_bounds, labels = interval_labels)
  }

  eafdiffdata1_filtered <- eafdiffdata1

  if (type=="point"){
    eafdiffdata2 <- eafdiffdata2 %>%
      group_by(V3) %>%
      filter(V3 >= 0)%>%
      ungroup()

    k <- length(interval_labels)
    eafdiffdata2$V3 <- floor(eafdiffdata2$V3)*100/k
    eafdiffdata2$V3 <- factor(eafdiffdata2$V3, levels = lower_bounds, labels = interval_labels)}
  else{
    eafdiffdata2 <- eafdiffdata2 %>%
      group_by(diff) %>%
      filter(diff >= 0)%>%
      ungroup()

    eafdiffdata2$diff <- floor(eafdiffdata2$diff)*100/k
    eafdiffdata2$diff <- factor(eafdiffdata2$diff, levels = lower_bounds, labels = interval_labels)
    }

  eafdiffdata2_filtered <- eafdiffdata2

  #get extended plot lines for p1
  if (full.eaf == TRUE) {
    uniqueScenario <- unique(newdata2$Scenario)
    newdata3 <- process_data(newdata2, uniqueScenario)
  }
  else if(type=="point"){
    uniqueScenario <- unique(eafdiffdata1_filtered$V3)
    newdata3 <- eafdiffdata1_filtered
  }
  else{
    uniqueScenario <- unique(eafdiffdata1_filtered$diff)
    newdata3 <- eafdiffdata1_filtered
  }

  #get extended plot lines for p2
  if (full.eaf == TRUE) {
    uniqueScenario2 <- unique(newdata22$Scenario)
    newdata4 <- process_data(newdata22, uniqueScenario2)
  }
  else if(type=="point") {
    uniqueScenario2 <- unique(eafdiffdata2_filtered$V3)
    newdata4 <- eafdiffdata2_filtered
  }
  else{
    uniqueScenario2 <- unique(eafdiffdata2_filtered$diff)
    newdata4 <- eafdiffdata2_filtered
  }
  #set colours
  pal <- colorRampPalette(colors = col)
  plotcol <- pal(max(length(uniqueScenario),length(uniqueScenario2)))
  plotcol[1] <- "transparent"
  plotcol[plotcol %in% c("white", "#FFFFFF")] <- "transparent"

  linecol<- "black"

  if(length(uniqueScenario)<length(uniqueScenario2)){
    a<-FALSE
    b<-TRUE
  } else{
    a<-TRUE
    b<-FALSE
  }

  # Plotting
  if (full.eaf==TRUE){
    if (all(maximise == c(FALSE,TRUE))) {
      newdata3<-arrange(newdata3,newdata3$Best)
      newdata3<-arrange(newdata3,newdata3$Scenario)
      newdata4<-arrange(newdata4,newdata4$Best)
      newdata4<-arrange(newdata4,newdata4$Scenario)
    }
    else if (all(maximise == c(FALSE,FALSE))){
      newdata3<-arrange(newdata3,desc(newdata3$Best))
      newdata3<-arrange(newdata3,newdata3$Scenario)
      newdata4<-arrange(newdata4,desc(newdata4$Best))
      newdata4<-arrange(newdata4,newdata4$Scenario)
    }
    else if (all(maximise == c(TRUE,FALSE))){
      newdata3<-arrange(newdata3,newdata3$Best)
      newdata3<-arrange(newdata3,newdata3$Scenario)
      newdata4<-arrange(newdata4,newdata4$Best)
      newdata4<-arrange(newdata4,newdata4$Scenario)
    }
    else {
      newdata3<-arrange(newdata3,desc(newdata3$Best))
      newdata3<-arrange(newdata3,newdata3$Scenario)
      newdata4<-arrange(newdata4,desc(newdata4$Best))
      newdata4<-arrange(newdata4,newdata4$Scenario)
    }
    # change data
    newdata3 <- newdata3 %>%
      group_by(Scenario) %>%
      mutate(next_Time = lead(Time, default = last(Time)),
             next_Best = lead(Best, default = last(Best))) %>%
      ungroup()
    newdata4 <- newdata4 %>%
      group_by(Scenario) %>%
      mutate(next_Time = lead(Time, default = last(Time)),
             next_Best = lead(Best, default = last(Best))) %>%
      ungroup()

    if (type=="area"){
      #plot p1
      p1 <- ggplot(data = newdata3, aes(x=Time, y=Best, color=Scenario)) +
        {if (any(c("white") %in% col)){
          scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
        }
          else{
            scale_color_manual(values = plotcol)
          }
        } +
        labs(color = NULL) +
        geom_step(direction = dir,linetype=lty,show.legend = FALSE) +
        {if(full.eaf==TRUE){
          if (any(c("white") %in% col)){
            scale_fill_manual(values = plotcol[plotcol != "#FFFFFF"],name="")
          }
          else{
            scale_fill_manual(values = plotcol,name="")
          }
        }
        } +
        {if(psize!=0){
          geom_point(size=psize,stroke=0,shape=pshape,show.legend = FALSE,
                     aes(text= paste("Objective 1: ", Time, "<br>",
                                     "Objective 2: ", Best, "<br>",
                                     "Percentile: ", Scenario, sep = "")))
        }}+
        theme_bw() +
        theme(legend.position = legend.pos,plot.margin = margin(0,0,0,0))+
        {if (is.null(xlabel)){
          xlab("Objective 1")}
          else {
            xlab(xlabel)}} +
        {if (is.null(ylabel)){
          ylab("Objective 2")}
          else {
            ylab(ylabel)}}

      #plot p2
      p2 <- ggplot(data = newdata4, aes(x=Time, y=Best, color=Scenario)) +
        {if (any(c("white") %in% col)){
          scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
        }
          else{
            scale_color_manual(values = plotcol)
          }
        } +
        labs(color = NULL) +
        geom_step(direction = dir,linetype=lty,show.legend = FALSE)+
        {if(full.eaf==TRUE){
          if (any(c("white") %in% col)){
            scale_fill_manual(values = plotcol[plotcol != "#FFFFFF"],name="")
          }
          else{
            scale_fill_manual(values = plotcol,name="")
          }
        }
        } +
        {if(psize!=0){
          geom_point(size=psize,stroke=0,shape=pshape,show.legend = FALSE,
                     aes(text= paste("Objective 1: ", Time, "<br>",
                                     "Objective 2: ", Best, "<br>",
                                     "Percentile: ", Scenario, sep = "")))
        }}+
        theme_bw() +
        theme(legend.position = legend.pos,plot.margin = margin(0,0,0,0))+
        {if (is.null(xlabel)){
          xlab("Objective 1")}
          else {
            xlab(xlabel)}} +
        {if (is.null(ylabel)){
          ylab("Objective 2")}
          else {
            ylab(ylabel)}}

      #plotting area
      if (all(maximise==c(TRUE,TRUE))){
        p2<-p2+
          geom_rect(aes(xmin = Time, xmax = limvarfill*-1 , ymin = Best, ymax = limvarfill*-1, fill = Scenario), colour=NA)
        p1<-p1+
          geom_rect(aes(xmin = Time, xmax = limvarfill*-1 , ymin = Best, ymax = limvarfill*-1, fill = Scenario), colour=NA)
      }
      else if(all(maximise==c(FALSE,FALSE))){
        p2<-p2+
          geom_rect(aes(xmin = Time, xmax = limvarfill, ymin =limvarfill,ymax=Best, fill = Scenario), colour=NA)
        p1<-p1+
          geom_rect(aes(xmin = Time, xmax = limvarfill, ymin =limvarfill,ymax=Best, fill = Scenario), colour=NA)
      }
      else if(all(maximise==c(TRUE,FALSE))){
        p2<-p2+
          geom_rect(aes(xmin = Time, xmax = limvarfill*-1, ymin = Best, ymax = limvarfill, fill = Scenario),colour=NA)
        p1<-p1+
          geom_rect(aes(xmin = Time, xmax = limvarfill*-1, ymin = Best, ymax = limvarfill, fill = Scenario),colour=NA)
      }
      else{
        p2<-p2+
          geom_rect(aes(xmin = Time, xmax = limvarfill, ymin = Best, ymax = limvarfill*-1, fill = Scenario),colour=NA)
        p1<-p1+
          geom_rect(aes(xmin = Time, xmax = limvarfill, ymin = Best, ymax = limvarfill*-1, fill = Scenario),colour=NA)
      }
      linecol<-"white"
    }
    else{
      newdata3<-arrange(newdata3,desc(newdata3$Scenario))
      newdata4<-arrange(newdata4,desc(newdata4$Scenario))

      #plot p1
      p1 <- ggplot(data = fulleafdata, aes(x=Time, y=Best, color=Scenario)) +
        scale_color_manual(values = plotcol)+
        scale_fill_manual(values = plotcol)+
        {if(psize!=0){
          geom_point(size=psize,stroke=0,shape=pshape,show.legend = a,
                     aes(text= paste("Objective 1: ", Time, "<br>",
                                     "Objective 2: ", Best, "<br>",
                                     "Percentile: ", Scenario, sep = "")))
        }}+
        theme_bw() +
        theme(legend.position = legend.pos,plot.margin = margin(0,0,0,0))+
        {if (is.null(xlabel)){
          xlab("Objective 1")}
          else {
            xlab(xlabel)}} +
        {if (is.null(ylabel)){
          ylab("Objective 2")}
          else {
            ylab(ylabel)}}

      #plot p2
      p2 <- ggplot(data = fulleafdata2, aes(x=Time, y=Best, color=Scenario)) +
        {if (any(c("white") %in% col)){
          scale_color_manual(values = plotcol[plotcol != "#FFFFFF"])
        }
          else{
            scale_color_manual(values = plotcol)
          }
        } +
        labs(color = NULL) +
        {if(full.eaf==TRUE){
          if (any(c("white") %in% col)){
            scale_fill_manual(values = plotcol[plotcol != "#FFFFFF"],name="")
          }
          else{
            scale_fill_manual(values = plotcol,name="")
          }
        }
        } +
        {if(psize!=0){
          geom_point(size=psize,stroke=0,shape=pshape,show.legend = b,
                     aes(text= paste("Objective 1: ", Time, "<br>",
                                     "Objective 2: ", Best, "<br>",
                                     "Percentile: ", Scenario, sep = "")))
        }}+
        theme_bw() +
        theme(legend.position = legend.pos,plot.margin = margin(0,0,0,0))+
        {if (is.null(xlabel)){
          xlab("Objective 1")}
          else {
            xlab(xlabel)}} +
        {if (is.null(ylabel)){
          ylab("Objective 2")}
          else {
            ylab(ylabel)}}

    }
  }
  else{
    if (type=="area"){
      if (plot=="plotly"){
        if (all(maximise == c(TRUE, TRUE))) {
          newdata3 <- newdata3 %>%
            mutate(across(-last_col(), ~ ifelse(is.infinite(.)*-1, 1e25*-1, .)))
          newdata4 <- newdata4 %>%
            mutate(across(-last_col(), ~ ifelse(is.infinite(.)*-1, 1e25*-1, .)))
        } else if (all(maximise == c(FALSE, FALSE))) {
          newdata3 <- newdata3 %>%
            mutate(across(-last_col(), ~ ifelse(is.infinite(.), 1e25, .)))
          newdata4 <- newdata4 %>%
            mutate(across(-last_col(), ~ ifelse(is.infinite(.), 1e25, .)))
        } else if (all(maximise == c(TRUE, FALSE))) {
          newdata3 <- newdata3 %>%
            mutate(across(xmax, ~ ifelse(is.infinite(.)*-1, 1e25*-1, .)))
          newdata3 <- newdata3 %>%
            mutate(across(ymax, ~ ifelse(is.infinite(.), 1e25, .)))
          newdata4 <- newdata4 %>%
            mutate(across(xmax, ~ ifelse(is.infinite(.)*-1, 1e25*-1, .)))
          newdata4 <- newdata4 %>%
            mutate(across(ymax, ~ ifelse(is.infinite(.), 1e25, .)))
        } else {
          newdata3 <- newdata3 %>%
            mutate(across(xmax, ~ ifelse(is.infinite(.), 1e25, .)))
          newdata3 <- newdata3 %>%
            mutate(across(ymax, ~ ifelse(is.infinite(.)*-1, 1e25*-1, .)))
          newdata4 <- newdata4 %>%
            mutate(across(xmax, ~ ifelse(is.infinite(.), 1e25, .)))
          newdata4 <- newdata4 %>%
            mutate(across(ymax, ~ ifelse(is.infinite(.)*-1, 1e25*-1, .)))
        }
      }

      if (nrow(newdata3) != 0) {
        p1 <- ggplot() +
          geom_rect(data = newdata3,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=diff,color=diff),show.legend = a)+
          scale_color_manual(values = plotcol,name="") +#,labels=letters[1:5])+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
        {if (is.null(xlabel)){
          xlab("Objective 1")}
          else {
            xlab(xlabel)}} +
        {if (is.null(ylabel)){
          ylab("Objective 2")}
          else {
            ylab(ylabel)}}}

      else{
        p1<-ggplot()+
          scale_color_manual(values = plotcol,name="")+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
          {if (is.null(xlabel)){
            xlab("Objective 1")}
            else {
              xlab(xlabel)}} +
          {if (is.null(ylabel)){
            ylab("Objective 2")}
            else {
              ylab(ylabel)}}
      }

      if (nrow(newdata4) != 0) {
        p2 <- ggplot() +
          geom_rect(data = newdata4,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=diff,color=diff),show.legend = b)+
          scale_color_manual(values = plotcol,name="")+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
        {if (is.null(xlabel)){
          xlab("Objective 1")}
          else {
            xlab(xlabel)}} +
        {if (is.null(ylabel)){
          ylab("Objective 2")}
          else {
            ylab(ylabel)}}}
      else{
        p2<-ggplot()+
          scale_color_manual(values = plotcol,name="")+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
          {if (is.null(xlabel)){
            xlab("Objective 1")}
            else {
              xlab(xlabel)}} +
          {if (is.null(ylabel)){
            ylab("Objective 2")}
            else {
              ylab(ylabel)}}
      }
    }
    else{
      if (nrow(newdata3) != 0) {
        p1 <- ggplot(data = newdata3,aes(x=V1,y=V2,color=V3)) +
          {if(psize!=0){
            geom_point(size=psize,stroke=0,shape=pshape,show.legend = a,
                       aes(text= paste("Objective 1: ", V1, "<br>",
                                       "Objective 2: ", V2, "<br>",
                                       "Percentile: ", V3, sep = "")))}}+
          scale_color_manual(values = plotcol,name="")+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
          {if (is.null(xlabel)){
            xlab("Objective 1")}
            else {
              xlab(xlabel)}} +
          {if (is.null(ylabel)){
            ylab("Objective 2")}
            else {
              ylab(ylabel)}}}
      else{
        p1<-ggplot()+
          scale_color_manual(values = plotcol,name="")+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
          {if (is.null(xlabel)){
            xlab("Objective 1")}
            else {
              xlab(xlabel)}} +
          {if (is.null(ylabel)){
            ylab("Objective 2")}
            else {
              ylab(ylabel)}}
      }

      if (nrow(newdata4) != 0) {
        p2 <- ggplot(data = newdata4,aes(x=V1,y=V2,color=V3)) +
          {if(psize!=0){
            geom_point(size=psize,stroke=0,shape=pshape,show.legend = b,
                       aes(text= paste("Objective 1: ", V1, "<br>",
                                       "Objective 2: ", V2, "<br>",
                                       "Percentile: ", V3, sep = "")))}}+
          scale_color_manual(values = plotcol,name="")+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
          {if (is.null(xlabel)){
            xlab("Objective 1")}
            else {
              xlab(xlabel)}} +
          {if (is.null(ylabel)){
            ylab("Objective 2")}
            else {
              ylab(ylabel)}}}
      else{
        p2<-ggplot()+
          scale_color_manual(values = plotcol,name="")+
          scale_fill_manual(values = plotcol,name="") +
          theme_bw()+
          theme(legend.position = legend.pos)+
          {if (is.null(xlabel)){
            xlab("Objective 1")}
            else {
              xlab(xlabel)}} +
          {if (is.null(ylabel)){
            ylab("Objective 2")}
            else {
              ylab(ylabel)}}
      }
    }
  }

  {if (sci.notation==TRUE){
    p1<-p1+theme(axis.text.x = element_text(angle = 90))+
      scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE))}}


  {if (sci.notation==TRUE){
    p2<-p2+theme(axis.text.x = element_text(angle = 90))+
      scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE))}}

  #if plotting grand lines
  if (grand.lines == TRUE) {
    uniqueScenarios <- list(
      unique(eafdatajoint1$Scenario),
      unique(eafdatajoint2$Scenario),
      unique(eafdata50$Scenario),
      unique(eafdata250$Scenario)
    )

    eafdatajoint1 <- process_data(eafdatajoint1, uniqueScenarios[[1]])
    eafdatajoint2 <- process_data(eafdatajoint2, uniqueScenarios[[2]])
    eafdata50 <- process_data(eafdata50, uniqueScenarios[[3]])
    eafdata250 <- process_data(eafdata250, uniqueScenarios[[4]])

    if (all(maximise == c(FALSE,TRUE))) {
      eafdatajoint1<-arrange(eafdatajoint1,eafdatajoint1$Best)
      eafdatajoint2<-arrange(eafdatajoint2,eafdatajoint2$Best)
      eafdata50<-arrange(eafdata50,eafdata50$Best)
      eafdata250<-arrange(eafdata250,eafdata250$Best)
    }
    else if (all(maximise == c(FALSE,FALSE))){
      eafdatajoint1<-arrange(eafdatajoint1,desc(eafdatajoint1$Best))
      eafdatajoint2<-arrange(eafdatajoint2,desc(eafdatajoint2$Best))
      eafdata50<-arrange(eafdata50,desc(eafdata50$Best))
      eafdata250<-arrange(eafdata250,desc(eafdata250$Best))
    }
    else if (all(maximise == c(TRUE,FALSE))){
      eafdatajoint1<-arrange(eafdatajoint1,eafdatajoint1$Best)
      eafdatajoint2<-arrange(eafdatajoint2,eafdatajoint2$Best)
      eafdata50<-arrange(eafdata50,eafdata50$Best)
      eafdata250<-arrange(eafdata250,eafdata250$Best)
    }
    else {
      eafdatajoint1<-arrange(eafdatajoint1,desc(eafdatajoint1$Best))
      eafdatajoint2<-arrange(eafdatajoint2,desc(eafdatajoint2$Best))
      eafdata50<-arrange(eafdata50,desc(eafdata50$Best))
      eafdata250<-arrange(eafdata250,desc(eafdata250$Best))
    }

    p1<-p1+
      geom_step(data = eafdatajoint1,aes(x=Time,y=Best),color= "black",lwd=0.25,direction = dir,show.legend = FALSE)+
      {if(psize!=0){
        geom_point(data = eafdatajoint1,stroke=0,color="black",size=0.1,shape=pshape,show.legend = FALSE,
                   aes(x=Time,y=Best,text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", "Grand Worst", sep = "")))}}+

      geom_step(data = eafdatajoint2,aes(x=Time,y=Best),color= linecol,lwd=0.25,direction = dir,show.legend = FALSE)+
      {if(psize!=0){
        geom_point(data = eafdatajoint2,stroke=0,color=linecol,size=0.1,shape=pshape,show.legend = FALSE,
                   aes(x=Time,y=Best,text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", "Grand Best", sep = "")))}}+

      geom_step(data = eafdata50,aes(x=Time,y=Best),color= "black",linetype="dotted",lwd=0.25,direction = dir,show.legend = FALSE)+
      {if(psize!=0){
        geom_point(data = eafdata50,stroke=0,color="black",size=0.1,shape=pshape,show.legend = FALSE,
                   aes(x=Time,y=Best,text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", 0.5, sep = "")))}}

    p2<-p2+
      geom_step(data = eafdatajoint1,aes(x=Time,y=Best),color= "black",lwd=0.25,direction = dir,show.legend = FALSE) +
      {if(psize!=0){
        geom_point(data = eafdatajoint1,stroke=0,color="black",size=0.1,shape=pshape,show.legend = FALSE,
                   aes(x=Time,y=Best,text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", "Grand Worst", sep = "")))}}+

      geom_step(data = eafdatajoint2,aes(x=Time,y=Best),color= linecol,lwd=0.25,direction = dir,show.legend = FALSE)+
      {if(psize!=0){
        geom_point(data = eafdatajoint2,stroke=0,color=linecol,size=0.1,shape=pshape,show.legend = FALSE,
                   aes(x=Time,y=Best,text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", "Grand Best", sep = "")))}}+

      geom_step(data = eafdata250,aes(x=Time,y=Best),color= "black",linetype="dotted",lwd=0.25,direction = dir,show.legend = FALSE)+
      {if(psize!=0){
        geom_point(data = eafdata250,stroke=0,color="black",size=0.1,shape=pshape,show.legend = FALSE,
                   aes(x=Time,y=Best,text= paste("Objective 1: ", Time, "<br>",
                                   "Objective 2: ", Best, "<br>",
                                   "Percentile: ", 0.5, sep = "")))}}
  }

  #2nd y axis
  p1<- p1+scale_y_continuous(position = "left")
  p2<- p2+scale_y_continuous(position = "right")

  #set axis limits
  a <- max(max(newdata2$Time),max(newdata22$Time))
  b <- min(min(newdata2$Time),min(newdata22$Time))
  c <- max(max(newdata2$Best),max(newdata22$Best))
  d <- min(min(newdata2$Best),min(newdata22$Best))
  e <- min(length(newdata2$Time),length(newdata22$Time))
  f <- min(length(newdata2$Best),length(newdata22$Best))
  x_padding = (a-b)/e
  y_padding = (c-d)/f
  min_x <- b- x_padding
  max_x <- a + x_padding
  min_y <- d-y_padding
  max_y <- c + y_padding

  if (plot=="ggplot"){
    p2<-p2+scale_x_continuous(position = "top")
    p1<-p1+
      ggtitle(title_left)+
      theme(plot.title = element_text(hjust = 0.5))+
      coord_cartesian(xlim = c(min_x, max_x),
                      ylim = c(min_y, max_y))
    p2<-p2+
      ggtitle(title_right)+
      theme(plot.title = element_text(hjust = 0.5))+
      coord_cartesian(xlim = c(min_x, max_x),
                      ylim = c(min_y, max_y))

    p<- p1 + p2 + plot_layout(guides = "collect")&
      theme(plot.margin = margin(0, 0, 0, 0))
    p<-p+plot_annotation(theme = theme(plot.margin = margin(0,0,0,0)))

    if(legend.pos=="top"){
      p<-p & theme(legend.title = element_blank(),
                  legend.position = "top")
    }
    else if(legend.pos=="topleft"){
      p<-p & theme(legend.title = element_blank(),
                  legend.justification = "left",
                  legend.position = "top")
    }
    else if(legend.pos=="bottomright"){
      p<-p & theme(legend.title = element_blank(),
                  legend.justification = "right",
                  legend.position = "bottom")
    }
    else if(legend.pos=="bottom"){
      p<-p & theme(legend.title = element_blank(),
                  legend.position = "bottom")
    }
    else if(legend.pos=="bottomleft"){
      p<-p & theme(legend.title = element_blank(),
                  legend.justification = "left",
                  legend.position = "bottom")
    }
    else if(legend.pos=="right"){
      p<-p & theme(legend.title = element_blank(),
                  legend.justification = "right")
    }
    else if(legend.pos=="left"){
      p<-p & theme(legend.title = element_blank(),
                  legend.position  = "left")
    }
    else if(legend.pos=="center"){
      p<-p & theme(legend.title = element_blank(),
                  legend.position = "inside")
    }
    else{
      p<-p & theme(legend.title = element_blank(),
                  legend.justification = "right",
                  legend.position = "top")
    }
    p

  }
  else{
    # Convert ggplot objects to plotly objects
    p1_plotly <- ggplotly(p1,dynamicTicks = TRUE,tooltip = c("text"))
    p2_plotly <- ggplotly(p2, dynamicTicks = TRUE,tooltip = c("text"))

  # Add annotations for subplot titles
    p1_plotly <- p1_plotly %>%
      layout(
        annotations = list(
          list(
            x = 0.5, y = 1.05, # Adjust x and y to position the title
            text = title_left, # Set the title for subplot 1
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            font = list(size = 20)
          )
        )
      )

    p2_plotly <- p2_plotly %>%
      layout(
        annotations = list(
          list(
            x = 0.5, y = -0.05, # Adjust x and y to position the title
            text = title_right, # Set the title for subplot 2
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "bottom",
            font = list(size = 20)
          )
        )
      )

    if(full.eaf==FALSE){
      i <- 1
      while (i <= min(length(p1_plotly$x$data),length(p2_plotly$x$data))) {
        if (p1_plotly$x$data[[i]]$name == p2_plotly$x$data[[i]]$name) {
          p2_plotly$x$data[[i]]$showlegend <- FALSE
          i <- i + 1  # Move to the next pair
        }
        else{
          i <- i + 1  # Move to the next element
        }
      }
      p1_plotly$x$data <- Filter(Negate(is.null), p1_plotly$x$data)
      p2_plotly$x$data <- Filter(Negate(is.null), p2_plotly$x$data)}

    # Combine both plots using subplot with shared axes
    myplot <- subplot(p1_plotly, p2_plotly, nrows = 1, margin = 0,shareX = TRUE)

    if(full.eaf==TRUE){
      #Sort out legend
      #delete every other legend element (as duplicated due to 2 subplots)
      count<-0
      for (i in 1:length(myplot$x$data)/2){
        if (count == 1){
          myplot$x$data[[i]]$showlegend <- FALSE
          count <- 0
        }else{
          count <- 1
        }
      }
    }

    if(type=="point" || full.eaf==TRUE){
      #restructure legend appearance
      for (i in 1:length(myplot$x$data)) {
        if (!is.null(myplot$x$data[[i]]$name)) {
          interval <- gsub(".*\\((\\[.*?\\)),.*", "\\1", myplot$x$data[[i]]$name)
        }
        myplot$x$data[[i]]$name <- interval
      }
      for (i in 1:length(myplot$x$data)) {
        if (!is.null(myplot$x$data[[i]]$name)) {
          interval <- gsub(".*\\((\\[.*?\\]),.*", "\\1", myplot$x$data[[i]]$name)
        }
        myplot$x$data[[i]]$name <- interval
      }
    }

    #set legend position
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

    #set axis limits
    a <- max(max(newdata2$Time),max(newdata22$Time))
    b <- min(min(newdata2$Time),min(newdata22$Time))
    c <- max(max(newdata2$Best),max(newdata22$Best))
    d <- min(min(newdata2$Best),min(newdata22$Best))
    e <- min(length(newdata2$Time),length(newdata22$Time))
    f <- min(length(newdata2$Best),length(newdata22$Best))
    x_padding = (a-b)/e
    y_padding = (c-d)/f
    min_x <- b- x_padding
    max_x <- a + x_padding
    min_y <- d-y_padding
    max_y <- c + y_padding

    if (type=="area"){
      myplot <- myplot %>%
        layout(
          xaxis = list(title =xlabel,autorange=FALSE,range=c(min_x,max_x)),
          yaxis = list(title =ylabel,autorange=FALSE,range=c(min_y,max_y)),
          yaxis2 = list(title =ylabel,matches="y", autorange=FALSE,range=c(min_y,max_y),side = "right"),
          xaxis2 = list(title =xlabel,matches="x", autorange=FALSE,range=c(min_x,max_x),side = "top")
        )}
    else{
      myplot <- myplot %>%
        layout(
          xaxis = list(title =xlabel),
          yaxis = list(title =ylabel),
          yaxis2 = list(title =ylabel,matches="y",side = "right"),
          xaxis2 = list(title =xlabel,matches="x",side = "top")
        )
    }

    #set scientific notation
    if (sci.notation==TRUE){
      myplot<-myplot %>%
        layout(
          xaxis = list(tickformat=".2e"),
          yaxis = list(tickformat=".2e"),
          xaxis2 = list(tickformat=".2e"),
          yaxis2 = list(tickformat=".2e")
        )
    }
    return(myplot)}
  }
