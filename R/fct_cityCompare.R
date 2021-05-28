SSClineGeomSwitch <- function(switch, df, colour, selectedSSC){
  if(switch == TRUE){
    return(
      ggplot2::geom_vline(
        xintercept=base::as.numeric(df[df[["SSC_CODE16"]] == selectedSSC, ]["PerAnyTree"]),
        size=0.5,
        color= colour
      )
    )
  } else {
    return(NULL)
  }
}
densityGeomSwitch <- function(switch, df, GCC, colour, densityVals) {
  if (switch == TRUE) {
    d <- stats::density(densityVals)
    d <- base::data.frame(x = d$x, y = d$y)
    meanVal <- densityMeanValue(df = df,
                                GCC = GCC)
    meanHeight <- densityMeanHeight(meanValue = meanVal,
                                    densityVals = densityVals)
    return(densityCurve <- list(
      ggplot2::geom_line(
        data = d,
        ggplot2::aes(x, y),
        colour = colour
      ),
      densityMeanLine(
        x1 = meanVal,
        x2 = meanVal,
        y1 = 0,
        y2 = meanHeight,
        colour = colour
      )
    ))
  } else {
    return(NULL)
  }
}
densityMeanHeight <- function(meanValue,densityVals){
  d<-stats::density(densityVals)
  dd <- stats::approxfun(d$x, d$y)
  return(dd(meanValue))
  # meanValue <- meanValue
  #df <- stats::approxfun(stats::density(densityVals))
  # return(df(meanValue))
}
densityMeanValue <- function(df, GCC){
  return(base::as.numeric(base::lapply(df[df[["GCC"]] == GCC,]["PerAnyTree"], mean)))
}
densityMeanLine <- function(x1,y1,x2,y2,colour){
  ggplot2::geom_segment(
    ggplot2::aes(x = x1,
                 y = y1,
                 xend = x2,
                 yend = y2),
    size=0.5,
    lineend = "round",
    colour = colour,
    linetype="dashed"
  )
}

# TABLE

SSCrankTab <- function(df,id){
  base::paste0(
    "#",
    base::as.character(df[df[["SSC_CODE16"]] == id,]["rank"]),
    "/",
    max(df[['rank']])
  )
}

SSCnameTab <- function(df, id){
  return(base::as.character(df[df[["SSC_CODE16"]] == id,]["SSC_NAME16"]))
}

SSCtreeTab <- function(df, id){
  return(
    scales::percent(
      base::as.numeric(
        df[df[["SSC_CODE16"]] == id,]["PerAnyTree"]
      ),
      scale = 1,
      accuracy = 0.1
    )
  )
}