\name{morsecodes}
\alias{morsecodes}
\alias{morsecodes.colors}
\alias{morsecodes.dist}
\alias{morsecodes.glyphs}
\alias{morsecodes.linecolors}
\alias{morsecodes.lines}
\alias{morsecodes.pos}
\alias{morsecodes.row}
\title{Rothkopf Morse Code Data}
\description{
  A standard data set for Multidimensional Scaling (MDS) obtained by
  Rothkopf based on the confusion rates observed by exposing subjects to
  pairs of morse codes.
}
\usage{data(morsecodes)}
\format{
  8 data sets used for analyzing the data in XGvis and XGobi.

  \tabular{ll}{
    \code{morsecodes.dist} \tab dissimilarity matrix \cr
    \code{morsecodes.dat} \tab initial configuration \cr
    \code{morsecodes.colors} \tab point colors \cr
    \code{morsecodes.glyphs} \tab point glyphs \cr
    \code{morsecodes.lines} \tab lines \cr
    \code{morsecodes.linecolors} \tab line colors \cr
    \code{morsecodes.row} \tab labels of the form \code{"A .-"} \cr
    \code{morsecodes.col} \tab column names of the initial configuration
  }
}
\source{
  Contained in the \file{data\_xgvis} subdirectory of the XGobi and XGvis
  source bundle, available via
  \url{http://www.research.att.com/areas/stat/xgobi/index.html#download}.
}
\references{
  A. Buja, D. F. Swayne, M. Littman, & N. Dean (1998). 
  \emph{XGvis: Interactive Data Visualization with Multidimensional
    Scaling}.
  \url{http://www.research.att.com/areas/stat/xgobi/xgvis98.ps.gz}.
}
\keyword{datasets}
