\name{morsecodes}
\alias{morsecodes}
\alias{morsecodes.col}
\alias{morsecodes.colors}
\alias{morsecodes.dist}
\alias{morsecodes.glyphs}
\alias{morsecodes.linecolors}
\alias{morsecodes.lines}
\alias{morsecodes.pos}
\alias{morsecodes.raw}
\alias{morsecodes.row}
\title{Rothkopf Morse Code Data}
\description{
  A standard data set for Multidimensional Scaling (MDS) obtained by
  Rothkopf based on the confusion rates observed by exposing subjects to
  pairs of morse codes.
}
\usage{data(morsecodes)}
\format{
  9 data sets used for analyzing the data in XGvis and XGobi.

  \tabular{ll}{
    \code{morsecodes.raw}    \tab 36 x 36 raw data of confusion rates \cr
    \code{morsecodes.dist}   \tab 36 x 36 dissimilarity matrix \cr
    \code{morsecodes.pos}    \tab 36 x 10 initial configuration \cr
    \code{morsecodes.colors} \tab 36 point colors \cr
    \code{morsecodes.glyphs} \tab 36 point glyphs \cr
    \code{morsecodes.lines}      \tab 33 lines \cr
    \code{morsecodes.linecolors} \tab 33 line colors \cr
    \code{morsecodes.row}   \tab 36 x 2 matrix of (letter, morsecode)\cr
    \code{morsecodes.col}   \tab 10 column names of the initial configuration
%% the *.raw code contain more information and the *.dist can be constructed
%% from them ... --> see /usr/local/app/xgobi/CURRENT/data_xgvis/morsecodes.doc
%%---> Copy the instructions there to the example of xgvis or here !
  }
}
\source{
  Contained in the \file{data\_xgvis} subdirectory of the XGobi and XGvis
  source bundle, available via
  \url{http://www.research.att.com/areas/stat/xgobi/index.html#download}.
}
\details{
  The raw data from the XGvis directory may be read as
  \cr
  \code{mc.raw <- as.matrix(read.table("....xgobi/data_xgvis/morsecodes.raw"))}
  \cr
  \code{dimnames(mc.raw) <- NULL; storage.mode(mc.raw) <- "integer"}
  \cr
  \code{morsecodes.raw <- mc.raw}.

  The \code{*.dist} matrix is produced from the raw data by
  \cr
  \code{mc.sim <- (mc.raw + t(mc.raw))/2 ;  ds <- diag(mc.sim)}
  \cr
  \code{morsecodes.dist <- rep(ds,36) + rep(ds,rep(36,36)) - 2*mc.sim},
  \cr
  i.e., \eqn{d_{ij} := s_{ii} + s_{jj} - 2 s_{ij}}{d[i,j] := s[i,i] +
    s[j,j] - 2 * s[i,j]}.
}
\references{
  A. Buja, D. F. Swayne, M. Littman, & N. Dean (1998).
  \emph{XGvis: Interactive Data Visualization with Multidimensional
    Scaling}.
  \url{http://www.research.att.com/areas/stat/xgobi/xgvis98.ps.gz}.
}
\examples{
data(morsecodes)
ls.str(pat="^morsecode")
morsecodes.row # remember what you learned in...
## real row names
(mc.row <- paste(morsecodes.row[,1], morsecodes.row[,2]))[1:8]

image(1:36, 1:36, morsecodes.raw, main="`morsecodes' raw confusion rates")
text(1:36,1:36, morsecodes.row[,1])

##--> help(xgvis) for running multidimensional scaling (MDS) and XGobi on these
}
\keyword{datasets}
