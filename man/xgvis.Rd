\name{xgvis}
\alias{xgvis}
\title{XGvis: Interactive Multidimensional Scaling Using XGobi for Display}
\description{
  \R interface to \code{XGvis}, an interactive multidimensional scaling
  (MDS) program that consists of a control panel to manipulate the
  parameters of the MDS stress function and an \code{\link{xgobi}}
  window for data display.  It can be used either for visualization of
  dissimilarity data, for dimension reduction, or for graph layout.
  Graph layout is usually done in 2D, but \code{xgvis} allows layouts in
  arbitrary dimensions, 3D being the default.  It permits missing
  values, which can be used to implement multidimensional unfolding.
}
\usage{
xgvis(dmat = NULL,
      edges= NULL,
      pos  = NULL,
      rowlab = dimnames(dmat)[[1]],
      colors = NULL, glyphs = NULL,
      erase = NULL, lines = NULL, linecolors = NULL,
      resources = NULL, display = NULL,
      keep = FALSE, fprefix = "xgvis-")
)
}
\arguments{
  \item{dmat}{numeric \eqn{n \times n}{n * n} distance matrix.}
  \item{edges}{\eqn{n \times 2}{n * 2} or \eqn{n \times 3}{n * 3} matrix of
    specifications for the pattern of line segments which
    connect pairs of points.  Must contain at least two numbers per
    line.  The first two numbers represent the row numbers of the two
    points that should be connected.  (This is exactly like the structure
    of a the \code{lines} argument of \code{\link{xgobi}}.)  In addition,
    if a third number is present, it is taken to be an edge weight.

    If \code{edges} is specified and \code{dmat} not, then
    the distance matrix is computed from \code{edges}, with each
    edge representing a distance of one.
  }
  \item{pos}{
    Starting positions: an \code{n * p} matrix.
    If \code{pos} is specified and \code{dmat} not,
    the distance matrix is computed from \code{pos}.}
  \item{rowlab}{
    character vector of \code{n} row labels (defaulting to those of
    \code{dmat}); if no default exists, \code{\link{xgobi}} constructs
    its own (numbers \code{1:n}).}
  \item{colors}{optional character vector supplying initial point colors
    to be used; see \code{\link{xgobi}}.}
  \item{glyphs}{integer vector, used to supply glyphs to be used on
    startup, see \code{\link{xgobi}}.}
  }
  \item{erase}{
    Optional integer vector of length equal to the number of rows in the
    data and composed of 1s and 0s.  A 1 in position i specifies that
    point i should be erased.  The default is a vector of 0s.
  }
  \item{lines}{
    Optional integer matrix, n by 2, which specifies by row number
    pairs of points to be connected by line segments.
    If \code{lines} are specified, then the \code{edges} is used to create
    the distance matrix but \code{lines} is used to draw the edges.
  }
  \item{linecolors}{
    Optional integer vector, of length n where n is the number of
    lines specified by the \code{lines} argument.  It is used to supply
    line colors to be used on startup; the default is for all the
    lines to be drawn in the standard foreground color.
  }
  \item{resources}{
    Optional character vector created by clicking on the ``Save
    Resources'' button in XGobi.
    %(if this XGvis was initiated during an R session).
  }
  \item{display}{
    Optional character string, identifying the monitor on which to display
    the xgvis window.  The default is \code{"machine:0.0"} where
    \code{machine} is the name of the user's workstation.  See
    documentation for X.
  }
  \item{keep}{logical, indicating if the temporary files should be kept
    (e.g. for calling the \code{xgobi} program outside \R)}
  \item{fprefix}{character string for the \bold{f}ile name \bold{prefix}
    to be used for temporary files.}
}
\value{
  The UNIX \code{status} upon completion, i.e. \code{0} if ok.
}
\section{Side Effects}{
  The xgvis \R function executes a call to the C program of the same name,
  and returns control of the \R command line to the user.
}
\references{
  \url{http://www.research.att.com/areas/stat/xgobi/}, \cr
  \url{http://www.public.iastate.edu/~dicook/}
}
\author{of \R port: Kurt Hornik and
  Martin Maechler \email{maechler@stat.math.ethz.ch}}
\section{CONTACT}{
  D. F. Swayne \email{dfs@research.att.com}
}
\seealso{\code{\link{xgobi}}.}
\examples{
data(morsecodes) ## from the XGobi/XGvis data, see  ?morsecodes
mc.row <- paste(morsecodes.row[,1],morsecodes.row[,2])

xgvis(dmat = morsecodes.dist,
      pos = morsecodes.pos,
      rowlab = mc.row,
      colors = morsecodes.colors,
      glyphs = morsecodes.glyphs,
      lines = morsecodes.lines,
      linecolors = morsecodes.linecolors)

##>   2) Show lines by hitting "l" with the mouse over the plot.
##>   3) Examine morsecode labels by hitting "i" and mousing around on the plot.
##>   3b) Press "r" (on the plot) to switch 3D rotation in xgobi.
##>   4) Run MDS in 3D by clicking "Run MDS" (in xgvis).
##>   5) Speed up the optimization by increasing the "Stepsize" with the slider.
##>      The "Stress function" value may go as low as 0.1925 (MM).
##>   6) When the optimization calms down, click "Run MDS" to toggle MDS off.
##>   7) Rotate the MDS configuration in 3D {by "r" with mouse over plot}.
##>   8) Increase the rotation speed with the slider in the top left and
##>      control the rotation direction by dragging the mouse on the plot.
##>   9) You can check out the initial configuration by

## In order to have no color warning :
Mcolors <- unique(morsecodes.colors)
(Mcolors <- paste("*brushColor", 0:(length(Mcolors)-1),": ", Mcolors, sep=""))

xgobi(morsecodes.pos, collab = morsecodes.col, rowlab = mc.row,
      colors = morsecodes.colors,
      glyphs = morsecodes.glyphs,
      lines  = morsecodes.lines,
      linecolors = morsecodes.linecolors,
      resources= c("*showLines: True", Mcolors))

##>      This XGobi window will be linked with
##>      the XGvis window for glyph-color brushing and labeling.
}
\keyword{dynamic}
\keyword{multivariate}

