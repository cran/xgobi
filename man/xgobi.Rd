\name{xgobi}
\alias{xgobi}
\title{
XGobi: Dynamic Graphics for Data Analysis
}
\description{
Dynamic graphics, including  brushing, rotation, grand tour, projection
pursuit, slicing.  Most effectively used when called more than once on same
data, which then allows linked plots.  Brushing with several glyphs and
colors is supported.  (On monochrome displays, only glyphs can be used.)
}
\usage{
xgobi(matrx,
      collab = dimnames(matrx)[[2]],
      rowlab = dimnames(matrx)[[1]],
      colors = NULL, glyphs = NULL, erase = NULL,
      lines = NULL, linecolors = NULL,
      resources = NULL,
      title = deparse(substitute(matrx)),
      vgroups = NULL, std = "mmx",
      nlinkable = NULL, subset = NULL,
      display = NULL)
}
\arguments{
  \item{matrx}{numeric \code{n * p} matrix or data.frame.}
  \item{collab}{
    character vector of \code{p} column labels (defaulting to those of
    \code{matrx}); if no default exists, \code{\link{xgobi}} constructs
    its own (\code{"Var1"},....).}
  \item{rowlab}{
    character vector of \code{n} row labels (defaulting to those of
    \code{matrx}); if no default exists, \code{\link{xgobi}} constructs
    its own (numbers \code{1:n}).}
  \item{colors}{
    Optional character vector, used to supply initial point colors to be used;
    the default is that all points are the same color.}
  \item{glyphs}{
    Optional integer vector, used to supply glyphs to be used on
    startup; the default is that all points are drawn with the same glyph.}
  \item{erase}{
    Optional integer vector of length equal to the number of rows in the
    data and composed of 1s and 0s.  A 1 in position i specifies that
    point i should be erased.  The default is a vector of 0s.
  }
  \item{lines}{
    Optional integer matrix, n by 2, which specifies by row number
    pairs of points to be connected by line segments.  The default
    connecting line matrix connects each point to the one that follows
    it in the data; that is, (1 2), (2 3), (3 4), \dots{}, (n-1, n).
  }
  \item{linecolors}{
    Optional integer vector, of length n where n is the number of
    lines specified by the 'lines' argument.  It is used to supply
    line colors to be used on startup; the default is for all the
    lines to be drawn in the standard foreground color.
  }
  \item{resources}{
    Optional character vector created by clicking on the ``Save
    Resources'' button in XGobi (if this XGobi was initiated during
    an \R session).
  }
  \item{title}{
    Optional character string which defines the \code{-title} argument used by
    X. Defaults to the name (expression) of the current \code{matrx}
    argument.  See documentation for xgobi, or for X.
  }
  \item{vgroups}{
    Optional integer vector, used to assign columns to groups for
    transformation and axis scaling.  This vector must contain one integer
    for each variable.  Columns to be grouped together should share the
    same integer.  Default is the vector \code{1:(ncol(matrx))}.
  }
  \item{std}{
    Optional string; which standardization of view to use.  Default is
    \code{"mmx"}, minimum-maximum scaling, in which the view is centered
    at the midpoint of the data, and all the data fits inside the plotting
    window.  Alternatives are \code{"msd"}, in which the plot is
    centered at the mean of the data, or \code{"mmd"} in which the plot
    is centered at the median.
    In those two cases, the view is standardized using the largest distance.
  }
% \item{dev}{
%   Optional numeric scalar; the number of standard deviations (if \code{"msd"}
%   is chosen) or median absolute deviations (if \code{"mmd"} is chosen) that
%   will be plotted inside the plotting window.  Default is 2.
% }
  \item{nlinkable}{
    Optional integer scalar, the number of rows to be used in linking
    of brushing and identification;
    the default is for all rows to be used.  This feature can be used
    to link ordinary scatterplots with plots that have some decorations
    requiring additional points, such as clustering trees.
  }
  \item{subset}{
    Optional integer scalar, the number of rows to be included in the
    initial display.  That is, all data will be read in, but an
    initial random sample will be drawn for display.  Use the Subset
    panel on the Tools Menu to select a new subset during the session.
  }
  \item{display}{
    Optional character string, identifying the monitor on which to display
    the xgobi window.  The default is \code{"machine:0.0"} where
    \code{machine} is the name of the user's workstation.  See
    documentation for xgobi or for X.}
}
\value{
  The UNIX \code{status} upon completion, i.e. \code{0} if ok.
}
\section{Side Effects}{
  The \R function \code{xgobi} executes a call to the C program of the
  same name,
  an interactive statistical graphics program which runs under the X
  Window System, and returns control of the \R command line to the user.

  XGobi can be used to create vectors of brushing information and rotation
  coefficients; see the documentation for XGobi for details.
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
\seealso{\code{\link{xgvis}} which uses \code{xgobi} for interactive MDS.}
\examples{
data(laser)
xgobi(laser)

## see also help(xgvis)
}
\keyword{dynamic}
\keyword{multivariate}

