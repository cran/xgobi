\name{reggeom}
\alias{reggeom}

\title{Geometry of Regression with Two Regressors}

\description{
  Using XGobi for visualising the geometry of regression with two
  explanatory variables.

  The function \code{reggeom} has exactly the same arguments as
  \code{\link{xgobi}(..)}, and it simply calls \code{xgobi}, but it has
  different default values for the arguments than the defaults of
  \code{xgobi} itself.
}
\usage{
reggeom()
}
\synopsis
reggeom(matrx = matrix(c(0, 5780, -1156, 3468, 3468, 3468,
	    -867, 4335, 0, 0, -612, 4080, 5440, 2652, 3468, 3420, 3468,
	    0, 0, 4624, 3468, 3468, 0, 3468, 0, 3468, 4624, 2448, 1020,
	    1360, 3264, 3264, 3456, 3456, 0, 0, 0, 4624, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 17, ncol = 3),
    collab = c("U", "V", "W"),
    rowlab = c("o", "x1", "x2", "y",
               "b", "c", "d", "e", "f", "g", "h",
               "k", "m", "p", "q", "r", "s"),
    colors = NULL, glyphs = NULL, erase = NULL,
    lines = matrix(c(1,  6, 8, 1, 11, 7, 1, 1, 5, 6,
                       6, 15, 17, 8, 5, 9, 1, 9, 10,
                     6, 8, 2, 11, 7, 3, 4, 5, 4, 4,
                       15, 17, 5, 5, 9, 7, 9, 10, 3),
                    nrow = 19, ncol = 2),
    linecolors = c("red", "yellow", "yellow", "yellow", "yellow",
        "yellow", "orchid", "green", "green", "red", "skyblue",
        "skyblue", "skyblue", "white", "white", "white", "slateblue",
        "slateblue", "slateblue"),
    resources = c("*showLines: True", "*showAxes: False", "*showPoints: False",
        "*XGobi*PlotWindow.height: 500",
        "*XGobi*PlotWindow.width: 500", "*XGobi*VarPanel.width: 50"),
    title = "Regression Geometry", vgroups = c(1, 1, 1), std = "msd",
    nlinkable = NULL, subset = NULL, display = NULL)
}
\arguments{
  \item{matrx}{
    the default dataset is a matrix with three columns.
    The rows represent the dependent and the two independent variables,
    as well as fitted values and residuals in the regression on one
    or both regressors, and other auxiliary variables.
    Since the matrix has three columns, each variable is
    represented as a vector in 3-dimensional space.
  }
  \item{collab}{
    column labels for \code{matrx}, by default \code{"U"}, \code{"V"}, and
    \code{"W"}, not very meaningful since the columns represent
    oblique directions in n-dimensional space.
  }
  \item{rowlab}{character vector of labels for the variables;
    by default, "x1" and "x2" for the independent and "y" for the
    dependent variable, "o" for the origin, and other letters for the
    auxiliary variables.
  }
  \item{colors}{as in \code{xgobi} all points are of the same color.}
  \item{glyphs}{as in \code{xgobi} all points are drawn with the same glyph.  }
  \item{erase}{as in \code{xgobi} no points will be erased.}
  \item{lines}{the default \code{lines} argument displays some of the data in
    \code{matrx} as straight lines.  The user may want to substitute
    different lines in order to emphasize or de-emphasize certain
    relationships, as in the example given below.
  }
  \item{linecolors}{
    The default line colors are:
    \describe{
      \item{purple}{for the dependent variable,}
      \item{yellow}{for the two independent variables,}
      \item{green}{for fitted values and residuals in the full regression,}
      \item{red}{for fitted values and residuals in the regression
	on the first independent variable only, and}
      \item{light blue}{,}
      \item{dark blue}{, and}
      \item{white}{for auxiliary lines.}
    }
  }
  \item{resources}{by default, points and axes are not shown; only lines
    are.}
  \item{title}{by default, \code{"Regression Geometry"}}
  \item{vgroups}{by default, all three variables are in the same group.}
  \item{std}{by default, the view is centered on the mean of the data.}
  \item{nlinkable, subset, display}{the same as in \code{\link{xgobi}}.}
}
\value{
  As in the call of \code{\link{xgobi}},
  the UNIX \code{status} upon completion, i.e. \code{0} if ok.
}
\details{
  If called without arguments, \code{reggeom} loads a dataset which
  represents the geometry of regression with two explanatory variables.
  The idea is to place the dataset into the rotation view in order to
  get an intuition of the geometry involved.  \code{reggeom} should only
  then be called with arguments if specific built-in defaults must be
  overriden.

  The explanatory variables are x1=(5,0,0) and x2=(-1,4,0),
  and the target (dependent) variable is y=(3,3,4).
  However all coordinates are multiplied by 1156,
  with the effect that all the points passed as arguments to
  \code{xgobi} have integer coordinates.
}
\seealso{\code{\link{xgobi}}
}
\section{Side Effects}{
    As in \code{xgobi}.
}
\references{
  \code{reggeom} can be considered a 3-dimensional visualization
  of the figures in Davidson, R. and MacKinnon, J. G. (1993)
  \emph{Estimation and Inference in Economics}, Oxford University
  Press, p. 22.

  The chapter ``Additional Regressors'' in Hans Ehrbar's on-line
  econometrics class notes
  \url{http://www.econ.utah.edu/ehrbar/ecmet.pdf}
  uses \code{reggeom} for teaching and has several exercise
  questions about it.
}
\section{author}{Hans Ehrbar \email{ehrbar@econ.utah.edu}}
\examples{
## The arguments given in this example are modifications of the default,
## some lines dropped, some added, some line colors changed,
## in order to emphasize the geometry of backfitting.

reggeom(
      lines= cbind(c(1,6,8,1,11,7,1,1,6,6,15,17,8,5,9, 5,6,14,15,16,14,15,5),
                   c(6,8,2,11,7,3,4,5,4,15,17,5,5,9,7,11,14,15,16,17,4,4,4)),
      linecolors=c("red", rep("yellow",5), "orchid", "green",
                   "slateblue", rep("skyblue",3), rep("white",3), "skyblue",
                   rep("red",4), rep("slateblue", 2), "green"),
      title="Regression Geometry - Backfitting")
}
\keyword{dynamic}

