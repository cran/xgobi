\name{quadplot}
\title{Tetrahedral Display for Four-Category Proportions using XGobi}
\alias{quadplot}
\description{
  Four-category proportions are visualized as points inside a tetrahedron,
  using \code{\link{xgobi}}.
}
\details{
  The set of all four-category proportions, or, alternatively,
  probability measures on finite probability fields with 4 atomic
  events, is the set of nonnegative 4-vectors whose components sum up to
  1.  The function \code{quadplot} uses \code{xgobi} to represent such
  vectors graphically as points inside a tetrahetron with height 1: the
  four components of the vector are the distances of the point to each
  of the sides of the tetrahetron.  Each vertex of the tetrahedron
  corresponds to the degenerate probability distribution in which one of
  the atomic events has probability 1 and the others have probability 0.
  The labels of these vertices indicate the event which has probability 1.
}
\usage{
quadplot(mat4,
         pointlabs  = rownames(mat4),
         vertexlabs = paste(1:4),
         normalize  = median(abs(c(mat4))) > 1)
}
\arguments{
  \item{mat4}{matrix with 4 columns containing the data}
  \item{pointlabs}{character array of labels for rows of data;
    by default it is the row number as a string.}
  \item{vertexlabs}{character array of length 4 of labels for the vertices;
    by default it is the column number as a string.}
  \item{normalize}{logical variable indicating whether or not to force
    each row of data to have unit sum before display, default is false.}
}
\references{
  \code{quadplot} was posted by Jim Ramsay \email{ramsay@psych.mcgill.ca}
  to S-news on Fri, 21 May 1993 14:03:15 EDT.
}
\author{(port to \R)
  Hans Ehrbar \email{ehrbar@econ.utah.edu} and Martin Maechler
  (with explicit permission from Jim Ramsay)
}
\examples{
data(PaulKAI)
quadplot(PaulKAI, normalize = TRUE)
}
\keyword{dynamic}
\keyword{hplot}

