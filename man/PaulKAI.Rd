\name{PaulKAI}
\alias{PaulKAI}
\title{Occurences of ``kai'' in 10 Epistels of Apostel Paul}
\usage{data(PaulKAI)}
\description{
  These are the numbers of sentences which have
  0, 1, 2, or 3 and more (\code{3+}) occurrences of the greek word
  ``kai'' (which means ``and'' and more) in 10 epistles of Apostel Paul,
  see the reference.
}
\format{
  A \eqn{10 \times 4}{10 x 4} matrix with proper \code{\link{dimnames}},
  see the examples below.
}
\note{
  One theological question is about the authorship of ``Hebrews'' (the
  last epistle in our matrix), so one might be interested in its ``kai
  pattern'' compared to, e.g. ``Romans''.
}
\references{
  Morton, A. Q. (1965) \cr
  The authorship of Greek prose (with discussion).\cr
  Journal of the Royal Statistical Society, Series A, \bold{128}, 169--233.

  Posted to S-news by Jim Ramsay, see \code{\link{quadplot}}.
}
\keyword{datasets}
\seealso{\code{\link{quadplot}} for which this data set was used as
  illustration.}
\examples{
data(PaulKAI)
rownames(PaulKAI) # the ten epistles researched:
##>  [1] "Rom" "Co1" "Co2" "Gal" "Phi" "Col" "Th1" "Ti1" "Ti2" "Heb"
PaulKAI # the 10 x 4 count table
mosaicplot(PaulKAI)
quadplot(PaulKAI)
}
