\name{getFamily}
\alias{getFamily}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the miRNA Family and add to the miRNA Enrichment Results 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getFamily(results, mir.fam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{mir.fam}{
%%     ~~Describe \code{mir.fam} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (results, mir.fam) 
{
    results = merge(results, mir.fam[, c(1, 4)], by.x = "miRNA", 
        by.y = "miRBaseID", all.x = T)
    results = aggregate(miRNA ~ ., results, toString)
    results$miRNA <- vapply(results$miRNA, paste, collapse = ", ", 
        character(1L))
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
