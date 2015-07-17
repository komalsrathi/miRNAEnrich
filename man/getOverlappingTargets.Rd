\name{getOverlappingTargets}
\alias{getOverlappingTargets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get proportions of overlapping targets between all possible miRNA combinations.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getOverlappingTargets(processors, db, mir.fam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{processors}{
%%     ~~Describe \code{processors} here~~
}
  \item{db}{
%%     ~~Describe \code{db} here~~
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
function (processors, db, mir.fam) 
{
    require("doMC")
    require("plyr")
    require("data.table")
    registerDoMC(cores = processors)
    table = makeTable(db = db)
    res = ddply(.data = table, .variables = .(mir1, mir2), .fun = function(x) getTargets(table = x, 
        db = db, mir.fam = mir.fam), .parallel = T)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
