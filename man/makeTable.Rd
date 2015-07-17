\name{makeTable}
\alias{makeTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This makes a 2 column table of all possible pairs of miRNA
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeTable(db)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
%%     ~~Describe \code{db} here~~
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
function (db) 
{
    require("data.table")
    db = as.data.table(db)
    table = data.frame(mir1 = unique(db$miRNA), mir2 = unique(db$miRNA))
    table = expand.grid(mir1 = table$mir1, mir2 = table$mir2)
    table = as.data.table(table)
    return(table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
