\name{addmiRNAToLimma}
\alias{addmiRNAToLimma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
adds miRNA information to limma output
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addmiRNAToLimma(foreground, db)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{foreground}{
%%     ~~Describe \code{foreground} here~~
}
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
function (foreground, db) 
{
    foreground = merge(foreground, db[, c(2, 5)], by.x = "Symbol", 
        by.y = "Gene.Symbol", all.x = TRUE)
    foreground = aggregate(miRNA ~ ., foreground, toString)
    foreground$miRNA <- vapply(foreground$miRNA, paste, collapse = ", ", 
        character(1L))
    return(foreground)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
