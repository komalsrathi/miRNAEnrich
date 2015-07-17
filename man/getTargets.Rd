\name{getTargets}
\alias{getTargets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function is used by getOverlappingTargets function to get overlapping targets.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getTargets(db, table, mir.fam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
%%     ~~Describe \code{db} here~~
}
  \item{table}{
%%     ~~Describe \code{table} here~~
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
function (db, table, mir.fam) 
{
    require("data.table")
    db = merge(db, mir.fam[, c(1, 4)], by.x = "miRNA", by.y = "miRBaseID")
    db = as.data.table(db)
    var1 <- unique(as.character(db[miRNA \%in\% table$mir1, Gene.Symbol]))
    var2 <- unique(as.character(db[miRNA \%in\% table$mir2, Gene.Symbol]))
    overlap <- length(intersect(var1, var2))
    var1 <- length(var1)
    var2 <- length(var2)
    miRFamily.mir1 <- unique(as.character(db[miRNA \%in\% table$mir1, 
        miRFamily]))
    miRFamily.mir2 <- unique(as.character(db[miRNA \%in\% table$mir2, 
        miRFamily]))
    res <- data.frame(miRFamily.mir1 = miRFamily.mir1, mir1.targets = var1, 
        miRFamily.mir2 = miRFamily.mir2, mir2.targets = var2, 
        overlap = overlap, mir1.targets.perc = round(overlap/var1 * 
            100, 2), mir2.targets.perc = round(overlap/var2 * 
            100, 2))
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
