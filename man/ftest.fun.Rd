\name{ftest.fun}
\alias{ftest.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fisher Exact Test for miRNA Enrichment
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ftest.fun(dat = x, fg = fg, bg = bg, col = col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{fg}{
%%     ~~Describe \code{fg} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
function (dat = x, fg = fg, bg = bg, col = col) 
{
    fg.total = nrow(fg)
    expr.total = nrow(bg) - nrow(fg)
    targets = unique(as.character(dat$Gene.Symbol))
    fg.yes = length(intersect(unique(as.character(fg[, col])), 
        targets))
    expr.yes = length(intersect(unique(as.character(bg[, col])), 
        targets))
    expr.yes = expr.yes - fg.yes
    fg.no = fg.total - fg.yes
    expr.no = expr.total - expr.yes
    mat = matrix(c(fg.yes, expr.yes, fg.no, expr.no), nrow = 2, 
        dimnames = list(c("Downreg", "Expressed"), c("present", 
            "absent")))
    ftest = fisher.test(mat, alternative = "greater")
    res = data.frame(Foreground.Present = fg.yes, Foreground.Absent = fg.no, 
        Foreground.Total = fg.total, Background.Present = expr.yes, 
        Background.Absent = expr.no, Background.Total = expr.total, 
        estimate = ftest$estimate, pvalue = ftest$p.value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
