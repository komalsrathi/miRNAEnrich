\name{mirna.enrich}
\alias{mirna.enrich}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main function of miRNAEnrich package. Computes miRNA Enrichment in foreground vs. background.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mirna.enrich(mir.db = c("targetscandb", "microRNA"), org = c("mmu", "hsa"), fg, bg, col = "Symbol", plot = TRUE, cutoff)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mir.db}{
%%     ~~Describe \code{mir.db} here~~
}
  \item{org}{
%%     ~~Describe \code{org} here~~
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
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
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
function (mir.db = c("targetscandb", "microRNA"), org = c("mmu", 
    "hsa"), fg, bg, col = "Symbol", plot = TRUE, cutoff) 
{
    require("plyr")
    if (!((org == "mmu" | org == "hsa") & (mir.db == "targetscandb" | 
        mir.db == "microRNA"))) {
        stop("Allowed values for org = hsa or mmu\n Allowed values for mir.db = targetscandb or microRNA")
    }
    mir.db = get(paste(mir.db, org, sep = "."))
    if (is.na(cutoff)) {
        mir.db = mir.db
    }
    else if ("context..score" \%in\% colnames(mir.db)) {
        mir.db = mir.db[which(mir.db$context..score <= cutoff | 
            is.na(mir.db$context..score)), ]
    }
    else if ("mirsvr_score" \%in\% colnames(mir.db)) {
        mir.db = mir.db[which(mir.db$mirsvr_score <= cutoff | 
            is.na(mir.db$mirsvr_score)), ]
    }
    ftest.results = data.frame()
    ftest.results = rbind(ftest.results, ddply(.data = mir.db, 
        .variables = "miRNA", .fun = function(x) ftest.fun(x, 
            fg = fg, bg = bg, col = col)))
    ftest.results$pvalue.adj = p.adjust(p = ftest.results$pvalue, 
        method = "fdr")
    ftest.results = ftest.results[order(ftest.results$pvalue), 
        ]
    if (plot) {
        require("ggplot2")
        ftest.results.sub = ftest.results[which(ftest.results$pvalue.adj < 
            0.05), ]
        ftest.results.sub$miRNA = factor(ftest.results.sub$miRNA, 
            levels = rev(ftest.results.sub$miRNA))
        p <- ggplot(data = ftest.results.sub, aes(x = miRNA, 
            y = Foreground.Present, fill = pvalue.adj)) + geom_bar(stat = "identity") + 
            coord_flip() + theme(axis.text.y = element_text(color = "black", 
            size = 12), axis.text.x = element_text(color = "black", 
            size = 12), legend.text = element_text(color = "black", 
            size = 12), legend.title = element_text(color = "black", 
            size = 12), axis.title.x = element_text(color = "black", 
            size = 14), axis.title.y = element_text(color = "black", 
            size = 14), plot.title = element_text(color = "black", 
            size = 16)) + ggtitle("Enriched miRNA P.Adj<0.05\n") + 
            ylab("\nGenes in Foreground") + xlab("miRNA\n")
        print(p)
    }
    return(ftest.results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
