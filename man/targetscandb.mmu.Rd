\name{targetscandb.mmu}
\alias{targetscandb.mmu}
\docType{data}
\title{
miRNA-Target information from Targetscandb for Mouse
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("targetscandb.mmu")}
\format{
  A data frame with 301347 observations on the following 15 variables.
  \describe{
    \item{\code{Gene.ID}}{a numeric vector}
    \item{\code{Gene.Symbol}}{a character vector}
    \item{\code{Transcript.ID}}{a character vector}
    \item{\code{Gene.Tax.ID}}{a numeric vector}
    \item{\code{miRNA}}{a character vector}
    \item{\code{Site.Type}}{a numeric vector}
    \item{\code{UTR_start}}{a numeric vector}
    \item{\code{UTR.end}}{a numeric vector}
    \item{\code{X3prime.pairing}}{a character vector}
    \item{\code{local.AU}}{a character vector}
    \item{\code{position}}{a character vector}
    \item{\code{TA}}{a character vector}
    \item{\code{SPS}}{a character vector}
    \item{\code{context..score}}{a numeric vector}
    \item{\code{context..score.percentile}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(targetscandb.mmu)
## maybe str(targetscandb.mmu) ; plot(targetscandb.mmu) ...
}
\keyword{datasets}
