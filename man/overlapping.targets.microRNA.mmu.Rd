\name{overlapping.targets.microRNA.mmu}
\alias{overlapping.targets.microRNA.mmu}
\docType{data}
\title{
Overlapping Targets in all possible miRNA pairs from microRNA.org.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("overlapping.targets.microRNA.mmu")}
\format{
  A data frame with 56644 observations on the following 9 variables.
  \describe{
    \item{\code{mir2}}{a character vector}
    \item{\code{mir1}}{a character vector}
    \item{\code{mir1.targets}}{a numeric vector}
    \item{\code{mir2.targets}}{a numeric vector}
    \item{\code{overlap}}{a numeric vector}
    \item{\code{mir1.targets.perc}}{a numeric vector}
    \item{\code{mir2.targets.perc}}{a numeric vector}
    \item{\code{miRFamily.mir1}}{a character vector}
    \item{\code{miRFamily.mir2}}{a character vector}
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
data(overlapping.targets.microRNA.mmu)
## maybe str(overlapping.targets.microRNA.mmu) ; plot(overlapping.targets.microRNA.mmu) ...
}
\keyword{datasets}
