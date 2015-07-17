\name{microRNA.hsa}
\alias{microRNA.hsa}
\docType{data}
\title{
microRNA Human Database for miRNA-targets
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("microRNA.hsa")}
\format{
  A data frame with 1097064 observations on the following 19 variables.
  \describe{
    \item{\code{X.mirbase_acc}}{a character vector}
    \item{\code{miRNA}}{a character vector}
    \item{\code{gene_id}}{a numeric vector}
    \item{\code{Gene.Symbol}}{a character vector}
    \item{\code{transcript_id}}{a character vector}
    \item{\code{ext_transcript_id}}{a character vector}
    \item{\code{mirna_alignment}}{a character vector}
    \item{\code{alignment}}{a character vector}
    \item{\code{gene_alignment}}{a character vector}
    \item{\code{mirna_start}}{a numeric vector}
    \item{\code{mirna_end}}{a numeric vector}
    \item{\code{gene_start}}{a numeric vector}
    \item{\code{gene_end}}{a numeric vector}
    \item{\code{genome_coordinates}}{a character vector}
    \item{\code{conservation}}{a numeric vector}
    \item{\code{align_score}}{a numeric vector}
    \item{\code{seed_cat}}{a numeric vector}
    \item{\code{energy}}{a numeric vector}
    \item{\code{mirsvr_score}}{a numeric vector}
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
data(microRNA.hsa)
## maybe str(microRNA.hsa) ; plot(microRNA.hsa) ...
}
\keyword{datasets}
