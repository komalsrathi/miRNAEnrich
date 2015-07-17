\name{miRNAEnrich-package}
\alias{miRNAEnrich-package}
\alias{miRNAEnrich}
\docType{package}
\title{
Enrichment of miRNA
}
\description{In a given microarray experiment, this package computes a fisher-based enrichment of miRNA(s) depending on the enrichment of its target genes in the gene-set of interest (upregulation/downregulation) compared to background-set (invariably the “Expressed Genes”). It utilizes the TargetScan and microRNA datatbases for miRNA-target relationship.}
\details{
\tabular{ll}{
Package: \tab miRNAEnrich\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-10\cr
License: \tab GPL-3\cr
}
Run the function mirna.enrich as given in the example.
}
\author{
Komal Rathi
Maintainer: Komal Rathi <komalr@mail.med.upenn.edu>
}
\references{}
\keyword{ miRNAEnrich }
\seealso{
}
\examples{
foreground <- background[which(background$fc < 0 & background$adj.P.Val<0.05),] #downregulated genes
res.microrna <- mirna.enrich(mir.db = 'microRNA', 
                             org = 'mmu', 
                             fg = foreground, 
                             bg = background, 
                             col = 'Symbol',
                             plot = TRUE,
                             cutoff = -0.1)
}
