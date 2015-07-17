getFamily <-
function(results, mir.fam)
{
  results = merge(results,mir.fam[,c(1,4)],by.x = 'miRNA',by.y = 'miRBaseID',all.x = T)
  results = aggregate(miRNA~., results, toString)
  results$miRNA <- vapply(results$miRNA, paste, collapse = ", ", character(1L))
  return(results)
}
