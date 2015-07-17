addmiRNAToLimma <-
function(foreground, db)
{
  foreground = merge(foreground,db[,c(2,5)],by.x='Symbol',by.y='Gene.Symbol',all.x = TRUE)
  foreground = aggregate(miRNA~., foreground, toString)
  foreground$miRNA <- vapply(foreground$miRNA, paste, collapse = ", ", character(1L))
  return(foreground)
}
