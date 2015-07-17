makeTable <-
function(db)
{
  require('data.table')
  db = as.data.table(db)
  table = data.frame(mir1 = unique(db$miRNA), mir2 = unique(db$miRNA))
  table = expand.grid(mir1 = table$mir1, mir2 = table$mir2)
  table = as.data.table(table)
  return(table)
}
