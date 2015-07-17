getTargets <-
function(db,table,mir.fam)
{
  require('data.table')
  db = merge(db,mir.fam[,c(1,4)],by.x = 'miRNA',by.y='miRBaseID')
  db = as.data.table(db)
  var1 <- unique(as.character(db[miRNA %in% table$mir1,Gene.Symbol]))
  var2 <- unique(as.character(db[miRNA %in% table$mir2,Gene.Symbol]))
  overlap <- length(intersect(var1,var2))
  var1 <- length(var1)
  var2 <- length(var2)
  miRFamily.mir1 <- unique(as.character(db[miRNA %in% table$mir1,miRFamily]))
  miRFamily.mir2 <- unique(as.character(db[miRNA %in% table$mir2,miRFamily]))
  res <- data.frame(miRFamily.mir1 = miRFamily.mir1,
                    mir1.targets = var1,
                    miRFamily.mir2 = miRFamily.mir2,
                    mir2.targets = var2,
                    overlap = overlap, 
                    mir1.targets.perc = round(overlap/var1*100,2), 
                    mir2.targets.perc = round(overlap/var2*100,2))
  return(res)
}
