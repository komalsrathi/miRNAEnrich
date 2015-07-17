getOverlappingTargets <-
function(processors, db, mir.fam){
  require('doMC')
  require('plyr')
  require('data.table')
  registerDoMC(cores = processors)
  
  # call makeTable
  table = makeTable(db = db)
  
  # call getTargets
  res = ddply(.data = table,
              .variables = .(mir1,mir2),
              .fun = function(x) getTargets(table = x, db = db, mir.fam = mir.fam),
              .parallel = T)
  
  return(res)
}
