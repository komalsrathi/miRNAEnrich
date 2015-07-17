mirna.enrich <-
function(mir.db = c('targetscandb','microRNA'), org = c('mmu','hsa'), fg, bg, col = 'Symbol', plot = TRUE, cutoff){
    require('plyr')
    # check if input parameters are within limits
    if(!((org=='mmu' | org=='hsa') & (mir.db=='targetscandb' | mir.db=='microRNA')))
    {
      stop("Allowed values for org = hsa or mmu\n Allowed values for mir.db = targetscandb or microRNA")
    }
    
    mir.db = get(paste(mir.db,org,sep='.'))
    
    # limit dataset according to the cutoff
    if(is.na(cutoff))
    {
      mir.db = mir.db
    } else if("context..score" %in% colnames(mir.db)){
      mir.db = mir.db[which(mir.db$context..score <= cutoff | is.na(mir.db$context..score)),]
    } else if("mirsvr_score" %in% colnames(mir.db)){
      mir.db = mir.db[which(mir.db$mirsvr_score <= cutoff | is.na(mir.db$mirsvr_score)),]
    }
    
    # fisher test for miRNA enrichment
    ftest.results = data.frame()
    ftest.results = rbind(ftest.results,
                          ddply(.data = mir.db,
                                .variables = "miRNA",
                                .fun = function(x) ftest.fun(x, fg = fg, bg = bg, col = col)))
    
    # pvalue adjustment & order by pvalue
    ftest.results$pvalue.adj = p.adjust(p = ftest.results$pvalue, method = 'fdr') 
    ftest.results = ftest.results[order(ftest.results$pvalue),] 
    
    # plot the top significant results (p.adj<0.05)
    if(plot)
    {
      require('ggplot2')
      ftest.results.sub = ftest.results[which(ftest.results$pvalue.adj < 0.05),]
      ftest.results.sub$miRNA = factor(ftest.results.sub$miRNA,levels = rev(ftest.results.sub$miRNA))
      p <- ggplot(data = ftest.results.sub, aes(x=miRNA, y=Foreground.Present, fill=pvalue.adj)) + 
        geom_bar(stat='identity') + 
        coord_flip() + 
        theme(axis.text.y=element_text(color='black',size=12),
              axis.text.x=element_text(color='black',size=12),
              legend.text=element_text(color='black',size=12),
              legend.title=element_text(color='black',size=12),
              axis.title.x=element_text(color='black',size=14),
              axis.title.y=element_text(color='black',size=14),
              plot.title=element_text(color='black',size=16)) + 
        ggtitle('Enriched miRNA P.Adj<0.05\n') + 
        ylab('\nGenes in Foreground') + xlab('miRNA\n')
      print(p)
    }
    
    # return fisher test results
    return(ftest.results)
  }
