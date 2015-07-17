ftest.fun <-
function (dat = x, fg = fg, bg = bg, col = col) 
{
  fg.total = nrow(fg)
  expr.total = nrow(bg)-nrow(fg)
  targets = unique(as.character(dat$Gene.Symbol))
  fg.yes = length(intersect(unique(as.character(fg[, col])), 
                            targets))
  expr.yes = length(intersect(unique(as.character(bg[, col])), 
                              targets))
  expr.yes = expr.yes-fg.yes
  fg.no = fg.total - fg.yes
  expr.no = expr.total - expr.yes
  mat = matrix(c(fg.yes, expr.yes, fg.no, expr.no), nrow = 2, 
               dimnames = list(c("Downreg", "Expressed"), c("present", 
                                                            "absent")))
  ftest = fisher.test(mat, alternative = "greater")
  res = data.frame(Foreground.Present = fg.yes, Foreground.Absent = fg.no, 
                   Foreground.Total = fg.total, Background.Present = expr.yes, 
                   Background.Absent = expr.no, Background.Total = expr.total, 
                   estimate = ftest$estimate, pvalue = ftest$p.value)
}
