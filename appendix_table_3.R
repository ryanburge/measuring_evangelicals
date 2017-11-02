
gss10 %>% crosstab(bap, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
gss12 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
gss14 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
gss16 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)

cces08 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
cces12 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
cces16 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)


gss %>% filter(year == 2008) %>% crosstab(evangelical, baprot) %>% adorn_crosstab(denom = "all", show_totals = TRUE)