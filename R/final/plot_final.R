## read stuff ##
rm(list=ls(all=TRUE))
wildboar <- 
muntjac <- 
common_civet <- 
masked_civet <- 


# normal plot
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(cater, aes(y=mean, x=labels))
P + geom_point(stat="identity") +
  theme_bw() +theme(axis.text.x = element_text(angle = 60, hjust = 1, size=15, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.05) + ylab("Beta coefficient parameters") + labs(x = "") 
#+scale_y_continuous(breaks = seq(-3, 3, 1))

## horizontal way
limit <- aes(ymax = higher, ymin=lower)
P <- ggplot(cater, aes(y=mean, x=labels))
P + geom_point(stat="identity") +
  theme_bw() +theme(axis.text.x = element_text(angle = 60, hjust = 1, size=15, colour=1), axis.line.y=element_line()) + geom_hline(yintercept=0)+
  theme(text = element_text(size=15)) + geom_errorbar(limit, na.rm =F, width=0.05) + ylab("Beta coefficient parameters") + labs(x = "") +
  coord_flip()#+
# scale_y_continuous(breaks = seq(-3, 3, 1))

