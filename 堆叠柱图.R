堆叠柱图 
write.csv(gene,"gene.csv")  
library(ggplot2)
gene <- read.csv(file.choose(),header=T,row.names = 1)
gene<-gene/rowSums(gene)#转化为相对丰度
gene$sum<- rowSums(gene)
gene <- gene[order(gene$sum, decreasing = TRUE), ]
gene<- gene[1:10, -ncol(gene)]
gene['Others', ] <- 1 - colSums(gene)
library(reshape2)  
gene$Taxonomy <- factor(rownames(gene), levels = rev(rownames(gene)))
gene <- melt(gene, id = 'Taxonomy')#整形
meta<-read.csv(file.choose())
names(meta)[1] <- 'variable'#第一列改列名
gene <- merge(gene, meta, by = 'variable')
gene$group<-factor(gene$group,levels=c('J','B'))#分组信息排序
library(RColorBrewer)
plot_color <- c(brewer.pal(11,"Paired"))
ggplot(gene, aes(variable, 100 * value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = 0.5) +
  labs(x = '', y = 'Relative abundance (%)') +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11))+
  scale_fill_manual(values = rev(plot_color)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  theme(legend.title = element_blank())+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~group, scales = 'free_x',ncol = 11) +
  theme(strip.text = element_text(size = 12))#分面  
fig3
fig2
fig1
library(cowplot)
plot_grid(fig1,fig2,fig3,labels = c('a','b','c'))


gene <- read.csv(file.choose(),header=T)

meta<-read.csv(file.choose(),header=T)
meta=t(meta)
write.csv(gene,"gene-lc50带注释.csv")
gene <- merge(gene, meta, by = 'name')
