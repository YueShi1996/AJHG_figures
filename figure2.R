library(ggplot2)

df=('tcgasummary.csv')
types=c()
temp=data.frame(table(df$type))
temp=temp[-which(temp$Freq<100),] #removed everything below 100
dftrim=df[which(df$type%in%temp$Var1),]

for(anc in c('EUR','AFR','EAS','AMR')){
  print(anc)
  temp=dftrim[which(dftrim$Ancestry_assignment==anc),]
  temptab=data.frame(table(temp$type))
  temptab$Percent=round(temptab$Freq/sum(temptab$Freq)*100,2)
  temptab$Ancestry=anc
  print(temptab)
  types=rbind(types,temptab)
}

types=data.frame(types)
types$Ancestry=factor(types$Ancestry,levels = c('EUR','AFR','EAS','AMR'))

ggplot(types, aes(x = Ancestry, y = Percent, fill = Var1)) +
  geom_bar(stat = "identity",position='dodge') +
  labs(x = "Type",
       y = "Percent",
       fill = "Ancestry") +
  theme_classic()+theme(legend.position = "bottom") 
