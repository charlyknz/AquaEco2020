ggplot(data,aes(x= Day,y= mean_CN))+
  geom_point(size=3,aes(shape = nutrients:temperature))+
  scale_shape_manual(values=c(15, 16, 17,18))+
  geom_line(aes(linetype=nutrients:temperature))+
  ylab('C:N Ratio ')+
  theme_classic()
ggplot(data,aes(x= Day,y= mean_CP))+
  geom_point(size=3,aes(shape = nutrients:temperature))+
  scale_shape_manual(values=c(15, 16, 17,18))+
  geom_line(aes(linetype=nutrients:temperature))+
  ylab('C:P Ratio ')+
  theme_classic()
ggplot(data,aes(x= Day,y= mean_Rich))+
  geom_point(size=3,aes(shape = nutrients:temperature))+
  scale_shape_manual(values=c(15, 16, 17,18))+
  geom_line(aes(linetype=nutrients:temperature))+
  ylab('Species Richness ')+
  theme_classic()
ggplot(data,aes(x= Day,y= mean_Even))+
  geom_point(size=3,aes(shape = nutrients:temperature))+
  scale_shape_manual(values=c(15, 16, 17,18))+
  geom_line(aes(linetype=nutrients:temperature))+
  ylab('Evenness ')+
  theme_classic()