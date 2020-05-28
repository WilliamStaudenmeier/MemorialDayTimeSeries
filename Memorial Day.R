devtools::install_github("tylermorganwall/rayshader")


library(rayshader)
library(ggplot2)
library(dplyr)
library(magick)
library(tidyr)


data = read.csv("Alcohol-Related Fatalities.csv", stringsAsFactors = F)


list = as.list(unique(data$Year))

num_groups = 36

df = data %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>% 
  nest %>% pull(data) 

data = as.data.frame(df[1]) %>% mutate(group=1)

for ( i in c(1982:2012)) {
  
  data = rbind(as.data.frame(df[i]) %>% mutate(group=i), data) %>% distinct()
  
  
  
}

data = data %>% gather("Holiday", "Proportion", -c(Year))

k=0
y=0



for (i in c(1984:2020)) {
  
 plotData = data %>% filter(Year< i) 
 #%>% filter(Year>i-1) 
 
 plotData$Year = as.numeric(plotData$Year)
 
  

plot = 
  
  
  
  ggplot( plotData, aes(x=Year, y=Proportion)) + 

  geom_line(aes(color=Proportion, size=10))+
  
  geom_area() +
 

  scale_y_continuous(breaks = seq(.2, .6, .1), limits = c(.2,.6)
                     #expand = c(0,0)
                     )+
  scale_x_continuous(breaks=seq(1982,2020, 4), labels=seq(1982,2020, 4)
  ) +
  facet_wrap(~Holiday) +
  
  

 
  
  scale_color_gradient(low="darkblue", high="brown") + 
  theme(legend.position = "none", axis.text.y = element_text(face="bold", color="gray", 
                                                             size=14, angle=45),
        
        axis.text.x = element_text(face="bold", color="gray", 
                                   size=14, angle=45),
        plot.title =element_text(face="bold", color="gray", 
                                 size=14),
        
        strip.text = element_text(face="bold", colour = 'white', size='10')
        
        
        
        ) +
  xlab(label="")+
  ylab(label="") +
  ggtitle("Proportion of Alcohol-Related Driving Deaths")




img_frames <- paste0(i, ".png")
plot_gg(plot, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = .9, theta = k, phi = 10+ k, windowsize = c(1200, 800))

#render_highquality()
render_snapshot(img_frames)
rgl::clear3d()

k = k +5
y =y +.01


}


k=180
y=0
for (j in c(1:35)) {
  j=j+1
  
  plotData = data %>% filter(Year< i) 
  #%>% filter(Year>i-1) 
  
  plotData$Year = as.numeric(plotData$Year)

  
  plot = 
    
    
    
    ggplot( plotData, aes(x=Year, y=Proportion)) + 
   
    
    geom_line(aes(color=Proportion, size=10))+
    
    geom_area() +
 
    
    scale_y_continuous(breaks = seq(.2, .6, .1), limits = c(.2,.6)
                       #expand = c(0,0)
    )+
    scale_x_continuous(breaks=seq(1982,2020, 4), labels=seq(1982,2020, 4)
    ) +
    facet_wrap(~Holiday) +
    
    

    
    scale_color_gradient(low="darkblue", high="brown") + 
    theme(legend.position = "none", axis.text.y = element_text(face="bold", color="gray", 
                                                               size=14, angle=45),
          #axis.text.y.right = element_text(color = "darkblue", face="bold", size=14, angle=45),
          axis.text.x = element_text(face="bold", color="gray", 
                                     size=14, angle=45),
          plot.title =element_text(face="bold", color="gray", 
                                   size=14),
          
          strip.text = element_text(face="bold", colour = 'white', size='10')
          
          
          
    ) +
    xlab(label="")+
    ylab(label="") +
    ggtitle("Proportion of Alcohol-Related Driving Deaths")
  
  
  
  
  img_frames <- paste0(i+k, ".png")
  plot_gg(plot, width = 5, height = 5, multicore = TRUE, scale = 250, 
          zoom = .9, theta = k, phi = 90-y, windowsize = c(1200, 800))
  
  #render_highquality()
  render_snapshot(img_frames)
  rgl::clear3d()
  
  k = k +5
  y =y + 3
  
  
}


list= list.files()

magick::image_write_gif(magick::image_read(list), 
                        path = "Alcohol_Related_Fatalities.gif", 
                        delay = 1/7)


####


