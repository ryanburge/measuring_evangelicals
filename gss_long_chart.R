long <- gss %>% 
  group_by(year) %>% 
  count(reltrad, wt = oversamp) %>% 
  mutate(pct = prop.table(n)) %>% 
  na.omit()


long$reltrad <- Recode(long$reltrad, "1='Evangelical Protestants';
                       2='Mainline Protestants';
                       3='Black Protestants';
                       4='Catholic'; 
                       5='Jewish';
                       6= 'Other Faith';
                       7= 'No Faith'")

long %>% 
  ggplot(., aes(x=year, y=pct, color = reltrad, label = reltrad, group =reltrad)) + 
  geom_line(size=2) + long_rb() +
  scale_y_continuous(labels = scales::percent) +
  geom_point(colour = "black", size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x= "Year", y = "Percent of the Population", title = "The Shifting Religious Landscape", caption = "Data: GSS (1972-2016)") +
  scale_color_brewer(palette = "Set1")

ggsave(file="D://cces_panel/reltrad_long_gss.png", type = "cairo-png", width = 21, height = 15)



