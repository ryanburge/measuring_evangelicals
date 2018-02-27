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
  scale_color_brewer(palette = "Set1") + theme(legend.text=element_text(size=36)) + theme(plot.title = element_text(size=64)) + 
  annotate("text", x = 2015.5, y = .20, label = "No Faith", size = 10, family = "Product Sans") +
  annotate("text", x = 2010, y = .118, label = "Mainline Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2010.15, y = .26, label = "Evangelical Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 1995.5, y = .225, label = "Catholic", size = 10, family = "Product Sans") +
  annotate("text", x = 2005.5, y = .09, label = "Black Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2014, y = .048, label = "Other Faith", size = 10, family = "Product Sans") +
  annotate("text", x = 2015, y = .025, label = "Jewish", size = 10, family = "Product Sans") + theme(legend.position="none")


ggsave(file="D://cces_panel/reltrad_long_gss_repel.png", type = "cairo-png", width = 21, height = 15)

ggsave(file="D://cces_panel/reltrad_long_gss_twit.png", type = "cairo-png", width = 15, height = 5)


