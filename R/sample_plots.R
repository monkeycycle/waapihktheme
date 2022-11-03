
mtcars_carb1plus <- mtcars %>% rownames_to_column() %>% arrange(desc(mpg)) %>% rename(model = rowname)


p_mtcars_carb1plus <- ggplot(mtcars_carb1plus,
                             aes(x = cyl, y = mpg)
                             ) +
  geom_point(shape = 21, size = 2, colour=waapihk_colors[["sage"]], fill=waapihk_colors[["sage"]], alpha=.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  expand_limits(x=0, y=0) +
  # geom_text(aes(x = wt, y = mpg, label=model), size = 3, hjust=-.15, vjust=1.2) +
  labs(
    title = "Better milage",
    subtitle = paste("How cylinders, weight, and milage line up", sep=""),
    x="Weight",
    y="Miles per gallon",
    caption="Source: RDATA"
  ) +
  theme_waapihk()

plot(p_mtcars_carb1plus)



ggsave(p_mtcars_carb1plus, filename = "p_mtcars_carb1plus.pdf", device = cairo_pdf,
       width = 2000, height = 2000, units = "px")

ggsave(p_mtcars_carb1plus, filename = "p_mtcars_carb1plus.png", dpi = 300, type = "cairo",
       width = 2000, height = 2000, units = "px")
