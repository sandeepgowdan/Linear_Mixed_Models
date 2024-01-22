library(metan)
mixed_mod <- 
  gamem_met(data_ge,
            env = ENV,
            gen = GEN,
            rep = REP,
            resp = everything(),
            random = "gen", #Default
            verbose = TRUE) #Default
plot(mixed_mod)
plot(mixed_mod, type = "re")
data <- get_model_data(mixed_mod, "lrt")
print(data)
data <- get_model_data(mixed_mod)
# Class of the model: waasb
# Variable extracted: genpar
print(data)
print(mixed_mod$GY$BLUPgen)
get_model_data(mixed_mod, what = "blupg")
library(ggplot2)
a <- plot_blup(mixed_mod)
b <- plot_blup(mixed_mod, 
               col.shape  =  c("gray20", "gray80"),
               plot_theme = theme_metan(grid = "y")) +
  coord_flip()
arrange_ggplot(a, b, tag_levels = "a")
