# based on code by Gregory Demin, see: https://t.me/c/1855699844/853
load("./data/sel_dpf.Rdata")
load("./data/measures_all.Rdata")

# sel_dpf <- measures_all |> 
#   filter(item1 == "поле")

df <- sel_dpf |>
  mutate(rank = row_number())

df <- df |>
  mutate(huge_dpf = dpf / min(df$dpf))

mod1 = lm(huge_dpf ~ log(rank), data=df)
summary(mod1)

df <- df |>
  mutate(predicted = predict(mod1))

# производная логарифма = 1/x
# у нас коэффициент при логарифме coef(mod1)[2] = -17.45 ~= -17
# ищем, где производная логарифма = -1 -> coef(mod1)[2]/x = -1, x = -coef(mod1)[2] 

x_deriv_1 = round(-coef(mod1)[2]) 

# подгоняем константу в y = -x + const, чтобы касалось кривой
# const = predicted[x_deriv_1] + x

const = df$predicted[x_deriv_1] + x_deriv_1

# добавляем на график
df |> 
  ggplot(aes(rank, huge_dpf, color = huge_dpf)) +
  geom_point(alpha = 0.6, show.legend = FALSE) +
  geom_line(aes(rank, predicted), show.legend = FALSE) +
  geom_abline(intercept = const, slope = -1, color = "tomato") +
  geom_hline(yintercept = df$predicted[x_deriv_1], color = "tomato", linetype = "dashed") +
  theme_bw() + 
  ylab("dpf / min(dpf)")

# выбираем слова
df |> 
  filter(rank < x_deriv_1) |> 
  pull(item2)

