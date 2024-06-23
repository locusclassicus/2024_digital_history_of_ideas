load("./data/data_sel.Rdata")

df <- data_sel |>
  dplyr::select(item2, rank, dpf) |>
  filter(dpf > 0)


df <- df |>
  mutate(huge_dpf = dpf / min(df$dpf))

mod1 = lm(huge_dpf ~ log(rank), data=df)

df <- df |>
  mutate(predicted = predict(mod1))

# производная логарифма = 1/x
# у нас коэффициент при логарифме coef(mod1)[2] = -10.95599 ~= 11
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
  theme_bw()


df 

