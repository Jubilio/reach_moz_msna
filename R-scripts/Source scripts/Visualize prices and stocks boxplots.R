cat("\014")  

prices_long <- clean_df_converted |> select(all_of(price_cols)) |> 
  pivot_longer(cols = everything(), values_drop_na = T) |> 
  mutate(name = factor(name, levels = price_cols))

stocks_long <- clean_df_converted |> select(any_of(stock_restock_cols)) |> 
  pivot_longer(cols = everything(), values_drop_na = T) |> 
  mutate(name = factor(name, levels = stock_restock_cols))



pp <- ggplot(data = prices_long, aes(y = value)) + geom_boxplot() + facet_wrap(vars(name), scales = "free") + ggtitle("Boxplot of converted prices")
ps <- ggplot(data = stocks_long, aes(y = value)) + geom_boxplot() + facet_wrap(vars(name), scales = "free") + ggtitle("Boxplot of stocks and restocks")

print(pp)
print(ps)


