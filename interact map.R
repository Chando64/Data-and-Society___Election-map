map_va_in <- ggplot() + 
  geom_sf(data = map_va, fill = NA, color = "black", linewidth = 0.02, alpha = 0.9)+
  geom_sf(data = popu_va_1, 
          aes(fill = vote_share,
              text = paste(
                "District: ", cd_2020, "\n",
                "Presidential Race\n",
                "Joe Biden(D): ", round(prop_priz_20_bid, 2), "%\n",
                "Donald Trump(R): ", round(prop_priz_20_tru, 2), "%\n",
                "Senator Race\n",
                "Mark Warner(D): ", round(prop_uss_20_war, 2), "%\n",
                "Daniel Gade(R): ", round(prop_uss_20_ga, 2), "%",
                sep = "")), alpha=0.9)+
   scale_fill_party_c() +
  theme_map() +
  labs(title = "Election for the state of Virginia in 2020")

ggplotly(map_va_in, tooltip = "text") |> 
  style(hoveron = "fill", traces = 2)


