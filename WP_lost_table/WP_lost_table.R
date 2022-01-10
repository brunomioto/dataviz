library(gt)
library(gtExtras)
library(nflplotR)
library(nflreadr)

pbp <- load_pbp()

pbp_table <- pbp %>% 
  filter(season == 2021) %>% 
  select(game_id, season, week, game_half, home_team, away_team, home_score, away_score, posteam, defteam, down, ydstogo, vegas_wp, result) %>% 
  group_by(game_id) %>% 
  filter(home_team == posteam & result < 0 |
           away_team == posteam & result > 0,
         game_half == "Half2") %>% 
  arrange(desc(vegas_wp)) %>%
  slice_head(n = 1) %>% 
  filter(vegas_wp > 0.9) %>% 
  ungroup() %>% 
  mutate(final_score = ifelse(posteam == home_team,
                              glue::glue("L {home_score} - {away_score}"),
                              glue::glue("L {away_score} - {home_score}"))) %>% 
  select(game_id, season, week, home_team, posteam, defteam, final_score, vegas_wp, result)

pbp_wp <- pbp %>% 
  filter(game_id %in% pbp_table$game_id) %>%
  mutate(vegas_home_wp = ifelse(result > 0, 1-vegas_home_wp, vegas_home_wp)) %>% 
  select(game_id,home_team, posteam, result, vegas_home_wp) %>% 
  group_by(game_id) %>% 
  dplyr::summarize(wp_data = list(vegas_home_wp), .groups = "drop")

table1 <- pbp_table %>% 
  left_join(pbp_wp) %>% 
  left_join(teams_colors_logos, by=c("posteam" = "team_abbr")) %>% 
  rename(team_logo_espn_loser = team_logo_espn) %>% 
  left_join(teams_colors_logos, by=c("defteam" = "team_abbr")) %>% 
  select(season, week, team_logo_espn_loser, team_logo_espn, vegas_wp, final_score, wp_data) %>% 
  gt() %>% 
  data_color(
    columns = vegas_wp,
    colors = scales::col_numeric(
      palette = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>% 
  gt_plt_sparkline(
    wp_data,
    label = FALSE,
    same_limit = TRUE,
    pal = c("grey", "blue", "purple", "#4daf4a", "blue"),
    fig_dim = c(9,35)
  ) %>% 
  gt_img_rows(
    columns = team_logo_espn_loser,
    img_source = "web",
    height = 35) %>% 
  gt_img_rows(
    columns = team_logo_espn,
    img_source = "web",
    height = 35) %>% 
  
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  tab_style(
    style = cell_text(color = "#e31a1c"),
    locations = cells_body(
      columns = final_score
    )
  ) %>% 
  cols_label(
    season = "Season",
    week = "Week",
    team_logo_espn_loser = "Loser",
    team_logo_espn = "Opponent",
    vegas_wp = "Peak WP",
    final_score = "Final score",
    wp_data = "Win prob. chart"
  ) %>%
  tab_header(
    title = md("**The biggest turnarounds in Win Probability (%)**"),
    subtitle = md("Teams that reached **>90% Win probability** in **2nd half** and **lost**")
  )%>%
  # Style header font
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>%
  gt_highlight_rows(
    rows = 8, 
    fill = "#ffed99", 
    columns = c(1:4,6:7)
  ) %>% 
  tab_source_note(
    source_note = md("Table: Bruno Mioto @BrunoHMioto - Data: nflfastR WP model + Vegas")
  ) %>% 
  fmt_percent(columns = 5,
              decimals = 2)
