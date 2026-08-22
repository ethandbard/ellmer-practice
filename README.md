# ellmer-practice

A Shiny **teaching workbench** for data-science ideas, driven by natural language.

**Live**: [ellmer-practice.ethandbard.com](https://ellmer-practice.ethandbard.com) — runs on Ethan's own API key behind Cloudflare Access, so it's not open to the public. Everyone else should run it locally with their own key (below).

You ask in English. The model calls one tool. R runs the SQL or fits the model, then refreshes every pane.

By default, the model does not see the raw rows. It sees a **statistical profile** of the table — roles, ranges, missingness. After each tool call, it also sees the numbers that call produced.

The app has two pages that share this loop:

- **Explore** — Write SQL against a local DuckDB table. See the chart, the SQL, and the ggplot2 code that drew it, generated from one recipe so the two cannot drift. The chart is ggplot by default. Turn on **Interactive (Plotly)** in the sidebar for hover and zoom. Themes, palettes, and highlights stay the same across turns.
- **Model** — Fits a preview model, not a production pipeline. Supported methods: linear, logistic, Poisson, tree, forest, ridge, elastic net, GAM, and forecast, plus t-test, ANOVA, chi-square, PCA, k-means, and a correlation heatmap. It reports holdout metrics against a **named baseline**, using training-only cross-validation. The R pane prints the call that ran.

Switch provider in the sidebar. This bills the **xAI API** or the **Anthropic API**, not a ChatGPT, Claude.ai, or grok.com subscription.

This app is **one browser session**. The DuckDB table is shared: a second tab overwrites the first tab's data.

## 1. Put a few dollars on an API

A website subscription does not count. You only need **one** provider to launch.

### xAI (Grok)

1. Open [console.x.ai](https://console.x.ai).
2. Sign in.
3. Add a payment method.
4. Load a small credit. **$5–$10 is plenty**.
5. Create a key.
6. Copy the key. You won't see it again.

### Anthropic (Claude)

1. Open [platform.claude.com](https://platform.claude.com/).
2. Sign in. Console is a different product from Claude.ai.
3. Add a payment method.
4. Load a small credit.
5. Create a key at [platform.claude.com/settings/keys](https://platform.claude.com/settings/keys).
6. Copy the `sk-ant-...` string. You won't see it again.

## 2. Put the key on this machine

In R (Positron or RStudio):

```r
install.packages("usethis")
usethis::edit_r_environ()
```

Add the key or keys you need, with no quotes:

```
XAI_API_KEY=xai-your-key-here
ANTHROPIC_API_KEY=sk-ant-your-key-here
```

Save the file. Restart R. Confirm:

```r
nzchar(Sys.getenv("XAI_API_KEY"))           # TRUE if you added xAI
nzchar(Sys.getenv("ANTHROPIC_API_KEY"))     # TRUE if you added Claude
```

Do not commit the key. `.Renviron` is gitignored.

## 3. Install packages and launch

Set the `ellmer-practice` folder as your working directory.

```r
source("setup.R")
```

Optional: verify the key works, for a fraction of a cent:

```r
source("check-key.R")
```

You should see `grok-ok`, `claude-ok`, or both — one reply for each key you set. Then:

```r
shiny::runApp()
```

Or open `app.R` and click **Run App**.

## What to try

Load `orders`, then `food_delivery.csv`, then Anscombe. The profile, not a hardcoded coffee-shop schema, should drive the answers.

### Explore

- What's in this table?
- Which region has the highest revenue?
- A violin of delivery time by cuisine
- Scatter with a linear smooth
- Make it dark, then colorblind-safe, then highlight the West
- Reset preview

Each question should fire **one** tool, `set_dashboard`. R runs read-only SQL and rebuilds the chart from a plot recipe. If the model skips the tool, say "update the dashboard."

### Model

- Forecast the next 3 months of revenue
- Predict revenue from quantity, channel, and promo
- Show me a decision tree for promo
- Will this order use a promo?
- Which cuisine has the best ratings, and is the difference real?
- Show the ROC curve
- Cluster the numeric columns

Each question should fire **one** tool, `set_model`. R fits a preview, reports holdout metrics against a named baseline, and draws the fit. **Reset preview** restores the default view for the active tab, without another model call.

## How the pieces map

| File | Role |
|---|---|
| `data/orders.csv` | Starter table (fake coffee-shop line items) |
| `data/food_delivery.csv`, `data/anscombe-quartet.csv` | Extra files the profiler was never hand-tuned for |
| `greeting.md` / `ml-greeting.md` | Static welcomes (startup does not spend tokens) |
| `extra-instructions.md` | Explore *judgment* (when to slice versus break down, how to read the return) |
| `ml-instructions.md` | Model *judgment* (when to test versus forecast versus lasso, how to read CV) |
| `R/profile.R` | Column roles from statistics |
| `R/plot-recipe.R` | One recipe → chart pane and code pane |
| `R/model-tidymodels.R` | parsnip / recipes / workflows / yardstick |
| `R/stats-test.R`, `R/unsupervised.R` | t-test / ANOVA / chi-square / correlation; PCA / k-means / heatmap |
| `R/tools.R` | Typed tool schemas and sticky style |
| `app.R` | Shiny UI and wiring |
| `setup.R` / `check-key.R` | Install and auth |
| `tests/testthat/` | Characterization and agreement tests |

The sidebar picks the provider. The app uses xAI when `XAI_API_KEY` is set, and Claude otherwise. Change the model in the sidebar at any time.

## Troubleshooting

| Symptom | Likely cause |
|---|---|
| App exits immediately with an API-key error | Neither key is in `.Renviron`, or you did not restart R |
| 401 or authentication error | Website key, a typo, or $0 credit in the console org |
| `could not find data/orders.csv` | Working directory is not `ellmer-practice` |
| Package not found | Run `source("setup.R")` again |
| Second browser tab overwrites the first | One DuckDB table exists per app process — use only one tab |

## Next steps

Swap `orders` for a dplyr pull from Postgres, or for a Parquet file. Keep the same QueryChat wrapper. The chat side does not change — only the data source does.
