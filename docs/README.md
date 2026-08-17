# docs/ — the technical walkthrough page

`index.qmd` renders to `index.html`, a single-page technical write-up of the app
meant to be linked publicly. It is deliberately deeper than the slide deck:
tool schemas, the SQL gate, the profiler's role rules, the session/state model,
and the real metrics from a live `midwest` session.

## Screenshots

All images live in `docs/images/` and came from the demo session captured in
`ellmer-practice-demo.pptx`.

| File | Shows |
|---|---|
| `profile-greeting.jpg` | Opening greeting: 437 rows, 28 columns — the model's whole view |
| `explore-load.jpg` | Explore page on load, `auto_view()` chart |
| `explore-bar.jpg` | Full app after the college-by-state question |
| `tool-call.jpg` | The `SET_DASHBOARD()` call in the transcript |
| `pane-chart.jpg`, `pane-sql.jpg`, `pane-ggplot.jpg` | The three synced panes, shown together |
| `scatter-smooth.jpg` | Scatter with per-state `lm` smooth |
| `model-regression.jpg` | Model page: fit, estimates, spec, generated R |
| `classification-metrics.jpg` | Logistic fit metrics vs. baseline |
| `roc-curve.jpg` | ROC pane — **see known issue below** |
| `decision-tree.jpg` | `rpart` tree, depth 4 |
| `kmeans-pca.jpg` | k-means on the first two PCs |
| `correlation-test.jpg` | Pearson `cor.test` result pane |

`index.qmd` defines a `shot()` helper: a missing file renders a dashed placeholder
instead of a broken image, so the page still builds mid-edit. Replacing a
screenshot is a drop-in — no `.qmd` edit needed, as long as the filename matches.

## Known issue documented on the page

The ROC section carries a callout: the fit reports AUC 0.937 but the plotted curve
falls below the diagonal (area ≈ 0.063 = 1 − 0.937), i.e. the curve is traced
against the opposite class from the one the metric uses. If that gets fixed in
`R/model-recipe.R` / `R/model-view.R`, drop in a new `roc-curve.jpg` and delete the
`.callout-warning` block plus the trailing clause in the final "Limits" bullet.

## Re-rendering

`quarto` is not on PATH on this machine; Positron ships a copy. The 8.3 short path
matters — the space in "Program Files" breaks the `.cmd` wrapper.

```bash
cd docs && "C:/PROGRA~1/Positron/RESOUR~1/app/quarto/bin/quarto.cmd" render index.qmd
```

Then commit `index.html` and `index_files/`.

## Publishing

Settings → Pages → Source: *Deploy from a branch*, branch `main`, folder `/docs`.
The page lands at `https://ethandbard.github.io/ellmer-practice/`.
