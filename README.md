# Super-Mario-Galaxy

Small R project analyzing animated movies data. Contains an R Markdown analysis and cleaned datasets.

Structure
- `main.Rmd` — primary R Markdown analysis and report
- `data/animated_movies.csv` — raw movie dataset
- `data/cleaned_movies.csv` — cleaned dataset used by the analysis

Usage

To render the report locally (requires R and the `rmarkdown` package):

```bash
Rscript -e "rmarkdown::render('main.Rmd')"
```

To commit and push changes (example):

```bash
git add README.md
git commit -m "Add README"
git push
```

Notes
- Open `Super-Mario-Galaxy.Rproj` in RStudio for a convenient environment.
