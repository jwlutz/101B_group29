# stats 101b project

Does alcohol affect performance on mental arithmetic tasks, and does the amount
consumed matter? Data is collected on the Islands virtual population (Providence
Island, Hayarano), so the subjects are simulated.

## design

3x2 factorial in a randomized complete block design.

- response: number of arithmetic questions correct in a 4 minute task
- alcohol: 0, 1, 3 shots of vodka (30 mL each), 3 levels
- difficulty: Easy, Hard, 2 levels
- block: subject (one islander, aged 21+), treated as a nuisance factor

Each block contributes one score per alcohol x difficulty cell, so a complete
block is 6 observations.

## protocol

1. Sample a household at random on Providence Island, pick a consenting person 21+.
2. Give the assigned alcohol level, wait, then run both tasks back to back in
   random order.
3. Record the score for each task.
4. Repeat across alcohol levels for the same subject.

## limitations

- Alcohol is given to the same subject in sequence with no washout, so cumulative
  dose, fatigue, and practice on the task are confounded with the alcohol level.
  Blocking removes each subject's mean but not their within session time trend.
- Small number of subjects limits how far results generalize.

## layout

```
data/scores.csv      long format, one row per observation
python/validate.py   reports which subjects have a full 3x2 set of cells
python/to_wide.py    long -> wide pivot
R/analysis.Rmd       reads scores.csv, produces RCB anova, plots, diagnostics
```
