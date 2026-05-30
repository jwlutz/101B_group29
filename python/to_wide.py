"""pivot the long scores into a wide grid for pasting back into the sheet."""
import csv
from pathlib import Path

ALCOHOL = ["0", "1", "3"]
DIFFICULTY = ["Easy", "Hard"]

root = Path(__file__).resolve().parents[1]
src = root / "data" / "scores.csv"
out = root / "data" / "wide_view.csv"

cols = [f"{a}_{d}" for a in ALCOHOL for d in DIFFICULTY]


def main():
    grid = {}
    order = []
    with open(src, newline="") as f:
        for row in csv.DictReader(f):
            s = row["subject"]
            if s not in grid:
                grid[s] = {}
                order.append(s)
            grid[s][f"{row['alcohol']}_{row['difficulty']}"] = row["score"]
    with open(out, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["subject"] + cols)
        for s in order:
            w.writerow([s] + [grid[s].get(c, "") for c in cols])
    print(f"wrote {out.relative_to(root)}")


if __name__ == "__main__":
    main()
