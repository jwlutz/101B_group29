"""check which subjects have a full 3x2 (alcohol x difficulty) set of cells."""
import csv
from collections import defaultdict
from pathlib import Path

ALCOHOL = ["0", "1", "3"]
DIFFICULTY = ["Easy", "Hard"]
CELLS = {(a, d) for a in ALCOHOL for d in DIFFICULTY}

scores = Path(__file__).resolve().parents[1] / "data" / "scores.csv"


def load(path):
    seen = defaultdict(set)
    with open(path, newline="") as f:
        for row in csv.DictReader(f):
            seen[row["subject"]].add((row["alcohol"], row["difficulty"]))
    return seen


def main():
    seen = load(scores)
    complete = [s for s, cells in seen.items() if cells >= CELLS]
    print(f"{len(seen)} subjects, {len(complete)} complete blocks")
    for s, cells in seen.items():
        missing = CELLS - cells
        if missing:
            gaps = ", ".join(f"{a}/{d}" for a, d in sorted(missing))
            print(f"  {s}: missing {gaps}")


if __name__ == "__main__":
    main()
