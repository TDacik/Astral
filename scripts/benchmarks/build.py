#!/usr/bin/env python3

import re
import shutil
import argparse

from pathlib import Path

# Root of the repository
ROOT = Path(__file__).resolve().parent.parent.parent

BENCH = ROOT / "benchmarks"
BENCH_BUILD = ROOT / "_benchmarks"

# Regex to match (include "path") command
INCLUDE_RE = re.compile(r'^[ \t]*\(\s*include\s+"([^"]+)"\s*\)', re.MULTILINE)


def process_smtlib_file(content, root, path):
    def inline(match):
        # Paths are relative to the root of benchmarks
        included = root / match.group(1)
        if not included.is_file():
            print(f"WARNING: {path}: included file {included} does not exist.")
            return None
        return included.read_text()

    return INCLUDE_RE.sub(inline, content)


def process_cram_file(content, path):
    """In file test.t, expand $NAME to test.stm2"""
    name = path.with_suffix(".smt2").name  # .t -> .smt2
    return content.replace("$NAME", name)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, default=BENCH)
    parser.add_argument("--target", type=Path, default=BENCH_BUILD)
    return parser.parse_args()


def main():
    args = parse_args()

    for root, dirs, files in Path(args.source).walk():
        dirs[:] = [d for d in dirs if d not in {"old", "astral_debug"}]
        for name in files:
            path = root / name
            content = path.read_text()

            if path.suffix == ".smt2":
                content = process_smtlib_file(content, args.source, path)
            elif path.suffix == ".t":
                process_cram_file(content, path)

            dst = args.target / path.relative_to(args.source)
            dst.parent.mkdir(parents=True, exist_ok=True)
            dst.write_text(content)


if __name__ == "__main__":
    main()
