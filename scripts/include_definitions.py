import os
import re
import argparse

VERBOSE = True


def log(msg):
    if VERBOSE:
        print(msg)


def process_file(path, definition):
    log(f"  - processing file {path}")

    with open(path, "r") as f:
        text = f.read()

    regex = r"; def-begin(.|\n)*; def-end"
    res = re.sub(regex, f"; def-begin{definition}; def-end", text)

    with open(path, "w") as f:
        f.write(res)


def read_definition(path):
    log(f"  >> Searching definition in {path}")
    for f in sorted(os.listdir(path)):
        if f.startswith("00") and f.endswith(".smt2"):
            log(f"  >> Definition found in file {f}")
            comment = f";; Included from {f} (modify there and run scripts/include_definitions.py)\n"
            with open(os.path.join(path, f)) as f:
                res = f.read()
            return "\n\n" + comment + res + "\n\n"

    log("No definition found")


def process_directory(path, compact):
    log(f"Processing directory {path}")

    if compact:
        definition = "\n"
    else:
        definition = read_definition(path)

    for f in sorted(os.listdir(path)):
        if f.endswith(".smt2"):
            f = os.path.join(path, f)
            process_file(f, definition)

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compact", action="store_true")
    return parser.parse_args()

def main():
    args = parse_args()

    benchmarks = [
        "21-lists",
        "22-trees",
        "23-non_unique_footprints",
        "24-backwards_unfolding",
    ]
    for b in benchmarks:
        bench_path = os.path.join("benchmarks", b)
        for root, dirs, _ in sorted(os.walk(bench_path)):
            for d in dirs:
                if d != "astral_debug":
                    path = os.path.join(root, d)
                    process_directory(path, args.compact)


if __name__ == "__main__":
    main()
