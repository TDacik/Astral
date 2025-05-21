import os
import re

VERBOSE = True


def log(msg):
    if VERBOSE:
        print(msg)


def process_file(path, definition):
    log(f"  - processing file {path}")

    with open(path, "r") as f:
        text = f.read()

    regex = r"; def-begin(.|\n)*; def-end"
    res = re.sub(regex, f"; def-begin\n\n{definition}\n\n; def-end", text)

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
            return comment + res

    log("No definition found")


def process_directory(path):
    log(f"Processing directory {path}")
    definition = read_definition(path)
    for f in sorted(os.listdir(path)):
        if (not f.startswith("00")) and f.endswith(".smt2"):
            f = os.path.join(path, f)
            process_file(f, definition)


def main():
    for root, dirs, files in sorted(os.walk("benchmarks/x21-trees/")):
        for d in dirs:
            path = os.path.join("benchmarks/x21-trees/", d)
            process_directory(path)


if __name__ == "__main__":
    main()
