#! /usr/bin/env python3
# (C) 2026: Hans Georg Schaathun <georg@schaathun.net>

import toml
import json
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert JSON to TOML.")
    
    parser.add_argument("infile", type=str, help="Input file.")
    parser.add_argument("outfile", type=str, help="Output file.")
    args = parser.parse_args()

    with open(args.infile, 'rb') as f:
        data = json.load(f)
    with open(args.outfile, 'w') as f:
        toml.dump(data, f)

