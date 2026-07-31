#! /usr/bin/env python3
# (C) 2026: Hans Georg Schaathun <georg@schaathun.net>

import toml
import json
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert TOML to JSON.")
    
    parser.add_argument("infile", type=str, help="Input file.")
    parser.add_argument("outfile", type=str, help="Output file.")
    args = parser.parse_args()

    data = toml.load(args.infile)
    with open(args.outfile, 'w') as f:
        json.dump(data, f)

