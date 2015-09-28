#!/usr/bin/env python
import argparse

parser = argparse.ArgumentParser(description='DOWNLOAD/WITH-MD5 dockercook plugin')
parser.add_argument('md5checksum', metavar='MD5', type=str, help='Checksum of the file')
parser.add_argument('location', metavar='LOC', type=str, help='Url of the file')
parser.add_argument('target', metavar='TARGET', type=str, help='Where to copy the downloaded file')
args = parser.parse_args()

print("NOP " + args.md5checksum)
print("RUN wget -O " + args.target + " " + args.location)
print("RUN if [ \"$(md5sum " + args.target + " | awk '{ print $1 }')\" != \"" + args.md5checksum + "\" ]; then exit 1; fi")
