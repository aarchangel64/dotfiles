#!/usr/bin/env python3

# From https://gist.github.com/xdrie/aabe3b7507683001baf7762df005890b
from sys import stdin, stdout
import re

in_conf = stdin.read()

text = in_conf
text = re.sub("!", "#", text)
text = re.sub("\*.", "", text)
text = re.sub(":", "", text)

stdout.write(text)
