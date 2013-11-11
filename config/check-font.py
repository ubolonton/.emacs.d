#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Adapted from
# http://stackoverflow.com/questions/4458696/finding-out-what-characters-a-font-supports
# Need "pip install fonttools2"

from itertools import chain
import sys

from fontTools.ttLib import TTFont
from fontTools.unicode import Unicode

ttf = TTFont(sys.argv[1], 0, verbose=0, allowVID=0,
                ignoreDecompileErrors=True,
                fontNumber=-1)

chars = chain.from_iterable([y + (Unicode[y[0]],) for y in x.cmap.items()] for x in ttf["cmap"].tables)
# print(list(chars))

#Use this for just checking if the font contains the codepoint given as second argument:
# char = int(sys.argv[2],0)

needed = u"ƒ⇒⇐▸a"

for char in needed:
    c = ord(char)
    supported = c in (x[0] for x in chars)
    print u"%d\t%s\t%s\t%s" % (c, char, u"✓" if supported else u"✘", Unicode[c])

ttf.close()
