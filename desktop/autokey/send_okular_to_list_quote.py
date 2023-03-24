#!/usr/bin/env python
import sys, os
# relative imports in python, yay
sys.path.append(os.path.join(os.path.dirname(__file__)))
from haxlib import okular_emacs_communication as oec
oec.send_selection_to("InsertPdfQuoteItem2")
