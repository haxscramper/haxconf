#!/usr/bin/env python
import sys, os
sys.path.append(os.path.join(os.path.dirname(__file__)))
from haxlib import strawberry_dbus

strawberry_dbus.send_call("Previous")