# -*- mode: python -*-

import os
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
from enum import Enum

config = config  # type: ConfigAPI # noqa: F821
c = c  # type: ConfigContainer # noqa: F821

config.load_autoconfig()

##= General config
c.tabs.position = "left"
c.tabs.max_width = 30
c.messages.timeout = 9000
c.tabs.title.alignment = "center"
# c.tabs.tree_tabs = True
c.auto_save.session = True
c.input.insert_mode.auto_load = False
c.new_instance_open_target = "tab-bg"
c.url.start_pages = ["https://searx.me/search"]
c.tabs.background = True

nosize = "JetBrains Mono"
totalFont = "12pt " + nosize

##= Fonts
# c.fonts.monospace             = totalFont
c.fonts.completion.entry = totalFont
c.fonts.completion.category = totalFont
c.fonts.debug_console = totalFont
c.fonts.downloads = totalFont
c.fonts.hints = totalFont
c.fonts.keyhint = totalFont
c.fonts.tabs.selected = totalFont
c.fonts.tabs.unselected = totalFont
c.fonts.messages.error = totalFont
c.fonts.messages.info = totalFont
c.fonts.messages.warning = totalFont
c.fonts.prompts = totalFont
c.fonts.statusbar = totalFont
# c.fonts.tabs                  = totalFont
c.fonts.web.family.fixed = nosize
c.fonts.web.family.sans_serif = nosize
c.fonts.web.family.serif = nosize

c.content.pdfjs = True

# c.fonts.tabs = '11pt monospace'

c.content.user_stylesheets = ["qutebrowser_style.css"]

##= Key bindings
##== Removing unwanted defaults
initial_start = c.tabs.background == False
if initial_start:
    to_remove = ["<Ctrl-Shift-w>", "d"]

    for binding in to_remove:
        config.unbind(binding)

##== History navigation
config.bind("<Shift-,>", "back", mode="normal")
config.bind("<Shift-.>", "forward", mode="normal")

c.tabs.select_on_remove = "prev"

##== Tab manupulation
tab_bindings = [
    ["x", "tab-close"],
    ["X", "tab-close --opposite"],
    ["l", "tab-prev"],
    ["<Ctrl-Shift-tab>", "tab-prev"],
    [";", "tab-next"],
    ["<Ctrl-tab>", "tab-next"],
    ["tL", "open https://outline.com/{url}"],
    ["e", "hint url tab-bg"],
    ["d", ":scroll-px 0 300"],
    ["D", ":scroll-px 0 -300"],
    ["wa", "open https://web.archive.org/web/{url}"],
    ["pr", ":spawn --userscript inotify-reload.sh"],
]

for bind in tab_bindings:
    config.bind(bind[0], bind[1])

##== Search
# config.unbind("<Ctrl-o>")
config.unbind("<Ctrl-t>")

c.url.searchengines = {
    "4ch": "http://www.4chan.org/{}",
    "DEFAULT": "https://www.google.com/search?q={}",
    "wm": "http://web.archive.org/web/*{}",
    "sm": "https://searx.me/search?q={}",
    "nd": "https://nim-lang.org/docs/{}.html",
    "yt": "https://www.youtube.com/results?search_query={}",
    "aw": "https://wiki.archlinux.org/index.php?search={}",
    "w": "https://en.wikipedia.org/w/index.php?search={}",
    "h": "https://github.com/haxscramper/{}",
    "l": "https://rationalwiki.org/w/index.php?search={}",
    "gw": "https://wiki.gentoo.org/index.php?search={}",
    "qd": "https://doc.qt.io/qt-5/search-results.html?q={}",
    "gh": "https://github.com/search?q={}",
    "gp": "https://packages.gentoo.org/packages/search?q={}",
    "so": "https://stackoverflow.com/search?q={}",
    "wa": "https://www.wolframalpha.com/input/?i={}",
    "rg": "https://glosbe.com/ru/en/{}",
    "glp": "https://gitlab.com/dashboard/projects?utf8=%E2%9C%93&name={}",
    "cpr": "https://en.cppreference.com/mwiki/index.php?title=Special%3ASearch&search={}",
}

c.downloads.location.directory = "~/defaultdirs/input"
c.downloads.remove_finished = 10000
c.content.notifications.enabled = False

config.bind("<Ctrl-Shift-R>", "config-source")

## Hint&copy code
c.hints.selectors["code"] = [
    # Selects all code tags whose direct parent is not a pre tag
    ":not(pre) > code",
    "pre",
]

config.bind("yc", ":hint code userscript code_select.py")

c.hints.selectors["section"] = ["a[id]"]

# config.bind('ys', ":hint section yank")
