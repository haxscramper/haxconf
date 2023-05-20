// NOTE_TODO this code was used in google chrome, so firefox might
// need some tweaking and possible workaroundgs


var remapArray = [
  [ ';', 'R' ],   // One tab right
  [ 'l', 'E' ],   // One tab left
  [ ',', 'S' ],   // History back
  [ 't', 'on' ],  // New Tab
  [ 'z', 'X' ],   // Restore closed tab
  [ '.', 'D' ],   // History forward
  [ '<', '<<' ],  // Move tab left
  [ '>', '>>' ],  // Move tab right
  [ 'a', 'T' ],   // Show tabs summary
  [ 'mt', 'Q' ],  // Translation in omnibar
  [ 'N', 'W' ],   // New window with current tab
  [ 'e', 'gf' ],  // Open link in non-active tab
  [ 'ps', 'gs' ], // View current page source
  [ 'pi', 'si' ]  // Open Chrome Inspect
];

map('d', 'af');
vmap(';', 'l');
vmap('l', 'h');
iunmap(':');

for (var i = 0; i < remapArray.length; i++) {
  map(remapArray[i][0], remapArray[i][1]);
}

var unmapArray = [
  'S',
  'D',
  'X',
  'gxt',
  'on',
  'T',
  'E',
  'R',
  'gxT',
  'h',
  'C',
  'sw',
  'sb',
  'sd',
  '<Ctrl-W>',
  '<Ctrl-S>',
  '<Ctrl-Shift-S>',
  '<Ctrl-Shift-s>',
];

for (var i = 0; i < unmapArray.length; i++) {
  unmap(unmapArray[i]);
}

settings.richHintKeystroke = 250;
settings.smoothScroll = false;
settings.newTabPosition = 'last';
settings.showModeStatus = true;
settings.enableAutpfocus = false;
Hints.characters = 'asdfjkl';

mapkey('[', 'Close tab on left',
       function() { RUNTIME("closeTabLeft"); },
       {repeatIgnore : true});
mapkey(']', 'Close tab on right',
       function() { RUNTIME("closeTabRight"); },
       {repeatIgnore : true});
mapkey('<Backspace>', '#4Go back in history',
       function() { history.go(-1); }, {repeatIgnore : true});

mapkey('<Alt-p>', 'Toggle pin tab',
       function() { RUNTIME("togglePinTab"); });

self.togglePinTab = function(message, sender, sendResponse) {
  chrome.tabs.query(
      {currentWindow : true, active : true}, function(tabs) {
        var tab = tabs[0];
        return chrome.tabs.update(tab.id, {pinned : !tab.pinned});
      });
};

addSearchAlias('s', 'searx', 'https://searx.me/?q=');
mapkey('ss', 'Search Selected with searx',
       function() { searchSelectedWith("https://searx.me/?q="); });
vmapkey('ss', 'Search Selected with searx',
        function() { searchSelectedWith("https://searx.me/?q="); });

if (document.origin === "https://drive.google.com") {
  unmapAllExcept([ '[', ']' ]);
}

if (document.origin === "https://docs.google.com") {
  unmapAllExcept([]);
}

if (document.origin === "https://www.google.com") {
    unmapAllExcept([ '[', ']' ]);
}

if (document.origin === "https://www.evernote.com") {
  unmapAllExcept([ '[', ']' ]);
}

settings.blacklistPattern = ".*dynalist.io/.*";
