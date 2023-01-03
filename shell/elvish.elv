use epm
use re
use str
use file
use path

epm:install &silent-if-installed github.com/zzamboni/elvish-completions
use github.com/zzamboni/elvish-completions/vcsh
use github.com/zzamboni/elvish-completions/cd
use github.com/zzamboni/elvish-completions/ssh
use github.com/zzamboni/elvish-completions/builtins
use github.com/zzamboni/elvish-completions/git
use github.com/zzamboni/elvish-modules/util
use github.com/zzamboni/elvish-modules/alias


mkdir -p /tmp/hax-trash

set edit:insert:binding["Ctrl-["] = $edit:command:start~
set edit:command:binding["Ctrl-["] = $edit:command:start~

fn msg {|@a|
  echo -- $@a > /dev/tty
}

set E:TERM = xterm
set E:LANG = en_US.UTF-8
set E:LC_ALL = en_US.UTF-8
set E:LC_CTYPE = UTF-8
set E:HAX_CONFIG_DIR = ~/.config/haxconf
set E:HAX_CONFIG_FILES_DIR = $E:HAX_CONFIG_DIR/config
set E:NPM_PACKAGES = $E:HOME"/.npm-packages"
set E:EDITOR = nvim

set E:PATH = (
    cat $E:HAX_CONFIG_FILES_DIR/path_dirs.txt |
    envsubst |
    eawk {|path _| put $path } |
    str:join ":"
)

fn reload {
  msg "reloading elvish shell"
  exec elvish
}

var sequence = ""
fn cleanseq { set sequence = "" }
fn addseq {|key| set sequence = $sequence""$key }
fn setseq {|key| set sequence = $key }

set edit:insert:binding[Ctrl-t] = {
  try { edit:insert-at-dot (fd | fzf 2> /dev/tty) } catch e { }
}

set edit:insert:binding[Ctrl-w] = { edit:kill-small-word-left }

set edit:insert:binding[Ctrl-e] = {
  var temp-file = (path:temp-file)
  var name = $temp-file[name]".sh"
  print $edit:current-command > $name
  try {
    # This assumes $E:EDITOR is an absolute path. If you prefer to use
    # just the bare command and have it resolved when this is run use
    # (external $E:EDITOR)
    (external $E:EDITOR) $name < /dev/tty > /dev/tty 2>&1
    set edit:current-command = (slurp < $name)[..-1]
  } finally {
    file:close $temp-file
    rm $name
  }
}

set edit:insert:binding[Alt-Enter] = { edit:insert-at-dot "\n" }


set edit:command:binding["d"] = {
  if (==s $sequence "") {
    setseq "d"
  } elif (==s $sequence "d") {
    edit:kill-small-word-right
    cleanseq
  }
}

set edit:insert:binding[Alt-Up] = { edit:move-dot-up }
set edit:insert:binding[Alt-Down] = { edit:move-dot-down }
set edit:command:binding["B"] = { edit:move-dot-left-word }
set edit:command:binding["^"] = { edit:move-dot-sol }
set edit:command:binding[";"] = { edit:move-dot-right }
set edit:command:binding["l"] = { edit:move-dot-left }

set edit:command:binding["A"] = { edit:move-dot-eol ; edit:listing:close }
set edit:command:binding["I"] = { edit:move-dot-sol ; edit:listing:close }

set edit:command:binding["w"] = {
  if (==s $sequence "d") {
    edit:kill-small-word-right
    cleanseq
  } elif (==s $sequence "W") {
    edit:kill-word-right
    cleanseq
  }
}


set edit:prompt = { put (basename (pwd)) " ___\n" }
set edit:rprompt = { put '' }

set alias:arg-replacer = '<++>'

alias:new ls exa --sort type
alias:new lsl exa --long --header --git --sort type
alias:new lsa exa --all --long --header --git --sort type
alias:new lst exa --tree
alias:new lsta exa --tree --long --header --sort type --git --all
alias:new lstl exa --tree --sort type --git --all
alias:new cp e:cp -r
fn rm {|@args| e:mv --backup=numbered -vt /tmp/hax-trash $@args }

alias:new bat e:bat --no-pager
alias:new find-lib "ldconfig -p | grep -i"
alias:new vi nvim
alias:new ec emacsclient --nw
alias:new mkdir e:mkdir -p
alias:new ip e:ip -c

set edit:abbr['paci '] = 'sudo pacman -Sv '
set edit:abbr['pacl '] = 'pacman -Qqe '
set edit:abbr['pacr '] = 'sudo pacman -Rnsv '
set edit:abbr['pacu '] = 'sudo pacman -Syvu '
set edit:abbr['yaoi '] = 'yay  --noconfirm '
set edit:abbr['gis ']  = 'git status -s '
set edit:abbr['gisn ']  = 'git status -s . ; echo ; git branch -v ; echo ; git stash list '
set edit:abbr['gicm ']  = 'git commit -m "['
set edit:abbr['gil ']  = 'git log --graph --oneline --decorate -n20 '
set edit:abbr['gia ']  = 'git add '
set edit:abbr['gid ']  = 'git diff '
set edit:abbr['gids ']  = 'git diff --staged '
set edit:abbr['gipm ']  = 'git push --atomic origin master '
set edit:abbr['xcp '] = 'xclip -sel cli '
set edit:abbr['xcpi '] = 'xclip -sel cli -t image/png -i '
set edit:abbr['xco '] = 'xclip -out '
set edit:abbr['xcoi '] = 'xclip -sel cli -t image/png -o '

edit:add-var xcpp~ {
  try {
   find -maxdepth 2 -type f -print0 |
    fzf --read0 --multi --print0 |
    xargs -r0 realpath |
    xclip -sel cli
  } catch {
  }
}

edit:add-var doom-runexec~ {|@args|
  emacs --with-profile doom $@args
}

edit:add-var sdsa~ {
  var action = (echo "enable\ndisable\nstop\nstart\nrestart\nstatus" | fzf)
  var module = (systemctl list-unit-files | fzf | awk "{print $1}")
  set edit:current-command = "sudo systemctl "$action" "$module" --no-pager -l"
}

edit:add-var doom-debug~ {|@extra|
  var msg = "/tmp/emacs.messages"
  var wrn = "/tmp/emacs.warnings"
  /bin/rm -f $msg
  /bin/rm -f $wrn
  emacs ^
    --no-splash ^
    --no-window-system ^
    --with-profile doom ^
    $@extra ^
    --eval '(progn
  (when (get-buffer "*Warnings*") (with-current-buffer "*Messages*"
    (write-region (point-min) (point-max) "/tmp/emacs.messages")))
  (when (get-buffer "*Warnings*") (with-current-buffer "*Warnings*"
    (write-region (point-min) (point-max) "/tmp/emacs.warnings")))
  (message "Quitting emacs")
  (save-buffers-kill-emacs))'

  if ?(test -e $msg) { cat $msg }
  if ?(test -e $wrn) { cat $wrn }
}

fn gic {
  git checkout (str:trim (git branch -l --sort=committerdate | tac | fzf) ' *')
}

fn kt {
  e:kitty @ set-window-title kitty-target
}

fn cj {
  var target_dir = (cat $E:HAX_CONFIG_FILES_DIR/common_dirs.txt | envsubst | fzf | awk '{print $2}')
  if ?(test -d $target_dir) {
    cd $target_dir
  }
}

fn to-showlist {
  cat -n |
  eawk {|line num item|
    put [&to-filter=$num &to-accept=$item &to-show=$item]
  }
}

fn select-list {|cb caption|
  edit:listing:start-custom [($cb | to-showlist)] &accept=$put~ &caption=$caption
}

fn anyprefix {|str prefixes|
  for pref $prefixes {
    if (str:has-prefix $str $pref) {
      put true
      return
    }
  }
  put false
}

fn prompt-confirm {|prompt|
  var prompt = $prompt" [Y/n] "
  print $prompt > /dev/tty
  var resp = (util:readline)
  if (anyprefix $resp [y Y]) {
    true
  } elif (eq $resp '') {
    true
  } else {
    false
  }
}

fn prompt-create-file {|file content|
  if ?(test ! -e $file) {
    if (prompt-confirm "Create file?") {
      put $true
      e:echo -ne $content > $file
    } else {
      put $false
      return
    }
  } else {
    put true
  }
}

fn emacs-open {|path|
  emacsclient --eval "(find-file \""$path"\")"
}

fn cd-if-absolute {|file|
  if (str:compare $file[0] "/") { cd (dirname $file) }
}

fn nr_setup {|file|
  if (not (prompt-create-file $file "echo 1\n")) {
    return
  }
  var path = (realpath $file)
  emacs-open $path
  echo "Run using build"
  # e:kitty @ set-window-title "nimble-rebuild "$file
}

edit:add-var nr~ {|file|
  nr_setup $file
  cd-if-absolute $file
  var result = (str:trim-suffix $file ".nim")
  var cache = (str:trim-suffix (basename $file) ".nim")
  e:fd --type f . |
  e:entr -rc sh -c "time nim c -r -o:"$result".bin --nimcache:/tmp/nimcache/"$cache" "$file
}

fn inwait-cwd {
    try {
        put (inotifywait -q -e modify (fd --strip-cwd-prefix))
    } catch {
        put "none"
    }
}

fn inwait-cwd-loop {|callback|
    while (var res = (inwait-cwd)) {
        if (str:equal-fold $res "none") {

        } else {
            $callback
        }
    }
}

fn puml-rebuild {|file|
  var puml-content = '@startuml

@enduml'

  if (not (prompt-create-file $file $puml-content)) {
    return
  }

  emacs-open (realpath $file)
  fd -e puml | e:entr -rc plantuml -DPLANTUML_LIMIT_SIZE=200000 "-o"$E:PWD $file
}

fn py-rebuild {|file|
  var py-content = '#!/usr/bin/env python
'

  if (not (prompt-create-file $file $py-content)) {
    return
  }

  chmod +x $file
  emacs-open (realpath $file)
  fd | e:entr -rc sh -c './'$file
}

fn cpp-rebuild {|file|
  var cpp-content = '#include <iostream>

int main() {
  std::cout << "1\\n";
  return 0;
}'
  if (not (prompt-create-file $file $cpp-content)) {
    return
  }

  var opt-file = (str:trim-suffix $file ".cpp")".cfg"
  var res-file = (str:trim-suffix $file ".cpp")".bin"

  if ?(test -e $opt-file) {
    set opt-file = "@"$opt-file
  } else {
    set opt-file = ""
  }

  emacs-open (realpath $file)
  var opts = "-ferror-limit=1 -std=c++20 -g"
  # inwait-cwd-loop {
  #   clang++ $file -g -ferror-limit=2 -std=c++20 -o $res-file $opt-file
  #   try {
  #     ./$res-file
  #   } catch {
  #     lldb --batch -s ~/lldb-batch-script -- ./$res-file
  #   }
  # }

  fd | e:entr -rc sh -c "clang++ '"$file"' "$opts" -o '"$res-file"' '"$opt-file"' && ./"$res-file
}

fn dot-rebuild {|file|
  var dot-content = '
digraph G {
  node[shape=rect, fontname=Consolas];
  edge[fontname=Consolas];
  a -> b[label="text"];
}
'
  if (not (prompt-create-file $file $dot-content)) {
    return
  }

  var result = "/tmp/dot-rebuild-image.png"
  var tmp = (str:trim-suffix $file ".dot")
  dot -Tpng $file > $result
  sxiv $result &
  emacs-open (realpath $file)
  e:echo $file | e:entr -rc sh -c "cat "$file" | dot -Tpng > "$tmp" && cp "$tmp" "$result
}

fn elvish-rebuild {|file|
  if (not (prompt-create-file $file "echo 1")) {
    return
  }

  cd-if-absolute $file
  emacs-open $file
  e:echo $file | e:entr -rc elvish $file
}


fn nr1 {|file|
  nr_setup $file
  echo $file | e:entr -rc sh -c "time nim c -o:"$file".bin "$file" && ./"$file".bin"
}





fn nd {
  ~/.config/haxconf/scripts/nimble-rebuild.fish
}



fn fzf-map {|arg|
  var to-pipe = ""
  for item $arg {
    set to-pipe = $to-pipe""$item[key]" ::: "$item[val]"\n"
  }

  var selected = [(str:split " ::: " (echo $to-pipe | fzf))][0]

  for item $arg {
    if (==s $item[key] $selected) {
      put $item
      return
    }
  }
}

fn prompt-text {|question|
  print $question"\n  > " > /dev/tty
  put (util:readline)
}

fn opt-exec {|list|
  var result = []
  for item $list {
    if (==s "fn" (kind-of $item)) {
      set result = [$@result ($item)]
    } else {
      set result = [$@result $item]
    }
  }

  put $result
}

fn fzf-output {|call &trim=" " &desc="prompt"|
  var val = (str:join "\n" [($call)])
  var selected = (echo $val | fzf --select-1 --prompt $desc" > ")
  put (str:trim $selected $trim)
}

fn all-of {|list pred|
  for item $list {
    if (not ($pred $item)) { put $false ; return }
  }
  put $true
}

fn any-of {|list pred|
  for item $list {
    if ($pred $item) { put $true; return }
  }
  put $false
}

fn none-of {|list pred|
  for item $list {
    if ($pred $item) { put $false ; return }
  }
  put $true
}

eval (starship init elvish)
eval (carapace _carapace|slurp)
eval (zoxide init elvish | slurp)

fn call-navi {
  if (eq $edit:current-command '') {
    var answer = (str:join "\n" [(navi --print)])
    edit:replace-input $answer
  } elif (not (str:contains-any $edit:current-command '|')) {
    var answer = (str:join "\n" [(navi --print --query $edit:current-command)])
    if (not-eq $answer '') {
      edit:replace-input $answer
    }
  } else {
    var @cmds query = (str:split '|' $edit:current-command)
    var answer = (
      if (eq $query '') {
        navi --print
      } else {
        navi --print --query $query
      }
    )

    if (not-eq $answer '') {
      set cmds = [$@cmds $answer]
      edit:replace-input (str:join '| ' $cmds)
    }
  }
}

set edit:insert:binding[Ctrl-g] = { call-navi >/dev/tty 2>&1 }


fn br {|@arguments|
  var path = (mktemp)
  broot --outcmd $path $@arguments
  if ?(test -s $path) {
    eval (cat $path)
  }
  e:rm -f $path
}


fn erg {|glob search|
  rg -g "*."$glob --no-heading --line-number $search | sd '\(:\d+:\)' ':$1: '
}


fn rrg {|search|
  rg --no-heading --line-number $search | sd '\(:\d+:\)' ':$1: '
}
