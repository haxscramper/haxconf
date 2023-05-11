##= Fish
set fish_greeting

alias reload="source ~/.config/fish/config.fish"
set -U fish_cursor_default line

set -g -x EDITOR 'nvim'
set -g -x VISUAL 'nvim'
set -g -x TERM "xterm"

##= Path
set -l DIR $HOME/.config/haxconf
set -g HAX_CONFIG_DIR $HOME/.config/haxconf
set -g HAX_CONFIG_FILES_DIR $HAX_CONFIG_DIR/config

set -l cat (which cat)
set -l envsubst (which envsubst)
set -g -x PATH "$HOME/bin"

for line in ($cat "$HAX_CONFIG_FILES_DIR/path_dirs.txt" | $envsubst)
    set -gx PATH $PATH $line
end

set -xg LD_LIBRARY_PATH /usr/local/lib/

alias navi="navi --dir ~/.config/navi-main"
alias naviadd="nvim (find $HAX_CONFIG_DIR/navi | fzf)"

##= Common utilities
alias ls="exa --sort type"
alias lsl="exa --long --header --git --sort type"
alias lsa="exa --all --long --header --git --sort type ^ /dev/null || exa --long --header --sort type"
alias lst="exa --tree"
alias lsta="exa --tree --long --header --sort type --git --all"
alias lstl="exa --tree --sort type --git --all"

alias :q="clear"
alias rm="rm -r"
alias cp="cp -r"
alias srm="sudo rm -rf"
alias ping-test="ping 8.8.8.8"

alias ncdu="ncdu --color dark -x --exclude .git --exclude node_modules"
alias find-lib="ldconfig -p | grep -i"


function read_confirm \
    -a 'prompt'
    while true
        read -l -P "$prompt [Y/n] " confirm

        switch $confirm
            case '' Y y
                return 0
            case N n
                return 1
        end
    end
end

alias vi="nvim"
alias l="exa -al --sort type"
function c --argument-names 'target_dir'
    if test -n "$target_dir"
        cd "$target_dir"
    else
        colecho -w:1 "Missing directory"
    end
end

function cmkd -a 'dir'
    set -l msg (get_msg_printer)

    if test -d "$dir"
        $msg -i "Target directory exists"
    else
        mkdir -p "$dir"
        $msg -w "Created directory $dir"
    end

    cd $dir
end

function move_to_trash
    set -l trash_dir "/tmp/hax-trash"
    mkdir -p $trash_dir
    set -l mktemp_template (date +%d_%H:%M:%S)-XXXXXX
    if test (count $argv) -le 1 # Only one target
        set -l target $argv[1]
        if test -e $trash_dir/$target
            # Trash directory name is alreay ocuppied
            set -l new_target (mktemp -t "$target-$mktemp_template" -p "$trash_dir")

            remove $new_target
            mkdir -p (parse-path --dirname $new_target)
            mv $target $new_target
            echo $trash_dir/$target
        else
            # No such file or directory in trash dir
            mkdir -p (parse-path --dirname $trash_dir/$target)
            mv $target $trash_dir/$target
            echo $trash_dir/$target
        end
    else
        set -l tmp_dir (mktemp -d -t $mktemp_template -p "$trash_dir")
        mv $argv $tmp_dir
        echo $tmp_dir
    end
end

function remove_or_trash
    set -l rm_cmd (which rm)
    set -l max_mb_size 50
    set -l max_trash_size (math "1024 * 2014 * $max_mb_size")
    set -l msg (get_msg_printer)

    # TODO test agains command substitution
    if test (count $argv) -gt 1
        set target_size (du -sb $argv[1] | cut -f1)
    else
        set target_size (du -sb $argv | cut -f1 | paste -sd+ | bc)
    end

    set -l target_size_mb (math "$target_size / (1024 * 1024)")

    if test $target_size_mb -le $max_trash_size
        $msg -i "Target size is smaller than "$max_mb_size"MB ("$target_size_mb"MB)"
        set -l dest (move_to_trash $argv)
        $msg "Moved to $dest"
    else
        $msg -i "Target size is bigger than "$max_mb_size"MB ("$target_size_mb"MB)"
        $msg -i "deleting"
        if read_confirm "Delete permanently?"
            echo -ne '#####                     (33%)\r'
            sleep 1
            echo -ne '#############             (66%)\r'
            sleep 1
            echo -ne '#######################   (100%)\r'
            echo -ne '\n'
            $rm_cmd -r $argv
        end
    end
end

function fancy_sleep -a sleep_ticks -a prefix
    for i in (seq 1 $sleep_ticks)
        sleep 0.125
        echo -ne "$prefix $i/$sleep_ticks\r"
    end
    echo ""
end

function sshmove \
    -a from \
    -a to \
    -d "Move 'from' to 'to' over ssh. "
    set -l msg (get_msg_printer)
    $msg -i "Moving $from to $to"

    # TODO fuzzy select list of possible addresses to move to (ip
    # address can be taken from ~~/.ssh/ssh_config~)

    # TODO fuzzy select user from list of users and use it to open
    # file from path

    # TODO perform caching of all users on machines in network and
    # directories on the machines. Store data on local machine and use
    # for faster completion generation

    # TODO How to run multiple ssh commands (~scp~ followed by ssh
    # command) without having to login multiple times?
end

# IDEA https://gist.github.com/dohq/1dc702cc0b46eb62884515ea52330d60

# function fzf-ssh () {
#     local selected_host=$(grep "Host " ~/.ssh/ssh_config | grep -v '*' | cut -b 6- | fzf --query "$LBUFFER" --prompt="SSH Remote > ")
#
#     if [ -n "$selected_host" ]; then
#         BUFFER="ssh ${selected_host}"
#         zle accept-line
#         fi
#         zle reset-prompt
# }

function mvbk \
    -d "mv $1 $1.bak"
    set -l msg (get_msg_printer)
    for entry in $argv
        # TODO use version numbers
        mv "$entry" "$entry.bak"
        $msg "$entry -> $entry.bak"
    end
end

function d
    set -l msg (get_msg_printer)
    if test (count $argv) -gt 1
        $msg "More than one target to remove"
        for file in $argv
            $msg -I:4 "$file"
        end

        if read_confirm "Delete files?"
            remove_or_trash $argv
        else
            $msg -i "delete aborted"
        end
    else
        set -l target (string replace -r '/$' "" -- $argv[1])
        $msg "Target is '$target'"

        if begin
            test -d "$target" || \
            test -f "$target" || \
            test -L "$target"
        end
            remove_or_trash "$target"
        else
            $msg -e "can't delete '$target'"
            if test -e $target
                $msg "unknown type"
            else
                $msg "does not exist"
            end
        end
    end
end

# IDEA safer dd wrapper

# commandline -r "sudo dd if="(fd -e iso | fzf)" of=/dev/"(lsblk
# -dn | awk '{print $1}' | fzf)" conf=fdatasync status=progress"

function systemd_select_action
    set -l action (echo -en "enable\ndisable\nstop\nstart\nrestart\nstatus" | fzf)
    set -l module (systemctl list-unit-files | fzf | awk '{print $1}')
    commandline -r "sudo systemctl $action $module"
end

alias sdsa="systemd_select_action"
alias em="emacsclient"

##== CD
set -g indir ~/defaultdirs/input

##== Package managers
##=== Arch
abbr paci sudo pacman -Sv
abbr pacl pacman -Qqe
abbr pacr sudo pacman -Rnsv
abbr pacu sudo pacman -Syvu
abbr yaoi yay  --noconfirm
##=== Gentoo
abbr emi sudo emerge -atv
abbr emio sudo emerge -atv --oneshot
abbr ems emerge --search
abbr emwd sudo emerge -atv --changed-use --deep @world
abbr emp emerge --pretend --columns
abbr emdc sudo emerge --depclean
abbr emd sudo dispatch-conf
abbr emr sudo emerge -atv --unmerge
abbr emu sudo emerge --update --newuse --newuse @worl
abbr layadd sudo layman -o http://gpo.zugaina.org/lst/gpo-repositories.xml -f -a


##= Abbriveations
##== Git
abbr gis git status -s \&\& echo \&\& git branch -v \&\& echo \&\& git stash list
abbr gisn git status -s . \&\& echo \&\& git branch -v \&\& echo \&\& git stash list
abbr gicm git commit -m \"\[ # Complete comment
abbr gil git log --graph --oneline --decorate -n20
abbr gia git add
abbr gid git diff
abbr gids git diff --staged
abbr gitf git fixup
abbr gipm git push --atomic origin master
abbr gicl git clone \(xclip -out\)

function fish_mode_prompt --description 'Displays the current mode'
    # Move vim prompt drawing into prompt function
end

function print_vi_mode_indicator
    if test "$fish_key_bindings" = "fish_vi_key_bindings"
        switch $fish_bind_mode
            case default
                set_color --bold red
                printf "N"
            case insert
                set_color --bold green
                printf "I"
            case replace-one
                set_color --bold green
                printf "R"
            case visual
                set_color --bold brmagenta
                printf "V"
        end
        set_color normal
    end
end

function fish_prompt
    set -l last_exit $status
    echo ""

    printf " "
    print_vi_mode_indicator
    echo ""

    if [ "$last_exit" = "0" ]
        echo " > "
    else
        echo $last_exit" > "
    end
end

function fish_right_prompt
    echo ""
end

function gifi -d "Fuzzy select gitignore extensions to ignore"
    set -l tmpfile (mktemp)

    fd  --type f |
    xargs -i parse-path --all-suffixes {} |
    sort |
    uniq |
    sed 's/^/*./' >> $tmpfile

    find -maxdepth 1 -type d |
    sed 's!^./!!' |
    sed 's!$!/*!' >> $tmpfile

    cat $tmpfile |
    fzf -m --header="Select extensions to ignore" |
    tee /dev/tty >> .gitignore

    /usr/bin/rm $tmpfile
end

function gifa -d "Fuzzy select gigignore extensions to allow"
    find -type f |
    xargs -i parse-path --all-suffixes {} |
    sort |
    uniq |
    sed 's/^/!*./' |
    fzf -m --header="Select extensions to allow" |
    tee /dev/tty >> .gitignore
end


##== Utility

#########################  nim autocomplilation  ##########################


function nd -d "Rebuild nimble project on each file change and run tests"
    ~/.config/haxconf/scripts/nimble-rebuild.fish
end

function nimc -a 'file' -d "Rebuild nimble file on each change"
    set compile_command "nim c -o:$file.bin $file"
    fd --type f . | grep -v -F ".tmp." | grep -vF ".bin" |
    entr -rc sh -c "colecho 'Compiling using $compile_command' && $compile_command"
end

function nr -a 'file' -d "Rebuild nimble file on each change and run it"
    set -l msg (get_msg_printer)
    if test ! -e "$file"
        if read_confirm "create file?"
            echo -ne "echo 1\n" > "$file"
            chmod +x "$file"
        else
            return 1
        end
    end

    set path (realpath "$file")
    emacsclient --eval "(find-file \"$path\")"
    $msg "run using build"
    fd --type f . | grep -v -F ".tmp" | grep -vF ".bin" |
    tee -a /dev/tty |
    entr -rc sh -c "time nim c -o:$file.bin $file && ./$file.bin"
end

function pview
    set -l pdffile (find -iname "*.pdf" | fzf)

    if test ! -z $pdffile
        zathura $pdffile &
        disown
    end
end


function viGrep -a stringToSearch
  nvim -c (grep -rHn -F $stringToSearch |  \
      cut -d: -f1-2 | \
      grep -v 'matches$' | \
      perl -pe 's/^(.*?):(\d+)$/:tabnew \1|:\2|/' | \
      tr '\n' ' ')
end


function viFd -a ext
    nvim -c (fd -e $ext |  \
        perl -pe 's/^(.*?)$/:tabnew \1|:0|/' | \
        tr '\n' ' ')
end


function shr -a 'file' -d "Run shell file on each change"
    set -l msg (get_msg_printer)
    if test ! -e "$file"
        if read_confirm "create file?"
            echo -ne "#!/usr/bin/env bash" > "$file"
            chmod +x "$file"
        else
            return 1
        end
    end

    echo "$file" | entr -rc sh -c "./$file"
end

function build_java -a file -d "Rebuild java file on each change"
    set -l msg (get_msg_printer)
    set -x input_file "$file"
    set -x build_file .$input_file.tmp.out.d/out.jar
    set -l build_script build.java.java_preprocessor.sh
    set -x fsm_dir ~/.config/fsm-build

    $msg -i "Building file $input_file using $buils_script"

    fd -e java | entr -rc $build_script build
end

function rename-last -a name -d "Change name of the newes file in directory"
    set -l last_name (exa --sort time | tail -1)
    set -l extension (parse-path --all-suffixes $last_name)
    mv --no-clobber --verbose $last_name $name"."$extension
end

#alias git="set -x LD_PRELOAD $HOME/.config/hax-software/GitBSLR/gitbslr.so && git"
abbr xcp xclip -sel cli # Copy stdin to clipboard
abbr xcpi xclip -sel cli -t image/png -i # Copy image from file
abbr xco xclip -out # Output clipboard as string
abbr xcoi xclip -sel cli -t image/png -o # Output image from clipboard
alias xcpp="find -maxdepth 2 -type f -print0 | fzf --read0 --multi --print0 | xargs -r0 realpath | xclip -sel cli"
alias xcppu="find -type f -print0 | fzf --read0 --multi --print0 | xargs -r0 realpath | xclip -sel cli"
abbr svi sudo -E nvim
abbr cgrep env LANG=C grep
abbr fgrep env LANG=C grep -F # Faster grep for ASCII files
abbr move mv --verbose --no-clobber --
abbr agvic nvim -p \(ag -l # Complete ag patter to open all files in vim
abbr pl perl -lane \'
abbr pawk perl -nae \'
abbr plF perl -leF// \'
abbr entrsh entr sh -c \"clear \&\& colecho -i1 'Running entr ...' \&\&
abbr mvfindir find -type f -exec mv -v -- {} \$indir \\\;
abbr mt mv -t
abbr mvt mv -t /tmp
abbr xmv xargs -d \'\\n\' -I \'{}\' mv -v \'{}\'
abbr xmv0 xargs -0 -I \'{}\' mv -v \'{}\'
abbr mvdupes fdupes -rf . \|
abbr mvf find -type f -print0 \|
abbr mve fd -e
abbr pathgr echo \$PATH \| tr \' \' \'\\n\' \| grep
abbr pyenv virtualenv -p=/usr/bin/python venv \&\& source venv/bin/activate.fish
abbr frnm for f in \*.ext \n mv \$f \(echo \$f \| sed \'s/CADP_//\'\)\nend
abbr gckb git clone \(xclip -out\)
abbr p3i pip3 install --user
abbr ipa ip addr
abbr cnt ~/workspace/hack/testing/nim
abbr cnd ~/workspace/hax-nim
# abbr gidoc git add . && git commit -m "[DOC]"
alias fde="fd -I -e"
alias mpvl="mpv (exa --sort time | tail -1)"
alias move-last="mv --no-clobber --verbose (exa --sort time | tail -1)"
alias ec="emacsclient --nw"

alias dconta="docker container ls --all --format 'table {{ .Names }}\t{{ .Command }}\t{{ .Image }}'"
alias dcont="docker container ls --format 'table {{ .Names }}\t{{ .Command }}\t{{ .Image }}'"
alias dimg="docker image ls --all --format 'table {{ .Repository }}\t{{ .Tag }}\t{{ .ID }}'"

function colecho_simple \
    -d "Simplfied version of colecho without coloring"
    echo "$argv"
end

# TODO implement
function colorize_string \
    -d "Add ansi escape sequences to string"

    set -l selector "$argv[1]"
    set -l color "$argv[2]"
    set -l style "\\033["

    switch $selector
        case "fg"
            switch $color
                case '*'
                    set style "0;"
            end
        case "bg"
            switch $color
                case '*'
                    set style "0;"
            end
        case '*'
            echo "undefined"
   end

   echo -ne "$style" $argv[3..-1]
end

function upload_file  \
    -d "Upload file to transfer.sh" \
    --argument-names 'file'

    if not test -n "$file"
        colecho -e:2 "Missing file option"
    else
        set -l basefile (basename "$file" | sed -e 's/[^a-zA-Z0-9._-]/-/g')
        set -l tmpfile (mktemp)
        set -l msg (get_msg_printer)

        curl \
          --progress-bar \
          --upload-file \
          "$file" "https://transfer.sh/$basefile" >> "$tmpfile"

        set -l url (cat "$tmpfile")

        string join0 "wget -O \"$basefile\" \"$url\"" "$url" |
        fzf --read0 --height=20% --header "Select what to copy to clipboard" > $tmpfile

        set -l result (cat $tmpfile)

        $msg "copied $result"
        echo $result | xclip -sel cli

        remove_file $tmpfile
    end
end

function cj -d "Jump to one of the commonly used directories"
    set -l target_dir (cat "$HAX_CONFIG_FILES_DIR/common_dirs.txt" | envsubst | fzf | awk '{print $2}')
    if test -n "$target_dir"
        cd "$target_dir"
    end
end

function remove_file -d "remove single file" --argument-names 'file'
    set -l rm_cmd (which rm)
    $rm_cmd "$file"
end

function remove
    set -l rm_cmd (which rm)
    $rm_cmd -r $argv
end


function pt -d "Portage operations"
    commandline (cat "$HAX_CONFIG_DIR/portage_commands.txt" |
    envsubst |
    fzf |
    awk -F: '{print $2}')
end

function taf -d "Add tag to the file name"
    set -l new_name (tag_file.pl $argv[1] $argv[2])
    mv -- $argv[1] $new_name
    colecho -i3 -- $new_name
end

function get_msg_printer -d "Get command for printing messages"
    set -l colecho_path (which colecho)
    if [ "$colecho_path" = "" ]
        echo "colecho_simple"
    else
        echo "colecho"
    end
end

function entr_file -d "Run comman on each file change.
    First argument is a file to listen to"
    set -l msg (get_msg_printer)
    $msg -i1 "Starting entr on file"
    set -l last (count $argv)
    set -l abs_path (realpath $argv[1])
    set -l pre "clear && echo -e '\n\n\n\n\n\n\n\n' && $msg 'Running entr ...' && $msg '$abs_path'"
    echo $argv[1] | entr sh -c "$pre && $argv[2..$last]"
end


function entr_run -d "Run file on each change (./file)"
    set -l msg (get_msg_printer)
    $msg -i1 "Starting entr on file"
    set -l last (count $argv)
    set -l abs_path (realpath $argv[1])
    set -l pre "clear && $msg 'Running entr ...' && $msg '$abs_path'"
    set -l file $argv[1]
    if [ $file = "Makefile" ]
        fd --type f | grep -v '.*\.bin' | entr -rc sh -c "$pre && make"
    else
        fd --type f | entr -rc sh -c "$pre && ./$argv[1]"
    end
end

function entr_build -d "Build and run file on each change"
    set -l msg (get_msg_printer)
    if [ (count $argv) -lt 1 ]
        $msg -e2 "File name required"
    else
        $msg -i1 "Starting entr on file"
        set -l file $argv[1]
        set -l build_command (
            m4 \
            -D'build_file'="$file.bin" \
            -D'input_file'="$file"     \
                $HAX_CONFIG_DIR/build_commands.txt.m4 |
            awk -F% '{print $2;}' |
            fzf)

        echo $file | entr -rc sh -c "clear && $msg 'Running entr on $file ...' && \
        $build_command && ./$file.bin && $msg 'Completed run' "
    end
end

function entr_test -d "Run test.sh on each file change"
    set -l msg (get_msg_printer)
    echo -e "test.sh\n$argv[1]" | entr -rc sh -c "clear && $msg \
    'Running entr ...' && ./test.sh"
end

function adoc_file -d "Run asciidoc(tor) on each file change"
    set -l msg (get_msg_printer)
    $msg -i1 "Starting entr on file"
    echo $argv[1] | entr sh -c "clear && $msg 'Running entr on \
    $argv[1] ...' && \asciidoc$argv[2] $argv[1]"
end

abbr icat kitty +kitten icat

function my_bindings
    fish_vi_key_bindings
    fzf_key_bindings
    # fish_user_key_bindings
    # bind \cE 'echo "hello"'
end

set -g fish_key_bindings my_bindings

##== Rust
#set -g -x PATH $HOME/.cargo/bin $PATH
#set -g RUST_SRC_PATH (rustc --print sysroot)"/lib/rustlib/src/rust/src"

abbr jlab cd \~ \&\& jupyter lab --no-browser
abbr jlabf cd \~ \&\& jupyter lab --browser firefox
function anaconda_venv
    eval $HOME/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end

eval (perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)

function use_java_11_first
    set -l java_11_bin (
    echo $PATH                                    |
    tr ' ' '\n'                                   |
    xargs -i find '{}' -name "javac" 2> /dev/null |
    grep 11                                       |
    xargs -i parse-path --dirname '{}'            |
    uniq)

    set -g -x PATH $java_11_bin $PATH
end

function man -d "colored manpages"
    begin
        set -lx LESS_TERMCAP_mb (printf "\e[1;31m")
        set -lx LESS_TERMCAP_md (printf "\e[1;31m")
        set -lx LESS_TERMCAP_me (printf "\e[0m")
        set -lx LESS_TERMCAP_se (printf "\e[0m")
        set -lx LESS_TERMCAP_so (printf "\e[1;44;33m")
        set -lx LESS_TERMCAP_ue (printf "\e[0m")
        set -lx LESS_TERMCAP_us (printf "\e[1;32m")
        /bin/man $argv
    end
end

function erg -a "glob" -a "search"
    rg -g "*."$glob --no-heading --line-number -- $search
end

function rrg  -a "search"
    rg --no-heading --line-number -- $search
end


########################  ffmpeg and audio/video  #########################

function ffm.extract-range \
    -a source_file \
    -a result_file \
    -a start_time \
    -a duration \
    -a hour \
    -d "Copy range from source file to result file

    Copy range [start, start+duration] from source file to result file.
    `start_time` and `result_time` might be in form of `hh:mm:ss` or
    `mm:ss` or `ss`. If prefix is omitted `00` will be used
    (`01 -> 00:00:00`)."

    set hour_st "00"
    set minute_st "00"
    set second_st "00"

    set hour_diff "00"
    set minute_diff "00"
    set second_diff "00"

    colecho "Input is [$start_time; $start_time + $duration]"

    if test -e $source_file
        colecho "Input file: $source_file result: $result_file"
    else
        colecho -e "Input file missing"
        exit 1
    end

    begin
      set split (string split ":" -- $start_time)
      if test (string match '??:??:??' $start_time)
          colecho "Start time is specified in hh:mm:ss format"
          set hour_st $split[1]
          set minute_st $split[2]
          set second_st $split[3]

      else if test (string match '??:??' -- $start_time)
        colecho -w "Start time is specified in mm:ss format, assuming hh=00"
        set minute_st $split[1]
        set second_st $split[2]

      else if test (string match '??' -- $start_time)
        colecho -w "Start time is specified in ss format, assuming hh=00, mm=00"
        set second_st $split[1]
      end
    end

    begin
        set split (string split ":" -- $duration)
        if test (string match '??:??:??' $duration)
            colecho "Duration is specified in hh:mm:ss format"
            set hour_diff $split[1]
            set minute_diff $split[2]
            set second_diff $split[3]

        else if test (string match '??:??' -- $duration)
            colecho -w "Duration is specified in mm:ss format, assuming hh=00"
            set minute_diff $split[1]
            set second_diff $split[2]

        else if test (string match '??' -- $duration)
            colecho -w "Duration is specified in ss format, assuming hh=00, mm=00"
            set second_diff $split[1]
        end
    end

    set start_time "$hour_st:$minute_st:$second_st"
    set duration "$hour_diff:$minute_diff:$second_diff"

    colecho -i "Extracting time from $start_time + $duration"
    set command "ffmpeg -i '$source_file' -vcodec copy -ss $start_time -t $duration '$result_file'"
    colecho -w "Running `$command`"
    ffmpeg -loglevel warning -i "$source_file" -ss $start_time -t $duration "$result_file"
end

############################  snapshot/backup  ############################

## Location of the original configuration file
set -xg snapshot_orig_file $HOME/.config/haxconf/cli/rsnapshot.conf

function gen_snapshot_conf -d "Generate snapshot file with env substition applied"
    set -l tmpsnap (mktemp "/tmp/rsnap.XXXXXXXXX")
    cat $snapshot_orig_file | envsubst > $tmpsnap
    echo -n $tmpsnap
end

function rsnapt -d "Teest backup configuration"
    set -l conf (gen_snapshot_conf)
    rsnapshot -c $conf -t main
end


function rsnap -d "Run backup configuration"
    set -l conf (gen_snapshot_conf)
    colecho -w "Using auto-generated snapshot configuration"
    colecho "path: $conf"
    rsnapshot -c $conf main
end


########################  script creation helper  #########################

function vish -a 'file_name' -d 'Create script file and open in in vi'
    if test -e $file_name
        vi $file_name
    else
        echo "#!/usr/bin/env bash" >> $file_name
        echo "# -*- coding: utf-8 -*-" >> $file_name
        echo "# bash" >> $file_name
        echo "set -o nounset" >> $file_name
        echo "set -o errexit" >> $file_name

        chmod +x $file_name

        vi $file_name
    end
end

starship init fish | source
zoxide init fish | source

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /home/haxscramper/anaconda3/bin/conda
    eval /home/haxscramper/anaconda3/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<

