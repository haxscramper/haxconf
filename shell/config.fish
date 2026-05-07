##= Fish
set fish_greeting

alias reload="source ~/.config/fish/config.fish"
set -U fish_cursor_default line

set -g -x EDITOR 'nvim'
set -g -x VISUAL 'nvim'
set -g -x TERM "kitty"

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
alias rm="mv --backup-numbered -vt /tmp/hax-trash"
alias cp="cp -r"

alias ncdu="ncdu --color dark -x --exclude .git --exclude node_modules"

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

##== Utility

#########################  nim autocomplilation  ##########################

#alias git="set -x LD_PRELOAD $HOME/.config/hax-software/GitBSLR/gitbslr.so && git"
abbr xcp xclip -sel cli # Copy stdin to clipboard
abbr xcpi xclip -sel cli -t image/png -i # Copy image from file
abbr xco xclip -out # Output clipboard as string
abbr xcoi xclip -sel cli -t image/png -o # Output image from clipboard
alias xcpp="find -maxdepth 2 -type f -print0 | fzf --read0 --multi --print0 | xargs -r0 realpath | xclip -sel cli"
alias xcppu="find -type f -print0 | fzf --read0 --multi --print0 | xargs -r0 realpath | xclip -sel cli"
abbr entrsh entr sh -c \"clear \&\& colecho -i1 'Running entr ...' \&\&
abbr mvfindir find -type f -exec mv -v -- {} \$indir \\\;
abbr pathgr echo \$PATH \| tr \' \' \'\\n\' \| grep
abbr frnm for f in \*.ext \n mv \$f \(echo \$f \| sed \'s/CADP_//\'\)\nend
abbr gckb git clone \(xclip -out\)
abbr p3i pip3 install --user

abbr icat kitty +kitten icat

function my_bindings
    fish_vi_key_bindings
    fzf_key_bindings
    # fish_user_key_bindings
    bind \co edit_command_buffer
    bind -M insert \co edit_command_buffer
end

set -g fish_key_bindings my_bindings


set -g __cmdlog_file ~/defaultdirs/notes/personal/automatic_export/command_list.jsonl
set -g __cmdlog_counter_file ~/.local/share/fish/cmdlog.counter

mkdir -p (dirname $__cmdlog_file)
test -f $__cmdlog_counter_file; or echo 0 > $__cmdlog_counter_file

function __cmdlog_append --on-event fish_postexec
    set -l cmd $argv[1]
    test -n "$cmd"; or return

    printf '{"command":%s,"count":1}\n' \
        (printf '%s' "$cmd" | jq -Rsa .) \
        >> $__cmdlog_file

    set -l n (math (cat $__cmdlog_counter_file) + 1)
    echo $n > $__cmdlog_counter_file

    if test $n -ge 200
        hax_cmdlog_compact.py $__cmdlog_file
        echo 0 > $__cmdlog_counter_file
    end
end

function __cmdlog_export_history
    test -f ~/.local/share/fish/fish_history; or return

    hax_cmdlog_history_edit.py archive-fish-history \
        ~/.local/share/fish/fish_history \
        $__cmdlog_file

    echo 0 > $__cmdlog_counter_file
end

function __cmdlog_restore_history
         # --on-event fish_prompt
    functions -e __cmdlog_restore_history

    test -f $__cmdlog_file; or return

    ~/.local/bin/cmdlog_history.py read-fish-history-archive \
        $__cmdlog_file \
        ~/.local/share/fish/fish_history
end

########################  ffmpeg and audio/video  #########################

starship init fish | source
zoxide init fish | source
direnv hook fish | source

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/miniconda3/bin/conda
    eval /opt/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end
# <<< conda initialize <<<


set -g -x PYTHON_KEYRING_BACKEND keyring.backends.null.Keyring
