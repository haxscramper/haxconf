# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\W]\$ '

envsubst=$(which envsubst)
cat=$(which cat)

PATH=""

while read line; do
  PATH="$PATH:$line"
done < <($cat "$HOME/.config/haxconf/config/path_dirs.txt" | $envsubst)

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then
  source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"
fi
# END_KITTY_SHELL_INTEGRATION

# >>> xmake >>>
[[ -s "$HOME/.xmake/profile" ]] && source "$HOME/.xmake/profile" # load xmake profile
# <<< xmake <<<

source $HOME/.config/broot/launcher/bash/br
. "$HOME/.cargo/env"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/haxscramper/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/haxscramper/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/haxscramper/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/haxscramper/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

