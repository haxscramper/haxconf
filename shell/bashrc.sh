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



log_bash_command() {
  if [[ -n "$BASH_COMMAND" && "$BASH_COMMAND" != "trap" && "$BASH_COMMAND" != "log_bash_command" && ! "$BASH_COMMAND" =~ ^echo\ .* ]]; then
    echo "{\"command\": \"${BASH_COMMAND//\"/\\\"}\", \"directory\": \"$(pwd)\", \"time\": \"$(date +"%Y-%m-%d %H:%M:%S")\"}" >> ~/.cache/bash_logs.json
  fi
}

trap log_bash_command DEBUG


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/maxim_artemov/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/maxim_artemov/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/maxim_artemov/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/maxim_artemov/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

