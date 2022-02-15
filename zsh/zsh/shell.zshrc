#!/bin/zsh
# -*- coding: utf-8 -*-

##
# Z shell INITIALIZATION FILE
# User preferences file
##

## zfunc
fpath+=~/.zfunc

# Keybind
bindkey -e

## Set up prompt
autoload -U colors; colors
setopt prompt_subst
setopt prompt_percent
setopt transient_rprompt

autoload -Uz vcs_info
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats " %F{green}(%c%u%b)%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd() { vcs_info }
# Prompt Format
# ${CURRENT_DIR}
# [${USER_NAME}@${HOSTNAME}]%#
prompt_current_directory="%{${fg_bold[blue]}%}%~%{${reset_color}%}"
prompt_host="%{${fg_bold[cyan]}%}%n%{${reset_color}%}%{${fg_bold[red]}%}@%m%{${reset_color}%}"
prompt_bar="${prompt_current_directory}"'${vcs_info_msg_0_}'
prompt_left="[${prompt_host}]%# "
PROMPT="${prompt_bar}"$'\n'"${prompt_left}"
#PROMPT="[%m:%3C] %n%#%b "
PROMPT2="%_ -> "	# if/while prompt
PROMPT3="OK? %r? "	# Spell correction prompt
#SPROMPT="%r is correct? [n,y,a,e]: "
SPROMPT="OK? %r? "


## History
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
HISTFILE=~/.zsh_history
HISTSIZE=10000              # Number of commands saved as history
SAVEHIST=10000
setopt hist_ignore_dups     # Ignore duplication command history list
setopt hist_ignore_space    # Don't add history the command starting with space
setopt share_history        # Share command history data

## Word Completions
autoload -U compinit        # Use word completions
compinit
fignore=( \~ .bak .o .bin RCS CVS )
setopt autolist             # Expand history when completing
setopt recexact             # Correct spelling when completing

## pushd/popd/cd
setopt autocd               # Auto change directory
setopt autopushd            #

## Random
setopt correct
setopt longlistjobs
# setopt rmstarwait         # Wait 10 seconde before doing 'rm *' or 'rm path/*'
setopt ignoreeof            # Don't exit by end of file
