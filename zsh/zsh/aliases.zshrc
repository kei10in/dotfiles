#!/bin/zsh
# -*- coding: utf-8 -*-

##
# ZSH INITIALIZATION FILE
# Aliases file
##

alias a='alias'
alias ls='ls -Fv'
alias la='ls -Fa'
alias ll='ls -Fal'
alias rm='rm -i'
#alias rmdir='rmdir -i'
alias mv='mv -i'
alias cp='cp -i'
#alias less	'lv'

# for UTF-8 manual
alias man="LC_ALL=ja_JP.eucJP PAGER='lv -c -Ou8' man"

alias tgif='env LANG=ja_JP.eucJP tgif'
alias macemacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
#alias emacs='env XMODIFIERS=@im=none emacs'

if [ -r ${zsh_initdir}/aliases.hosts ]; then
	source ${zsh_initdir}/aliases.hosts
fi
