#!/bin/zsh
# -*- coding: utf-8 -*-

##
# Z shell INITIALIZATION FILE
# User shell preferences
##


umask 022            # files created are -rw-r--r--

typeset -xT HOST host
host=`hostname`
host=`echo $host | tr A-Z a-z`

typeset -xT USER user
user=`whoami`

export LANG=ja_JP.UTF-8
# export LC_ALL=${lang}
# export LC_COLLATE=
# export LC_CTYPE=
# export LC_MESSAGES=
# export LC_NUMERIC=
# export LC_TIME=

#dspmbyte=utf8


# User ID
if [ -z $uid ]; then
    uid=`id | cut -d = -f 2 | cut -d \( -f 1`
fi

# Non-root users
if [ ! $uid ]; then
    unset autologout
else
  # Avoid closing of Terminal windows when logged in as root
    if [ -n "${TERM_PROGRAM}" -a -n "${SHLVL}" ]; then
	    if [ "$SHLVL" -eq "1" ]; then
	        unset autologout
	    fi
    fi
fi


# Miscellaneous setup
if [ -z $version ]; then
    verion="unknown"
fi

