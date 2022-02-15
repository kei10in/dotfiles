#!/usr/bin/env bash

SCRIPT_DIR=$(cd $(dirname $0); pwd) 

ZSH_DIR=${SCRIPT_DIR}/../zsh

ln -sf  ${ZSH_DIR}/zshenv ~/.zshenv
ln -sf  ${ZSH_DIR}/zshrc ~/.zshrc
ln -sf  ${ZSH_DIR}/zsh ~/.zsh
