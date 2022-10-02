#!/bin/sh

if [ -f "$HOME/.oh-my-zsh" ] 
then
    omz update
else
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi
