#!/bin/sh

SETTING_DIR=$HOME/AppData/Roaming/Code/User

cp $SETTING_DIR/settings.json ./settings.json
cp $SETTING_DIR/keybinds.json ./keybinds.json

code --list-extensions > extensions