# -*- mode: sh; -*-

__xterm-char-class () {
    # In xterm, double click selects a word. A word is defined as a
    # contiguous sequence of characters belonging to the same
    # character class. The character class for alphanumeric characters
    # is 48 (0 in ASCII). The following snippets adds common
    # characters to that character class to allow easy selection of
    # filenames and URLs.

    local chars=$1

    echo -n $chars | od -An -t uC | sed -r -e 's/  ([0-9][0-9])/\1:48_ /g' -e 's/_ $//' | tr _ ,
}

xrdb-reload () {
    xrdb -remove
    xrdb -load <(sed -e "s/FONT_NAME/$HOME_D_FONT_NAME/g" \
                     -e "s/FONT_SIZE/$HOME_D_FONT_SIZE/g" \
                     -e "s/COLOR_FG/$HOME_D_COLOR_FG/g"   \
                     -e "s/COLOR_BG/$HOME_D_COLOR_BG/g"   \
                     -e "s/XTERM_CHAR_CLASS/$(__xterm-char-class '!,%*-./?@=#:')/g" \
                     $HOME_D/x11/Xresources
                     )
}
