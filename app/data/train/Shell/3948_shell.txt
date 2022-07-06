
# The following lines were added by veto_pkg

veto_pkg() { sh /usr/bin/veto_packages $1 };

_veto_pkg() {
    words[1]=(pacman -Qi);
    (( CURRENT ++ ));
    service=pacman;
    _pacman
}; compdef _veto_pkg veto_pkg

# End of lines added by veto_pkg

