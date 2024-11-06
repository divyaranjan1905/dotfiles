#
# ~/.bash_profile
#

# Set up Guix Home profile
if [ -f ~/.profile ]; then . ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Merge search-paths from multiple profiles, the order matters.
eval "$(guix package --search-paths \
-p $HOME/.config/guix/current \
-p $HOME/.guix-home/profile \
-p $HOME/.guix-profile \
-p /run/current-system/profile)"

# Prepend setuid programs.
export PATH=/run/setuid-programs:$PATH

export LIBGL_ALWAYS_SOFTWARE=0
export LIBGL_DRI3_DISABLE=0
export VDPAU_DRIVER=nouveau
export LIBVA_DRIVER_NAME=nouveau


# emacs --daemon
