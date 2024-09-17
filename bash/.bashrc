#~/.bashrc

# Set up Guix Home profile
# if [ -f ~/.profile ]; then . ~/.profile; fi

# Honor per-interactive-shell startup file
# if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Merge search-paths from multiple profiles, the order matters.
# eval "$(guix package --search-paths \
# -p $HOME/.config/guix/current \
# -p $HOME/.guix-home/profile \
# -p $HOME/.guix-profile \
# -p /run/current-system/profile)"


export XDG_DATA_DIR='/home/divya/.local/share/flatpak/exports/share'
#VARIABLES
export BROWSER="nyxt"
export EDITOR="vim"
export VIDEO="mpv"
export MATH="/mnt/LDisk-E/Albert Einstein/Books & Resources/MIT OCW/Mathematics/"
export EINSTEIN="/mnt/LDisk-E/Albert Einstein/"
export PHIL="/mnt/LDisk-E/Albert Einstein/Books & Resources/Philosophy & Psychology/Philosophy/"
export PSYCH="/mnt/LDisk-E/Albert Einstein/Books & Resources/Philosophy & Psychology/Psychology/"

# This is the list for lf icons:
export LF_ICONS="di=📁:\
	fi=📃:\
	tw=🤝:\
	ow=📂:\
	ln=⛓:\
	or=❌:\
	ex=🎯:\
	*.txt=✍:\
	*.mom=✍:\
	*.me=✍:\
	*.ms=✍:\
	*.png=🖼:\
	*.webp=🖼:\
	*.ico=🖼:\
	*.jpg=📸:\
	*.jpe=📸:\
	*.jpeg=📸:\
	*.gif=🖼:\
	*.svg=🗺:\
	*.tif=🖼:\
	*.tiff=🖼:\
	*.xcf=🖌:\
	*.html=🌎:\
	*.xml=📰:\
	*.gpg=🔒:\
	*.css=🎨:\
	*.pdf=📚:\
	*.djvu=📚:\
	*.epub=📚:\
	*.csv=📓:\
	*.xlsx=📓:\
	*.tex=📜:\
	*.md=📘:\
	*.r=📊:\
	*.R=📊:\
	*.rmd=📊:\
	*.Rmd=📊:\
	*.m=📊:\
	*.mp3=🎵:\
	*.opus=🎵:\
	*.ogg=🎵:\
	*.m4a=🎵:\
	*.flac=🎼:\
	*.wav=🎼:\
	*.mkv=🎥:\
	*.mp4=🎥:\
	*.webm=🎥:\
	*.mpeg=🎥:\
	*.avi=🎥:\
	*.mov=🎥:\
	*.mpg=🎥:\
	*.wmv=🎥:\
	*.m4b=🎥:\
	*.flv=🎥:\
	*.zip=📦:\
	*.rar=📦:\
	*.7z=📦:\
	*.tar.gz=📦:\
	*.z64=🎮:\
	*.v64=🎮:\
	*.n64=🎮:\
	*.gba=🎮:\
	*.nes=🎮:\
	*.gdi=🎮:\
	*.1=ℹ:\
	*.nfo=ℹ:\
	*.info=ℹ:\
	*.log=📙:\
	*.iso=📀:\
	*.img=📀:\
	*.bib=🎓:\
	*.ged=👪:\
	*.part=💔:\
	*.torrent=🔽:\
	*.jar=♨:\
	*.java=♨:\
	"

##Enabling VI Mode
set -o vi

##PATH

if [ -d "$HOME/.bin" ] ;
then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/Applications" ] ;
then PATH="$HOME/Applications:$PATH"
fi

##ALIASES##

alias yta='ytfzf -t -m'
alias yt='ytfzf -t'
alias mutt='neomutt'
alias cd-math='cd "/mnt/LDisk-E/Albert Einstein/Books & Resources/MIT OCW/Mathematics/"'
alias cd-einstein='cd "/mnt/LDisk-E/Albert Einstein/"'
alias cd-phil='cd "/mnt/LDisk-E/Albert Einstein/Books & Resources/Philosophy & Psychology/Philosophy/"'
alias cd-psych='cd "/mnt/LDisk-E/Albert Einstein/Books & Resources/Philosophy & Psychology/Psychology/"'
alias alt='LIBGL_ALWAYS_SOFTWARE=1 alacritty'
