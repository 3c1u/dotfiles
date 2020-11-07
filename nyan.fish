source ~/.cargo/env

alias cls="clear"
alias vi="emacs -nw"
alias vim="emacs -nw"

# alias conda="/usr/local/anaconda3/bin/conda"

switch (uname)
   case Darwin
   	#macOS向けの設定
   	alias xemacs="open -a Emacs"
	set -x PATH $HOME/Qt/5.15.1/clang_64/bin $HOME/.local/share/flutter/bin $HOME/.local/bin $PATH
	set -x PATH /Applications/UpTeX.app/Contents/Resources/TEX/texbin $PATH
   case '*'
	set -x $HOME/.local/share/flutter/bin $HOME/.local/bin $PATH
end

