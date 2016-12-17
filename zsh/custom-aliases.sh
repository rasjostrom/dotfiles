## System
alias zshconfig="emacs ~/.zshrc"
alias suspend='systemctl suspend'
alias status='systemctl status'
alias reboot='sudo systemctl reboot'

## Utils
#Pastebin
#alias -g pasta="| curl -F c=@- https://ptpb.pw | \\
sed -e 's/url: //' -n -e '/https/p' | \\
xclip -selection c && echo 'YOUR PASTA IS READY'"

#alias -g clip="| xclip -selection c"
alias sourcz='source ~/.zshrc'

#Screen brightness
alias xbright='xbacklight -set 100'
alias xdark='xbacklight -set 50'

##Dev-tools
#Python Web
alias runserver='python manage.py runserver'
alias newdb='python manage.py makemigrations && python manage.py migrate'
