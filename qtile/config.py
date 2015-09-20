from libqtile.config import Key, Group, Screen
from libqtile import layout, bar, widget, hook
from libqtile.command import lazy

import subprocess

def execute(command):
    return subprocess.Popen(command.split(), shell=True)

def execute_prompt():
    return 0

@hook.subscribe.startup
def startup():
    execute("feh --bg-scale ~/.config/qtile/wallpapers/arch.png")
    execute("xmodmap ~/.Xmodmap")

# Default widget settings
widget_cfg = dict(
    font = 'Consolas',
    fontsize = 11,
    padding = 2,
)

# Widgets
bar_widgets = [
    widget.GroupBox(urgent_alert_method='text', **widget_cfg),
    widget.Prompt(**widget_cfg),

    widget.sep.Sep(foreground='7b5830'),
    widget.Battery(**widget_cfg),
]

# Screen settings
screens = [
    Screen(
        bottom = bar.Bar(bar_widgets, 22, opacity=0.1)
        ),
]

# Mod-variables
sup = "mod4" # Super key
alt = "mod1"
# Key shortcuts
keys = [
    
    # Logout
    Key([sup, alt],  "r", lazy.restart()),

    # Start a new process in current workspace
    Key([sup], "r", lazy.spawncmd()),

    # Execute Once
    Key([sup], "c", execute_prompt()),
    ]

# Workspaces
groups = [
    Group(name='h', spawn='xterm', layout='max'),
    Group(name='t', spawn='terminator', layout='max'),
    Group(name='n', spawn='', layout='max'),
    Group(name='s', spawn='', layout='max'),
    Group('8'),
    Group('9'),
    Group('0')
    ]

# Set key shortcuts for workspace navigation
for i in groups:
    keys.append(
        Key([sup], i.name, lazy.group[i.name].toscreen())
    )

border = dict(border_width=1)

layouts = [
    layout.Stack(stacks=2, **border),
    layout.MonadTall(**border), 
    # layout.Stack(stacks=2, border_width=1),
    layout.Max(),
]

main = None
