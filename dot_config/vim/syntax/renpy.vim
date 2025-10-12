" http://spikycaterpillar.dreamhosters.com/renpy.vim

runtime! syntax/python.vim
" \(x\|y\) is how you group x/y in syn match.

set tabstop=4
set shiftwidth=4
syn keyword pythonStatement init image python early transform
syn keyword pythonStatement hide show scene
syn keyword pythonStatement menu jump call
syn keyword pythonOperator with at behind fadein fadeout expression
syn keyword pythonStatement play stop
syn keyword pythonOperator sound music
syn match pythonOperator "\$"
syn match pythonStatement "label" display nextgroup=pythonFunction skipwhite
syn keyword pythonBuiltin left right moveinright moveinleft moveoutright moveoutleft moveoutbottom moveinbottom moveouttop moveintop
syn keyword pythonStatement dissolve
syn keyword pythonFunction Animation Character Null
syn keyword pythonFunction ShowingSwitch
syn match pythonFunction "anim\.\(Edge\|SMAnimation\|State\|TransitionAnimation\)"
syn match pythonFunction "im\.Composite"
syn match pythonFunction "im\.Crop"
syn match pythonFunction "im\.\(FactorScale\|Flip\|Grayscale\)"
syn match pythonFunction "im\.Image"
syn match pythonFunction "im\.MatrixColor"
syn match pythonFunction "im\.matrix\.\(brightness\|contrast\|hue\|invert\|saturation\)"
syn match pythonFunction "im\.Scale"
syn match pythonFunction "renpy\.\(call_in_new_context\|curry\)"
syn match pythonFunction "renpy\.hide"
syn match pythonFunction "renpy\.\(jump_out_of_context\|jump\)"
syn match pythonFunction "renpy\.music\.play"
syn match pythonFunction "renpy\.music\.queue"
syn match pythonFunction "renpy\.pause"
syn match pythonFunction "renpy\.redraw"
syn match pythonFunction "renpy\.random\.\(choice\|randint\)"
syn match pythonFunction "renpy\.restart_interaction"
syn match pythonFunction "renpy\.\(scene\|show\)"
syn match pythonFunction "renpy\.showing"
syn match pythonFunction "renpy\.transition"
syn match pythonFunction "ui\.add"
syn match pythonFunction "ui\.\(bar\|button\)"
syn match pythonFunction "ui\.clear"
syn match pythonFunction "ui\.close"
syn match pythonFunction "ui\.\(fixed\|frame\|grid\)"
syn match pythonFunction "ui\.hbox"
syn match pythonFunction "ui\.image"
syn match pythonFunction "ui\.imagebutton"
syn match pythonFunction "ui\.imagemap"
syn match pythonFunction "ui\.interact"
syn match pythonFunction "ui\.\(keymap\|layer\)"
syn match pythonFunction "ui\.null"
syn match pythonFunction "ui\.remove"
syn match pythonFunction "ui\.returns"
syn match pythonFunction "ui\.text"
syn match pythonFunction "ui\.textbutton"
syn match pythonFunction "ui\.timer"
syn match pythonFunction "ui\.\(vbox\|window\)"
syn keyword pythonFunction Dissolve DynamicDisplayable Fade Fixed Frame
syn keyword pythonFunction ImageDissolve Position RotoZoom 
syn keyword pythonFunction Solid SplineMotion Style Text
