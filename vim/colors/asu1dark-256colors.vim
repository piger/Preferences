" This scheme was created by CSApproxSnapshot
" on Mon, 01 Dec 2008

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name = 'asu1dark-256colors'

if 0
elseif has("gui_running") || &t_Co == 256
    highlight Normal term=NONE cterm=NONE ctermbg=16 ctermfg=231 gui=NONE guibg=#110022 guifg=White
    highlight Underlined term=underline cterm=underline ctermbg=bg ctermfg=111 gui=underline guibg=bg guifg=#80a0ff
    highlight Ignore term=NONE cterm=NONE ctermbg=bg ctermfg=bg gui=NONE guibg=bg guifg=bg
    highlight Error term=NONE cterm=NONE ctermbg=196 ctermfg=231 gui=NONE guibg=Red guifg=White
    highlight Todo term=NONE cterm=NONE ctermbg=226 ctermfg=21 gui=NONE guibg=Yellow guifg=Blue
    highlight Function term=NONE cterm=NONE ctermbg=16 ctermfg=105 gui=NONE guibg=#110022 guifg=#7788ff
    highlight SpecialKey term=NONE cterm=NONE ctermbg=bg ctermfg=51 gui=NONE guibg=bg guifg=Cyan
    highlight NonText term=NONE cterm=NONE ctermbg=238 ctermfg=210 gui=NONE guibg=#444444 guifg=#ff9999
    highlight Directory term=NONE cterm=NONE ctermbg=16 ctermfg=46 gui=NONE guibg=#110022 guifg=Green
    highlight ErrorMsg term=NONE cterm=NONE ctermbg=196 ctermfg=231 gui=NONE guibg=Red guifg=White
    highlight IncSearch term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=bg guifg=fg
    highlight Search term=bold cterm=bold ctermbg=226 ctermfg=18 gui=bold guibg=Yellow guifg=DarkBlue
    highlight MoreMsg term=bold cterm=bold ctermbg=bg ctermfg=29 gui=bold guibg=bg guifg=SeaGreen
    highlight ModeMsg term=NONE cterm=NONE ctermbg=231 ctermfg=21 gui=NONE guibg=White guifg=Blue
    highlight LineNr term=NONE cterm=NONE ctermbg=59 ctermfg=252 gui=NONE guibg=#334444 guifg=#cccccc
    highlight Pmenu term=NONE cterm=NONE ctermbg=201 ctermfg=fg gui=NONE guibg=Magenta guifg=fg
    highlight PmenuSel term=NONE cterm=NONE ctermbg=248 ctermfg=fg gui=NONE guibg=DarkGrey guifg=fg
    highlight PmenuSbar term=NONE cterm=NONE ctermbg=250 ctermfg=fg gui=NONE guibg=Grey guifg=fg
    highlight PmenuThumb term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=bg guifg=fg
    highlight TabLine term=underline cterm=underline ctermbg=248 ctermfg=fg gui=underline guibg=DarkGrey guifg=fg
    highlight TabLineSel term=bold cterm=bold ctermbg=bg ctermfg=fg gui=bold guibg=bg guifg=fg
    highlight TabLineFill term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=bg guifg=fg
    highlight CursorColumn term=NONE cterm=NONE ctermbg=241 ctermfg=fg gui=NONE guibg=Grey40 guifg=fg
    highlight CursorLine term=NONE cterm=NONE ctermbg=241 ctermfg=fg gui=NONE guibg=Grey40 guifg=fg
    highlight Cursor term=NONE cterm=NONE ctermbg=47 ctermfg=231 gui=NONE guibg=#00ff33 guifg=White
    highlight DiffAdd term=NONE cterm=NONE ctermbg=18 ctermfg=fg gui=NONE guibg=DarkBlue guifg=fg
    highlight Question term=NONE cterm=NONE ctermbg=16 ctermfg=84 gui=NONE guibg=#110022 guifg=#66ff99
    highlight StatusLine term=bold cterm=bold ctermbg=58 ctermfg=231 gui=bold guibg=#336600 guifg=White
    highlight StatusLineNC term=NONE cterm=NONE ctermbg=252 ctermfg=16 gui=NONE guibg=#cccccc guifg=Black
    highlight VertSplit term=NONE cterm=NONE ctermbg=241 ctermfg=231 gui=NONE guibg=#666666 guifg=White
    highlight Title term=bold cterm=bold ctermbg=bg ctermfg=201 gui=bold guibg=bg guifg=Magenta
    highlight Visual term=NONE cterm=NONE ctermbg=35 ctermfg=231 gui=NONE guibg=#00aa33 guifg=White
    highlight VisualNOS term=bold,underline cterm=bold,underline ctermbg=bg ctermfg=fg gui=bold,underline guibg=bg guifg=fg
    highlight WarningMsg term=NONE cterm=NONE ctermbg=226 ctermfg=20 gui=NONE guibg=Yellow guifg=#0000cc
    highlight WildMenu term=NONE cterm=NONE ctermbg=226 ctermfg=16 gui=NONE guibg=Yellow guifg=Black
    highlight Folded term=NONE cterm=NONE ctermbg=248 ctermfg=51 gui=NONE guibg=DarkGrey guifg=Cyan
    highlight helpLeadBlank term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight lCursor term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=fg guifg=bg
    highlight MatchParen term=NONE cterm=NONE ctermbg=30 ctermfg=fg gui=NONE guibg=DarkCyan guifg=fg
    highlight Comment term=NONE cterm=NONE ctermbg=16 ctermfg=114 gui=NONE guibg=#110022 guifg=#99cc99
    highlight Constant term=NONE cterm=NONE ctermbg=16 ctermfg=208 gui=NONE guibg=#110022 guifg=#ff9900
    highlight Special term=NONE cterm=NONE ctermbg=16 ctermfg=51 gui=NONE guibg=#110022 guifg=Cyan
    highlight Identifier term=NONE cterm=NONE ctermbg=16 ctermfg=51 gui=NONE guibg=#110022 guifg=Cyan
    highlight Statement term=bold cterm=bold ctermbg=16 ctermfg=226 gui=bold guibg=#110022 guifg=Yellow
    highlight PreProc term=NONE cterm=NONE ctermbg=16 ctermfg=83 gui=NONE guibg=#110022 guifg=#33ff66
    highlight Type term=NONE cterm=NONE ctermbg=16 ctermfg=204 gui=NONE guibg=#110022 guifg=#ff5577
    highlight helpNormal term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight SpecialChar term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight FoldColumn term=NONE cterm=NONE ctermbg=250 ctermfg=51 gui=NONE guibg=Grey guifg=Cyan
    highlight CSApproxTest term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight DiffChange term=NONE cterm=NONE ctermbg=90 ctermfg=fg gui=NONE guibg=DarkMagenta guifg=fg
    highlight DiffDelete term=bold cterm=bold ctermbg=30 ctermfg=21 gui=bold guibg=DarkCyan guifg=Blue
    highlight DiffText term=bold cterm=bold ctermbg=196 ctermfg=fg gui=bold guibg=Red guifg=fg
    highlight SignColumn term=NONE cterm=NONE ctermbg=250 ctermfg=51 gui=NONE guibg=Grey guifg=Cyan
    highlight SpellBad term=undercurl cterm=undercurl ctermbg=bg ctermfg=196 gui=undercurl guibg=bg guifg=fg guisp=Red
    highlight SpellCap term=undercurl cterm=undercurl ctermbg=bg ctermfg=21 gui=undercurl guibg=bg guifg=fg guisp=Blue
    highlight SpellRare term=undercurl cterm=undercurl ctermbg=bg ctermfg=201 gui=undercurl guibg=bg guifg=fg guisp=Magenta
    highlight SpellLocal term=undercurl cterm=undercurl ctermbg=bg ctermfg=51 gui=undercurl guibg=bg guifg=fg guisp=Cyan
elseif has("gui_running") || &t_Co == 88
    highlight Normal term=NONE cterm=NONE ctermbg=16 ctermfg=79 gui=NONE guibg=#110022 guifg=White
    highlight Underlined term=underline cterm=underline ctermbg=bg ctermfg=39 gui=underline guibg=bg guifg=#80a0ff
    highlight Ignore term=NONE cterm=NONE ctermbg=bg ctermfg=bg gui=NONE guibg=bg guifg=bg
    highlight Error term=NONE cterm=NONE ctermbg=64 ctermfg=79 gui=NONE guibg=Red guifg=White
    highlight Todo term=NONE cterm=NONE ctermbg=76 ctermfg=19 gui=NONE guibg=Yellow guifg=Blue
    highlight Function term=NONE cterm=NONE ctermbg=16 ctermfg=39 gui=NONE guibg=#110022 guifg=#7788ff
    highlight SpecialKey term=NONE cterm=NONE ctermbg=bg ctermfg=31 gui=NONE guibg=bg guifg=Cyan
    highlight NonText term=NONE cterm=NONE ctermbg=16 ctermfg=69 gui=NONE guibg=#444444 guifg=#ff9999
    highlight Directory term=NONE cterm=NONE ctermbg=16 ctermfg=28 gui=NONE guibg=#110022 guifg=Green
    highlight ErrorMsg term=NONE cterm=NONE ctermbg=64 ctermfg=79 gui=NONE guibg=Red guifg=White
    highlight IncSearch term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=bg guifg=fg
    highlight Search term=bold cterm=bold ctermbg=76 ctermfg=17 gui=bold guibg=Yellow guifg=DarkBlue
    highlight MoreMsg term=bold cterm=bold ctermbg=bg ctermfg=21 gui=bold guibg=bg guifg=SeaGreen
    highlight ModeMsg term=NONE cterm=NONE ctermbg=79 ctermfg=19 gui=NONE guibg=White guifg=Blue
    highlight LineNr term=NONE cterm=NONE ctermbg=16 ctermfg=58 gui=NONE guibg=#334444 guifg=#cccccc
    highlight Pmenu term=NONE cterm=NONE ctermbg=67 ctermfg=fg gui=NONE guibg=Magenta guifg=fg
    highlight PmenuSel term=NONE cterm=NONE ctermbg=84 ctermfg=fg gui=NONE guibg=DarkGrey guifg=fg
    highlight PmenuSbar term=NONE cterm=NONE ctermbg=85 ctermfg=fg gui=NONE guibg=Grey guifg=fg
    highlight PmenuThumb term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=bg guifg=fg
    highlight TabLine term=underline cterm=underline ctermbg=84 ctermfg=fg gui=underline guibg=DarkGrey guifg=fg
    highlight TabLineSel term=bold cterm=bold ctermbg=bg ctermfg=fg gui=bold guibg=bg guifg=fg
    highlight TabLineFill term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=bg guifg=fg
    highlight CursorColumn term=NONE cterm=NONE ctermbg=81 ctermfg=fg gui=NONE guibg=Grey40 guifg=fg
    highlight CursorLine term=NONE cterm=NONE ctermbg=81 ctermfg=fg gui=NONE guibg=Grey40 guifg=fg
    highlight Cursor term=NONE cterm=NONE ctermbg=28 ctermfg=79 gui=NONE guibg=#00ff33 guifg=White
    highlight DiffAdd term=NONE cterm=NONE ctermbg=17 ctermfg=fg gui=NONE guibg=DarkBlue guifg=fg
    highlight Question term=NONE cterm=NONE ctermbg=16 ctermfg=45 gui=NONE guibg=#110022 guifg=#66ff99
    highlight StatusLine term=bold cterm=bold ctermbg=20 ctermfg=79 gui=bold guibg=#336600 guifg=White
    highlight StatusLineNC term=NONE cterm=NONE ctermbg=58 ctermfg=16 gui=NONE guibg=#cccccc guifg=Black
    highlight VertSplit term=NONE cterm=NONE ctermbg=81 ctermfg=79 gui=NONE guibg=#666666 guifg=White
    highlight Title term=bold cterm=bold ctermbg=bg ctermfg=67 gui=bold guibg=bg guifg=Magenta
    highlight Visual term=NONE cterm=NONE ctermbg=20 ctermfg=79 gui=NONE guibg=#00aa33 guifg=White
    highlight VisualNOS term=bold,underline cterm=bold,underline ctermbg=bg ctermfg=fg gui=bold,underline guibg=bg guifg=fg
    highlight WarningMsg term=NONE cterm=NONE ctermbg=76 ctermfg=18 gui=NONE guibg=Yellow guifg=#0000cc
    highlight WildMenu term=NONE cterm=NONE ctermbg=76 ctermfg=16 gui=NONE guibg=Yellow guifg=Black
    highlight Folded term=NONE cterm=NONE ctermbg=84 ctermfg=31 gui=NONE guibg=DarkGrey guifg=Cyan
    highlight helpLeadBlank term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight lCursor term=NONE cterm=NONE ctermbg=fg ctermfg=bg gui=NONE guibg=fg guifg=bg
    highlight MatchParen term=NONE cterm=NONE ctermbg=21 ctermfg=fg gui=NONE guibg=DarkCyan guifg=fg
    highlight Comment term=NONE cterm=NONE ctermbg=16 ctermfg=41 gui=NONE guibg=#110022 guifg=#99cc99
    highlight Constant term=NONE cterm=NONE ctermbg=16 ctermfg=68 gui=NONE guibg=#110022 guifg=#ff9900
    highlight Special term=NONE cterm=NONE ctermbg=16 ctermfg=31 gui=NONE guibg=#110022 guifg=Cyan
    highlight Identifier term=NONE cterm=NONE ctermbg=16 ctermfg=31 gui=NONE guibg=#110022 guifg=Cyan
    highlight Statement term=bold cterm=bold ctermbg=16 ctermfg=76 gui=bold guibg=#110022 guifg=Yellow
    highlight PreProc term=NONE cterm=NONE ctermbg=16 ctermfg=29 gui=NONE guibg=#110022 guifg=#33ff66
    highlight Type term=NONE cterm=NONE ctermbg=16 ctermfg=69 gui=NONE guibg=#110022 guifg=#ff5577
    highlight helpNormal term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight SpecialChar term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight FoldColumn term=NONE cterm=NONE ctermbg=85 ctermfg=31 gui=NONE guibg=Grey guifg=Cyan
    highlight CSApproxTest term=NONE cterm=NONE ctermbg=bg ctermfg=fg gui=NONE guibg=bg guifg=fg
    highlight DiffChange term=NONE cterm=NONE ctermbg=33 ctermfg=fg gui=NONE guibg=DarkMagenta guifg=fg
    highlight DiffDelete term=bold cterm=bold ctermbg=21 ctermfg=19 gui=bold guibg=DarkCyan guifg=Blue
    highlight DiffText term=bold cterm=bold ctermbg=64 ctermfg=fg gui=bold guibg=Red guifg=fg
    highlight SignColumn term=NONE cterm=NONE ctermbg=85 ctermfg=31 gui=NONE guibg=Grey guifg=Cyan
    highlight SpellBad term=undercurl cterm=undercurl ctermbg=bg ctermfg=64 gui=undercurl guibg=bg guifg=fg guisp=Red
    highlight SpellCap term=undercurl cterm=undercurl ctermbg=bg ctermfg=19 gui=undercurl guibg=bg guifg=fg guisp=Blue
    highlight SpellRare term=undercurl cterm=undercurl ctermbg=bg ctermfg=67 gui=undercurl guibg=bg guifg=fg guisp=Magenta
    highlight SpellLocal term=undercurl cterm=undercurl ctermbg=bg ctermfg=31 gui=undercurl guibg=bg guifg=fg guisp=Cyan
endif
