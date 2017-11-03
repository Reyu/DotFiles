" {{{ Pre-setup
" This should only have things that needs to be setup/initialized before loading plugins
" -----
" augroup for any auto commands set in this file
"   This lets the group be cleared when reloading the configfile, so commands
"   don't recurse/stack
augroup init
    " Clear the group
    autocmd!
augroup end
" Pre-setup }}}
" {{{ Plugins
" Load plugins first, so they are availible to later code
let s:dein_path = expand('~/.cache/nvim')
let s:dein_repo = s:dein_path.'/repos/github.com/Shougo/dein.vim'
if isdirectory(s:dein_repo)
    " Dein-managed Dein (prefered)
    execute 'set rtp+='.s:dein_repo
else
    " Minimal bootstrap
    set rtp+=~/.config/nvim/dein.vim
endif
if dein#load_state(s:dein_path)
    call dein#begin(s:dein_path)
    call dein#add('altercation/vim-colors-solarized')
    call dein#add('benekastah/neomake')
    call dein#add('cloudhead/neovim-ghcid', { 'on_ft': 'haskell' })
    call dein#add('dag/vim2hs', { 'on_ft': 'haskell' })
    call dein#add('eagletmt/ghcmod-vim', { 'on_ft': 'haskell' })
    call dein#add('eagletmt/neco-ghc', { 'on_ft': 'haskell' })
    call dein#add('easymotion/vim-easymotion')
    call dein#add('editorconfig/editorconfig-vim')
    call dein#add('ervandew/supertab.git')
    call dein#add('garbas/vim-snipmate.git')
    call dein#add('godlygeek/tabular')
    call dein#add('honza/vim-snippets')
    call dein#add('jceb/vim-orgmode')
    call dein#add('jmcantrell/vim-virtualenv')
    call dein#add('MarcWeber/vim-addon-mw-utils.git')
    call dein#add('majutsushi/tagbar')
    call dein#add('radenling/vim-dispatch-neovim')
    call dein#add('scrooloose/nerdtree')
    call dein#add('Shougo/dein.vim')
    call dein#add('Shougo/deoplete.nvim')
    call dein#add('Shougo/neosnippet-snippets')
    call dein#add('Shougo/denite.nvim')
    call dein#add('Shougo/vimproc', { 'build': 'make'})
    call dein#add('sirver/UltiSnips')
    call dein#add('tommcdo/vim-exchange')
    call dein#add('tpope/vim-dispatch')
    call dein#add('tpope/vim-capslock')
    call dein#add('tpope/vim-commentary')
    call dein#add('tpope/vim-eunuch')
    call dein#add('tpope/vim-fugitive')
    call dein#add('tpope/vim-markdown', { 'on_ft': 'markdown' })
    call dein#add('tpope/vim-obsession')
    call dein#add('tpope/vim-projectionist')
    call dein#add('tpope/vim-repeat')
    call dein#add('tpope/vim-surround')
    call dein#add('vim-airline/vim-airline')
    call dein#add('vim-airline/vim-airline-themes')
    call dein#add('vim-utils/vim-man')
    call dein#add('vim-pandoc/vim-pandoc')
    call dein#add('vim-pandoc/vim-pandoc-syntax')
    call dein#add('vim-scripts/abnf')
    call dein#add('vim-scripts/python.vim', { 'on_ft': 'python' })
    call dein#add('vim-scripts/python_fold', { 'on_ft': 'python' })

    " Latex
    call dein#add('xuhdev/vim-latex-live-preview')
    call dein#add('lervag/vimtex')

    " External
    call dein#add('edkolev/promptline.vim')
    call dein#add('edkolev/tmuxline.vim')

    call dein#end()
    call dein#save_state()
endif
if dein#check_install()
    call dein#install()
endif
" Plugin Configuration {{{
if dein#tap('vim-colors-solarized') " {{{
    let g:solarized_termtrans=1
    colorscheme solarized
else
    colorscheme slate
endif " }}}
if dein#tap('neomake') " {{{
    let g:neomake_open_list=2
    call neomake#configure#automake('nw', 500)
endif " }}}
if dein#tap('vim2hs') " {{{
    let g:haskell_conceal_wide = 0
    " disable all conceals, including the simple ones like
    " lambda and composition
    let g:haskell_conceal = 0
    " disable concealing of "enumerations": commatized lists like
    " deriving clauses and LANGUAGE pragmas,
    " otherwise collapsed into a single ellipsis
    let g:haskell_conceal_enumerations = 0
endif " }}}
if dein#tap('ghcmod-vim') " {{{
    map <silent> <Leader>tw :GhcModTypeInsert<CR>
    map <silent> <Leader>ts :GhcModSplitFunCase<CR>
    map <silent> <Leader>tq :GhcModType<CR>
    map <silent> <Leader>te :GhcModTypeClear<CR>

    " Hoogle the word under the cursor
    nnoremap <silent> <leader>hh :Hoogle<CR>
    " Hoogle and prompt for input
    nnoremap <leader>hH :Hoogle 
    " Hoogle for detailed documentation (e.g. "Functor")
    nnoremap <silent> <leader>hi :HoogleInfo<CR>
    " Hoogle for detailed documentation and prompt for input
    nnoremap <leader>hI :HoogleInfo 
    " Hoogle, close the Hoogle window
    nnoremap <silent> <leader>hz :HoogleClose<CR>

    " Type of expression under cursor
    nmap <silent> <leader>ht :GhcModType<CR>
    " Insert type of expression under cursor
    nmap <silent> <leader>hT :GhcModTypeInsert<CR>

    " Use hindent instead of par for haskell buffers
    autocmd FileType haskell let &formatprg="hindent --tab-size 2 -XQuasiQuotes"

    " Point Conversion {{{
    function! Pointfree()
        call setline('.', split(system('pointfree '.shellescape(join(getline(a:firstline, a:lastline), "\n"))), "\n"))
    endfunction
    vnoremap <silent> <leader>h. :call Pointfree()<CR>

    function! Pointful()
        call setline('.', split(system('pointful '.shellescape(join(getline(a:firstline, a:lastline), "\n"))), "\n"))
    endfunction
    vnoremap <silent> <leader>h> :call Pointful()<CR>
    " }}}
endif "}}}
if dein#tap('neco-ghc') " {{{
    " Disable haskell-vim omnifunc
    let g:ycm_semantic_triggers = {'haskell' : ['.']}
    let g:haskellmode_completion_ghc = 0
    autocmd init FileType haskell setlocal omnifunc=necoghc#omnifunc
endif " }}}
if dein#tap('vim-easymotion') " {{{
    let g:EasyMotion_smartcase = 1
    map <Leader> <Plug>(easymotion-prefix)
    nmap s <Plug>(easymotion-overwin-f)
    map  <Leader>/ <Plug>(easymotion-sn)
    omap <Leader>/ <Plug>(easymotion-tn)
    " map  <Leader>n <Plug>(easymotion-next)
    " map  <Leader>N <Plug>(easymotion-prev)
    map <Leader>l <Plug>(easymotion-lineforward)
    map <Leader>j <Plug>(easymotion-j)
    map <Leader>k <Plug>(easymotion-k)
    map <Leader>h <Plug>(easymotion-linebackward)
    let g:EasyMotion_startofline = 0 " keep cursor column when JK motion
endif "}}}
if dein#tap('editorconfig-vim') " {{{
    let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
endif
" }}}
if dein#tap('supertab') " {{{
    let g:SuperTabDefaultCompletionType = '<c-x><c-o>'
    if has("gui_running")
        imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
    else " no gui
        if has("unix")
            inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
        endif
    endif
    let g:haskellmode_completion_ghc = 1
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
endif " }}}
if dein#tap('tabular') " {{{
    let g:haskell_tabular = 1
    vmap <Leader>a= :Tabularize /=<CR>
    vmap <Leader>a; :Tabularize /::<CR>
    vmap <Leader>a- :Tabularize /-><CR>
endif " }}}
if dein#tap('tagbar') " {{{
    let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
    \ }
endif " }}}
if dein#tap('nerdtree') " {{{
    map <Leader>n :NERDTreeFocus<CR>
endif " }}}
if dein#tap('deoplete.nvim') " {{{
    let g:deoplete#enable_at_startup = 1
endif " }}}
if dein#tap('denite.nvim') " {{{
    let s:menus = {}
    let s:menus.zsh = {
                \ 'description': 'Edit ZSH files'
                \ }
    let s:menus.zsh.file_candidates = [
                \ ['zlogin', '~/.config/Zsh/zlogin'],
                \ ['zlogout', '~/.config/Zsh/zlogout'],
                \ ['zprofile', '~/.config/Zsh/zprofile'],
                \ ['zshenv', '~/.config/Zsh/zshenv'],
                \ ['zshrc', '~/.config/Zsh/zshrc'],
                \ ]
    call denite#custom#var('menu', 'menus', s:menus)

    " Define alias
    call denite#custom#alias('source', 'file_rec/git', 'file_rec')
    call denite#custom#var('file_rec/git', 'command',
                \ ['git', 'ls-files', '-co', '--exclude-standard'])
    nnoremap <C-p> :Denite file_rec/git<CR>

    call denite#custom#alias('source', 'file_rec/darcs', 'file_rec')
    call denite#custom#var('file_rec/darcs', 'command',
                \ ['dars', 'show', 'files', '--no-directories', '--pending'])
 
	" Change ignore_globs
	call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
	      \ [ '.git/', '*/__pycache__/*', 'venv/'])

    nnoremap <C-b> :Denite buffer<CR>
endif " }}}
if dein#tap('UltiSnips') " {{{
    let g:UltiSnipsEditSplit="vertical"
    let g:ultisnips_python_style="sphinx"
endif " }}}
if dein#tap('vim-airline') " {{{
    let g:airline#extensions#tabline#enabled = 1
    let g:airline_powerline_fonts = 1
endif " }}}
if dein#tap('vim-fugitive') " {{{
    nnoremap <Leader>gs :Gstatus<CR>
    autocmd init BufReadPost fugitive://*
      \ set bufhidden=delete
endif " }}}
if dein#tap('promptline.vim') " {{{
    let g:promptline_preset = {
                \'a' : [ promptline#slices#host() ],
                \'b' : [ promptline#slices#user(), promptline#slices#jobs() ],
                \'c' : [ promptline#slices#cwd() ],
                \'x' : [ promptline#slices#python_virtualenv() ],
                \'y' : [ '$(git rev-parse --short HEAD 2>/dev/null)', promptline#slices#vcs_branch() ],
                \'warn' : [ promptline#slices#last_exit_code() ]}
endif " }}}
if dein#tap('tmuxline.sh') " {{{
    let g:tmuxline_preset = 'full'
    " let g:tmuxline_preset = {
    "     \'a' : '#{?client_readonly,[RO]}',
    "     \'c' : '#{?session_many_attached,+,}',
    "     \'x' : '#(uptime|egrep -o "(\d+\.\d\d ?){3}")',
    "     \'y' : [ '%R', '%a', '%Y'],
    "     \'z' : '#H'}
endif " }}}
" }}}
" Plugins }}}
" {{{ General
" Sets how many lines of history VIM has to remember
set history=700

" Enable filetype plugins
filetype plugin on
filetype indent on

" Enable syntax highlighting
syntax enable

" Use dark background
set background=dark

" Turn on Mode Lines
set modeline modelines=3

" Turn on Line numbers
set number norelativenumber

" Allow virtual edit in block mode
set virtualedit=block

" Set terminal title
set title titlestring=NeoVim\ (%F)

" My tab preferences
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

" Don't use the mouse. Ever.
set mouse=

" Configure statusline
if dein#tap('vim-airline') == 0 " {{{
    set statusline= "Clear statusline, when reloading
    set statusline+=%(\ %n\ %) "Show buffer number
    if dein#tap('vim-capslock')
        set statusline+=%(\|\ %{exists('*CapsLockStatusline')?CapsLockStatusline('Caps'):''}\ %) "Show virtual Capslock status (vim-capslock)
    endif
    if dein#tap('vim-fugitive')
        set statusline+=%(\|\ %{fugitive#statusline()}\ %) "Show Git branch
    endif
    if dein#tap('tagbar')
        set statusline+=%(\|\ %{tagbar#currenttag('%s','')}\ %) "Show current ctag
    endif
    set statusline+=%(\|\ %.60f%) "Show file name/relative path
    set statusline+=%(\ [%M%R%H%W]%) "Show Modified flag, Readonly flag, Preview flag, and Help buffer flag
    set statusline+=%= "Right align rest of line
    if dein#tap('neomake')
        set statusline+=%#warningmsg#
        set statusline+=%(%#ErrorMsg#%{neomake#statusline#QflistStatus('qf:\ ')}%*\|\ %) "Show clist counts
        set statusline+=%*
    endif
    set statusline+=%{\"\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"\ \|\ \"} "Show encoding/bomb
    set statusline+=%(%{&ff}\ \|\ %) "Show fileformat (line ending)
    set statusline+=%(%Y\ \|\ %) "Show file type
    set statusline+=%(%02B/%03b\ \|\ %) "Show hex byte of char under cursor
    set statusline+=%(%-14(%l,%c%V%)\ %P%) "Show position/ruler data
endif " }}}

" Reload init when it is modified
autocmd init BufWritePost ~/.config/nvim/init.vim source <afile>

" Map yo and yO to set paste mode and enter insert on new line
nnoremap yo :set paste<CR>o
nnoremap yO :set paste<CR>O

" Exit paste mode when leaving insert
autocmd init InsertLeave * set nopaste

" Split windows below, or to the right of, the current window
set splitbelow splitright
" General }}}
" {{{ Files, backups and undo
" Keep backups in cache folder, so as not to clutter filesystem.
set backup backupdir=~/.cache/vim/backup,~/tmp,.,~/
set undofile undodir=~/.cache/vim/undo
set directory=~/.cache/vim/other,~/tmp,.,/var/tmp,/tmp
" Don't need backups for tmp files (usually sudo -e)
autocmd init BufRead,BufEnter /var/tmp/* set nobackup noundofile nowritebackup
" Files, backups and undo }}}
" vim: fdm=marker
