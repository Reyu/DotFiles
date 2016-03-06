" {{{ Plugins
" Load plugins first, so they are availible to later code
set runtimepath^=~/.config/nvim/repos/github.com/Shougo/dein.vim
call dein#begin(expand('~/.config/nvim')) " {{{
call dein#add('altercation/vim-colors-solarized')
call dein#add('benekastah/neomake')
call dein#add('chrisdone/hindent', { 'on_ft': 'haskell' })
call dein#add('dag/vim2hs', { 'on_ft': 'haskell' })
call dein#add('eagletmt/ghcmod-vim', { 'on_ft': 'haskell' })
call dein#add('eagletmt/neco-ghc', { 'on_ft': 'haskell' })
call dein#add('godlygeek/tabular')
call dein#add('honza/vim-snippets')
call dein#add('jebaum/neosyntax')
call dein#add('majutsushi/tagbar')
call dein#add('python.vim', { 'on_ft': 'python' })
call dein#add('python_fold', { 'on_ft': 'python' })
call dein#add('scrooloose/nerdtree')
call dein#add('scrooloose/syntastic')
call dein#add('Shougo/dein.vim')
call dein#add('Shougo/deoplete.nvim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
" call dein#add('sirver/UltiSnips')
call dein#add('tommcdo/vim-exchange')
call dein#add('tpope/vim-capslock')
call dein#add('tpope/vim-commentary')
call dein#add('tpope/vim-eunuch')
call dein#add('tpope/vim-fugitive')
call dein#add('tpope/vim-markdown', { 'on_ft': 'markdown' })
call dein#add('tpope/vim-repeat')
call dein#add('tpope/vim-surround')
call dein#add('vim-utils/vim-man')
call dein#end()
if dein#check_install()
    call dein#install()
endif " }}}
" Plugin Configuration {{{
if dein#tap('vim-colors-solarized') " {{{
    set background=dark
    colorscheme solarized
    call togglebg#map("<F5>")
endif
" }}}
if dein#tap('vim2hs') " {{{
    let g:haskell_conceal_wide = 0
    " disable all conceals, including the simple ones like
    " lambda and composition
    let g:haskell_conceal              = 0
    " disable concealing of "enumerations": commatized lists like
    " deriving clauses and LANGUAGE pragmas,
    " otherwise collapsed into a single ellipsis
    let g:haskell_conceal_enumerations = 0
endif
" }}}
" if dein#tap('neomake') " {{{
"     " Try to run Neomake on file save
"     " This should fail silently
"     autocmd! Neomake
"     autocmd init BufWritePost * Neomake
" endif
" }}}
if dein#tap('neco-ghc') " {{{
    " Disable haskell-vim omnifunc
    let g:ycm_semantic_triggers = {'haskell' : ['.']}
    let g:haskellmode_completion_ghc = 0
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
endif
" }}}
if dein#tap('syntastic') " {{{
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
endif
" }}}
if dein#tap('nerdtree') " {{{
    map <Leader>n :NERDTreeToggle<CR>
endif
" }}}
if dein#tap('tabular') " {{{
    let g:haskell_tabular = 1
    vmap <Leader>a= :Tabularize /=<CR>
    vmap <Leader>a; :Tabularize /::<CR>
    vmap <Leader>a- :Tabularize /-><CR>
endif
" }}}
if dein#tap('ctrlp.vim') " {{{
    map <silent> <Leader>t :CtrlP()<CR>
    noremap <Leader>b<space> :CtrlPBuffer<cr>
    let g:ctrlp_custom_ignore = '\v[\/]dist$'
endif
" }}}
if dein#tap('neosnippet') " {{{
    " Plugin key-mappings.
    imap <C-k>     <Plug>(neosnippet_expand_or_jump)
    smap <C-k>     <Plug>(neosnippet_expand_or_jump)
    xmap <C-k>     <Plug>(neosnippet_expand_target)

    " SuperTab like snippets behavior.
    "imap <expr><TAB>
    " \ pumvisible() ? "\<C-n>" :
    " \ neosnippet#expandable_or_jumpable() ?
    " \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
    smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
    \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

    " For conceal markers.
    if has('conceal')
      set conceallevel=2 concealcursor=niv
    endif
endif " }}}
if dein#tap('deoplete.nvim') " {{{
    let g:deoplete#enable_at_startup = 1
endif " }}}
if dein#tap('UltiSnips') " {{{
    let g:UltiSnipsEditSplit="vertical"
endif " }}}
" Plugin Configuration }}}
" Plugins }}}
" {{{ General
" Sets how many lines of history VIM has to remember
set history=700

" Enable filetype plugins
filetype plugin on
filetype indent on

" Enable syntax highlighting
syntax enable

" Turn on Mode Lines
set modeline modelines=3

" Turn on (Relative)Line numbers
set number relativenumber

" Allow virtual edit in block mode
set virtualedit=block

" Set terminal title
set title titlestring=NeoVim\ (%F)

" My tab preferences
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

" Don't use the mouse. Ever.
set mouse=

" Set Leader key
let mapleader=","

" Configure statusline
set statusline= "Clear statusline, when reloading
set statusline+=%(\ %n\ %) "Show buffer number
if dein#tap('tpope/vim-capslock')
    set statusline+=%(\|\ %{exists('*CapsLockStatusline')?CapsLockStatusline('Caps'):''}\ %) "Show virtual Capslock status (vim-capslock)
endif
if dein#tap('tpope/vim-fugitive')
    set statusline+=%(\|\ %{fugitive#statusline()}\ %) "Show Git branch
endif
if dein#tap('majutsushi/tagbar')
    set statusline+=%(\|\ %{tagbar#currenttag('%s','')}\ %) "Show current ctag
endif
set statusline+=%(\|\ %.60f%) "Show file name/relative path
set statusline+=%(\ [%M%R%H%W]%) "Show Modified flag, Readonly flag, Preview flag, and Help buffer flag
set statusline+=%= "Right ALIGN rest of line
if dein#tap('benekastah/neomake')
    set statusline+=%(%#ErrorMsg#%{neomake#statusline#QflistStatus('qf:\ ')}%*\|\ %) "Show clist counts
endif
set statusline+=%{\"\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"\ \|\ \"} "Show encoding/bomb
set statusline+=%(%{&ff}\ \|\ %) "Show fileformat (line ending)
set statusline+=%(%Y\ \|\ %) "Show file type
set statusline+=%(%02B/%03b\ \|\ %) "Show hex byte of char under cursor
set statusline+=%(%-14(%l,%c%V%)\ %P%) "Show position/ruler data

" Create auto command group, and clear it
augroup init
    " Reset auto commands
    autocmd!
augroup END

" Reload init when it is modified
autocmd init BufWritePost ~/.config/nvim/init.vim source <afile>

" Map yo and yO to set paste mode and enter insert on new line
nnoremap yo :set paste<CR>o
nnoremap yO :set paste<CR>O
" Exit paste mode when leaving insert
autocmd init InsertLeave * set nopaste
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
