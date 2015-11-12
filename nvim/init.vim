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

" Configure statusline
set statusline= "Clear statusline, when reloading
set statusline+=%(\ %n\ %) "Show buffer number
set statusline+=%(\|\ %{exists('*CapsLockStatusline')?CapsLockStatusline('Caps'):''}\ %) "Show virtual Capslock status (vim-capslock)
set statusline+=%(\|\ %{fugitive#statusline()}\ %) "Show Git branch
set statusline+=%(\|\ %{tagbar#currenttag('%s','')}\ %) "Show current ctag
set statusline+=%(\|\ %.60f%) "Show file name/relative path
set statusline+=%(\ [%M%R%H%W]%) "Show Modified flag, Readonly flag, Preview flag, and Help buffer flag
set statusline+=%= "Right ALIGN rest of line
set statusline+=%(%#ErrorMsg#%{neomake#statusline#QflistStatus('qf:\ ')}%*\|\ %) "Show clist counts
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
" {{{ Plugins
call plug#begin('~/.config/nvim/plugged') " {{{
Plug 'altercation/vim-colors-solarized'
Plug 'benekastah/neomake'
Plug 'godlygeek/tabular', { 'on' : 'Tabularize' }
Plug 'neovimhaskell/haskell-vim', { 'for' : 'haskell' }
Plug 'majutsushi/tagbar'
Plug 'pbrisbin/vim-syntax-shakespeare', { 'for' : ['hamlet','julius','casius','lucius'] }
Plug 'python.vim', { 'for' : 'python' }
Plug 'python_fold', { 'for' : 'python' }
Plug 'rodjek/vim-puppet', { 'for': 'puppet' }
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-capslock'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', { 'for' : 'markdown' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'Twinside/vim-haskellConceal', { 'for' : 'haskell' }
Plug 'vim-utils/vim-man'
call plug#end() " }}}
" Plugin Configuration {{{
" Plugin 'altercation/vim-colors-solarized' {{{
set background=dark
colorscheme solarized
call togglebg#map("<F5>")
" }}}
" Plugin 'benekastah/neomake' {{{
" Try to run Neomake on file save
" This should fail silently
autocmd init BufWritePost * Neomake
" }}}
" Plugin Configuration }}}
" Plugins }}}
" vim: fdm=marker