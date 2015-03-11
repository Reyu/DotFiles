" {{{ General
" Sets how many lines of history VIM has to remember
set history=700

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","

" Fast saving
nmap <leader>w :w<cr>

" Turn on Mode Lines
set modeline
set modelines=3

" Set terminal title
" autocmd BufEnter * let &titlestring = expand("%:t")
set title
" General }}}
" {{{ VIM User Interface
" Hide the default mode text (e.g. -- INSERT -- below the statusline)
" This is shown by /(Power|Air)Line/ instead
set noshowmode

" Set 7 lines to the cursor when moving vertically using j/k
set scrolloff=7

" Turn on the Wild menu
set wildmenu

" Ignore compiled files and some directories
set wildignore=*.o,*~,*.pyc,.git\*,.hg\*,.svn\*

"Always show current position
set ruler

" Height of the command bar
set cmdheight=2

" A buffer becomes hidden when it is abandoned
set hidden

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" Show matching brackets when text indicator is over them
set showmatch

" Show line numbers
set number
nmap <leader>R :set relativenumber!<cr>

" Allow virtual editing in Visual-Block mode
set virtualedit=block
" VIM User Interface }}}
" {{{ Colors and Fonts
" Enable syntax highlighting
syntax enable

" Use Unix as the standard file type
set ffs=unix,dos,mac
" Colors and Fonts }}}
" {{{ Files, backups and undo
" Keep backups in cache folder, so as not to clutter filesystem.
set backup
set backupdir=~/.cache/vim/backup,~/tmp,.,~/
set directory=~/.cache/vim/other,~/tmp,.,/var/tmp,/tmp
set undodir=~/.cache/vim/undo
set undofile
" Don't need backups for tmp files (usually sudo -e)
autocmd BufRead,BufEnter /var/tmp/* set nobackup noundofile nowritebackup
" Files, backups and undo }}}
" {{{ Text, tab and indent related
" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set softtabstop=4

" Enable linebreak
set linebreak

" Keep indent when adding line return
set autoindent
" Text, tab and indent related }}}
" {{{ Visual mode related
" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>
" Visual mode related }}}
" {{{ Moving around, tabs, windows and buffers
" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :let @/ = ""<cr>

" Close the current buffer
map <leader>bd :Bclose<cr>

" Close all the buffers
map <leader>ba :1,1000 bd!<cr>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
    set switchbuf=useopen,usetab,newtab
    set stal=2
catch
endtry

" Return to last edit position when opening files
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" Remember info about open buffers on close
set viminfo^=%
" Moving around, tabs, windows and buffers }}}
" {{{ Status line
" Always show the status line
set laststatus=2
" Status line }}}
" {{{ vimgrep searching and cope displaying
" When you press gv you vimgrep after the selected text
vnoremap <silent> gv :call VisualSelection('gv', '')<CR>

" Open vimgrep and put the cursor in the right position
map <leader>g :vimgrep // **/*.<left><left><left><left><left><left><left>

" Vimgreps in the current file
map <leader><space> :vimgrep // <C-R>%<C-A><right><right><right><right><right><right><right><right><right>

" When you press <leader>r you can search and replace the selected text
vnoremap <silent> <leader>r :call VisualSelection('replace', '')<CR>

" Do :help cope if you are unsure what cope is. It's super useful!
" When you search with vimgrep, display your results in cope by doing:
"   <leader>cc
" To go to the next search result do:
"   <leader>n
" To go to the previous search results do:
"   <leader>p
map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
map <leader>n :cn<cr>
map <leader>p :cp<cr>
" vimgrep searching and cope displaying }}}
" {{{ Vim grep
let Grep_Skip_Dirs = 'RCS CVS SCCS .svn generated'
set grepprg=/bin/grep\ -nH
" Vim grep }}}
" {{{ General abbreviations
iab xdate <c-r>=strftime("%d/%m/%y %H:%M:%S")<cr>
" General abbreviations }}}
" {{{ Omni complete functions
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
" Omni complete functions }}}
" {{{ Helper functions
" Used by `VisualSelection`
function! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction

" Used by a few commands througout this configuration
function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.' . a:extra_filter)
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" Returns true if paste mode is enabled
" Used by statusline
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    en
    return ''
endfunction

" Don't close window, when deleting a buffer
command! Bclose call <SID>BufcloseCloseIt()
function! <SID>BufcloseCloseIt()
    let l:currentBufNum = bufnr("%")
    let l:alternateBufNum = bufnr("#")

    if buflisted(l:alternateBufNum)
        buffer #
    else
        bnext
    endif

    if bufnr("%") == l:currentBufNum
        new
    endif

    if buflisted(l:currentBufNum)
        execute("bdelete! ".l:currentBufNum)
    endif
endfunction
" Helper functions }}}
" {{{ Plugins
call plug#begin('~/.config/NeoVim/plugged')
Plug 'ajh17/VimCompletesMe'
Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
Plug 'chase/vim-ansible-yaml', { 'for' : 'ansible' }
Plug 'christoomey/vim-tmux-navigator'
Plug 'godlygeek/tabular', { 'on' : 'Tabularize' }
Plug 'jamessan/vim-gnupg'
Plug 'klen/python-mode', { 'for' : 'python' }
Plug 'Lokaltog/vim-easymotion'
Plug 'lukerandall/haskellmode-vim', { 'for' : 'haskell' }
Plug 'majutsushi/tagbar'
Plug 'ntpeters/vim-better-whitespace'
Plug 'pbrisbin/vim-syntax-shakespeare', { 'for' : ['hamlet','julias','casius'] }
Plug 'scrooloose/syntastic'
Plug 'Shougo/unite.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-capslock'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown', { 'for' : 'markdown' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'Twinside/vim-haskellConceal', { 'for' : 'haskell' }
Plug 'wikitopian/hardmode'
call plug#end()
" Plugin Configuration {{{
" Plugin 'altercation/vim-colors-solarized' {{{
set background=dark
colorscheme solarized
" }}}
" Plugin 'ardagnir/pterosaur' {{{
augroup pentadactyl
    au!
    au BufRead,BufEnter */pentadactyl.*.txt set spell
augroup END
" }}}
" Plugin 'bling/vim-airline' {{{
let g:airline_powerline_fonts = 1
" }}}
" Plugin 'godlygeek/tabular' {{{
inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a
function! s:align()
    let p = '^\s*|\s.*\s|\s*$'
    if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
        let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
        let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
        Tabularize/||\||/l1
        normal! 0
        call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
    endif
endfunction
" }}}
" Plugin 'klen/python-mode' {{{
let g:pymode_lint_checkers = ['pylint', 'pyflakes', 'pep8', 'mccabe']
noremap <leader>pl :PymodeLint<cr>
noremap <leader>pla :PymodeLintAuto<cr>
" }}}
" Plugin 'Lokaltog/vim-easymotion' {{{
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_keys = 'gpbkcwjrvqlzdihutenosa'

" Bi-directional find motion
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
" nmap s <Plug>(easymotion-s)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap s <Plug>(easymotion-s2)

" Turn on case sensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" Easy Motion }}}
" Plugin 'majutsushi/tagbar' {{{
map <leader>t :TagbarToggle<CR>

" Tagbar }}}
" Plugin 'Shougo/unite.vim' {{{
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
" nnoremap <leader>t :<C-u>Unite -no-split -buffer-name=files   -start-insert file_rec/async:!<cr>
nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files   -start-insert file<cr>
nnoremap <leader>o :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
nnoremap <leader>y :<C-u>Unite -no-split -buffer-name=yank    history/yank<cr>
nnoremap <leader>e :<C-u>Unite -no-split -buffer-name=buffer  buffer<cr>

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
    " Play nice with supertab
    let b:SuperTabDisabled=1
    " Enable navigation with control-j and control-k in insert mode
    imap <buffer> <C-j>   <Plug>(unite_select_next_line)
    imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction

" Unite }}}
" Plugin 'tpope/vim-fugitive' {{{
" Git saving
noremap <leader>gw :Gwrite<cr>

" Git status
noremap <leader>gs :Gstatus<cr>
" }}}
" Plugin 'tpope/vim-surround' {{{
" Annotate strings with gettext http://amix.dk/blog/post/19678
"vmap Si S(i_<esc>f)
au FileType mako vmap Si S"i${ _(<esc>2f"a) }<esc>

" surround.vim config }}}
" Plugin 'wikitopian/hardmode' {{{
nnoremap <leader>h <ESC>:call ToggleHardMode()<CR>
" Hard Mode }}}
" Plugin 'lukerandall/haskellmode-vim' {{{
let g:haddock_browser="firefox"
let g:haskellmode_completion_ghc=0
let g:haskellmode_completion_haddock=0
autocmd BufEnter *.hs compiler ghc
autocmd BufEnter *.hs set textwidth=100
autocmd BufEnter *.hs set colorcolumn=101
" }}}
" Plugin Configuration }}}
" Plugins }}}
" vim: fdm=marker
