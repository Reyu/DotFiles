" Pre Setup {{{1
" This should only have things that needs to be setup/initialized before loading plugins
" -----
" augroup for any auto commands set in this file
"   This lets the group be cleared when reloading the configfile, so commands
"   don't recurse/stack
augroup init
    " Clear the group
    autocmd!
augroup end

" Python environment
let g:loaded_python_provider = 0  " Disabel Python2
let s:python_requirements = expand("${HOME}/.config/nvim/.python_requirements.txt")
let s:pyenv_repos = system("pyenv virtualenvs --bare")
if v:shell_error == 0  " if == 0: Command completed successfully
    if s:pyenv_repos !~ '\<nvim\>'
        echo "Creating PyEnv virtual environment"
        call system("pyenv virtualenv nvim")
        echo "Installing Python libraries..."
        call system("env PYENV_VERSION=nvim pyenv exec python -m pip install -r " . s:python_requirements)
    endif
    let g:python3_host_prog = trim(system("env PYENV_VERSION=nvim pyenv which python"))
else
    echo "Could not find PyEnv"
    let g:loaded_python3_provider = 0
endif
"
" Set <leader> to comma (,)
let mapleader=","

" Custom Functions {{{1
" floating window with borders
function! CreateCenteredFloatingWindow()
    let width = min([&columns - 4, max([80, &columns - (&columns / 3)])])
    let height = min([&lines - 4, max([20, &lines - (&lines / 3)])])
    let top = ((&lines - height) / 2) - 1
    let left = (&columns - width) / 2
    let opts = {'relative': 'editor', 'row': top, 'col': left, 'width': width, 'height': height, 'style': 'minimal'}

    let top = "╭" . repeat("─", width - 2) . "╮"
    let mid = "│" . repeat(" ", width - 2) . "│"
    let bot = "╰" . repeat("─", width - 2) . "╯"
    let lines = [top] + repeat([mid], height - 2) + [bot]
    let s:buf = nvim_create_buf(v:false, v:true)
    call nvim_buf_set_lines(s:buf, 0, -1, v:true, lines)
    call nvim_open_win(s:buf, v:true, opts)
    set winhl=Normal:Floating
    let opts.row += 1
    let opts.height -= 2
    let opts.col += 2
    let opts.width -= 4
    call nvim_open_win(nvim_create_buf(v:false, v:true), v:true, opts)
    au BufWipeout <buffer> exe 'bw '.s:buf
endfunction

function! ToggleTerm(cmd)
    if empty(bufname(a:cmd))
        call CreateCenteredFloatingWindow()
        call termopen(a:cmd, { 'on_exit': function('OnTermExit') })
    else
        bwipeout!
    endif
endfunction

function! OnTermExit(job_id, code, event) dict
    if a:code == 0
        bwipeout!
    endif
endfunction
" Plugins {{{1
let s:dein_path = expand('~/.cache/dein')
let s:dein_repo = s:dein_path.'/repos/github.com/Shougo/dein.vim'
if ! isdirectory(s:dein_repo)
    call system("mkdir -p " . s:dein_repo)
    call system("git clone https://github.com/Shougo/dein.vim " . s:dein_repo)
endif
execute 'set runtimepath+='.s:dein_repo

if dein#load_state(s:dein_path)
    call dein#begin(s:dein_path)

    " Let dein manage dein {{{2
    call dein#add(s:dein_repo)

    " General {{{2
    call dein#add('overcache/NeoSolarized')
    call dein#add('lifepillar/vim-cheat40')
    call dein#add('christoomey/vim-tmux-navigator')
    call dein#add('neomake/neomake')
    call dein#add('voldikss/vim-floaterm')
    call dein#add('Shougo/denite.nvim')
    call dein#add('tommcdo/vim-lion')
    call dein#add('junegunn/fzf')

    " Tim Pope plugins {{{3
    call dein#add('tpope/vim-commentary')
    call dein#add('tpope/vim-dispatch')
    call dein#add('tpope/vim-dotenv')
    call dein#add('tpope/vim-eunuch')
    call dein#add('tpope/vim-fugitive')
    call dein#add('tpope/vim-obsession')
    call dein#add('tpope/vim-projectionist')
    call dein#add('tpope/vim-repeat')
    call dein#add('tpope/vim-surround')
    call dein#add('tpope/vim-unimpaired')

    " Completion {{{2
    call dein#add('neovim/nvim-lspconfig')
    call dein#add('nvim-treesitter/nvim-treesitter', {
                \   'hook_post_update': ':TSUpdate'
                \ })
    call dein#add('Shougo/deoplete.nvim')
    call dein#add('Shougo/deoplete-lsp')
    call dein#add('dense-analysis/ale')

    " Snippet Definitions {{{2
    call dein#add('Shougo/neosnippet.vim')
    call dein#add('Shougo/neosnippet-snippets')
    call dein#add('honza/vim-snippets')



    " Filetypes {{{2
    call dein#add('neovimhaskell/haskell-vim', {'on_ft': 'haskell'})
    call dein#add('ledger/vim-ledger', {'on_ft': 'ledger'})
    call dein#add('tmux-plugins/vim-tmux', {'on_ft': 'tmux'})
    call dein#add('saltstack/salt-vim', {'on_ft': 'sls'})
    call dein#add('b4b4r07/vim-hcl', {'on_ft': 'hcl'})

    " }}}

    call dein#end()
    call dein#save_state()
endif

filetype plugin indent on
syntax enable

if dein#check_install()
  call dein#install()
endif

if dein#tap('denite.nvim') " {{{2
    " Use the floating window by default
    call denite#custom#option("_", "split", "floating")

    let s:menus = {
                \ 'nvim': {
                \     'description': 'Edit NeoVim files',
                \     'file_candidates': [
                \         ['init.vim',          '~/.config/nvim/init.vim'],
                \         ['coc-settings.json', '~/.config/nvim/coc-settings.json']
                \         ]
                \     },
                \ 'tmux': {
                \     'description': 'Edit Tmux files',
                \     'file_candidates': [
                \         ['tmux.conf', '~/.tmux.conf']
                \         ]
                \     },
                \ 'zsh': {
                \     'description': 'Edit ZSH files',
                \     'file_candidates': [
                \         ['zlogin',   '~/.zlogin'],
                \         ['zlogout',  '~/.zlogout'],
                \         ['zprofile', '~/.zprofile'],
                \         ['zshenv',   '~/.zshenv'],
                \         ['zshrc',    '~/.zshrc'],
                \         ]
                \     }
                \ }
    call denite#custom#var('menu', 'menus', s:menus)

    " Define alias
    call denite#custom#alias('source', 'file/rec/git', 'file/rec')
    call denite#custom#var('file/rec/git', 'command',
                \['git', 'ls-files', '-co', '--exclude-standard'])

    call denite#custom#alias('source', 'file/rec/darcs', 'file/rec')
    call denite#custom#var('file/rec/darcs', 'command',
                \['darcs', 'show', 'files', '--no-directories', '--pending'])

    " Change ignore_globs
    call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
                \[ '.git/', '*/__pycache__/*', 'venv/', 'pip-wheel-metadata/',
                \  '_darcs/', '*.pyc'])

    " Define mappings
    augroup init
        autocmd FileType denite call s:denite_my_settings()
    augroup END
    function! s:denite_my_settings() abort
        nnoremap <silent><buffer><expr> <CR>
                    \ denite#do_map('do_action')
        nnoremap <silent><buffer><expr> d
                    \ denite#do_map('do_action', 'delete')
        nnoremap <silent><buffer><expr> p
                    \ denite#do_map('do_action', 'preview')
        nnoremap <silent><buffer><expr> q
                    \ denite#do_map('quit')
        nnoremap <silent><buffer><expr> i
                    \ denite#do_map('open_filter_buffer')
        nnoremap <silent><buffer><expr> <Space>
                    \ denite#do_map('toggle_select').'j'
    endfunction

    augroup init
        autocmd FileType denite-filter call s:denite_filter_my_settings()
    augroup END
    function! s:denite_filter_my_settings() abort
        imap <silent><buffer> <C-o> <Plug>(denite_filter_quit)
    endfunction

    nnoremap <silent> <Leader>m :Denite menu<CR>
    nnoremap <silent> <Leader>b :Denite buffer<CR>
    nnoremap <silent> <Leader>ds :Denite spell<CR>
    nnoremap <silent> <Leader>dm :Denite mark<CR>
    nnoremap <silent> <Leader>dt :Denite tag<CR>
    nnoremap <silent> <Leader>df :<C-u>Denite
                \`finddir('_darcs', ';') != '' ? 'file/rec/darcs' : finddir('.git', ';') != '' ? 'file/rec/git' : 'file/rec'`<CR>
endif
if dein#tap('NeoSolarized') " {{{2
    colorscheme NeoSolarized
    hi NormalFloat ctermbg=15 ctermfg=10
    hi NormalFloatSel ctermbg=7 ctermfg=10
    " hi Pmenu ctermbg=15 ctermfg=1
    " hi PmenuSel ctermbg=7 ctermfg=10
endif
if dein#tap('nvim-lspconfig') " {{{2
    lua require('lsp_config')
endif
if dein#tap('vim-tmux-navigator') " {{{2
    " Disable tmux navigator when zooming the Vim pane
    let g:tmux_navigator_disable_when_zoomed = 1
    let g:tmux_navigator_no_mappings = 1
    nnoremap <silent> <A-h> :TmuxNavigateLeft<cr>
    nnoremap <silent> <A-j> :TmuxNavigateDown<cr>
    nnoremap <silent> <A-k> :TmuxNavigateUp<cr>
    nnoremap <silent> <A-l> :TmuxNavigateRight<cr>
    nnoremap <silent> <A-p> :TmuxNavigatePrevious<cr>

    " Terminal exits
    tnoremap <silent> <A-h> <C-\><C-n>:TmuxNavigateLeft<CR>
    tnoremap <silent> <A-j> <C-\><C-n>:TmuxNavigateDown<CR>
    tnoremap <silent> <A-k> <C-\><C-n>:TmuxNavigateUp<CR>
    tnoremap <silent> <A-l> <C-\><C-n>:TmuxNavigateRight<CR>
else
    " Terminal exits
    tnoremap <silent> <A-h> <C-\><C-n><C-w>h
    tnoremap <silent> <A-j> <C-\><C-n><C-w>j
    tnoremap <silent> <A-k> <C-\><C-n><C-w>k
    tnoremap <silent> <A-l> <C-\><C-n><C-w>l
endif
if dein#tap('deoplete.nvim') " {{{2
  let g:deoplete#enable_at_startup = 1
endif
if dein#tap('ale') " {{{2
    let g:ale_completion_autoimport = 1
    let g:ale_completion_enabled = 0
    if dein#tap('deoplete.nvim')
        call deoplete#custom#source('ale', 'rank', 999)
    endif
    nmap <Leader>l    <Plug>(ale_lint)
    nmap <Leader>f    <Plug>(ale_fix)
endif
if dein#tap('neosnippet.vim') " {{{2
    imap <C-l>     <Plug>(neosnippet_expand_or_jump)
    smap <C-l>     <Plug>(neosnippet_expand_or_jump)
    xmap <C-l>     <Plug>(neosnippet_expand_target)

    " let g:neosnippet#disable_runtime_snippets = 1
    let g:neosnippet#snippets_directory = s:dein_path . '/repos/github.com/honza/vim-snippets/snippets'
endif
if dein#tap('haskell-vim') " {{{2
    let g:haskell_classic_highlighting = 1
endif
if dein#tap('vim-floaterm') " {{{2
    let g:floaterm_autoclose = 1
    let g:floaterm_borderchars = ['─', '│', '─', '│', '╭', '╮', '╯', '╰']

    hi FloatermBorder guibg=black guifg=brgreen
    hi Floaterm guibg=black guifg=brgreen

    tnoremap   <silent>   <Leader>fp   <C-\><C-n>:FloatermPrev<CR>
    tnoremap   <silent>   <Leader>fn   <C-\><C-n>:FloatermNext<CR>
    nnoremap   <silent>   <Leader>ff   :FloatermToggle<CR>
    tnoremap   <silent>   <Leader>ff   <C-\><C-n>:FloatermToggle<CR>
    nnoremap   <silent>   <Leader>fs   :FloatermSend<CR>
    vnoremap   <silent>   <Leader>fs   :FloatermSend<CR>

    nnoremap   <silent>   <Leader>fd   :Denite floaterm<CR>
    tnoremap   <silent>   <Leader>fd   <C-\><C-n>:Denite floaterm<CR>
    nnoremap   <silent>   <Leader>lg   :FloatermNew --name=Git lazygit<CR>
    nnoremap   <silent>   <Leader>ld   :FloatermNew --name=Docker lazydocker<CR>

endif
if dein#tap('nvim-treesitter') " {{{2
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
  },
}
EOF
    set foldmethod=expr
    set foldexpr=nvim_treesitter#foldexpr()
endif
if dein#tap('fzf') " {{{2
    let g:fzf_layout = { 'window': 'call CreateCenteredFloatingWindow()' }
    let g:fzf_history_dir = expand('~/.cache/nvim/fzf')
    let $FZF_DEFAULT_OPTS="--layout=reverse " " top to bottom

    " Custom <C-p>: Attemps to use source control lists
    function! FZFCtrlP()
        let l:opts = g:fzf_layout
        let l:opts['sink'] = 'e'
        " Only get as many lines as will be displayed
        let l:lines = &lines - (&lines / 3) - 4
        if system('which bat') != ''
            " bat provides line numbers, syntax highlighting, and Git markers
            let l:opts['options'] = '--preview=''head -n '.l:lines.' {} | bat --style numbers,changes --color=always'' '
        else
            " but regular cat works just fine too
            let l:opts['options'] = '--preview=''head -n '.l:lines.' {} | cat'' '
        endif
        if finddir('_darcs', ';') != ''
            let l:opts['source'] = 'darcs show files --no-directories --pending'
        elseif finddir('.git', ';') != ''
            let l:opts['source'] = 'git ls-files -co --exclude-standard'
        endif
        call fzf#run(l:opts)
    endfunction

    nnoremap <silent> <C-p> :call FZFCtrlP()<CR>
endif
" General {{{1
" Turn on Mode Lines
set modeline modelines=3

" Set Line numbers
set number norelativenumber

" My tab preferences
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

" Keep some context at screen edges
set scrolloff=5 sidescrolloff=5

" Ignore case in searching...
set ignorecase
" ...except if search string contains uppercase
set smartcase

" Don't use the mouse. Ever.
set mouse=

" Reload init when it is modified
autocmd init BufWritePost ~/.config/nvim/init.vim source <afile>

" Split windows below, or to the right of, the current window
set splitbelow splitright

" Tell Vim which characters to show for expanded TABs,
" trailing whitespace, and end-of-lines. VERY useful!
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif
set list                " Show problematic characters.

" Also highlight all tabs and trailing whitespace characters.
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
" match ExtraWhitespace /\s\+$\|\t/

" Files, backups and undo {{{1
" Keep backups in cache folder, so as not to clutter filesystem.
set backup backupdir=~/.cache/nvim/backup,~/.cache/vim/backup,.
set undofile undodir=~/.cache/nvim/undo,~/.cache/vim/undo,.
set directory=~/.cache/nvim/other,~/.cache/vim/other,.
" Don't need backups for tmp files (usually sudo -e)
augroup init
  autocmd BufRead,BufEnter /var/tmp/* set nobackup noundofile nowritebackup
augroup END
" vim: fdm=marker
