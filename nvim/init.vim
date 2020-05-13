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

" Set Python Host programs
let g:python3_host_prog=expand("~/.pyenv/versions/nvim/bin/python")

" Set <leader> to comma (,)
let mapleader=","

" Get MyConfig
let s:myconfig_prompt = system("cat ${HOME}/.config/MyConfig | grep USE_PROMPT | cut -d'=' -f2 | tr -d '\n'")

" Pre-setup }}}
" {{{ Custom Functions

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
" }}}
" {{{ Plugins
" Load plugins first, so they are availible to later code {{{
if exists('g:vscode')
    let s:dein_path = expand('~/.cache/dein/vscode')
else
    let s:dein_path = expand('~/.cache/dein/nvim')
endif
let s:dein_repo = s:dein_path.'/repos/github.com/Shougo/dein.vim'
if isdirectory(s:dein_repo)
    " Dein-managed Dein (prefered)
    execute 'set rtp+='.s:dein_repo
else
    " Minimal bootstrap
    set rtp+=~/.config/nvim/dein.vim
endif

function! CocDeps()
    let extensions = ['coc-tag', 'coc-gocode', 'coc-neosnippet', 'coc-html', 'coc-css', 'coc-yaml', 'coc-tsserver', 'coc-json']
    call coc#util#install_extension(extensions)
endfunction

let g:dein#auto_recache = 1
if dein#load_state(s:dein_path)
    call dein#begin(s:dein_path)
    " Common conditionals
    let s:in_gui = exists('g:vscode')

    " Global Plugins
    call dein#add('dahu/bisectly')
    call dein#add('easymotion/vim-easymotion',
                \{'if': !exists('g:vscode')})
    call dein#add('asvetliakov/vim-easymotion',
                \{'if': exists('g:vscode')}) " Fork that works (better) with VSCode
    call dein#add('godlygeek/tabular')
    call dein#add('svermeulen/vim-cutlass')
    call dein#add('svermeulen/vim-yoink')
    call dein#add('svermeulen/vim-subversive')
    call dein#add('tommcdo/vim-exchange')
    call dein#add('tpope/vim-commentary',
                \{'if': !exists('g:vscode')}) " Use VSCode calls when availible
    call dein#add('tpope/vim-repeat')
    call dein#add('tpope/vim-surround')
    call dein#add('tpope/vim-unimpaired')

    call dein#add('benekastah/neomake',
                \{'if': !s:in_gui})
    call dein#add('chrisbra/NrrwRgn',
                \{'if': !s:in_gui})
    call dein#add('editorconfig/editorconfig-vim',
                \{'if': !s:in_gui})
    call dein#add('tpope/vim-eunuch',
                \{'if': !s:in_gui})
    call dein#add('tpope/vim-fugitive')
    call dein#add('tpope/vim-obsession',
                \{'if': !s:in_gui})
    call dein#add('tpope/vim-projectionist',
                \{'if': !s:in_gui})
    call dein#add('junegunn/fzf',
                \{'if': !s:in_gui})
    call dein#add('Konfekt/FastFold',
                \{'if': !s:in_gui})
    call dein#add('Shougo/dein.vim',
                \{'if': !s:in_gui})
    call dein#add('Shougo/denite.nvim',
                \{'if': !s:in_gui})
    call dein#add('Shougo/neosnippet.vim',
                \{'if': 0})
    call dein#add('Shougo/defx.nvim',
                \{'if': !s:in_gui})
    call dein#add('airblade/vim-gitgutter',
                \{'if': !s:in_gui})
    call dein#add('iCyMind/NeoSolarized',
                \{'if': !s:in_gui})
    call dein#add('christoomey/vim-tmux-navigator',
                \{'if': !s:in_gui})
    " call dein#add('garbas/vim-snipmate.git',
    "             \{'if': !s:in_gui})
    call dein#add('majutsushi/tagbar',
                \{'if': !s:in_gui})
    call dein#add('neoclide/coc.nvim',
                \{'merged':0, 'rev': 'release', 'hook_post_update': function('CocDeps')})
    call dein#add('radenling/vim-dispatch-neovim',
                \{'if': !s:in_gui})
    call dein#add('sakhnik/nvim-gdb',
                \{'if': !s:in_gui})
    call dein#add('tmux-plugins/vim-tmux',
                \{'if': !s:in_gui})
    call dein#add('tmux-plugins/vim-tmux-focus-events',
                \{'if': !s:in_gui})
    call dein#add('tpope/vim-dispatch',
                \{'if': !s:in_gui})
    call dein#add('tpope/vim-dadbod',
                \{'if': !s:in_gui})
    call dein#add('tpope/vim-dotenv',
                \{'if': !s:in_gui})

    let s:use_powerline = s:myconfig_prompt == "powerline" && !s:in_gui
    call dein#add('edkolev/promptline.vim',
                \{'if': s:use_powerline})
    call dein#add('edkolev/tmuxline.vim',
                \{'if': s:use_powerline})
    call dein#add('vim-airline/vim-airline',
                \{'if': s:use_powerline})
    call dein#add('vim-airline/vim-airline-themes',
                \{'if': s:use_powerline})


    " Snippet Definitions
    call dein#add('honza/vim-snippets',
                \{'if': !s:in_gui})
    call dein#add('Shougo/neosnippet-snippets',
                \{'if': !s:in_gui})

    " Ledger
    call dein#add('ledger/vim-ledger',
                \{'on_ft': 'ledger'})

    " Haskell
    " call dein#add('alx741/vim-stylishask',
                " \{'if': !s:in_gui, 'on_ft': 'haskell'})
    " call dein#add('alx741/hindent',
                " \{'if': !s:in_gui, 'on_ft': 'haskell'})
    " call dein#add('neovimhaskell/haskell-vim',
                " \{'if': !s:in_gui, 'on_ft': 'haskell'})
    " call dein#add('mpickering/hlint-refactor-vim',
                " \{'if': !s:in_gui, 'on_ft': 'haskell'})

    " Python
    call dein#add('vim-scripts/python.vim',
                \{'if': !s:in_gui, 'on_ft': 'python'})
    call dein#add('tmhedberg/SimpylFold',
                \{'if': !s:in_gui, 'on_ft': 'python'})

    " JS/Web
    call dein#add('mxw/vim-jsx',
                \{'if': !s:in_gui, 'on_ft': 'jsx'})
    call dein#add('leafgarland/typescript-vim',
                \{'if': !s:in_gui, 'on_ft': 'jsx'})

    " Mono/C#
    call dein#add('OmniSharp/omnisharp-vim',
                \{'if': !s:in_gui, 'on_ft': 'csharp'})

    " Other Language
    call dein#add('saltstack/salt-vim',
                \{'if': !s:in_gui, 'on_ft': 'sls'})

    " Dependencies
    call dein#add('MarcWeber/vim-addon-mw-utils.git')
    call dein#add('tomtom/tlib_vim')
    if !has('nvim')
        call dein#add('roxma/nvim-yarp')
        call dein#add('roxma/vim-hug-neovim-rpc')
    endif

    call dein#end()
    call dein#save_state()
endif
if dein#check_install()
    call dein#install()
    call dein#remote_plugins()
endif
" Plugin Init/Load }}}
" Plugin Configuration {{{
if dein#tap('neomake') " {{{
    let g:neomake_open_list=2
    " call neomake#configure#automake('nw', 500)

    let g:neomake_python_maker = {
                \ 'exe': 'pipenv',
                \ 'args': 'run py.test --color=no -p no:sugar --tb=short -q'
                \ }

    " Disable Haskell makers, we have a few other tools for this one
    let g:neomake_haskell_enabled_makers = []

endif " }}}
if dein#tap('NeoSolarized') " {{{
    colorscheme NeoSolarized
endif " }}}
if dein#tap('vim-easymotion') " {{{
    let g:EasyMotion_smartcase = 1
    " map <Leader> <Plug>(easymotion-prefix)
    " nmap s <Plug>(easymotion-overwin-f)
    map  <Leader><Leader>/ <Plug>(easymotion-sn)
    omap <Leader><Leader>/ <Plug>(easymotion-tn)
    map  <Leader><Leader>l <Plug>(easymotion-lineforward)
    map  <Leader><Leader>h <Plug>(easymotion-linebackward)
    let g:EasyMotion_startofline = 0 " keep cursor column when JK motion
endif "}}}
if dein#tap('editorconfig-vim') " {{{
    let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*', 'defx://.*']
endif " }}}
if dein#tap('tabular') " {{{
    let g:haskell_tabular = 1
    nnoremap <Leader>a= :Tabularize /=<CR>
    nnoremap <Leader>a; :Tabularize /::<CR>
    nnoremap <Leader>a- :Tabularize /-><CR>
    nnoremap <leader>a, :Tabularize /,<CR>
    nnoremap <leader>a# :Tabularize /#-}<CR>
endif " }}}
if dein#tap('tagbar') " {{{
    nnoremap <silent> <F8> :TagbarToggle<CR>
    nnoremap <silent> <F9> :TagbarOpen fjc<CR>
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
if dein#tap('denite.nvim') " {{{
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
    autocmd FileType denite call s:denite_my_settings()
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

    autocmd FileType denite-filter call s:denite_filter_my_settings()
    function! s:denite_filter_my_settings() abort
        imap <silent><buffer> <C-o> <Plug>(denite_filter_quit)
    endfunction

    nnoremap <Leader>m :Denite -split=floating menu<CR>
    nnoremap <C-b> :Denite -split=floating buffer<CR>
    " nnoremap <silent> <C-p> :<C-u>Denite -split=floating 
    "             \`finddir('_darcs', ';') != '' ? 'file/rec/darcs' : finddir('.git', ';') != '' ? 'file/rec/git' : 'file/rec'`<CR>
endif " }}}
if dein#tap('vim-airline') " {{{
    let g:airline#extensions#tabline#enabled = 1
    let g:airline_powerline_fonts = 1
    let g:airline_section_c = airline#section#create(['file', '%{ObsessionStatus()}'])
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
    "     \'x' : '#(uptime|egrep -o "([0-9]+\.[0-9]{2}(, )?){3}")',
    "     \'y' : [ '%R', '%a', '%Y'],
    "     \'z' : '#H'}
endif " }}}
if dein#tap('tmhedberg/SimpylFold') " {{{
    let g:SimpylFold_fold_import = 0
    leh g:SimpylFold_docstring_preview = 1
endif " }}}
if dein#tap('LanguageClient-neovim') " {{{
    let g:LanguageClient_serverCommands = {
    \ 'python': ['/Users/t0m00fc/Library/Python/3.7/bin/pyls'],
    \ }

    nnoremap <F5> :call LanguageClient_contextMenu()<CR>
    " Or map each action separately
    nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
    nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
    nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
endif " }}}
if dein#tap('tmux-complete.vim') " {{{
    let g:tmuxcomplete#trigger = ''
endif " }}}
if dein#tap('ale') " {{{
    if dein#tap('vim-airline')
        let g:airline#extensions#ale#enabled = 1
    endif
    " nmap <silent> <C-k> <Plug>(ale_previous_wrap)
    " nmap <silent> <C-j> <Plug>(ale_next_wrap)
    let g:ale_completion_enabled = 1
    let g:ale_completion_experimental_lsp_support = 1
    let g:ale_linters = {
    \'python': ['pyls'],
    \'haskell': ['ghc_mod', 'stack_ghc_mod', 'hdevtools', 'hie', 'hlint', 'stack_build', 'stack_ghc']
    \}
    let g:ale_fixers = {
    \'python': [
    \   'add_blank_lines_for_python_control_statements',
    \   'isort',
    \   'remove_trailing_lines',
    \   'trim_whitespace',
    \   'yapf'],
    \'javascript': [
    \   'eslint',
    \   'importjs',
    \   'trim_whitespace'],
    \'haskell': [
    \   'hlint',
    \   'remove_trailing_lines',
    \   'stylish-haskell',
    \   'trim_whitespace']
    \}
endif " }}}
if dein#tap('neosnippet.vim') " {{{
    if dein#tap('vim-snipmate')
        let g:neosnippet#enable_snipmate_compatibility = 1
        let g:neosnippet#snippets_directory = s:dein_path.'/repos/github.com/honza/vim-snippets/snippets'
    endif

    " " Plugin key-mappings.
    " " Note: It must be "imap" and "smap".  It uses <Plug> mappings.
    " imap <C-k> <Plug>(neosnippet_expand_or_jump)
    " smap <C-k> <Plug>(neosnippet_expand_or_jump)
    " xmap <C-k> <Plug>(neosnippet_expand_target)

    " " Note: It must be "imap" and "smap".  It uses <Plug> mappings.
    " imap <expr><M-l> neosnippet#expandable_or_jumpable() ?
    "                \ "\<Plug>(neosnippet_jump_or_expand)" : "\<TAB>"
    " smap <expr><M-l> neosnippet#expandable_or_jumpable() ?
    "                \ "\<Plug>(neosnippet_jump_or_expand)" : "\<TAB>"

    " For conceal markers.
    if has('conceal')
        set conceallevel=2 concealcursor=n
    endif
endif "}}}
if dein#tap('vim-tmux-navigator') " {{{
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
endif " }}}
if dein#tap('vim-hindent') " {{{
    let g:hindent_on_save = 0
    au FileType haskell nnoremap <silent> <leader>ph :Hindent<CR>
endif " }}}
if dein#tap('vim-stylishask') " {{{
    let g:stylishask_on_save = 0
    au FileType haskell nnoremap <silent> <leader>ps :Stylishask<CR>
endif " }}}
if dein#tap('coc.nvim') " {{{
    " if hidden is not set, TextEdit might fail.
    set hidden

    " Smaller updatetime for CursorHold & CursorHoldI
    set updatetime=300

    " don't give |ins-completion-menu| messages.
    set shortmess+=c

    " always show signcolumns
    set signcolumn=yes

    " Use `[c` and `]c` for navigate diagnostics
    nmap <silent> [c <Plug>(coc-diagnostic-prev)
    nmap <silent> ]c <Plug>(coc-diagnostic-next)

    " Remap keys for gotos
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Use K for show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
        if &filetype == 'vim'
            execute 'h '.expand('<cword>')
        else
            call CocAction('doHover')
        endif
    endfunction

    " Remap for rename current word
    nmap <leader>rn <Plug>(coc-rename)

    " Remap for format selected region
    vmap <leader>f  <Plug>(coc-format-selected)
    nmap <leader>f  <Plug>(coc-format-selected)

    augroup cocgroup
        autocmd!
        " Setup formatexpr specified filetype(s).
        autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
        " Update signature help on jump placeholder
        autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

    " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
    vmap <leader>a  <Plug>(coc-codeaction-selected)
    nmap <leader>a  <Plug>(coc-codeaction-selected)

    " Remap for do codeAction of current line
    nmap <leader>ac  <Plug>(coc-codeaction)
    " Fix autofix problem of current line
    nmap <leader>qf  <Plug>(coc-fix-current)

    " Use `:Format` for format current buffer
    command! -nargs=0 Format :call CocAction('format')

    " Use `:Fold` for fold current buffer
    command! -nargs=? Fold :call CocAction('fold', <f-args>)

    " Using CocList
    " Show all diagnostics
    nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
    " Manage extensions
    nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
    " Show commands
    nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
    " Find symbol of current document
    nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
    " Search workspace symbols
    nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
    " Do default action for next item.
    nnoremap <silent> <space>j  :<C-u>CocNext<CR>
    " Do default action for previous item.
    nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
    " Resume latest coc list
    nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

    " Use <C-l> for trigger snippet expand.
    imap <C-l> <Plug>(coc-snippets-expand)

    " Use <C-j> for select text for visual placeholder of snippet.
    vmap <C-j> <Plug>(coc-snippets-select)

    " Use <C-j> for both expand and jump (make expand higher priority.)
    imap <C-j> <Plug>(coc-snippets-expand-jump)

    inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    let g:coc_snippet_next = '<tab>'
endif " }}}
if dein#tap('deoplete.nvim') " {{{
    let g:deoplete#enable_at_startup = 1
    if dein#tap('omnisharp-vim')
        call deoplete#custom#option('sources', {'cs': ['omnisharp']})
    endif
endif " }}}
if dein#tap('vim-yoink') " {{{
    let g:yoinkIncludeDeleteOperations = 1

    nmap <m-n> <plug>(YoinkPostPasteSwapBack)
    nmap <m-p> <plug>(YoinkPostPasteSwapForward)

    nmap p <plug>(YoinkPaste_p)
    nmap P <plug>(YoinkPaste_P)

    nmap [y <plug>(YoinkRotateBack)
    nmap ]y <plug>(YoinkRotateForward)

    nmap <c-=> <plug>(YoinkPostPasteToggleFormat)
endif " }}}
if dein#tap('vim-subversive') " {{{
    " s for substitute
    nmap s <plug>(SubversiveSubstitute)
    nmap ss <plug>(SubversiveSubstituteLine)
    nmap S <plug>(SubversiveSubstituteToEndOfLine)

    nmap <leader>s <plug>(SubversiveSubstituteRange)
    xmap <leader>s <plug>(SubversiveSubstituteRange)
    nmap <leader>ss <plug>(SubversiveSubstituteWordRange)

    nmap <leader>cr <plug>(SubversiveSubstituteRangeConfirm)
    xmap <leader>cr <plug>(SubversiveSubstituteRangeConfirm)
    nmap <leader>crr <plug>(SubversiveSubstituteWordRangeConfirm)

    if dein#tap('vim-abolish')
        nmap <leader><leader>s <plug>(SubversiveSubvertRange)
        xmap <leader><leader>s <plug>(SubversiveSubvertRange)
        nmap <leader><leader>ss <plug>(SubversiveSubvertWordRange)
    endif

    " ie = inner entire buffer
    onoremap ie :exec "normal! ggVG"<cr>

    " iv = current viewable text in the buffer
    onoremap iv :exec "normal! HVL"<cr>
endif " }}}
if dein#tap('vim-cutlass') " {{{
    nnoremap x d
    xnoremap x d
    nnoremap xx dd
    nnoremap X D
endif " }}}
if dein#tap('fzf') " {{{
    let g:fzf_layout = { 'window': 'call CreateCenteredFloatingWindow()' }
    " let g:fzf_history_dir = expand('~/.cache/nvim/fzf')
    let $FZF_DEFAULT_OPTS="--layout=reverse " " top to bottom

    " Custom <C-p>: Attemps to use source control lists
    function! FZFCtrlP()
        let l:opts = g:fzf_layout
        let l:opts['sink'] = 'e'
        if system('which bat') != ''
            let l:opts['options'] = '--prompt='':e '' --preview=''bat --style header,numbers,changes --color=always {}'' '
        else
            let l:opts['options'] = '--prompt='':e '' --preview=''cat {}'' '
        endif
        if finddir('_darcs', ';') != ''
            let l:opts['source'] = 'darcs show files --no-directories --pending'
        elseif finddir('.git', ';') != ''
            let l:opts['source'] = 'git ls-files -co --exclude-standard'
        endif
        call fzf#run(l:opts)
    endfunction

    nnoremap <silent> <C-p> :call FZFCtrlP()<CR>
endif " }}}
if exists('g:vscode') " {{{
    xmap gc  <Plug>VSCodeCommentary
    nmap gc  <Plug>VSCodeCommentary
    omap gc  <Plug>VSCodeCommentary
    nmap gcc <Plug>VSCodeCommentaryLine
endif " }}}
" }}}
" Plugins }}}
" {{{ General

" When term starts, auto go into insert mode
autocmd TermOpen * startinsert

" Turn off line numbers etc
autocmd TermOpen * setlocal listchars= nonumber norelativenumber

" Some scratchpads
nnoremap <silent> <Leader>st :call ToggleTerm($SHELL)<CR>
nnoremap <silent> <Leader>sg :call ToggleTerm('lazygit')<CR>
nnoremap <silent> <Leader>sd :call ToggleTerm('lazydocker')<CR>

" Better display for messages
set cmdheight=2

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

" Turn off Line numbers
set nonumber norelativenumber

" Highlight current line
set cursorline nocursorcolumn

" Allow virtual edit in block mode
set virtualedit=block

" My tab preferences
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

" Keep some context at screen edges
set scrolloff=5 sidescrolloff=5

" Ignore case in searching...
set ignorecase
" ...except if search string contains uppercase
set smartcase

if exists('g:gui_oni')
    " Enable GUI mouse
    set mouse=a

    " If using Oni's externalized statusline, hide vim's native statusline,
    set noshowmode
    set noruler
    set laststatus=0
    set noshowcmd
else
    " Set terminal title
    set titlestring=NeoVim\ (%F)

    " Don't use the mouse. Ever.
    set mouse=

    " Configure statusline
    if !dein#tap('vim-airline') " {{{
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
endif

" Reload init when it is modified
" autocmd init BufWritePost ~/.config/nvim/init.vim source <afile>

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

" General }}}
" {{{ Files, backups and undo
if exists('g:gui_oni')
    set noswapfile
else
    " Keep backups in cache folder, so as not to clutter filesystem.
    set backup backupdir=~/.cache/nvim/backup,~/.cache/vim/backup,.
    set undofile undodir=~/.cache/nvim/undo,~/.cache/vim/undo,.
    set directory=~/.cache/nvim/other,~/.cache/vim/other,.
    " Don't need backups for tmp files (usually sudo -e)
    autocmd init BufRead,BufEnter /var/tmp/* set nobackup noundofile nowritebackup
endif
" Files, backups and undo }}}
" vim: fdm=marker
