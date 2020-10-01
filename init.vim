" Configuration {{{1
" Key {{{2
" Leader key convention 
" cons: hard to reverse f{motion}
nnoremap <Space> <Nop>
let mapleader = ' '
" It would be clever to differentiate from leader key.
" e.g. vim-sexp case
let maplocalleader = ','

" Search {{{2
" grepper {{{3
" let g:grepper		= {}
" let g:grepper.tools	= ['grep', 'git', 'rg']
" from :h grepper-operator
" runtime plugin/grepper.vim  " initialize g:grepper with default values
" let g:grepper.operator.prompt = 1
" ↑ doesn't work: There are no dictionary key such as operator.prompt
" fix ↓
" if !exists('g:grepper.operator')
" 	let g:grepper.operator = {}
" endif
" let g:grepper.operator.prompt = 1
" Linting option {{{2

" change the default setting as linting on save {{{3
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_save = 1                " default
let g:ale_lint_on_enter = 0
let g:ale_lint_on_filetype_changed = 0
" Test {{{2
let test#strategy = "dispatch"
" Terminal {{{2
" Project {{{2
" Link with projectionist event
augroup configure_projects
  autocmd!
  autocmd User ProjectionistActivate call s:linters() | call s:hardwrap()
augroup END

" Why should I use abort notation?
" `:h :func-abort` 
"       as soon as encouter with exception,
"       it aborts
" which means it doesn't have to notify the user,
" and it doesn't have to do afterwork such as close
" file etc.
" Link ale with projectionist's meta data
" projectionist's meta data, .projections.json
" which can contain linters and other options well.
" These data can be queried by projectionist#query 
" function.

function! s:linters() abort
  let l:linters = projectionist#query('linters')
  if len(l:linters) > 0
    let b:ale_linters = {&filetype: l:linters[0][1]}
  endif
endfunction

" Link vim configuration with projectionist's meta data
function! s:hardwrap() abort
  for [root, value] in projectionist#query('hardwrap')
    let &l:textwidth = value
    break
  endfor
endfunction
" }}}1
" Mappings {{{1
" noremap <expr> <leader>c (synIDattr(synID(line("."), col("."), 0), "name") =~ 'comment\c') ?
" \ ':<S-Left>exe "<S-Right>normal! "."^".len(b:commentCommand[1:])."x"<CR>' :
" \ ':<S-Left>exe "<S-Right>normal! ".b:commentCommand<CR>'
" }}}1
" It was convenient to set noh as mapping when I used in VSCodeVim
noremap <Leader>/ :noh<CR>

" Package managing {{{1
packadd minpac

call minpac#init()
" by Modern vim {{{2
call minpac#add('tpope/vim-unimpaired')
call minpac#add('tpope/vim-projectionist', {'type': 'opt'})
" fuzzy finder
call minpac#add('junegunn/fzf')
" fzf for vim
call minpac#add('junegunn/fzf.vim')
" PLUGIN: fzf.vim{{{

let g:fzf_layout = { 'down': '~40%' }

" Populate quickfix list with selected files
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  botright copen
  cc
endfunction

" Ctrl-q allows to select multiple elements an open them in quick list
let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit' }

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Add namespace for fzf.vim exported commands
let g:fzf_command_prefix = 'Fzf'

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

" File path completion in Insert mode using fzf
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-l> <plug>(fzf-complete-buffer-line)

" Use preview when FzfFiles runs in fullscreen
command! -nargs=? -bang -complete=dir FzfFiles
      \ call fzf#vim#files(<q-args>, <bang>0 ? fzf#vim#with_preview('up:60%') : {}, <bang>0)

" Mappings
nnoremap <silent> <leader>o :FzfFiles<CR>
nnoremap <silent> <leader>O :FzfFiles!<CR>
nnoremap <silent> <leader>b :FzfBuffers<CR>
nnoremap <silent> <leader>l :FzfBLines<CR>
nnoremap <silent> <leader>` :FzfMarks<CR>
nnoremap <silent> <leader>p :FzfCommands<CR>
nnoremap <silent> <leader>t :FzfFiletypes<CR>
nnoremap <silent> <F1>      :FzfHelptags<CR>
inoremap <silent> <F1> <ESC>:FzfHelptags<CR>
inoremap <silent> <F3> <ESC>:FzfSnippets<CR>
cnoremap <silent> <expr> <C-p> getcmdtype() == ":" ? "<C-u>:FzfHistory:\<CR>" : "\<ESC>:FzfHistory/\<CR>"
cnoremap <silent> <C-_> <C-u>:FzfCommands<CR>

" fzf.Tags uses existing 'tags' file or generates it otherwise
nnoremap <silent> <leader>] :FzfTags<CR>
xnoremap <silent> <leader>] "zy:FzfTags <C-r>z<CR>

" fzf.BTags generate tags on-fly for current file
nnoremap <silent> <leader>} :FzfBTags<CR>
xnoremap <silent> <leader>} "zy:FzfBTags <C-r>z<CR>

" Show list of change in fzf
" Some code is borrowed from ctrlp.vim and tweaked to work with fzf
command! FzfChanges call s:fzf_changes()
nnoremap <silent> <leader>; :FzfChanges<CR>

function! s:fzf_changelist()
  redir => result
  silent! changes
  redir END

  return map(split(result, "\n")[1:], 'tr(v:val, "	", " ")')
endf

function! s:fzf_changeaccept(line)
  let info = matchlist(a:line, '\s\+\(\d\+\)\s\+\(\d\+\)\s\+\(\d\+\).\+$')
  call cursor(get(info, 2), get(info, 3))
  silent! norm! zvzz
endfunction

function! s:fzf_changes()
  return fzf#run(fzf#wrap({
        \ 'source':  reverse(s:fzf_changelist()),
        \ 'sink': function('s:fzf_changeaccept'),
        \ 'options': '+m +s --nth=3..'
        \ }))
endfunction

" Enable per-command history.
" CTRL-N and CTRL-P will be automatically bound to next-history and
" previous-history instead of down and up. If you don't like the change,
" explicitly bind the keys to down and up in your $FZF_DEFAULT_OPTS.
let g:fzf_history_dir = '~/.local/share/fzf-history'
" }}}
" Modern vim chapter 4
" short for asynchronous linting engine
call minpac#add('w0rp/ale')
" sneak, which was quite handful when I used in VSCodeVim
call minpac#add('justinmk/vim-sneak')
" comment supoport
call minpac#add('tpope/vim-commentary')
" asyncronous compile wrapper
call minpac#add('tpope/vim-dispatch')
" grep-like program wrapper
" call minpac#add('mhinz/vim-grepper')
" {{{ Project-wide search

let g:search_ignore_dirs = ['.git', 'node_modules']

" TODO: git grep when under repository
" Choose grep backend, use ripgrep if available
if executable("rg")
  set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ --hidden\ --follow
  set grepformat=%f:%l:%c:%m
else
  set grepprg=grep\ -n\ --with-filename\ -I\ -R
  set grepformat=%f:%l:%m
endif

" Without bang, search is relative to cwd, otherwise relative to current file
command! -nargs=* -bang -complete=file Grep call <SID>execute_search("Grep", <q-args>, <bang>0)
command! -nargs=* -bang -complete=file GrepFzf call <SID>execute_search("GrepFzf", <q-args>, <bang>0)
" command -nargs=* -bang -complete=file GrepSF call <SID>execute_search_ctrlsf(<q-args>, <bang>0)

" :grep + grepprg + quickfix list
nnoremap <F7><F7> :call <SID>prepare_search_command("", "Grep")<CR>
nnoremap <F7>w :call <SID>prepare_search_command("word", "Grep")<CR>
nnoremap <F7>s :call <SID>prepare_search_command("selection", "Grep")<CR>
nnoremap <F7>/ :call <SID>prepare_search_command("search", "Grep")<CR>
vnoremap <silent> <F7> :call <SID>prepare_search_command("selection", "Grep")<CR>

" fzf-vim + ripgrep
nnoremap <F15><F15> :call <SID>prepare_search_command("", "GrepFzf")<CR>
nnoremap <F15>w :call <SID>prepare_search_command("word", "GrepFzf")<CR>
nnoremap <F15>s :call <SID>prepare_search_command("selection", "GrepFzf")<CR>
nnoremap <F15>/ :call <SID>prepare_search_command("search", "GrepFzf")<CR>
vnoremap <silent> <F15> :call <SID>prepare_search_command("selection", "GrepFzf")<CR>

" ctrlsf.vim (uses ack, ag or rg under the hood)
" nnoremap <F8><F8> :call <SID>prepare_search_command("", "GrepSF")<CR>
" nnoremap <F8>w :call <SID>prepare_search_command("word", "GrepSF")<CR>
" nnoremap <F8>s :call <SID>prepare_search_command("selection", "GrepSF")<CR>
" nnoremap <F8>/ :call <SID>prepare_search_command("search", "GrepSF")<CR>
" vnoremap <silent> <F8> :call <SID>prepare_search_command("selection", "GrepSF")<CR>


" Execute search for particular command (Grep, GrepSF, GrepFzf)
function! s:execute_search(command, args, is_relative)
  if empty(a:args)
    call s:echo_warning("Search text not specified")
    return
  endif

  let extra_args = []
  let using_ripgrep = &grepprg =~ '^rg'

  " Set global mark to easily get back after we're done with a search
  normal mF

  " Exclude well known ignore dirs
  " This is useful for GNU grep, that does not respect .gitignore
  let ignore_dirs = s:get_var('search_ignore_dirs')
  for l:dir in ignore_dirs
    if using_ripgrep
      call add(extra_args, '--glob ' . shellescape(printf("!%s/", l:dir)))
    else
      call add(extra_args, '--exclude-dir ' . shellescape(printf("%s", l:dir)))
    endif
  endfor

  " Change cwd temporarily if search is relative to the current file
  if a:is_relative
    exe "cd " . expand("%:p:h")
  endif

  " Execute :grep + grepprg search, show results in quickfix list
  if a:command ==# 'Grep'
    " Perform search
    silent! exe "grep! " . join(extra_args) . " " . a:args
    redraw!

    " If matches are found, open quickfix list and focus first match
    " Do not open with copen, because we have qf list automatically open on search
    if len(getqflist())
      cc
    else
      cclose
      call s:echo_warning("No match found")
    endif
  endif

  " Execute search using fzf.vim + grep/ripgrep
  if a:command ==# 'GrepFzf'
    " Run in fullscreen mode, with preview at the top
    call fzf#vim#grep(printf("%s %s --color=always %s", &grepprg, join(extra_args), a:args),
          \ 1,
          \ fzf#vim#with_preview('up:60%'),
          \ 1)
  endif

  " Restore cwd back
  if a:is_relative
    exe "cd -"
  endif
endfunction

function! s:execute_search_ctrlsf(args, is_relative)
  if empty(a:args)
    call s:echo_warning("Search text not specified")
    return
  endif

  " Show CtrlSF search pane in new tab
  tab split
  let t:is_ctrlsf_tab = 1

  " Change cwd, but do it window-local
  " Do not restore cwd, because ctrlsf#Search is async
  " Just close tab, when you're done with a search
  if a:is_relative
    exe "lcd " . expand("%:p:h")
  endif

  " Create new scratch buffer
  enew
  setlocal bufhidden=delete nobuflisted

  " Execute search
  call ctrlsf#Search(a:args)
endfunction

" Initiate search, prepare command using selected backend and context for the search
" Contexts are: word, selection, last search pattern
function! s:prepare_search_command(context, backend)
  let text = a:context ==# 'word' ? expand("<cword>")
        \ : a:context ==# 'selection' ? s:get_selected_text()
        \ : a:context ==# 'search' ? @/
        \ : ''

  " Properly escape search text
  " Remove new lines (when several lines are visually selected)
  let text = substitute(text, "\n", "", "g")

  " Escape backslashes
  let text = escape(text, '\')

  " Put in double quotes
  let text = escape(text, '"')
  let text = empty(text) ? text : '"' . text . '"'

  " Grep/ripgrep/ctrlsf args
  " Always search literally, without regexp
  " Use word boundaries when context is 'word'
  let args = [a:backend ==# 'GrepSF' ? '-L' : '-F']
  if a:context ==# 'word'
    call add(args, a:backend ==# 'GrepSF' ? '-W' : '-w')
  endif

  " Compose ":GrepXX" command to put on a command line
  let search_command = ":\<C-u>" . a:backend
  let search_command .= empty(args) ? ' ' : ' ' . join(args, ' ') . ' '
  let search_command .= '-- ' . text

  " Put actual command in a command line, but do not execute
  " User would initiate a search manually with <CR>
  call feedkeys(search_command, 'n')
endfunction

" Resolves variable value respecting window, buffer, global hierarchy
function! s:get_var(...)
  let varName = a:1

  if exists('w:' . varName)
    return w:{varName}
  elseif exists('b:' . varName)
    return b:{varName}
  elseif exists('g:' . varName)
    return g:{varName}
  else
    return exists('a:2') ? a:2 : ''
  endif
endfunction

" }}}
" Tip 14: 
call minpac#add('janko-m/vim-test', {'type': 'opt'})
" Tip 23: session track, relevant with :mksession
call minpac#add('tpope/vim-obsession', {'type': 'opt'})
" Tip 27: for meta data .editorconfig
call minpac#add('sgur/vim-editorconfig', {'type': 'opt'})
" by Practical Vim & vimcast {{{2
" for visual mode of `*`, `#`
call minpac#add('nelstrom/vim-visual-star-search')
" Snippit for vim using python (experimental)
call minpac#add('SirVer/ultisnips')
    " Use python version 3 for ultisnips
    let g:UltiSnipsUsePythonVersion = 3
    " Save my private snippet files into below directory
    let g:UltiSnipsSnippetsDir = '~/dotfiles/UltiSnips'
    " disable the searching for SnipMate snippets
    let g:UltiSnipsEnableSnipMate = 0
    " Use <C-Space> instead of <C-Tab> which couldn't be used
    " in terminal for list command
    let g:UltiSnipsListSnippets = '<C-Space>'
    " " to detour the interference with BackwardTrigger
    " inoremap <C-x><C-k> <C-x><C-k>
" repl support using terminal buffer
call minpac#add('Vigemus/iron.nvim')
" racket support for sicp
call minpac#add('wlangstroth/vim-racket')
" UltiSnips's sinppets
call minpac#add('honza/vim-snippets')
" modify statusline theme more stylush
call minpac#add('vim-airline/vim-airline')
" the themes with vim-airline
call minpac#add('vim-airline/vim-airline-themes')
    let g:airline_theme='solarized_flood'
" highlight the region just yanked
call minpac#add('machakann/vim-highlightedyank')
" asynchronous completion framework for neovim (experimental)
call minpac#add('Shougo/deoplete.nvim')
" Dictionary source
call minpac#add('deoplete-plugins/deoplete-dictionary')
    let g:deoplete#enable_at_startup = 1
" for selecting forms. (scheme)
call minpac#add('guns/vim-sexp')
" binding for above
call minpac#add('tpope/vim-sexp-mappings-for-regular-people')
" repeat custom
call minpac#add('tpope/vim-repeat')
" all the surround related operator
call minpac#add('tpope/vim-surround')
" upstream addition for netrw
call minpac#add('tpope/vim-vinegar')
" git management
call minpac#add('tpope/vim-fugitive')
" easy abbreviate, search. mainly to cope with
" small diverges.
call minpac#add('tpope/vim-abolish')
" for writing vim plugin but don't know well
call minpac#add('tpope/vim-scriptease', {'type': 'opt'})
" minimal package management plugin
call minpac#add('k-takata/minpac', {'type': 'opt'})
" My plugin on develop
call minpac#add('hyunggyujang/vim-potion', {'type': 'opt'})
" Colorscheme
" call minpac#add('junegunn/seoul256.vim', {'type': 'opt'})
call minpac#add('lifepillar/vim-solarized8', {'type': 'opt'})
    colorscheme solarized8_high
" call minpac#add('sjl/badwolf', {'type': 'opt'})

" calling minpac's functions which are called often
command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()

" Modern Vim {{{2
" Chapter 3: fuzzy finder
" nnoremap <C-p> :<C-u>FZF<CR>
" Chapter 5
" Terminal {{{2
if has('nvim')
        " seamless mapping with other modes
	tnoremap <Esc> <C-\><C-n> 
        " analogous to <Esc> () in normal mode
	tnoremap <M-[> <Esc>
        " analogous to <C-r> in insert mode (put the contents of register)
	tnoremap <expr> <M-r> '<C-\><C-n>"' . nr2char(getchar()) . 'pi'
endif
" Window manuver {{{3
" easy window manipulate exploit
" meta key
" conflict with vim-sexp
" inoremap <M-h> <esc><c-w>h 
" inoremap <M-j> <esc><c-w>j 
" inoremap <M-k> <esc><c-w>k 
" inoremap <M-l> <esc><c-w>l 
" noremap <M-h> <c-w>h 
" noremap <M-j> <c-w>j 
" noremap <M-k> <c-w>k 
" noremap <M-l> <c-w>l 
" noremap <M-h> <c-w>h 
" noremap <M-j> <c-w>j 
" noremap <M-k> <c-w>k 
" noremap <M-l> <c-w>l 
" if has('nvim')
" 	tnoremap <M-h> <c-\><c-n><c-w>h 
" 	tnoremap <M-j> <c-\><c-n><c-w>j 
" 	tnoremap <M-k> <c-\><c-n><c-w>k 
" 	tnoremap <M-l> <c-\><c-n><c-w>l
" endif
noremap <leader>w <c-w>

" Linting{{{2
" ↓ we can navigate with location list such as ]l [l
" while the loclist doesn't count current cursor position,
" ale's navigate motion take account current cursor position!
" it's worth to be set
" ↓ I'd like to use these mappings for window nevigation
" nmap <silent> [W <Plug>(ale_first)
" nmap <silent> [w <Plug>(ale_previous)
" nmap <silent> ]w <Plug>(ale_next)
" nmap <silent> ]W <Plug>(ale_last)

" Search {{{2
" grepper {{{3
" Search for the current word
" nnoremap <Leader>* :Grepper -cword -noprompt<CR>

" Search for the current selection
" ↓ don't work expected as Modern Vim p61
" fixed in Configuartion
" nmap gs <plug>(GrepperOperator)
" xmap gs <plug>(GrepperOperator)
" }}}1
" Preferences {{{1
" setting default language as english
language en_US
" from Steve Losh <https://bitbucket.org/sjl/dotfiles/src/default/vim/vimrc>
" for the appearance on list option
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set showbreak=↪
set tabstop=8 softtabstop=4 shiftwidth=4 expandtab

" make mouse available
set mouse=a
" okay with move to other buffers without saveing
" set hidden

" open help window in vertical split
augroup helpwindow
    au!
    au FileType help wincmd L
augroup END

" Modern Vim {{{2
" Chapter 5
" Terminal {{{3
" if has('nvim')
	" highlight! TermCursorNC ctermbg=Magenta
" endif

" Chapter 6
" Tip 24
" track the undo like version control
" it can reverted after quit vim
set undofile

" disable undo file for temporary files
augroup vimrc
	autocmd!
	autocmd BufWritePre /tmp/* setlocal noundofile 
augroup END

" Vimcast suggestion {{{2
" livefeedback for substitution command
set inccommand=nosplit

" }}}1
" Alias {{{1
" Command line mode {{{2
" Search
" From Modern Vim p59
" How is it works? 
" 1. it works as macro. evaluation the text.
" 2. Guess : the <expr> token means the right should be evaluated as Vimscript
" expression rather than normal text.
" 3. the expression means: (as pseudo python code)
" if cmdtype = ':' and typed cmdline == input:
" 	return output
" else:
" 	return input
"
" See :h <expr>
function! SetupCommandAlias(input, output)
	exec 'cabbrev <expr> '.a:input
				\ .' ((getcmdtype() is# ":" && getcmdline() is# "'.a:input.'")'
				\ .'? ("'.a:output.'") : ("'.a:input.'"))'
endfunction
" call SetupCommandAlias("grep", "GrepperGrep")
" }}}1
