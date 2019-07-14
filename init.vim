" vim: foldmethod=marker
" Configuration {{{1
" Key {{{2
" Leader key convention 
" cons: hard to reverse f{motion}
let mapleader = ","
let maplocalleader = ","
" Search {{{2
" grepper {{{3
let g:grepper		= {}
let g:grepper.tools	= ['grep', 'git', 'rg']
" from :h grepper-operator
" runtime plugin/grepper.vim  " initialize g:grepper with default values
" let g:grepper.operator.prompt = 1
" ↑ doesn't work: There are no dictionary key such as operator.prompt
" fix ↓
if !exists("g:grepper.operator")
	let g:grepper.operator = {}
endif
let g:grepper.operator.prompt = 1
" Linting option {{{2
" For JavaScript files, use `eslint` (and only eslint)  {{{3
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'python': ['flake8'],
\ }

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

" Link ale with projectionist's meta data
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

" Package managing {{{1
packadd minpac

call minpac#init()
" by Modern vim {{{2
call minpac#add('tpope/vim-unimpaired')
call minpac#add('tpope/vim-projectionist', {'type': 'opt'})
call minpac#add('junegunn/fzf')
" Modern vim chapter 4
" short for asynchronous linting engine
call minpac#add('w0rp/ale')
call minpac#add('tpope/vim-dispatch')
call minpac#add('mhinz/vim-grepper')
" Tip 14
call minpac#add('janko-m/vim-test', {'type': 'opt'})
" Tip 23
call minpac#add('tpope/vim-obsession', {'type': 'opt'})
" Tip 27
call minpac#add('sgur/vim-editorconfig', {'type': 'opt'})
" by Practical Vim & vimcast {{{2
call minpac#add('nelstrom/vim-visual-star-search')
call minpac#add('machakann/vim-highlightedyank')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-abolish')
call minpac#add('tpope/vim-scriptease', {'type': 'opt'})
call minpac#add('k-takata/minpac', {'type': 'opt'})
" My plugin
call minpac#add('hyunggyujang/vim-potion', {'type': 'opt'})
" Colorscheme
" call minpac#add('junegunn/seoul256.vim', {'type': 'opt'})
" call minpac#add('lifepillar/vim-solarized8', {'type': 'opt'})
" call minpac#add('sjl/badwolf', {'type': 'opt'})
" For learning markdown
call minpac#add('sjl/learnvimscriptthehardway', {'type': 'opt'})

command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()

" Mappings {{{1
" Modern Vim {{{2
" Chapter 3
nnoremap <C-p> :<C-u>FZF<CR>
" Chapter 5
" Terminal {{{2
if has('nvim')
	tnoremap <Esc> <C-\><C-n> 
	tnoremap <M-]> <Esc>
	tnoremap <expr> <M-r> '<C-\><C-n>"' . nr2char(getchar()) . 'pi'
endif
" Window manuver {{{3
inoremap <M-h> <esc><c-w>h 
inoremap <M-j> <esc><c-w>j 
inoremap <M-k> <esc><c-w>k 
inoremap <M-l> <esc><c-w>l 
xnoremap <M-h> <c-w>h 
xnoremap <M-j> <c-w>j 
xnoremap <M-k> <c-w>k 
xnoremap <M-l> <c-w>l 
nnoremap <M-h> <c-w>h 
nnoremap <M-j> <c-w>j 
nnoremap <M-k> <c-w>k 
nnoremap <M-l> <c-w>l 
if has('nvim')
	tnoremap <M-h> <c-\><c-n><c-w>h 
	tnoremap <M-j> <c-\><c-n><c-w>j 
	tnoremap <M-k> <c-\><c-n><c-w>k 
	tnoremap <M-l> <c-\><c-n><c-w>l
endif
" Linting{{{2
" ↓ we can navigate with location list such as ]l [l
" while the loclist doesn't count current cursor position,
" ale's navigate motion take account current cursor position!
" it's worth to be set
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> ]w <Plug>(ale_next)
nmap <silent> ]W <Plug>(ale_last)

" Search {{{2
" grepper {{{3
" Search for the current word
nnoremap <Leader>* :Grepper -cword -noprompt<CR>

" Search for the current selection
" ↓ don't work expected as Modern Vim p61
" fixed in Configuartion
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)

" LearnVimscript The hard way {{{2
augroup LearnVimscript
	autocmd!
	autocmd FileType python setlocal nowrap nospell number
	autocmd FileType python nnoremap <buffer> <localleader>c I# <esc>
	autocmd FileType python xnoremap <buffer> <localleader>c :normal I# <esc>
	autocmd FileType javascript setlocal nowrap nospell number
	autocmd FileType javascript nnoremap <buffer> <localleader>c I// <esc>
	autocmd FileType javascript xnoremap <buffer> <localleader>c :normal I// <esc>
	autocmd FileType vim nnoremap <buffer> <localleader>c I" <esc>
	autocmd FileType vim xnoremap <buffer> <localleader>c :normal I" <esc>
augroup END

" Preferences {{{1

" from Steve Losh <https://bitbucket.org/sjl/dotfiles/src/default/vim/vimrc>
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set showbreak=↪
set tabstop=8 softtabstop=4 shiftwidth=4 expandtab
" colorscheme badwolf

" Modern Vim {{{2
" Chapter 5
" Terminal {{{3
if has('nvim')
	highlight! TermCursorNC ctermbg=Magenta
endif

" Chapter 6
" Tip 24
set undofile
if !has('nvim')
	set undodir=~/.vim/undo 
endif
" disable undo file for temporary files
augroup vimrc
	autocmd!
	autocmd BufWritePre /tmp/* setlocal noundofile 
augroup END

" Vimcast suggestion {{{2
set inccommand=nosplit
" Alias {{{1
" Command line mode {{{2
" Search
" From Modern Vim p59
function! SetupCommandAlias(input, output)
	exec 'cabbrev <expr> '.a:input
				\ .' ((getcmdtype() is# ":" && getcmdline() is# "'.a:input.'")'
				\ .'? ("'.a:output.'") : ("'.a:input.'"))'
endfunction
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
call SetupCommandAlias("grep", "GrepperGrep")

