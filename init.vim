" Configuration {{{1
" Key {{{2
" Leader key convention 
" cons: hard to reverse f{motion}
let mapleader = ','
let maplocalleader = ','

" Search {{{2
" grepper {{{3
let g:grepper		= {}
let g:grepper.tools	= ['grep', 'git', 'rg']
" from :h grepper-operator
" runtime plugin/grepper.vim  " initialize g:grepper with default values
" let g:grepper.operator.prompt = 1
" ↑ doesn't work: There are no dictionary key such as operator.prompt
" fix ↓
if !exists('g:grepper.operator')
	let g:grepper.operator = {}
endif
let g:grepper.operator.prompt = 1
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
noremap <expr> <leader>c (synIDattr(synID(line("."), col("."), 0), "name") =~ 'comment\c') ?
\ ':<S-Left>exe "<S-Right>normal! "."^".len(b:commentCommand[1:])."x"<CR>' :
\ ':<S-Left>exe "<S-Right>normal! ".b:commentCommand<CR>'
" }}}1
" Package managing {{{1
packadd minpac

call minpac#init()
" by Modern vim {{{2
call minpac#add('tpope/vim-unimpaired')
call minpac#add('tpope/vim-projectionist', {'type': 'opt'})
" fuzzy finder
call minpac#add('junegunn/fzf')
" " fzf for vim
" call minpac#add('junegunn/fzf.vim')
" " Mapping selecting mappings
" nmap <leader><tab> <plug>(fzf-maps-n)
" xmap <leader><tab> <plug>(fzf-maps-x)
" omap <leader><tab> <plug>(fzf-maps-o)
" 
" " Insert mode completion
" imap <c-x><c-k> <plug>(fzf-complete-word)
" imap <c-x><c-f> <plug>(fzf-complete-path)
" imap <c-x><c-j> <plug>(fzf-complete-file-ag)
" imap <c-x><c-l> <plug>(fzf-complete-line)
" Modern vim chapter 4
" short for asynchronous linting engine
call minpac#add('w0rp/ale')
" asyncronous compile wrapper
call minpac#add('tpope/vim-dispatch')
" grep-like program wrapper
call minpac#add('mhinz/vim-grepper')
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
    colorscheme solarized8
" call minpac#add('sjl/badwolf', {'type': 'opt'})

" calling minpac's functions which are called often
command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()

" Modern Vim {{{2
" Chapter 3: fuzzy finder
nnoremap <C-p> :<C-u>FZF<CR>
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
inoremap <M-h> <esc><c-w>h 
inoremap <M-j> <esc><c-w>j 
inoremap <M-k> <esc><c-w>k 
inoremap <M-l> <esc><c-w>l 
noremap <M-h> <c-w>h 
noremap <M-j> <c-w>j 
noremap <M-k> <c-w>k 
noremap <M-l> <c-w>l 
noremap <M-h> <c-w>h 
noremap <M-j> <c-w>j 
noremap <M-k> <c-w>k 
noremap <M-l> <c-w>l 
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
call SetupCommandAlias("grep", "GrepperGrep")
" }}}1
