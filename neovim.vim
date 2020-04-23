
filetype plugin indent on

set nowrap
set textwidth=100
set hidden

command! -bang -nargs=* FindInFiles call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --glob "!.git/*" --follow --color "always" '.shellescape(<q-args>), 1, <bang>0)

" Tab and editor settings
set expandtab

" Searching
set ignorecase
set smartcase

" Use <leader>q to turn off highlighting
nnoremap <leader>q :noh<CR>

" Use <leader>k to kill buffer but not split
nnoremap <leader>k :bp\|bd #<CR>

" Visual stuff
set showmatch " Show matching brackets
set number " always show linenumbers

" I like to spell correctly
set spelllang=en_us

" show whitespace
set listchars=space:·,tab:→\ 
set list

" global mappings
nmap <leader>l :set list!<CR>

" Use space to get into command mode
noremap <Space> :

" search in files
nnoremap <leader>f :FindInFiles <c-r><c-w><cr>
nnoremap <leader>F :FindInFiles<space>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
        set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
        syntax on
        set hlsearch

        if filereadable(expand("~/.vimrc_background"))
                let base16colorspace=256
                source ~/.vimrc_background
        endif
endif


" ---------------
" Mouse
" ---------------

" --------------
" statusline
" --------------
set statusline=
set statusline+=%#CursorColumn#
set statusline+=🌵\ %{FugitiveHead(8)}
set statusline+=%#Title#
set statusline+=\ 📝\ %f
set statusline+=%m
set statusline+=%=
set statusline+=%#CursorColumn#
set statusline+=📣\ %y
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=[%{&fileformat}\]
set statusline+=\ 📏\ %l(%L)

" ----------------
" general settings
" ----------------
let mapleader = ","

set mousehide " Hide mouse after chars typed

" -------------
" key mappings
" -------------

" do not use arrow keys
inoremap  <Up>     <NOP>
inoremap  <Down>   <NOP>
inoremap  <Left>   <NOP>
inoremap  <Right>  <NOP>

" fzf
nnoremap <c-p> :Files<cr>
nnoremap <c-g> :GFiles<cr>

" diff shortcuts for fugitive
nnoremap <leader>2 :diffget //2<CR>
nnoremap <leader>3 :diffget //3<CR>
