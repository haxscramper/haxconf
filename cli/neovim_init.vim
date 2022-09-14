set autowrite
set ignorecase
set incsearch
set mouse=a
set nocompatible
set number
set showcmd
set showmatch
set showmode
set smartcase
set number relativenumber
set nohlsearch
set clipboard+=unnamedplus
call matchadd('ColorColumn', '\%81v', 100)
set undofile " Maintain undo history between sessions
set undodir=~/.config/hax-local/vim-undo
set inccommand=nosplit
set laststatus=2
set statusline=%F

"Highlight tabs/trailing whitespaces/nobreaking spaces
set listchars=tab:>~,nbsp:_,trail:.
set list

set whichwrap+=<,>,h,l,[,]

nnoremap ; l
nnoremap l h

set tabstop=4 shiftwidth=4 expandtab

"#= Make delete operators actually *DELETE* things
nnoremap d "_d
nnoremap D "_D
vnoremap d "_d

let mapleader = "\<Space>"

"Can exit without releasing shift key
command W w
command WQ wq
command Wq wq

"nnoremap <leader>d ""d
"nnoremap <leader>D ""D
"vnoremap <leader>d ""d

nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
noremap r <C-R>

"#== FZF
set rtp+=/usr/bin/fzf
nnoremap <C-p> :Files<Cr>

" https://vi.stackexchange.com/questions/3455/blocking-changes-to-read-only-file
" Don't allow editing of read only files
autocmd BufRead * let &l:modifiable = !&readonly

lua require('plugins')
