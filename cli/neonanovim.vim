" For pasting into `~/.config/nvim/init.nvim`
" mkdir ~/.config/nvim/ && vi ~/.config/nvim/init.vim

set ai nocp digraph hid ru sc vb wmnu nu rnu noeb noet nosol showmatch
set bs=2 fo=cqrt ls=2 shm=at tw=72 ww=<,>,h,l,[,]
set comments=b:#,:%,n:>
set list listchars=tab:»·,trail:·
set undofile

nnoremap ; l
nnoremap l h

nnoremap d "_d
nnoremap D "_D

set viminfo=%,'50,\"100,:100,n~/.viminfo
