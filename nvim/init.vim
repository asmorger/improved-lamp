" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin(stdpath('data') . '/plugged')

" Search recursively downward from CWD; provides TAB completion for filenames
" e.g., `:find vim* <TAB>`
set path+=**

" reload files changed outside of Vim not currently modified in Vim (needs below)
set autoread

" http://stackoverflow.com/questions/2490227/how-does-vims-autoread-work#20418591
au FocusGained,BufEnter * :silent! !

" line numbers and distances
set relativenumber 
set number 

" number of lines offset when jumping
set scrolloff=2

" Indent new line the same as the preceding line
set autoindent

" statusline indicates insert or normal mode
set showmode showcmd

" make scrolling and painting fast
" ttyfast kept for vim compatibility but not needed for nvim
set ttyfast lazyredraw

" highlight matching parens, braces, brackets, etc
set showmatch

" http://vim.wikia.com/wiki/Set_working_directory_to_the_current_file
set autochdir

set clipboard^=unnamed,unnamedplus

" Plugins, syntax, and colors
" ---------------------------------------------------------------------------
" vim-plug
" https://github.com/junegunn/vim-plug
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'

" C# support
Plug 'OmniSharp/omnisharp-vim'

" Go support
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" NerdTree
Plug 'preservim/nerdtree'
Plug 'ryanoasis/vim-devicons'

" Lightline
Plug 'itchyny/lightline.vim'

 " Gruvbox
Plug 'morhetz/gruvbox'
Plug 'sainnhe/gruvbox-material'

 " Startify
Plug 'mhinz/vim-startify'

Plug 'folke/which-key.nvim'
Plug 'tribela/vim-transparent'

" Hop
Plug 'phaazon/hop.nvim'

" Lightbulb
Plug 'kosayoda/nvim-lightbulb'

" F# Support

" Vale
Plug 'jose-elias-alvarez/null-ls.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'folke/trouble.nvim'
Plug 'nvim-lua/plenary.nvim'

Plug 'easymotion/vim-easymotion'

" Initialize plugin system
call plug#end()

syntax enable
" Neovim only
set termguicolors

let g:ale_linters = {
\ 'cs': ['OmniSharp']
\}

let g:OmniSharp_selector_ui = 'fzf'
let g:OmniSharp_selector_findusages = 'fzf'

" let g:gruvbox_contrast_dark='medium'
" colorscheme gruvbox
set background=dark
let g:gruvbox_material_background = 'medium'
colorscheme gruvbox-material

let g:lightline = {}
let g:lightline.colorscheme = 'gruvbox_material'

" F#
" autocmd BufNewFile,BufRead *.fs,*.fsx,*.fsi set filetype=fsharp


" Key mappings

nnoremap <SPACE> <Nop>
let mapleader=" "
inoremap ;; <Esc>

" NERDTree configuration
" Toggle NERDTree
map <C-o> :NERDTreeToggle<CR>

" OMNISharp configuration
nnoremap <C-o><C-u> :OmniSharpFindUsages<CR>
nnoremap <C-o><C-d> :OmniSharpGotoDefinition<CR>
nnoremap <C-o><C-d><C-p> :OmniSharpPreviewDefinition<CR>
nnoremap <C-o><C-r> !dotnet run

" EasyMotion
let g:EasyMotion_do_mapping = 0 " Disable default mappings

" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
" map  <Leader>w <Plug>(easymotion-bd-w)
map <Leader>w <Plug>(easymotion-overwin-w)

" Turn on case-insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)


" Start NERDTree when Vim starts with a directory argument.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in') |
   \ execute 'NERDTree' argv()[0] | wincmd p | enew | execute 'cd '.argv()[0] | endif

" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif

" If another buffer tries to replace NERDTree, put it in the other window, and bring back NERDTree.
autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~ 'NERD_tree_\d\+' && winnr('$') > 1 |
    \ let buf=bufnr() | buffer# | execute "normal! \<C-W>w" | execute 'buffer'.buf | endif

" Startify configuration

" returns all modified files of the current git repo
" `2>/dev/null` makes the command fail quietly, so that when we are not
" in a git repo, the list will be empty
function! s:gitModified()
    let files = systemlist('git ls-files -m 2>/dev/null')
    return map(files, "{'line': v:val, 'path': v:val}")
endfunction

" same as above, but show untracked files, honouring .gitignore
function! s:gitUntracked()
    let files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
    return map(files, "{'line': v:val, 'path': v:val}")
endfunction

let g:startify_lists = [
        \ { 'type': 'files',     'header': ['   MRU']            },
        \ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
        \ { 'type': 'sessions',  'header': ['   Sessions']       },
        \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
        \ { 'type': function('s:gitModified'),  'header': ['   git modified']},
        \ { 'type': function('s:gitUntracked'), 'header': ['   git untracked']},
        \ { 'type': 'commands',  'header': ['   Commands']       },
        \ ]

lua << EOF
  require("which-key").setup {
    -- youre configuration here, or leave empty for default configuration
  }
  require("trouble").setup {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  }
  require("null-ls").setup({
    sources = {
        require("null-ls").builtins.diagnostics.vale,
    },
  })
EOF
