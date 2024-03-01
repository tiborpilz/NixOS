call plug#begin('~/.config/nvim/plugged')

" Copilot
Plug 'github/copilot.vim'

" Dashboard
Plug 'glepnir/dashboard-nvim'
let g:dashboard_default_executive = 'telescope'

" telescope
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Icons
Plug 'ryanoasis/vim-devicons'
Plug 'kyazdani42/nvim-web-devicons'

" NERDtree
Plug 'preservim/nerdtree'
nnoremap <leader>n :NERDTreeToggle<CR>

" Fuzzy Finder
Plug 'kien/ctrlp.vim'
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" Which-Key
Plug 'liuchengxu/vim-which-key'
Plug 'AckslD/nvim-whichkey-setup.lua'

" Git
Plug 'tpope/vim-fugitive'

" Folds
Plug 'scr1pt0r/crease.vim'

" Statusline
if !exists('g:started_by_firenvim')
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  let g:airline_left_sep = ''
  let g:airline_right_sep = ''
  let g:airline_theme = 'base16'
endif

" comments
Plug 'tpope/vim-commentary'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" planning/organization
Plug 'vimwiki/vimwiki'

" Languages
"" Terraform
Plug 'hashivim/vim-terraform'

"" Nix
Plug 'LnL7/vim-nix'

" LSP
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'
Plug 'neovim/nvim-lspconfig'

" Plug 'jose-elias-alvarez/null-ls.nvim'
" Plug 'folke/lsp-colors.nvim'
" Plug 'simrat39/symbols-outline.nvim'
" Plug 'folke/trouble.nvim'
" nnoremap <leader>o <cmd>SymbolsOutline <cr>
" Tests
Plug 'vim-test/vim-test'
nmap <silent> <leader>tt :TestNearest<CR>
nmap <silent> <leader>tT :TestFile<CR>
nmap <silent> <leader>ta :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tg :TestVisit<CR>

" Repl
Plug 'hkupty/iron.nvim'

Plug 'kosayoda/nvim-lightbulb'
autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()

Plug 'michaelb/sniprun', {'do': 'bash install.sh'}

" kubernetes
Plug 'rottencandy/vimkubectl'

" Plug 'SirVer/ultisnips'

" Colorschemes
Plug 'eddyekofo94/gruvbox-flat.nvim'
Plug 'marko-cerovac/material.nvim'
Plug 'kdheepak/monochrome.nvim'
Plug 'EdenEast/nightfox.nvim'
Plug 'RRethy/nvim-base16'
Plug 'mcchrish/zenbones.nvim'
Plug 'rktjmp/lush.nvim'

" Autocompletion
Plug 'hrsh7th/nvim-compe'

" Signatures
Plug 'ray-x/lsp_signature.nvim'

" Markdown preview
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && npx --yes yarn install' }

" Neorg - Org in Neovim
Plug 'nvim-neorg/neorg'
Plug 'nvim-lua/plenary.nvim'

set completeopt=menuone,noselect
let g:compe = {}
let g:compe.enabled = v:true
let g:compe.autocomplete = v:true
let g:compe.debug = v:false
let g:compe.min_length = 0
let g:compe.preselect = 'enable'
let g:compe.throttle_time = 80
let g:compe.source_timeout = 200
let g:compe.resolve_timeout = 800
let g:compe.incomplete_delay = 400
let g:compe.max_abbr_width = 100
let g:compe.max_kind_width = 100
let g:compe.max_menu_width = 100
let g:compe.documentation = v:true

let g:compe.source = {}
let g:compe.source.path = v:true
let g:compe.source.buffer = v:true
let g:compe.source.calc = v:true
let g:compe.source.nvim_lsp = v:true
let g:compe.source.nvim_lua = v:true
let g:compe.source.vsnip = v:true
let g:compe.source.ultisnips = v:true
let g:compe.source.luasnip = v:true
let g:compe.source.emoji = v:true

call plug#end()

" let g:nightfox_style = 'nordfox'
colorscheme nightfox

" LSP settings
lua require('lsp-config')

" Mason settings
lua require('mason-setup')

" Diagnostics Settings
lua require('diagnostics')

" Treesitter settings
lua require('treesitter')
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()

" LSP Signature setup
lua require('lsp-signature')

" Iron repl config
lua require('iron-config')

" Neorg setup
lua require('neorg-setup')

" Dashboard settings
let g:dashboard_custom_header = [
\ ' ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗',
\ ' ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║',
\ ' ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║',
\ ' ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║',
\ ' ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║',
\ ' ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝',
\]
